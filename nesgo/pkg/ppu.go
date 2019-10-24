package nesgo

import (
	"log"
)

// Reference https://wiki.nesdev.com/w/index.php/Mirroring#Nametable_Mirroring

type nameTable struct {
	data [2048]byte
	mode Mirroring
}

func (n *nameTable) mirrored(table uint16) uint16 {
	if n.mode.Horizontal() {
		// Table 1 maps to 0 and table 3 maps to 2
		return ([...]uint16{0, 0, 1, 1})[table]
	}
	if n.mode.Vertical() {
		// Table 2 maps to 0 and table 3 maps to 1
		return ([...]uint16{0, 1, 0, 1})[table]
	}
	log.Fatalf("Unknown mirroring mode %d", n.mode)
	return 0 // Unreachable
}

func (n *nameTable) adjust(addr Address) Address {
	// $3000-$3EFF are mirrors of $2000-$2EFF
	const tableSize = 0x400
	addr = (addr - 0x2000) % (tableSize * 4)
	table := uint16(addr) / tableSize
	offset := addr % tableSize
	return Address(n.mirrored(table)*tableSize) + offset
}

func (n *nameTable) read(addr Address) byte {
	return n.data[n.adjust(addr)]
}

func (n *nameTable) write(addr Address, value byte) {
	n.data[n.adjust(addr)] = value
}

// Reference https://wiki.nesdev.com/w/index.php/PPU_palettes

type pallete [32]byte

func (*pallete) adjust(addr Address) Address {
	// $3F20-$3FFF are mirrors of $3F00-$3F1F
	addr = addr % 32
	// $3F10/$3F14/$3F18/$3F1C are mirrors of $3F00/$3F04/$3F08/$3F0C,
	// containing the universal background color.
	if addr == 16 {
		return 0
	}
	return addr
}

func (p *pallete) read(addr Address) byte {
	return p[p.adjust(addr)]
}

func (p *pallete) write(addr Address, value byte) {
	p[p.adjust(addr)] = value
}

// Reference https://wiki.nesdev.com/w/index.php/PPU_memory_map

type ppuMemory struct {
	rom       CHR
	nameTable nameTable
	pallete   pallete
}

func (mem *ppuMemory) Read(addr Address) byte {
	switch {
	case addr < 0x2000: // The 8KB of ROM
		return mem.rom[addr]
	case addr < 0x3F00:
		return mem.nameTable.read(addr)
	case addr < 0x4000:
		return mem.pallete.read(addr)
	default:
		log.Fatalf("PPU memory read out-of-bounds at %s", addr)
		return 0 // Unreachable
	}
}

func (mem *ppuMemory) Write(addr Address, value byte) {
	switch {
	case addr < 0x2000:
		mem.rom[addr] = value
	case addr < 0x3F00:
		mem.nameTable.write(addr, value)
	case addr < 0x4000:
		mem.pallete.write(addr, value)
	default:
		log.Fatalf("PPU memory write out-of-bounds at %s", addr)
	}
}

// References:
//   https://wiki.nesdev.com/w/index.php/PPU_registers
//   https://wiki.nesdev.com/w/index.php/PPU_frame_timing
// The 15 bit registers t and v are composed this way during rendering:
//
// yyy NN YYYYY XXXXX
// ||| || ||||| +++++-- coarse X scroll
// ||| || +++++-------- coarse Y scroll
// ||| ++-------------- nametable select
// +++----------------- fine Y scroll
type internalRegisters struct {
	// Current VRAM address (15 bits), set by PPUADDR.
	v uint16
	// Temporary VRAM address (15 bits); can also be thought of
	// as the address of the top left onscreen tile.
	t uint16
	// Fine X scroll (3 bits)
	x byte
	// First or second write toggle (1 bit)
	w byte
	// Even/odd flag that is toggled every frame
	f byte
}

// The scroll register is used to change the scroll position, that is,
// to tell the PPU which pixel of the nametable selected through
// PPUCTRL should be at the top left corner of the rendered screen.
// Reference: https://wiki.nesdev.com/w/index.php/PPU_scrolling
func (internal *internalRegisters) writeScroll(value byte) {
	if internal.w == 0 {
		// First write to the register sets the X position
		// t: ........ ...HGFED = d: HGFED...
		// x:               CBA = d: .....CBA
		// w:                   = 1
		internal.t = (internal.t & 0xFFE0) | (uint16(value) >> 3)
		internal.x = value & 0x07
		internal.w = 1
	} else {
		// Second write sets the Y position
		// t: .CBA..HG FED..... = d: HGFEDCBA
		// w:                   = 0
		internal.t = (internal.t & 0x8FFF) | ((uint16(value) & 0x07) << 12)
		internal.t = (internal.t & 0xFC1F) | ((uint16(value) & 0xF8) << 2)
		internal.w = 0
	}
}

// Because the CPU and the PPU are on separate buses, neither
// has direct access to the other's memory. The CPU writes to
// VRAM through a pair of registers on the PPU. First it loads
// an address into PPUADDR, and then it writes repeatedly to
// PPUDATA to fill VRAM.
func (internal *internalRegisters) writeAddr(value byte) {
	if internal.w == 0 {
		// t: ..FEDCBA ........ = d: ..FEDCBA
		// t: .X...... ........ = 0
		// w:                   = 1
		internal.t = (internal.t & 0x80FF) | ((uint16(value) & 0x3F) << 8)
		internal.w = 1
	} else {
		// t: ........ HGFEDCBA = d: HGFEDCBA
		// v                    = t
		// w:                   = 0
		internal.t = (internal.t & 0xFF00) | uint16(value)
		internal.v = internal.t
		internal.w = 0
	}
}

func (internal *internalRegisters) incrementV(across bool) {
	if across {
		internal.v++
	} else { // down
		internal.v += 32
	}
}

// Reference: https://wiki.nesdev.com/w/index.php/PPU_scrolling#Coarse_X_increment
func (internal *internalRegisters) incrementX() {
	if internal.v&0x1F == 0x1F { // if coarse X == 31
		internal.v &= 0xFFE0 // coarse X = 0
		internal.v ^= 0x0400 // switch horizontal nametable
	} else {
		internal.v++ // increment coarse X
	}
}

// Reference: https://wiki.nesdev.com/w/index.php/PPU_scrolling#Y_increment
func (internal *internalRegisters) incrementY() {
	// Fine Y indexes inside a tile, and coard Y selects the tile
	if internal.v&0x7000 != 0x7000 { // if fine Y < 7
		internal.v += 0x1000 // increment fine Y
	} else {
		internal.v &= 0x8FFF            // fine Y = 0
		y := (internal.v & 0x03E0) >> 5 // let y = coarse Y
		if y == 29 {
			y = 0                // coarse Y = 0
			internal.v ^= 0x0800 // switch vertical nametable
		} else if y == 31 {
			y = 0 // coarse Y = 0, nametable not switched
		} else {
			y++ // increment coarse Y
		}
		internal.v = (internal.v & 0xFC1F) | (y << 5) // put coarse Y back into v
	}
}

// Reference: https://wiki.nesdev.com/w/index.php/PPU_scrolling#At_dot_257_of_each_scanline
func (internal *internalRegisters) copyX() {
	// v: .....F.. ...EDCBA = t: .....F.. ...EDCBA
	internal.v = (internal.v & 0xFBE0) | (internal.t & 0x041F)
}

// Reference: https://wiki.nesdev.com/w/index.php/PPU_scrolling#During_dots_280_to_304_of_the_pre-render_scanline_.28end_of_vblank.29
func (internal *internalRegisters) copyY() {
	// v: .IHGF.ED CBA..... = t: .IHGF.ED CBA.....
	internal.v = (internal.v & 0x841F) | (internal.t & 0x7BE0)
}

// 7  bit  0
// ---- ----
// |||| ||||
// |||| ||++- Base nametable address
// |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
// |||| |+--- VRAM address increment per CPU read/write of PPUDATA
// |||| |     (0: add 1, going across; 1: add 32, going down)
// |||| +---- Sprite pattern table address for 8x8 sprites
// ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
// |||+------ Background pattern table address (0: $0000; 1: $1000)
// ||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels)
// |+-------- PPU master/slave select
// |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
// +--------- Generate an NMI at the start of the
//            vertical blanking interval (0: off; 1: on)
// Equivalently, bits 1 and 0 are the most significant bit of the scrolling coordinates.
// 7  bit  0
// ---- ----
// .... ..YX
// 	      ||
//        |+- 1: Add 256 to the X scroll position
//        +-- 1: Add 240 to the Y scroll position
type ppuctrl struct {
	nameTable       byte
	increment       byte
	spriteTable     byte
	backgroundTable byte
	spriteSize      byte
	masterSlave     byte
	generateNMI     bool
}

func (ctrl *ppuctrl) write(value byte, internal *internalRegisters) {
	*ctrl = ppuctrl{
		nameTable:       value & 3,
		increment:       (value >> 2) & 1,
		spriteTable:     (value >> 3) & 1,
		backgroundTable: (value >> 4) & 1,
		spriteSize:      (value >> 5) & 1,
		masterSlave:     (value >> 6) & 1,
		generateNMI:     ((value >> 7) & 1) == 1,
	}
	// t: ....BA.. ........ = d: ......BA
	internal.t = (internal.t & 0xF3FF) | (uint16(value) & 0x03 << 10)
}

func (ctrl *ppuctrl) spriteHeight() uint {
	if ctrl.spriteSize == 0 {
		return 8
	}
	return 16
}

