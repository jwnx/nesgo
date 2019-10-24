package nesgo

import (
	"image"
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

// 7  bit
// ---- ----
// BGRs bMmG
// |||| ||||
// |||| |||+- Greyscale (0: normal color, 1: produce a greyscale display)
// |||| ||+-- 1: Show background in leftmost 8 pixels of screen, 0: Hide
// |||| |+--- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
// |||| +---- 1: Show background
// |||+------ 1: Show sprites
// ||+------- Emphasize red
// |+-------- Emphasize green
// +--------- Emphasize blue
type ppumask struct {
	greyscale          byte
	showLeftBackground bool
	showLeftSprites    bool
	showBackground     bool
	showSprites        bool
	emphasizeRed       bool
	emphasizeGreen     bool
	emphasizeBlue      bool
}

func (mask *ppumask) write(value byte) {
	*mask = ppumask{
		greyscale:          (value >> 0) & 1,
		showLeftBackground: ((value >> 1) & 1) == 1,
		showLeftSprites:    ((value >> 2) & 1) == 1,
		showBackground:     ((value >> 3) & 1) == 1,
		showSprites:        ((value >> 4) & 1) == 1,
		emphasizeRed:       ((value >> 5) & 1) == 1,
		emphasizeGreen:     ((value >> 6) & 1) == 1,
		emphasizeBlue:      ((value >> 7) & 1) == 1,
	}
}

// 7  bit  0
// ---- ----
// VSO. ....
// |||| ||||
// |||+-++++- Least significant bits previously written into a PPU register
// |||        (due to register not being updated for this address)
// ||+------- Sprite overflow. The intent was for this flag to be set
// ||         whenever more than eight sprites appear on a scanline, but a
// ||         hardware bug causes the actual behavior to be more complicated
// ||         and generate false positives as well as false negatives; see
// ||         PPU sprite evaluation. This flag is set during sprite
// ||         evaluation and cleared at dot 1 (the second dot) of the
// ||         pre-render line.
// |+-------- Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 overlaps
// |          a nonzero background pixel; cleared at dot 1 of the pre-render
// |          line.  Used for raster timing.
// +--------- Vertical blank has started (0: not in vblank; 1: in vblank).
//            Set at dot 1 of line 241 (the line *after* the post-render
//            line); cleared after reading $2002 and at dot 1 of the
//            pre-render line.
type ppustatus struct {
	spriteOverflow bool
	spiteZeroHit   bool
	vblankStarted  bool
}

func (status *ppustatus) read(lastRegisterWrite byte, internal *internalRegisters) byte {
	res := lastRegisterWrite & 0x1F
	if status.spriteOverflow {
		res |= 1 << 5
	}
	if status.spiteZeroHit {
		res |= 1 << 6
	}
	if status.vblankStarted {
		res |= 1 << 7
		status.vblankStarted = false
	}
	internal.w = 0
	return res
}

// VRAM read/write data register.
// After access, the video memory address will increment
// by an amount determined by bit 2 of ppuctrl.
type ppudata struct {
	buffer byte
}

func (data *ppudata) read(mem *ppuMemory, ctrl *ppuctrl, internal *internalRegisters) byte {
	value := mem.Read(Address(internal.v))
	if internal.v < 0x3F00 {
		// When reading while the VRAM address is in the range 0-$3EFF
		// (i.e., before the palettes), the read will return the contents
		// of an internal read buffer. This internal buffer is updated only
		// when reading PPUDATA, and so is preserved across frames. After
		// the CPU reads and gets the contents of the internal buffer, the
		// PPU will immediately update the internal buffer with the byte at
		// the current VRAM address.
		data.buffer, value = value, data.buffer
	} else {
		// Reading palette data from $3F00-$3FFF works differently. The
		// palette data is placed immediately on the data bus, and hence
		// no dummy read is required. Reading the palettes still updates
		// the internal buffer though, but the data placed in it is the
		// mirrored nametable data that would appear instead of the palette.
		data.buffer = mem.Read(Address(internal.v - 0x1000))
	}
	internal.incrementV(ctrl.increment == 0)
	return value
}

func (data *ppudata) write(mem *ppuMemory, ctrl *ppuctrl, internal *internalRegisters, value byte) {
	mem.Write(Address(internal.v), value)
	internal.incrementV(ctrl.increment == 0)
}

// PPURegisters contains PPU registers that are mapped to the
// CPU's address space.
type PPURegisters struct {
	internalRegisters
	ctrl       ppuctrl
	mask       ppumask
	status     ppustatus
	data       ppudata
	oamAddress byte
	oam        [256]byte
	lastWrite  byte
	ppuMemory
}

func (regs *PPURegisters) reset() {
	regs.ctrl.write(0, &regs.internalRegisters)
	regs.mask.write(0)
	regs.oamAddress = 0
}

// OAM DMA is connected to the CPU. Writing $XX will upload 256
// bytes of data from CPU page $XX00-$XXFF to the internal OAM.
func (regs *PPURegisters) writeDMA(cpu Memory, onStall func(Cycles), value byte) {
	addr := Address(value) << 8
	for i := 0; i < 256; i++ {
		regs.oam[regs.oamAddress] = cpu.Read(addr)
		regs.oamAddress++
		addr++
	}
	onStall(513)
}

// Write `value` at `addr`
func (regs *PPURegisters) Write(cpu Memory, onStall func(Cycles), addr Address, value byte) {
	regs.lastWrite = value
	switch addr {
	case 0x2000:
		regs.ctrl.write(value, &regs.internalRegisters)
	case 0x2001:
		regs.mask.write(value)
	case 0x2003:
		regs.oamAddress = value
	case 0x2004:
		regs.oam[regs.oamAddress] = value
		regs.oamAddress++
	case 0x2005:
		regs.writeScroll(value)
	case 0x2006:
		regs.writeAddr(value)
	case 0x2007:
		regs.data.write(&regs.ppuMemory, &regs.ctrl, &regs.internalRegisters, value)
	case 0x4014:
		regs.writeDMA(cpu, onStall, value)
	}
}

// Read the byte at `addr`
func (regs *PPURegisters) Read(addr Address) byte {
	switch addr {
	case 0x2002:
		return regs.status.read(regs.lastWrite, &regs.internalRegisters)
	case 0x2004:
		return regs.oam[regs.oamAddress]
	case 0x2007:
		return regs.data.read(&regs.ppuMemory, &regs.ctrl, &regs.internalRegisters)
	}
	return 0
}

// Reference: https://wiki.nesdev.com/w/index.php/PPU_OAM

type rawSprite struct {
	y     byte
	tile  byte
	attrs byte
	x     byte
}

func newRawSprite(regs *PPURegisters, index uint) rawSprite {
	return rawSprite{
		y:     regs.oam[index*4+0],
		tile:  regs.oam[index*4+1],
		attrs: regs.oam[index*4+2],
		x:     regs.oam[index*4+3],
	}
}

func (s rawSprite) shouldFlipHorizontally() bool {
	return s.attrs&0x40 == 0x40
}

func (s rawSprite) shouldFlipVertically() bool {
	return s.attrs&0x80 == 0x80
}

func (s rawSprite) pallete() byte {
	return s.attrs & 0x03
}

func (s rawSprite) priority() byte {
	return (s.attrs >> 5) & 1
}

// PPU contains the state to emulate the PPU unit
type PPU struct {
	PPURegisters

	Cycle    uint   // 0-340
	ScanLine uint   // pre-render (261), visible (0-239), post-render (240), vblank (241-260)
	Frame    uint64 // frame counter

	background uint64 // Background shift registers

	// Double buffering
	current *image.RGBA
	next    *image.RGBA
}

// NewPPU returns a new PPU instance
func NewPPU(rom CHR, mode Mirroring) *PPU {
	ppu := PPU{
		PPURegisters: PPURegisters{
			ppuMemory: ppuMemory{rom: rom, nameTable: nameTable{mode: mode}},
		},
		current: image.NewRGBA(image.Rect(0, 0, 256, 240)),
		next:    image.NewRGBA(image.Rect(0, 0, 256, 240)),
	}
	ppu.Reset()
	return &ppu
}

// Reset prepares the initial PPU state
func (ppu *PPU) Reset() {
	ppu.PPURegisters.reset()
	ppu.Cycle = 340
	ppu.ScanLine = 240
	ppu.Frame = 0
}

// Buffer returns the current buffer with the rendered frame
func (ppu *PPU) Buffer() *image.RGBA {
	return ppu.current
}

func (ppu *PPU) renderingEnabled() bool {
	return ppu.mask.showBackground || ppu.mask.showSprites
}

func (ppu *PPU) visibleLine() bool {
	return ppu.ScanLine < 240
}

func (ppu *PPU) visibleCycle() bool {
	return ppu.Cycle >= 1 && ppu.Cycle <= 256
}

func (ppu *PPU) preRenderLine() bool {
	return ppu.ScanLine == 261
}

func (ppu *PPU) fetchCycle() bool {
	return ppu.visibleCycle() || (ppu.Cycle >= 321 && ppu.Cycle <= 336)
}

