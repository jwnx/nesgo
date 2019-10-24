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

