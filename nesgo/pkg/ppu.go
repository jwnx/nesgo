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

