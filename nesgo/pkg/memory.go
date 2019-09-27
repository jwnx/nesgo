package nesgo

import "fmt"

// Address is used to index the Memory.
type Address uint16

func (addr Address) String() string {
	return fmt.Sprintf("address{0x%04X}", uint16(addr))
}

// Memory can be read and written. There should exist
// concerete implementation for the CPU memory and the
// PPU memory.
type Memory interface {
	Read(addr Address) byte
	Write(addr Address, value byte)
}

// ReadAddress reads from two contiguous positions in memory,
// returning the result as a 16-bit number.
func ReadAddress(mem Memory, addr Address) Address {
	lo := uint16(mem.Read(addr))
	hi := uint16(mem.Read(addr + 1))
	return Address(hi<<8 | lo)
}
