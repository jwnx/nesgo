package nesgo

import "log"

// Registers contains all of the CPU registers
type Registers struct {
	PC Address // Program counter
	SP byte    // Stack pointer
	A  byte    // Accumulator
	X  byte    // X index register
	Y  byte    // Y index register
}

// Flags is an architectural registers containing the status flags
// 7  bit  0
// ---- ----
// NVss DIZC
// |||| ||||
// |||| |||+- Carry
// |||| ||+-- Zero
// |||| |+--- Interrupt Disable
// |||| +---- Decimal
// ||++------ No CPU effect, see: the B flag
// |+-------- Overflow
// +--------- Negative
type Flags struct {
	C byte
	Z bool
	I bool
	D bool
	V bool
	N bool
}

func (f Flags) pack() byte {
	res := f.C
	if f.Z {
		res |= 1 << 1
	}
	if f.I {
		res |= 1 << 2
	}
	if f.D {
		res |= 1 << 3
	}
	if f.V {
		res |= 1 << 6
	}
	if f.N {
		res |= 1 << 7
	}
	return res
}
func (f Flags) unpack(val byte) {
	f.C = val & 1
	f.Z = val&(1<<1) != 0
	f.I = val&(1<<2) != 0
	f.D = val&(1<<3) != 0
	f.V = val&(1<<6) != 0
	f.N = val&(1<<7) != 0
}

// Reference: https://wiki.nesdev.com/w/index.php/CPU_memory_map

// Interrupt vectors
const (
	NmiVector   = Address(0xFFFA)
	ResetVector = Address(0xFFFC)
	IrqVector   = Address(0xFFFE)
)

type cpuMemory struct {
	rom PRG
	ram []byte
}

func (mem *cpuMemory) Read(addr Address) byte {
	switch {
	case addr < 0x2000:
		return mem.ram[addr%0x0800] // [0x0800, 0x1FFF] are mirrors of [0x0000, 0x07FF]
	case addr >= 0x6000:
		return mem.rom[addr]
	default:
		log.Fatalf("CPU memory read out-of-bounds at %s", addr)
		return 0 // Unreachable
	}
}

func (mem *cpuMemory) Write(addr Address, value byte) {
	switch {
	case addr < 0x2000:
		mem.ram[addr%0x0800] = value
	case addr >= 0x6000:
		mem.rom[addr] = value
	default:
		log.Fatalf("CPU memory write out-of-bounds at %s", addr)
	}
}
