package nesgo

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
	rom        *PRG
	ram        []byte
	cycles     *Cycles
	stall      *Cycles
}

func (mem *cpuMemory) Read(addr Address) byte {
	switch {
	case addr < 0x2000:
		return mem.ram[addr%0x0800]
	case addr < 0x8000:
		return 0
	case addr >= 0x8000:
		return mem.rom.Read(addr)
	default:
		return 0 // Unreachable
	}
}

func (mem *cpuMemory) Write(addr Address, value byte) {
	switch {
	case addr < 0x2000:
		mem.ram[addr%0x0800] = value
	case addr < 0x8000:
	case addr >= 0x8000:
		mem.rom.data[addr] = value
	default:
	}
}

// CPUState contains CPU state that needs to be accessed externally
type CPUState struct {
	Cycles Cycles
	Stall  Cycles
}

// CPU contains the state to emulate the CPU unit
type CPU struct {
	Registers
	Flags
	cpuMemory
	Cycles    Cycles
	Stall     Cycles
	interrupt func()
}

// NewCPU returns a new CPU instance
func NewCPU(rom *PRG) *CPU {
	cpu := CPU{interrupt: func() {}}
	cpu.cpuMemory = cpuMemory{
		rom:        rom,
		ram:        make([]byte, 2048),
		cycles:     &cpu.Cycles,
		stall:      &cpu.Stall,
	}
	cpu.Reset()
	return &cpu
}

func (cpu *CPU) trigger(vector Address) {
	cpu.interrupt = func() {
		cpu.run(BRK)
		if vector != IrqVector {
			cpu.PC = ReadAddress(&cpu.cpuMemory, vector)
		}
		cpu.interrupt = func() {}
	}
}

// NMI triggers a non-maskable interrupt on the next cycle
func (cpu *CPU) NMI() {
	cpu.trigger(NmiVector)
}

// IRQ triggers an interrupt on the next cycle
func (cpu *CPU) IRQ() {
	if !cpu.I {
		cpu.trigger(IrqVector)
	}
}

func (cpu *CPU) run(opcode byte) {
	inst := Instructions[opcode]
	cpu.Cycles += inst.Run(&cpu.cpuMemory, &cpu.Flags, &cpu.Registers)
}

// Step advances the instruction stream
func (cpu *CPU) Step() Cycles {
	if cpu.Stall > 0 {
		cpu.Stall--
		return 1
	}
	cycles := cpu.Cycles
	cpu.interrupt()
	opcode := cpu.Read(cpu.PC)
	cpu.PC++
	cpu.run(opcode)
	return cpu.Cycles - cycles
}

// Reset prepares the initial CPU state
func (cpu *CPU) Reset() {
	cpu.PC = ReadAddress(cpu, ResetVector)
	cpu.SP = 0xFD
	cpu.Flags.unpack(0x24)
}
