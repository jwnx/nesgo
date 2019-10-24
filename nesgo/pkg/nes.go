package nesgo

// Cycles that a step function took
type Cycles = uint

// NES represents a virtual NES console, with mapper 0
type NES struct {
	CPU        *CPU
}

// NewNES returns a virtual NES console
func NewNES(cartridge *Cartridge) *NES {
	nes := NES{
		CPU:        NewCPU(cartridge.PRG),
	}
	return &nes
}

// Reset triggers the CPU reset interrupt vector
func (nes *NES) Reset() {
	nes.CPU.Reset()
}

// Step advances the console by a CPU instruction
func (nes *NES) Step() Cycles {
	cpuCycles := nes.CPU.Step()
	return cpuCycles
}
