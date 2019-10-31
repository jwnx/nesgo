package nesgo

// Cycles that a step function took
type Cycles = uint

// NES represents a virtual NES console, with mapper 0
type NES struct {
	CPU        *CPU
	PPU        *PPU
}

// NewNES returns a virtual NES console
func NewNES(cartridge *Cartridge) *NES {
	ppu := NewPPU(cartridge.CHR, cartridge.Mode)
	nes := NES{
		CPU:        NewCPU(cartridge.PRG, &ppu.PPURegisters),
		PPU:        ppu,
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
