package nesgo

import (
	"image"
)

// Cycles that a step function took
type Cycles = uint

// NES represents a virtual NES console, with mapper 0
type NES struct {
	CPU        *CPU
	PPU        *PPU
	Controller *Controller
}

// NewNES returns a virtual NES console
func NewNES(cartridge *Cartridge) *NES {
	controller := NewController()
	ppu := NewPPU(cartridge.CHR, cartridge.Mode)
	nes := NES{
		CPU:        NewCPU(cartridge.PRG, &ppu.PPURegisters, controller),
		PPU:        ppu,
		Controller: controller,
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
	ppuCycles := cpuCycles * 3
	for i := Cycles(0); i < ppuCycles; i++ {
		nes.PPU.Step(nes.CPU)
	}
	return cpuCycles
}

// StepFrame advances the console to the next frame
func (nes *NES) StepFrame() Cycles {
	cpuCycles := Cycles(0)
	frame := nes.PPU.Frame
	for frame == nes.PPU.Frame {
		cpuCycles += nes.Step()
	}
	return cpuCycles
}

// Buffer returns the current buffer with the rendered frame
func (nes *NES) Buffer() *image.RGBA {
	return nes.PPU.Buffer()
}
