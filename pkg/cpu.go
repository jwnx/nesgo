package nesgo

// Registers contains all of the CPU registers
type Registers struct {
	PC Address // Program counter
	SP byte    // Stack pointer
	A  byte    // Accumulator
	X  byte    // X index register
	Y  byte    // Y index register
}
