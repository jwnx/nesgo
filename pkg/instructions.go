package nesgo

func isNegative(reg byte) bool {
	return (reg & 0x80) != 0
}

// Reference: https://www.masswerk.at/6502/6502_instruction_set.html

// AddressingMode represents an instruction's mode, which can be:
//
// A		....	Accumulator	 		OPC A	 	operand is AC (implied single byte instruction)
// abs		....	absolute	 		OPC $LLHH	operand is address $HHLL *
// abs,X	....	absolute, X-indexed	OPC $LLHH,X	operand is address; effective address is address incremented by X with carry
// abs,Y	....	absolute, Y-indexed	OPC $LLHH,Y	operand is address; effective address is address incremented by Y with carry
// #		....	immediate	 		OPC #$BB	operand is byte BB
// impl		....	implied	 			OPC	 		operand implied
// ind		....	indirect	 		OPC ($LLHH)	operand is address; effective address is contents of word at address: C.w($HHLL)
// X,ind	....	X-indexed, indirect	OPC ($LL,X)	operand is zeropage address; effective address is word in (LL + X, LL + X + 1), inc. without carry: C.w($00LL + X)
// ind,Y	....	indirect, Y-indexed	OPC ($LL),Y	operand is zeropage address; effective address is word in (LL, LL + 1) incremented by Y with carry: C.w($00LL) + Y
// rel		....	relative	 		OPC $BB	 	branch target is PC + signed offset BB
// zpg		....	zeropage	 		OPC $LL	 	operand is zeropage address (hi-byte is zero, address = $00LL)
// zpg,X	....	zeropage, X-indexed	OPC $LL,X	operand is zeropage address; effective address is address incremented by X without carry
// zpg,Y	....	zeropage, Y-indexed	OPC $LL,Y	operand is zeropage address; effective address is address incremented by Y without carry
type AddressingMode byte

const (
	_ AddressingMode = iota
	modeAccumulator
	modeAbsolute
	modeAbsoluteX
	modeAbsoluteY
	modeImmediate
	modeImplied
	modeIndirect
	modeIndexedIndirect
	modeIndirectIndexed
	modeRelative
	modeZeroPage
	modeZeroPageX
	modeZeroPageY
)

type pagesCrossed bool

func differentPages(addr1, addr2 Address) pagesCrossed {
	const pageSizeMask = 0xFF00 // Page size is 256 bytes
	return addr1&pageSizeMask != addr2&pageSizeMask
}

func (mode AddressingMode) resolve(mem Memory, regs *Registers) (Address, pagesCrossed) {
	var crossed pagesCrossed = false
	var addr Address
	switch mode {
	case modeAccumulator:
		return 0, false
	case modeAbsolute:
		addr = ReadAddress(mem, regs.PC)
	case modeAbsoluteX:
		baseAddr := ReadAddress(mem, regs.PC)
		addr = baseAddr + Address(regs.X)
		crossed = differentPages(baseAddr, addr)
	case modeAbsoluteY:
		baseAddr := ReadAddress(mem, regs.PC)
		addr = baseAddr + Address(regs.Y)
		crossed = differentPages(baseAddr, addr)
	case modeImmediate:
		addr = regs.PC
	case modeImplied:
		return 0, false
	case modeIndirect:
		addr = ReadAddress(mem, ReadAddress(mem, regs.PC))
	case modeIndexedIndirect:
		addr = ReadAddress(mem, ReadAddress(mem, regs.PC)+Address(regs.X))
	case modeIndirectIndexed:
		baseAddr := ReadAddress(mem, ReadAddress(mem, regs.PC))
		addr = baseAddr + Address(regs.Y)
		crossed = differentPages(baseAddr, addr)
	case modeRelative:
		offset := mem.Read(regs.PC)
		addr = regs.PC + Address(1+offset)
		if isNegative(offset) {
			addr -= 0x100
		}
	case modeZeroPage:
		addr = Address(mem.Read(regs.PC))
	case modeZeroPageX:
		addr = Address(mem.Read(regs.PC)+regs.X) & 0xff
	case modeZeroPageY:
		addr = Address(mem.Read(regs.PC)+regs.Y) & 0xff
	default:
		return 0, false
	}
	return addr, crossed
}

func (mode AddressingMode) String() string {
	switch mode {
	case modeAccumulator:
		return "A"
	case modeAbsolute:
		return "oper"
	case modeAbsoluteX:
		return "oper,X"
	case modeAbsoluteY:
		return "oper,Y"
	case modeImmediate:
		return "#oper"
	case modeImplied:
		return "impl"
	case modeIndirect:
		return "(oper)"
	case modeIndexedIndirect:
		return "(oper, x)"
	case modeIndirectIndexed:
		return "(oper), y"
	case modeRelative:
		return "rel"
	case modeZeroPage:
		return "zpg"
	case modeZeroPageX:
		return "zpg, X"
	case modeZeroPageY:
		return "zpg, Y"
	default:
		return "" // Unreachable
	}
}
