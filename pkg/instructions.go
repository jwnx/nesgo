package nesgo

import (
	"fmt"
	"os"
	"reflect"
	"runtime"
	"strings"
	"text/tabwriter"
)

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

// RunFunc executes the instruction and returns the effective cycles.
// The PC should point to after the instruction's opcode.
type RunFunc = func(Memory, *Flags, *Registers) byte

// Instruction contains information about a particular instruction,
// and knows how to execute itself.
type Instruction struct {
	Name   string
	OpCode byte
	Size   byte
	Cycles byte
	Mode   AddressingMode
	Run    RunFunc
}

// Instructions contains all the valid CPU instructions, indexed
// by their opcode.
var Instructions = [256]Instruction{}

//PrintInstructions displays the supported instructions.
func PrintInstructions() {
	w := tabwriter.NewWriter(os.Stdout, 0, 0, 1, ' ', 0)
	fmt.Fprintln(w, "assembler\topcode\tbytes\tcycles\t")
	fmt.Fprintln(w, "-------------\t------\t-----\t------\t")
	for _, inst := range Instructions {
		if inst.Name == "" {
			continue
		}
		fmt.Fprintf(w, "%s %s\t0x%02X\t%d\t%d\t\n",
			inst.Name, inst.Mode, inst.OpCode, inst.Size, inst.Cycles)
	}
	w.Flush()
}

type position struct {
	mode AddressingMode
	addr Address
	val  byte
	regs *Registers
	mem  Memory
}

func newPosition(mode AddressingMode, addr Address, regs *Registers, mem Memory) position {
	var val byte
	if mode == modeAccumulator {
		val = regs.A
	} else if addr != 0 {
		val = mem.Read(addr)
	}
	return position{mode, addr, val, regs, mem}
}

func (moa *position) address() Address {
	return moa.addr
}

func (moa *position) read() byte {
	return moa.val
}

func (moa *position) write(val byte) {
	if moa.mode == modeAccumulator {
		moa.regs.A = val
	} else {
		moa.mem.Write(moa.addr, val)
	}
}

func sameSign(a byte, b byte) bool {
	return a^b&0x80 == 0
}

type context struct {
	mem   Memory
	flags *Flags
	regs  *Registers
}

type runWithCtxFunc = func(position, context)

// The stack is a 256-byte array whose location is hardcoded at page
// $01 ($0100-$01FF), using the SP register for a stack pointer.
func (ctx context) push(val byte) {
	ctx.mem.Write(Address(0x100|uint16(ctx.regs.SP)), val)
	ctx.regs.SP--
}

func (ctx context) pop() byte {
	ctx.regs.SP++
	return ctx.mem.Read(Address(0x100 | uint16(ctx.regs.SP)))
}

func (ctx context) pushAddress(addr Address) {
	ctx.push(byte(addr >> 8))
	ctx.push(byte(addr & 0xFF))
}

func (ctx context) popAddress() Address {
	lo := uint16(ctx.pop())
	hi := uint16(ctx.pop())
	return Address(hi<<8 | lo)
}

func (ctx context) setZNFlags(val byte) {
	ctx.flags.Z = val == 0
	ctx.flags.N = isNegative(val)
}

func nameOfFunc(f runWithCtxFunc) string {
	funcName := runtime.FuncForPC(reflect.ValueOf(f).Pointer()).Name()
	components := strings.Split(funcName, ".")
	return components[len(components)-1]
}

func instr(
	runWithCtx runWithCtxFunc,
	opcode byte,
	size byte,
	cycles byte,
	mode AddressingMode) {
	Instructions[opcode] = Instruction{
		nameOfFunc(runWithCtx), opcode, size, cycles, mode,
		func(mem Memory, flags *Flags, regs *Registers) byte {
			addr, pageCrossed := mode.resolve(mem, regs)
			regs.PC += Address(size - 1)
			curPC := regs.PC
			runWithCtx(newPosition(mode, addr, regs, mem), context{mem, flags, regs})
			actualCycles := cycles
			if pageCrossed {
				actualCycles++
			}
			if mode == modeRelative && curPC != regs.PC {
				actualCycles++
				if differentPages(curPC, regs.PC) {
					actualCycles++
				}
			}
			return actualCycles
		},
	}
}

func init() {
	// <name> <opcode> <bytes> <cycles> <addressing mode>
}
