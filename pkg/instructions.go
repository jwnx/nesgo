package nesgo

import (
	"fmt"
	"math"
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
	instr(adc, 0x69, 2, 2, modeImmediate)
	instr(adc, 0x65, 2, 3, modeZeroPage)
	instr(adc, 0x75, 2, 4, modeZeroPageX)
	instr(adc, 0x6D, 3, 4, modeAbsolute)
	instr(adc, 0x7D, 3, 4, modeAbsoluteX)
	instr(adc, 0x79, 3, 4, modeAbsoluteY)
	instr(adc, 0x61, 2, 6, modeIndexedIndirect)
	instr(adc, 0x71, 2, 5, modeIndirectIndexed)

	instr(and, 0x29, 2, 2, modeImmediate)
	instr(and, 0x25, 2, 3, modeZeroPage)
	instr(and, 0x35, 2, 4, modeZeroPageX)
	instr(and, 0x2D, 3, 4, modeAbsolute)
	instr(and, 0x3D, 3, 4, modeAbsoluteX)
	instr(and, 0x39, 3, 4, modeAbsoluteY)
	instr(and, 0x21, 2, 6, modeIndexedIndirect)
	instr(and, 0x31, 2, 5, modeIndirectIndexed)

	instr(asl, 0x0A, 1, 2, modeAccumulator)
	instr(asl, 0x06, 2, 5, modeZeroPage)
	instr(asl, 0x16, 2, 6, modeZeroPageX)
	instr(asl, 0x0E, 3, 6, modeAbsolute)
	instr(asl, 0x1E, 3, 7, modeAbsoluteX)

	instr(bcc, 0x90, 2, 2, modeRelative)

	instr(bcs, 0xB0, 2, 2, modeRelative)

	instr(beq, 0xF0, 2, 2, modeRelative)

	instr(bit, 0x24, 2, 3, modeZeroPage)
	instr(bit, 0x2C, 3, 4, modeAbsolute)

	instr(bmi, 0x30, 2, 2, modeRelative)

	instr(bne, 0xD0, 2, 2, modeRelative)

	instr(bpl, 0x10, 2, 2, modeRelative)

	instr(brk, 0x00, 1, 7, modeImplied)

	instr(bvc, 0x50, 2, 2, modeRelative)

	instr(bvs, 0x70, 2, 2, modeRelative)

	instr(clc, 0x18, 1, 2, modeImplied)

	instr(cld, 0xD8, 1, 2, modeImplied)

	instr(cli, 0x58, 1, 2, modeImplied)

	instr(clv, 0xB8, 1, 2, modeImplied)

	instr(cmp, 0xC9, 2, 2, modeImmediate)
	instr(cmp, 0xC5, 2, 3, modeZeroPage)
	instr(cmp, 0xD5, 2, 4, modeZeroPageX)
	instr(cmp, 0xCD, 3, 4, modeAbsolute)
	instr(cmp, 0xDD, 3, 4, modeAbsoluteX)
	instr(cmp, 0xD9, 3, 4, modeAbsoluteY)
	instr(cmp, 0xC1, 2, 6, modeIndexedIndirect)
	instr(cmp, 0xD1, 2, 5, modeIndirectIndexed)

	instr(cpx, 0xE0, 2, 2, modeImmediate)
	instr(cpx, 0xE4, 2, 3, modeZeroPage)
	instr(cpx, 0xEC, 3, 4, modeAbsolute)

	instr(cpy, 0xC0, 2, 2, modeImmediate)
	instr(cpy, 0xC4, 2, 3, modeZeroPage)
	instr(cpy, 0xCC, 3, 4, modeAbsolute)

	instr(dec, 0xC6, 2, 5, modeZeroPage)
	instr(dec, 0xD6, 2, 6, modeZeroPageX)
	instr(dec, 0xCE, 3, 3, modeAbsolute)
	instr(dec, 0xDE, 3, 7, modeAbsoluteX)

	instr(dex, 0xCA, 1, 2, modeImplied)

	instr(dey, 0x88, 1, 2, modeImplied)

	instr(eor, 0x49, 2, 2, modeImmediate)
	instr(eor, 0x45, 2, 3, modeZeroPage)
	instr(eor, 0x55, 2, 4, modeZeroPageX)
	instr(eor, 0x4D, 3, 4, modeAbsolute)
	instr(eor, 0x5D, 3, 4, modeAbsoluteX)
	instr(eor, 0x59, 3, 4, modeAbsoluteY)
	instr(eor, 0x41, 2, 6, modeIndirectIndexed)
	instr(eor, 0x51, 2, 5, modeIndexedIndirect)

	instr(inc, 0xE6, 2, 5, modeZeroPage)
	instr(inc, 0xF6, 2, 6, modeZeroPageX)
	instr(inc, 0xEE, 3, 6, modeAbsolute)
	instr(inc, 0xFE, 3, 7, modeAbsoluteX)

	instr(inx, 0xE8, 1, 2, modeImplied)

	instr(iny, 0xC8, 1, 2, modeImplied)

	instr(jmp, 0x4C, 3, 3, modeAbsolute)
	instr(jmp, 0x6C, 3, 5, modeIndirect)

	instr(jsr, 0x20, 3, 6, modeAbsolute)

	instr(lda, 0xA9, 2, 2, modeImmediate)
	instr(lda, 0xA5, 2, 2, modeZeroPage)
	instr(lda, 0xB5, 2, 4, modeZeroPageX)
	instr(lda, 0xAD, 3, 4, modeAbsolute)
	instr(lda, 0xBD, 3, 4, modeAbsoluteX)
	instr(lda, 0xB9, 3, 4, modeAbsoluteY)
	instr(lda, 0xA1, 2, 6, modeIndirectIndexed)
	instr(lda, 0xB1, 2, 5, modeIndexedIndirect)

	instr(ldx, 0xA2, 2, 2, modeImmediate)
	instr(ldx, 0xA6, 2, 3, modeZeroPage)
	instr(ldx, 0xB6, 2, 4, modeZeroPageY)
	instr(ldx, 0xAE, 3, 4, modeAbsolute)
	instr(ldx, 0xBE, 3, 4, modeAbsoluteY)

	instr(ldy, 0xA0, 2, 2, modeImmediate)
	instr(ldy, 0xA4, 2, 3, modeZeroPage)
	instr(ldy, 0xB4, 2, 4, modeZeroPageX)
	instr(ldy, 0xAC, 3, 4, modeAbsolute)
	instr(ldy, 0xBC, 3, 4, modeAbsoluteX)

	instr(lsr, 0x4A, 1, 2, modeAccumulator)
	instr(lsr, 0x46, 2, 5, modeZeroPage)
	instr(lsr, 0x56, 2, 6, modeZeroPageX)
	instr(lsr, 0x4E, 3, 6, modeAbsolute)
	instr(lsr, 0x5E, 3, 7, modeAbsoluteX)

	instr(nop, 0xEA, 1, 2, modeImplied)

	instr(ora, 0x09, 2, 2, modeImmediate)
	instr(ora, 0x05, 2, 3, modeZeroPage)
	instr(ora, 0x15, 2, 4, modeZeroPageX)
	instr(ora, 0x0D, 3, 4, modeAbsolute)
	instr(ora, 0x1D, 3, 4, modeAbsoluteX)
	instr(ora, 0x19, 3, 4, modeAbsoluteY)
	instr(ora, 0x01, 2, 6, modeIndirectIndexed)
	instr(ora, 0x11, 2, 5, modeIndexedIndirect)

	instr(pha, 0x48, 1, 3, modeImplied)

	instr(php, 0x08, 1, 3, modeImplied)

	instr(pla, 0x68, 1, 4, modeImplied)

	instr(plp, 0x28, 1, 4, modeImplied)

	instr(rts, 0x60, 1, 6, modeImplied)
}

func adc(pos position, ctx context) {
	curA := ctx.regs.A
	res := int(curA) + int(pos.read()) + int(ctx.flags.C)
	ctx.regs.A = byte(res)
	ctx.setZNFlags(ctx.regs.A)
	ctx.flags.V = sameSign(curA, pos.read()) && !sameSign(curA, ctx.regs.A)
	if res > math.MaxUint8 {
		ctx.flags.C = 1
	} else {
		ctx.flags.C = 0
	}
}

func and(pos position, ctx context) {
	ctx.regs.A &= pos.read()
	ctx.setZNFlags(ctx.regs.A)
}

func asl(pos position, ctx context) {
	ctx.flags.C = (pos.read() >> 7) & 1
	res := pos.read() << 1
	pos.write(res)
	ctx.setZNFlags(res)
}

func branchOn(cond bool, pos *position, regs *Registers) {
	if cond {
		regs.PC = pos.address()
	}
}

func bcc(pos position, ctx context) {
	branchOn(ctx.flags.C == 0, &pos, ctx.regs)
}

func bcs(pos position, ctx context) {
	branchOn(ctx.flags.C == 1, &pos, ctx.regs)
}

func beq(pos position, ctx context) {
	branchOn(ctx.flags.Z, &pos, ctx.regs)
}

func bit(pos position, ctx context) {
	val := pos.read()
	ctx.flags.V = val&(1<<6) != 0
	ctx.flags.N = val&(1<<7) != 0
	ctx.flags.Z = val&ctx.regs.A == 0
}

func bmi(pos position, ctx context) {
	branchOn(ctx.flags.N, &pos, ctx.regs)
}

func bne(pos position, ctx context) {
	branchOn(!ctx.flags.Z, &pos, ctx.regs)
}

func bpl(pos position, ctx context) {
	branchOn(!ctx.flags.N, &pos, ctx.regs)
}

func brk(pos position, ctx context) {
	ctx.pushAddress(ctx.regs.PC)
	php(pos, ctx)
	sei(pos, ctx)
	ctx.regs.PC = IrqVector
}

func bvc(pos position, ctx context) {
	branchOn(!ctx.flags.V, &pos, ctx.regs)
}

func bvs(pos position, ctx context) {
	branchOn(ctx.flags.V, &pos, ctx.regs)
}

func clc(_ position, ctx context) {
	ctx.flags.C = 0
}

func cld(_ position, ctx context) {
	ctx.flags.D = false
}

func cli(_ position, ctx context) {
	ctx.flags.I = false
}

func clv(_ position, ctx context) {
	ctx.flags.V = false
}

func compare(val byte, pos *position, ctx context) {
	ctx.setZNFlags(val - pos.read())
	if val > pos.read() {
		ctx.flags.C = 1
	} else {
		ctx.flags.C = 0
	}
}

func cmp(pos position, ctx context) {
	compare(ctx.regs.A, &pos, ctx)
}

func cpx(pos position, ctx context) {
	compare(ctx.regs.X, &pos, ctx)
}

func cpy(pos position, ctx context) {
	compare(ctx.regs.Y, &pos, ctx)
}

func dec(pos position, ctx context) {
	res := pos.read() - 1
	ctx.setZNFlags(res)
	pos.write(res)
}

func dex(_ position, ctx context) {
	ctx.regs.X--
	ctx.setZNFlags(ctx.regs.X)
}

func dey(_ position, ctx context) {
	ctx.regs.Y--
	ctx.setZNFlags(ctx.regs.Y)
}

func eor(pos position, ctx context) {
	ctx.regs.A ^= pos.read()
	ctx.setZNFlags(ctx.regs.A)
}

func inc(pos position, ctx context) {
	res := pos.read() + 1
	ctx.setZNFlags(res)
	pos.write(res)
}

func inx(_ position, ctx context) {
	ctx.regs.X++
	ctx.setZNFlags(ctx.regs.X)
}

func iny(_ position, ctx context) {
	ctx.regs.Y++
	ctx.setZNFlags(ctx.regs.Y)
}

func jmp(pos position, ctx context) {
	ctx.regs.PC = pos.address()
}

func jsr(pos position, ctx context) {
	ctx.pushAddress(ctx.regs.PC - 1)
	jmp(pos, ctx)
}

func lda(pos position, ctx context) {
	ctx.regs.A = pos.read()
	ctx.setZNFlags(ctx.regs.A)
}

func ldx(pos position, ctx context) {
	ctx.regs.X = pos.read()
	ctx.setZNFlags(ctx.regs.X)
}

func ldy(pos position, ctx context) {
	ctx.regs.Y = pos.read()
	ctx.setZNFlags(ctx.regs.Y)
}

func lsr(pos position, ctx context) {
	res := pos.read()
	ctx.flags.C = res & 1
	res >>= 1
	ctx.setZNFlags(res)
	pos.write(res)
}

func nop(_ position, _ context) {
}

func ora(pos position, ctx context) {
	ctx.regs.A |= pos.read()
	ctx.setZNFlags(ctx.regs.A)
}

func pha(_ position, ctx context) {
	ctx.push(ctx.regs.A)
}

func php(_ position, ctx context) {
	// https://wiki.nesdev.com/w/index.php/Status_flags#The_B_flag
	ctx.push(ctx.flags.pack() | 0x30)
}

func pla(_ position, ctx context) {
	ctx.regs.A = ctx.pop()
	ctx.setZNFlags(ctx.regs.A)
}

func plp(_ position, ctx context) {
	ctx.flags.unpack(ctx.pop())
}

func rts(_ position, ctx context) {
	ctx.regs.PC = ctx.popAddress() + 1
}

func sei(_ position, ctx context) {
	ctx.flags.I = true
}
