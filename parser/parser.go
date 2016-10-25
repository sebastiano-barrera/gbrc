package parser

import (
	"errors"
	"fmt"
	"io"
	"os"
)

//
// Architecture and code description
//

type (
	Address uint16
	Size    uint16

	// Operands
	Operand      interface{}
	Register8    int
	Register16   int
	CompositeReg struct{ lo, hi Register8 }
	Immediate16  uint16
	Immediate8   uint16
	Indirect     struct{ Target Operand }
	StaticParam  int
	Condition    string

	Operation   int
	Instruction struct {
		CodeSize  Size // Size of original instruction
		Operation Operation
		Operands  []Operand
	}
)

var (
	CondAlways   = Condition("always")
	CondZero     = Condition("zero")
	CondNonZero  = Condition("non zero")
	CondCarry    = Condition("carry")
	CondNonCarry = Condition("non carry")

	AF = CompositeReg{A, F}
	BC = CompositeReg{B, C}
	DE = CompositeReg{D, E}
	HL = CompositeReg{H, L}

	ErrInvalidInstrOffset = errors.New("Invalid offset for an instruction")
)

const (
	ADC Operation = iota
	ADD
	AND
	BIT
	CALL
	CCF
	CP
	CPL
	DAA
	DEC
	DI
	EI
	HALT
	INC
	JP
	JR
	LD
	LDD
	LDH
	LDHL
	LDI
	NOP
	OR
	POP
	PUSH
	RES
	RET
	RETI
	RL
	RLC
	RR
	RRC
	RST
	SBC
	SCF
	SET
	SLA
	SRA
	SRL
	STOP
	SUB
	SWAP
	XOR

	A Register8 = iota
	F
	B
	C
	D
	E
	H
	L

	SP Register16 = iota
)

//
// Program structure description
//

// Continuation represents the way execution continues after a block finishes.
type Continuation interface{}

type ContConditional struct {
	Cond       Condition
	Then, Else *BasicBlock
}

type ContDynamic struct{}
type ContHalt struct{}
type ContCall struct{ ContConditional }
type ContRet struct{ ContConditional }

type BasicBlock struct {
	Addr     Address // Starting address in the original machine code
	CodeSize Size    // Size of original machine code, *including last jump instruction*
	Body     []Instruction
	Cont     Continuation
}

func (b *BasicBlock) instrIndex(ofs Address) (int, error) {
	fmt.Fprintf(os.Stderr, "Locating instruction at %d\n", ofs)
	curOfs := Address(0)
	for i, instr := range b.Body {
		if ofs == curOfs {
			return i, nil
		} else if curOfs > ofs {
			return -1, ErrInvalidInstrOffset
		}

		curOfs += Address(instr.CodeSize)
	}

	fmt.Fprintf(os.Stderr, "end of block\n")
	if ofs == curOfs {
		return len(b.Body), nil
	}

	return -1, ErrInvalidInstrOffset
}

func (b *BasicBlock) Split(ofs Address) (prec, foll *BasicBlock, err error) {
	index, err := b.instrIndex(ofs)
	if err != nil {
		return nil, nil, err
	}

	fmt.Fprintf(os.Stderr, "splitting around index %d\n", index)

	if index == len(b.Body) {
		// Split around the jump instruction:
		foll = &BasicBlock{
			Body: nil,
			Cont: b.Cont,
		}
		prec = &BasicBlock{Body: b.Body, Cont: ContConditional{CondAlways, foll, nil}}
		return
	}

	foll = &BasicBlock{Body: b.Body[index:], Cont: b.Cont}
	prec = &BasicBlock{Body: b.Body[0:index], Cont: ContConditional{CondAlways, foll, nil}}
	return
}

//
// Parser (opcodes -> instructions -> structured program)
//
type BlockMap map[Address]*BasicBlock

type Rom struct {
	Blocks BlockMap
}

type parser struct {
	input        io.ReadSeeker
	blocks       BlockMap
	codeRegions  []Address
	curInstrAddr Address
}

func Parse(input io.ReadSeeker) (*Rom, error) {
	const InitAddr = 0
	input.Seek(InitAddr, 0)
	p := parser{
		input:        input,
		blocks:       make(map[Address]*BasicBlock),
		codeRegions:  []Address{InitAddr},
		curInstrAddr: Address(InitAddr),
	}
	p.blocks[InitAddr] = &BasicBlock{}

	if err := p.parse(); err != nil {
		return nil, err
	}

	rom := &Rom{
		Blocks: p.blocks,
	}
	return rom, nil
}

func (c *parser) parse() error {
	for len(c.codeRegions) > 0 {
		err := decodeOpcode(c)
		if err == io.EOF {
			if len(c.codeRegions) > 0 {
				c.PlaceHalt()
				continue
			}
			break
		} else if err != nil {
			return err
		}
	}
	return nil
}

func (c *parser) Address() Address {
	addr, err := c.input.Seek(0, 1)
	if err != nil {
		panic(fmt.Sprintf("can't get current pos in code stream: %v", err))
	}
	return Address(addr)
}

func (c *parser) curRegion() Address {
	return c.codeRegions[len(c.codeRegions)-1]
}

func (c *parser) pushRegion(addr Address) *BasicBlock {
	newBlock := &BasicBlock{Addr: addr}
	c.codeRegions = append(c.codeRegions, addr)
	c.blocks[addr] = newBlock
	return newBlock
}

func (c *parser) popRegion() *BasicBlock {
	addr := c.curRegion()
	c.codeRegions = c.codeRegions[:len(c.codeRegions)-1]
	return c.blocks[addr]
}

func (c *parser) PushInstr(operation Operation, operands ...Operand) {
	endAddr := c.Address()
	addr := c.curRegion()
	instr := Instruction{
		CodeSize:  Size(endAddr - c.curInstrAddr),
		Operation: operation,
		Operands:  operands,
	}

	fmt.Printf("instr %v %v\n", operation, operands)

	c.blocks[addr].Body = append(c.blocks[addr].Body, instr)
	c.curInstrAddr = endAddr
}

func (c *parser) TargetBlock(addr Address) (*BasicBlock, error) {
	if block, ok := c.blocks[addr]; ok {
		// `block` is a basic block starting exactly at
		// the needed address (a likely case, after a while)
		return block, nil
	}

	// Look for target block in block map
	for ofs, block := range c.blocks {
		if ofs <= addr && addr < ofs+Address(block.CodeSize) {
			fmt.Fprintf(os.Stderr, "splitting block %04x+%d around %08X\n",
				ofs, block.CodeSize, addr)
			// Split block around the jump dest
			prec, foll, err := block.Split(addr - ofs)
			if err != nil {
				return nil, err
			}
			c.blocks[ofs] = prec
			c.blocks[addr] = foll
			return foll, nil
		}
	}

	// Leave the message as a comment after debugging...
	fmt.Fprintf(os.Stderr, "\tjump destination is a new region of code\n")

	newBlock := c.pushRegion(addr)
	return newBlock, nil
}

func (c *parser) closeBlock(block *BasicBlock, cont Continuation) error {
	block.Cont = cont
	block.CodeSize = Size(c.Address() - block.Addr)
	if len(c.codeRegions) > 0 {
		_, err := c.input.Seek(int64(c.curRegion()), 0)
		c.curInstrAddr = c.curRegion()
		return err
	}
	return nil
}

func (c *parser) forkRegion(cond Condition, target *BasicBlock) ContConditional {
	cont := ContConditional{cond, target, nil}
	if cond != CondAlways {
		cont.Else = c.pushRegion(c.Address())
	}
	return cont
}

func (c *parser) PlaceJump(cond Condition, dest Address) error {
	regBlock := c.popRegion()
	endAddr := c.Address()
	fmt.Fprintf(os.Stderr, "PlaceJump: block@%04x, jump instr ends at %08x (%v => %08x)\n",
		regBlock.Addr, endAddr, cond, dest)

	// Close the current block; we need to determine which
	// is the current block's continuation.
	targetBlock, err := c.TargetBlock(dest)
	if err != nil {
		return err
	}

	cont := c.forkRegion(cond, targetBlock)
	return c.closeBlock(regBlock, cont)
}

func (c *parser) PlaceDynamicJump() error {
	// This is just a heuristic, that I expect to fail a good portion
	// of times
	regBlock := c.popRegion()
	endAddr := c.Address()
	fmt.Fprintf(os.Stderr, "PlaceJump: block@%04x, jump instr ends at %08x\n",
		regBlock.Addr, endAddr)

	return c.closeBlock(regBlock, ContDynamic{})
}

func (c *parser) PlaceCall(cond Condition, dest Address) error {
	regBlock := c.popRegion()

	targetBlock, err := c.TargetBlock(dest)
	if err != nil {
		return err
	}

	cont := c.forkRegion(cond, targetBlock)
	return c.closeBlock(regBlock, ContCall{cont})
}

func (c *parser) PlaceRet(cond Condition) error {
	block := c.popRegion()
	cont := c.forkRegion(cond, nil)
	return c.closeBlock(block, ContRet{cont})
}

func (c *parser) PlaceHalt() error {
	block := c.popRegion()
	return c.closeBlock(block, ContHalt{})
}
