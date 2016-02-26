package compiler

import (
	"errors"
	"fmt"
	"io"
	"os"
)

type Address uint16
type Size uint16
type Condition int

const (
	CondAlways Condition = iota
	CondZero
)

type InvalidOpcode uint8

func (ie InvalidOpcode) Error() string {
	return fmt.Sprintf("invalid opcode: 0x%02x", uint8(ie))
}

var (
	ErrInvalidInstrOffset = errors.New("Invalid offset for an instruction")
)

type instruction struct {
	codeSize Size   // Size of original instruction
	output   string // Compiled instruction
}

type BasicBlock struct {
	Body     []instruction // Compiled code
	CodeSize Address       // Size of original binary code, *including last jump instruction*
	Cond     Condition
	Dynamic  bool        // true => jump target is the runtime value of HL
	Cont     *BasicBlock // Static continuation (nil => HALT)
}

func (b *BasicBlock) instrIndex(ofs Address) (int, error) {
	fmt.Fprintf(os.Stderr, "Locating instruction at %d\n", ofs)
	curOfs := Address(0)
	for i, instr := range b.Body {
		fmt.Fprintf(os.Stderr, "%4d %s\n", instr.codeSize, instr.output)
		if ofs == curOfs {
			return i, nil
		} else if curOfs > ofs {
			return -1, ErrInvalidInstrOffset
		}

		curOfs += Address(instr.codeSize)
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
			Dynamic: b.Dynamic,
			Cond: b.Cond,
			Cont: b.Cont,
		}
		prec = &BasicBlock{Body: b.Body, Cond: CondAlways, Cont: foll}
		return
	}

	foll = &BasicBlock{Body: b.Body[index:], Cond: b.Cond, Cont: b.Cont}
	prec = &BasicBlock{Body: b.Body[0:index], Cond: CondAlways, Cont: foll}
	return
}

// type codeRegion struct {
// 	addr  Address
// 	block *BasicBlock
// }

type Arch interface {
	CompileOpcode(*Compiler) error
}

type Compiler struct {
	Input       io.ReadSeeker
	arch        Arch
	blocks      map[Address]*BasicBlock
	codeRegions []Address
	curInstrOfs Address
}

func NewCompiler(input io.ReadSeeker, arch Arch) Compiler {
	// Initialize the Compiler so that compilation starts
	// from address 0 in the code
	comp := Compiler{
		Input:  input,
		arch:   arch,
		blocks: make(map[Address]*BasicBlock),
		codeRegions: []Address{0},
		curInstrOfs: Address(0),
	}
	comp.blocks[0] = &BasicBlock{}
	return comp
}

func (c *Compiler) Compile() error {
	for len(c.codeRegions) > 0 {
		err := c.arch.CompileOpcode(c)
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

func (c *Compiler) address() Address {
	addr, err := c.Input.Seek(0, 1)
	if err != nil {
		panic(fmt.Sprintf("can't get current pos in code stream: %v", err))
	}
	return Address(addr)
}

func (c *Compiler) curRegion() Address {
	return c.codeRegions[len(c.codeRegions)-1]
}

func (c *Compiler) pushRegion(addr Address) *BasicBlock {
	newBlock := &BasicBlock{}
	c.codeRegions = append(c.codeRegions, addr)
	c.blocks[addr] = newBlock
	return newBlock
}

func (c *Compiler) popRegion() (Address, *BasicBlock) {
	addr := c.curRegion()
	c.codeRegions = c.codeRegions[:len(c.codeRegions)-1]
	return addr, c.blocks[addr]
}

func (c *Compiler) PushInstr(compiledInstr string) {
	addr := c.curRegion()
	instr := instruction{
		codeSize: Size(c.address() - c.curInstrOfs),
		output:   compiledInstr,
	}
	c.blocks[addr].Body = append(c.blocks[addr].Body, instr)
	c.curInstrOfs = c.address()
}

func (c *Compiler) PlaceJump(cond Condition, dest Address) error {
	regAddr, regBlock := c.popRegion()
	endAddr := c.address()
	fmt.Fprintf(os.Stderr, "PlaceJump: block@%04x, jump ends at %04x (%v => %v)\n",
		regAddr, endAddr, cond, dest)

	regBlock.Cond = cond

	// Close the current block; we need to determine which
	// is the current block's continuation.
	if block, ok := c.blocks[dest]; ok {
		// `block` is a basic block starting exactly at
		// the needed address (a likely case, after a while)
		regBlock.Cont = block
	} else if dest == regAddr {
		// Target block is the current block
		// (not yet in block map)
		regBlock.Cont = block
	} else {
		// Look for target block in block map
		for ofs, block := range c.blocks {
			if ofs <= dest && dest < ofs+block.CodeSize {
				fmt.Fprintf(os.Stderr, "splitting block %04x+%d around %d\n",
					ofs, block.CodeSize, dest)
				// Split block around the jump dest
				prec, foll, err := block.Split(dest - ofs)
				if err != nil {
					return err
				}
				c.blocks[ofs] = prec
				c.blocks[dest] = foll
				regBlock.Cont = foll
				break
			}
		}

		if regBlock.Cont == nil {
			// Still not found: jump destination is a new
			// region of code
			newBlock := c.pushRegion(dest)
			regBlock.Cont = newBlock
		}
	}

	// endAddr := c.address()
	regBlock.CodeSize = endAddr - regAddr
	if cond != CondAlways {
		c.pushRegion(endAddr)
	}
	return nil
}

func (c *Compiler) PlaceHalt() {
	addr, block := c.popRegion()
	// a nil map here marks a halting block
	block.Cont = nil
	c.blocks[addr] = block
}

func (c *Compiler) WriteTo(w io.Writer) {
	fmt.Fprintf(w, "package code\n")
	fmt.Fprintf(w, "type Machine struct{}\n")
	fmt.Fprintf(w, "type Block func(m *Machine) *Block\n")
	fmt.Fprintf(w, "// %d code regions\n", len(c.codeRegions))
	for _, addr := range c.codeRegions {
		fmt.Fprintf(w, "// %d: %v", addr, c.blocks[addr])
	}

	fmt.Fprintf(w, "// %d blocks\n", len(c.blocks))
	for addr, block := range c.blocks {
		fmt.Fprintf(w, "// %d bytes\n", block.CodeSize)
		fmt.Fprintf(w, "func Block_%04x(m *Machine) *Block {\n", addr)
		for _, stmt := range block.Body {
			fmt.Fprintf(w, "%s\n", stmt.output)
		}
		fmt.Fprintf(w, "}\n\n")
	}
}
