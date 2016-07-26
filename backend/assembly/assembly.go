package assembly

import (
	"gbrc/parser"
	"io"
	"strings"
)

type (
	Backend struct{}
	Operand struct {
		OutOperand string
		Prelude    string
		Epilogue   string
	}
)

var (
	regs8 = map[parser.Register8]string{
		parser.A: "al",
		parser.F: "ah",
		parser.B: "bl",
		parser.C: "bh",
		parser.D: "cl",
		parser.E: "ch",
		parser.H: "dl",
		parser.L: "dh",
	}

	regs16 = map[parser.Register16]string{SP: "si"}

	regsComp = map[parser.CompositeReg]string{
		parser.AF = "ax",
		parser.BC = "bx",
		parser.DE = "cx",
		parser.HL = "dx",
	}
)

func nameOf(block *parser.BasicBlock) string {
	return fmt.Sprintf("block_%08x", block.Addr)
}

func compileOperand(operand parser.Operand) Operand {
	switch operand := operand.(type) {
	case Register8:
		return Operand{regs8[operand]}
	case Register16:
		return Operand{regs16[operand]}
	case CompositeReg:
		return Operand{regsComp[operand]}
	case Immediate16:
		return Operand{fmt.Sprintf("0%04xh", uint16(operand))}
	case Immediate8:
		return Operand{fmt.Sprintf("0%02xh", uint8(operand))}
	case Indirect, StaticParam, Condition:
		return Operand{"LOOOL"}
	}
}

func compileInstr(instr *parser.Instruction, w io.Writer) error {
	operands := make([]Operand, len(instr.Operands))
	outOperands := make([]string, len(instr.Operands))
	for i, operand := range instr.Operands {
		operands[i] = compileOperand(operand)
		outOperands[i] = operands[i].OutOperand
		fmt.Fprintf(w, "%s\n", operands[i].Prologue)
	}

	operation := opers[instr.Operation]
	operandsStr := strings.Join(outOperands)
	fmt.Fprintf(w, "%s %s\n", operation, operandsStr)

	for i, operand := range operands {
		fmt.Fprintf(w, "%s\n", operands[i].Epilogue)
	}

	return nil
}

func compileCont(cont parser.Continuation, w io.Writer) error {
	return nil
}

func (b *Backend) Compile(p *parser.Parser, w io.Writer) error {
	for addr, block := range p.Blocks {
		fmt.Fprintf(w, "\n%s:\n", b.nameOf(block))
		for _, instr := range block.Body {
			err := compileInstr(&instr, w)
			if err != nil {
				return err
			}
		}
		err := compileCont(block.Cont, w)
		if err != nil {
			return err
		}
	}

	return nil
}
