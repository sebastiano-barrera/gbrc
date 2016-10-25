package assembly

import (
	"gbrc/parser"
	"io"
	"strings"
	"fmt"
	"errors"
)

var (
	// Mapping of GB regs to the target arch's (x86_64)
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

	regs16 = map[parser.Register16]string{parser.SP: "si"}

	regsComp = map[parser.CompositeReg]string{
		parser.AF: "ax",
		parser.BC: "bx",
		parser.DE: "cx",
		parser.HL: "dx",
	}

	opers = map[parser.Operation]string{
		parser.ADC: "ADC",
		parser.ADD: "ADD",
		parser.AND: "AND",
		parser.BIT: "BIT",
		parser.CALL: "CALL",
		parser.CCF: "CCF",
		parser.CP: "CP",
		parser.CPL: "CPL",
		parser.DAA: "DAA",
		parser.DEC: "DEC",
		parser.DI: "DI",
		parser.EI: "EI",
		parser.HALT: "HALT",
		parser.INC: "INC",
		parser.JP: "JP",
		parser.JR: "JR",
		parser.LD: "LD",
		parser.LDD: "LDD",
		parser.LDH: "LDH",
		parser.LDHL: "LDHL",
		parser.LDI: "LDI",
		parser.NOP: "NOP",
		parser.OR: "OR",
		parser.POP: "POP",
		parser.PUSH: "PUSH",
		parser.RES: "RES",
		parser.RET: "RET",
		parser.RETI: "RETI",
		parser.RL: "RL",
		parser.RLC: "RLC",
		parser.RR: "RR",
		parser.RRC: "RRC",
		parser.RST: "RST",
		parser.SBC: "SBC",
		parser.SCF: "SCF",
		parser.SET: "SET",
		parser.SLA: "SLA",
		parser.SRA: "SRA",
		parser.SRL: "SRL",
		parser.STOP: "STOP",
		parser.SUB: "SUB",
		parser.SWAP: "SWAP",
		parser.XOR: "XOR",
	}

	ErrInvalidOperand = errors.New("invalid operand")
)

func nameOf(block *parser.BasicBlock) string {
	return fmt.Sprintf("block_%08x", block.Addr)
}

func compileOperand(operand parser.Operand, w io.Writer) (string, error) {
	switch operand := operand.(type) {
	case parser.Register8:
		return regs8[operand], nil
	case parser.Register16:
		return regs16[operand], nil
	case parser.CompositeReg:
		return regsComp[operand], nil
	case parser.Immediate16:
		return fmt.Sprintf("0%04xh", uint16(operand)), nil
	case parser.Immediate8:
		return fmt.Sprintf("0%02xh", uint8(operand)), nil
	case parser.Indirect:
		addrOp, err := compileOperand(operand.Target, w)
		if err != nil { return "", err }
		fmt.Fprintf(w, "movzx rdi, %s\ncall gb_fetch\n", addrOp)
		return "rax", nil
	}

	return "", ErrInvalidOperand
}

func compileInstr(instr *parser.Instruction, w io.Writer) error {
	outOperands := make([]string, len(instr.Operands))
	for i, operand := range instr.Operands {
		var err error
		outOperands[i], err = compileOperand(operand, w)
		if err != nil {
			return ErrInvalidOperand
		}
	}

	operation := opers[instr.Operation]
	operandsStr := strings.Join(outOperands, ", ")
	fmt.Fprintf(w, "%s %s\n", operation, operandsStr)

	return nil
}

func compileCont(cont parser.Continuation, w io.Writer) error {
	return nil
}

func Compile(code parser.Rom, w io.Writer) error {
	for _, block := range code.Blocks {
		fmt.Fprintf(w, "\n%s:\n", nameOf(block))
		for _, instr := range block.Body {
			if err := compileInstr(&instr, w); err != nil {
				return err
			}
		}
		if err := compileCont(block.Cont, w); err != nil {
			return err
		}
	}

	return nil
}
