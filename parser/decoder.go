package parser

import (
	"fmt"
	"io"
	"os"
)

//
// Errors
//
type (
	InvalidOpcode uint8
	SizeMismatch   struct{ left, right Operand }
	ReadOnlyWrite  struct{ place Operand }
	InvalidOperand struct {
		instruction string
		operand     Operand
		shouldBe    string
	}
)

func (ie InvalidOpcode) Error() string {
	return fmt.Sprintf("invalid opcode: 0x%02x", uint8(ie))
}

func (sm SizeMismatch) Error() string {
	return fmt.Sprintf("size mismatch: %v <> %v (%d != %d)",
		sm.left, sm.right, sm.left.Size(), sm.right.Size())
}

func (ro ReadOnlyWrite) Error() string {
	return fmt.Sprintf("place is read-only: %v", ro.place)
}

func (invOp InvalidOperand) Error() string {
	return fmt.Sprintf("invalid operand for %s: %v (should be %s)",
		invOp.instruction, invOp.operand, invOp.shouldBe)
}

//
// Disassembly
//
// func compileInstrSLA(c *Compiler, operands ...Place) error {
// 	shiftExpr := MkExpr(operands[0].Size(), "%s << 1", operands[0])
// 	instr, err := operands[0].Setter(shiftExpr)
// 	if err != nil {
// 		return err
// 	}
// 	c.PushInstr(instr)
// 	return nil
// }

// func compileInstrHALT(c *Compiler, operands ...Place) error {
// 	c.PlaceHalt()
// 	return nil
// }

// func compileInstrLDI(c *Compiler, operands ...Place) error {

// 	instr, err := operands[0].Setter(operands[1])
// 	if err != nil {
// 		return err
// 	}
// 	c.PushInstr(instr)

// 	incExpr := MkExpr(HL.Size(), "%s + 1", HL)
// 	instr, err = HL.Setter(incExpr)
// 	if err != nil {
// 		return err
// 	}
// 	c.PushInstr(instr)
// 	return nil
// }

// func compileInstrSTOP(c *Compiler, operands ...Place) error {
// 	c.PlaceHalt()
// 	return nil
// }

// func compileInstrLDHL(c *Compiler, operands ...Place) error {

// 	instr, err := CompositeReg{H, L}.Setter(
// 		MkExpr(2, "%s + %s", SP, operands[1]))
// 	if err != nil {
// 		return err
// 	}
// 	c.PushInstr(instr)
// 	return nil
// }

// func compileInstrJP(c *Compiler, operands ...Place) error {
// 	var (
// 		cond   Condition
// 		target Place
// 	)

// 	if opCond, ok := operands[0].(CondOperand); ok {
// 		cond = opCond.Condition
// 		target = operands[1]
// 	} else {
// 		cond = CondAlways
// 		target = operands[0]
// 	}

// 	addr, ok := target.(Immediate16)
// 	if !ok {
// 		return InvalidOperand{"JP", target, "a 16-bit immediate"}
// 	}

// 	return c.PlaceJump(cond, Address(addr))
// }

// func compileInstrINC(c *Compiler, operands ...Place) error {
// 	expr := MkExpr(operands[0].Size(), "%s + 1", operands[0])
// 	stmt, err := operands[0].Setter(expr)
// 	if err == nil {
// 		c.PushInstr(stmt)
// 	}
// 	return err
// }

// func compileInstrCP(c *Compiler, operands ...Place) error {
// 	if operands[0].Size() != 1 {
// 		return InvalidSize{operands[0], 1}
// 	}

// 	c.PushInstr(fmt.Sprintf(`
// 		{
// 			diff := uint16(int16(%s) - int16(%s))
// 			m.SetFlagsFor(diff)
// 		}
// 	`, operands[0], A))
// 	return nil
// }

// func compileInstrRET(c *Compiler, operands ...Place) error {
// 	cond := CondAlways
// 	if len(operands) > 0 {
// 		condOp, ok := operands[0].(CondOperand)
// 		if !ok {
// 			return InvalidOperand{"RET", operands[0], "a condition"}
// 		}
// 		cond = condOp.Condition
// 	}

// 	c.PlaceRet(cond)
// 	return nil
// }

// func compileInstrBIT(c *Compiler, operands ...Place) error {
// 	bitIndex, ok := operands[0].(StaticParam)
// 	if !ok {
// 		return InvalidOperand{"BIT", operands[0], "an index for a bit in a byte (0-7)"}
// 	}

// 	stmt := fmt.Sprintf("m.SetFlag(FlagZero, ((%s >> %d) & 1) == 1)",
// 		operands[1], bitIndex)
// 	c.PushInstr(stmt)
// 	return nil
// }

// func compileInstrLDH(c *Compiler, operands ...Place) error {
// 	var stmt string
// 	var err error

// 	if operands[0] == A {
// 		addrOp := MkExpr(2, "(0xff00 | uint16(%s))", operands[1])
// 		stmt, err = operands[0].Setter(Indirect{addrOp})
// 	} else {
// 		addrOp := MkExpr(2, "(0xff00 | uint16(%s))", operands[0])
// 		stmt, err = Indirect{addrOp}.Setter(operands[1])
// 	}

// 	if err == nil {
// 		c.PushInstr(stmt)
// 	}
// 	return err
// }

// func compileInstrLD(c *Compiler, operands ...Place) error {
// 	stmt, err := operands[0].Setter(operands[1])
// 	if err == nil {
// 		c.PushInstr(stmt)
// 	}
// 	return err
// }

// func compileInstrSCF(c *Compiler, operands ...Place) error {
// 	c.PushInstr("m.SetFlag(FlagCarry)")
// 	return nil
// }

// func compileInstrDEC(c *Compiler, operands ...Place) error {
// 	expr := MkExpr(operands[0].Size(), "%s - 1", operands[0])
// 	stmt, err := operands[0].Setter(expr)
// 	if err == nil {
// 		c.PushInstr(stmt)
// 	}
// 	return err
// }

// func compileInstrCALL(c *Compiler, operands ...Place) error {
// 	var cond Condition
// 	targetIndex := 0
// 	if condOp, ok := operands[0].(CondOperand); ok {
// 		cond = condOp.Condition
// 		targetIndex = 1
// 	} else {
// 		cond = CondAlways
// 	}

// 	target, ok := operands[targetIndex].(Immediate16)
// 	if !ok {
// 		return InvalidOperand{"CALL", operands[targetIndex], "an address (16 bits)"}
// 	}

// 	c.PlaceCall(cond, Address(target))
// 	return nil
// }

// func compileInstrJR(c *Compiler, operands ...Place) error {
// 	var (
// 		cond     Condition
// 		offsetOp Place
// 	)

// 	if opCond, ok := operands[0].(CondOperand); ok {
// 		cond = opCond.Condition
// 		offsetOp = operands[1]
// 	} else {
// 		cond = CondAlways
// 		offsetOp = operands[0]
// 	}

// 	offset, ok := offsetOp.(Immediate8)
// 	if !ok {
// 		return InvalidOperand{"JR", offsetOp, "an 8-bit immediate"}
// 	}

// 	addr := int16(c.Address()) + int16(offset)
// 	return c.PlaceJump(cond, Address(addr))
// }

// func compileInstrXOR(c *Compiler, operands ...Place) error {
// 	// XOR H	- Logical XOR H against A
// 	// XOR E	- Logical XOR E against A
// 	// XOR (HL)	- Logical XOR value pointed by HL against A
// 	// XOR L	- Logical XOR L against A
// 	// XOR B	- Logical XOR B against A
// 	// XOR n	- Logical XOR 8-bit immediate against A
// 	// XOR A	- Logical XOR A against A
// 	// XOR D	- Logical XOR D against A
// 	// XOR C	- Logical XOR C against A
// 	expr := MkExpr(operands[0].Size(), "(uint8(%s) ^ %s)", operands[0], A)
// 	stmt, err := operands[0].Setter(expr)
// 	if err == nil {
// 		c.PushInstr(stmt)
// 	}
// 	return err
// }

func compileExtCB(c *Compiler) error {
	var bytebuf [1]byte
	_, err := c.Input.Read(bytebuf[:])
	if err != nil {
		return err
	}

	switch bytebuf[0] {
	case 0x00:
		// RLC B
		// Rotate B left with carry
		return c.PushInstr(RLC, B)
	case 0x01:
		// RLC C
		// Rotate C left with carry
		return c.PushInstr(RLC, C)
	case 0x02:
		// RLC D
		// Rotate D left with carry
		return c.PushInstr(RLC, D)
	case 0x03:
		// RLC E
		// Rotate E left with carry
		return c.PushInstr(RLC, E)
	case 0x04:
		// RLC H
		// Rotate H left with carry
		return c.PushInstr(RLC, H)
	case 0x05:
		// RLC L
		// Rotate L left with carry
		return c.PushInstr(RLC, L)
	case 0x06:
		// RLC (HL)
		// Rotate value pointed by HL left with carry
		return c.PushInstr(RLC, Indirect{HL})
	case 0x07:
		// RLC A
		// Rotate A left with carry
		return c.PushInstr(RLC, A)
	case 0x08:
		// RRC B
		// Rotate B right with carry
		return c.PushInstr(RRC, B)
	case 0x09:
		// RRC C
		// Rotate C right with carry
		return c.PushInstr(RRC, C)
	case 0x0A:
		// RRC D
		// Rotate D right with carry
		return c.PushInstr(RRC, D)
	case 0x0B:
		// RRC E
		// Rotate E right with carry
		return c.PushInstr(RRC, E)
	case 0x0C:
		// RRC H
		// Rotate H right with carry
		return c.PushInstr(RRC, H)
	case 0x0D:
		// RRC L
		// Rotate L right with carry
		return c.PushInstr(RRC, L)
	case 0x0E:
		// RRC (HL)
		// Rotate value pointed by HL right with carry
		return c.PushInstr(RRC, Indirect{HL})
	case 0x0F:
		// RRC A
		// Rotate A right with carry
		return c.PushInstr(RRC, A)
	case 0x10:
		// RL B
		// Rotate B left
		return c.PushInstr(RL, B)
	case 0x11:
		// RL C
		// Rotate C left
		return c.PushInstr(RL, C)
	case 0x12:
		// RL D
		// Rotate D left
		return c.PushInstr(RL, D)
	case 0x13:
		// RL E
		// Rotate E left
		return c.PushInstr(RL, E)
	case 0x14:
		// RL H
		// Rotate H left
		return c.PushInstr(RL, H)
	case 0x15:
		// RL L
		// Rotate L left
		return c.PushInstr(RL, L)
	case 0x16:
		// RL (HL)
		// Rotate value pointed by HL left
		return c.PushInstr(RL, Indirect{HL})
	case 0x17:
		// RL A
		// Rotate A left
		return c.PushInstr(RL, A)
	case 0x18:
		// RR B
		// Rotate B right
		return c.PushInstr(RR, B)
	case 0x19:
		// RR C
		// Rotate C right
		return c.PushInstr(RR, C)
	case 0x1A:
		// RR D
		// Rotate D right
		return c.PushInstr(RR, D)
	case 0x1B:
		// RR E
		// Rotate E right
		return c.PushInstr(RR, E)
	case 0x1C:
		// RR H
		// Rotate H right
		return c.PushInstr(RR, H)
	case 0x1D:
		// RR L
		// Rotate L right
		return c.PushInstr(RR, L)
	case 0x1E:
		// RR (HL)
		// Rotate value pointed by HL right
		return c.PushInstr(RR, Indirect{HL})
	case 0x1F:
		// RR A
		// Rotate A right
		return c.PushInstr(RR, A)
	case 0x20:
		// SLA B
		// Shift B left preserving sign
		return c.PushInstr(SLA, B)
	case 0x21:
		// SLA C
		// Shift C left preserving sign
		return c.PushInstr(SLA, C)
	case 0x22:
		// SLA D
		// Shift D left preserving sign
		return c.PushInstr(SLA, D)
	case 0x23:
		// SLA E
		// Shift E left preserving sign
		return c.PushInstr(SLA, E)
	case 0x24:
		// SLA H
		// Shift H left preserving sign
		return c.PushInstr(SLA, H)
	case 0x25:
		// SLA L
		// Shift L left preserving sign
		return c.PushInstr(SLA, L)
	case 0x26:
		// SLA (HL)
		// Shift value pointed by HL left preserving sign
		return c.PushInstr(SLA, Indirect{HL})
	case 0x27:
		// SLA A
		// Shift A left preserving sign
		return c.PushInstr(SLA, A)
	case 0x28:
		// SRA B
		// Shift B right preserving sign
		return c.PushInstr(SRA, B)
	case 0x29:
		// SRA C
		// Shift C right preserving sign
		return c.PushInstr(SRA, C)
	case 0x2A:
		// SRA D
		// Shift D right preserving sign
		return c.PushInstr(SRA, D)
	case 0x2B:
		// SRA E
		// Shift E right preserving sign
		return c.PushInstr(SRA, E)
	case 0x2C:
		// SRA H
		// Shift H right preserving sign
		return c.PushInstr(SRA, H)
	case 0x2D:
		// SRA L
		// Shift L right preserving sign
		return c.PushInstr(SRA, L)
	case 0x2E:
		// SRA (HL)
		// Shift value pointed by HL right preserving sign
		return c.PushInstr(SRA, Indirect{HL})
	case 0x2F:
		// SRA A
		// Shift A right preserving sign
		return c.PushInstr(SRA, A)
	case 0x30:
		// SWAP B
		// Swap nybbles in B
		return c.PushInstr(SWAP, B)
	case 0x31:
		// SWAP C
		// Swap nybbles in C
		return c.PushInstr(SWAP, C)
	case 0x32:
		// SWAP D
		// Swap nybbles in D
		return c.PushInstr(SWAP, D)
	case 0x33:
		// SWAP E
		// Swap nybbles in E
		return c.PushInstr(SWAP, E)
	case 0x34:
		// SWAP H
		// Swap nybbles in H
		return c.PushInstr(SWAP, H)
	case 0x35:
		// SWAP L
		// Swap nybbles in L
		return c.PushInstr(SWAP, L)
	case 0x36:
		// SWAP (HL)
		// Swap nybbles in value pointed by HL
		return c.PushInstr(SWAP, Indirect{HL})
	case 0x37:
		// SWAP A
		// Swap nybbles in A
		return c.PushInstr(SWAP, A)
	case 0x38:
		// SRL B
		// Shift B right
		return c.PushInstr(SRL, B)
	case 0x39:
		// SRL C
		// Shift C right
		return c.PushInstr(SRL, C)
	case 0x3A:
		// SRL D
		// Shift D right
		return c.PushInstr(SRL, D)
	case 0x3B:
		// SRL E
		// Shift E right
		return c.PushInstr(SRL, E)
	case 0x3C:
		// SRL H
		// Shift H right
		return c.PushInstr(SRL, H)
	case 0x3D:
		// SRL L
		// Shift L right
		return c.PushInstr(SRL, L)
	case 0x3E:
		// SRL (HL)
		// Shift value pointed by HL right
		return c.PushInstr(SRL, Indirect{HL})
	case 0x3F:
		// SRL A
		// Shift A right
		return c.PushInstr(SRL, A)
	case 0x40:
		// BIT 0, B
		// Test bit 0 of B
		return c.PushInstr(BIT, StaticParam(0), B)
	case 0x41:
		// BIT 0, C
		// Test bit 0 of C
		return c.PushInstr(BIT, StaticParam(0), C)
	case 0x42:
		// BIT 0, D
		// Test bit 0 of D
		return c.PushInstr(BIT, StaticParam(0), D)
	case 0x43:
		// BIT 0, E
		// Test bit 0 of E
		return c.PushInstr(BIT, StaticParam(0), E)
	case 0x44:
		// BIT 0, H
		// Test bit 0 of H
		return c.PushInstr(BIT, StaticParam(0), H)
	case 0x45:
		// BIT 0, L
		// Test bit 0 of L
		return c.PushInstr(BIT, StaticParam(0), L)
	case 0x46:
		// BIT 0, (HL)
		// Test bit 0 of value pointed by HL
		return c.PushInstr(BIT, StaticParam(0), Indirect{HL})
	case 0x47:
		// BIT 0, A
		// Test bit 0 of A
		return c.PushInstr(BIT, StaticParam(0), A)
	case 0x48:
		// BIT 1, B
		// Test bit 1 of B
		return c.PushInstr(BIT, StaticParam(1), B)
	case 0x49:
		// BIT 1, C
		// Test bit 1 of C
		return c.PushInstr(BIT, StaticParam(1), C)
	case 0x4A:
		// BIT 1, D
		// Test bit 1 of D
		return c.PushInstr(BIT, StaticParam(1), D)
	case 0x4B:
		// BIT 1, E
		// Test bit 1 of E
		return c.PushInstr(BIT, StaticParam(1), E)
	case 0x4C:
		// BIT 1, H
		// Test bit 1 of H
		return c.PushInstr(BIT, StaticParam(1), H)
	case 0x4D:
		// BIT 1, L
		// Test bit 1 of L
		return c.PushInstr(BIT, StaticParam(1), L)
	case 0x4E:
		// BIT 1, (HL)
		// Test bit 1 of value pointed by HL
		return c.PushInstr(BIT, StaticParam(1), Indirect{HL})
	case 0x4F:
		// BIT 1, A
		// Test bit 1 of A
		return c.PushInstr(BIT, StaticParam(1), A)
	case 0x50:
		// BIT 2, B
		// Test bit 2 of B
		return c.PushInstr(BIT, StaticParam(2), B)
	case 0x51:
		// BIT 2, C
		// Test bit 2 of C
		return c.PushInstr(BIT, StaticParam(2), C)
	case 0x52:
		// BIT 2, D
		// Test bit 2 of D
		return c.PushInstr(BIT, StaticParam(2), D)
	case 0x53:
		// BIT 2, E
		// Test bit 2 of E
		return c.PushInstr(BIT, StaticParam(2), E)
	case 0x54:
		// BIT 2, H
		// Test bit 2 of H
		return c.PushInstr(BIT, StaticParam(2), H)
	case 0x55:
		// BIT 2, L
		// Test bit 2 of L
		return c.PushInstr(BIT, StaticParam(2), L)
	case 0x56:
		// BIT 2, (HL)
		// Test bit 2 of value pointed by HL
		return c.PushInstr(BIT, StaticParam(2), Indirect{HL})
	case 0x57:
		// BIT 2, A
		// Test bit 2 of A
		return c.PushInstr(BIT, StaticParam(2), A)
	case 0x58:
		// BIT 3, B
		// Test bit 3 of B
		return c.PushInstr(BIT, StaticParam(3), B)
	case 0x59:
		// BIT 3, C
		// Test bit 3 of C
		return c.PushInstr(BIT, StaticParam(3), C)
	case 0x5A:
		// BIT 3, D
		// Test bit 3 of D
		return c.PushInstr(BIT, StaticParam(3), D)
	case 0x5B:
		// BIT 3, E
		// Test bit 3 of E
		return c.PushInstr(BIT, StaticParam(3), E)
	case 0x5C:
		// BIT 3, H
		// Test bit 3 of H
		return c.PushInstr(BIT, StaticParam(3), H)
	case 0x5D:
		// BIT 3, L
		// Test bit 3 of L
		return c.PushInstr(BIT, StaticParam(3), L)
	case 0x5E:
		// BIT 3, (HL)
		// Test bit 3 of value pointed by HL
		return c.PushInstr(BIT, StaticParam(3), Indirect{HL})
	case 0x5F:
		// BIT 3, A
		// Test bit 3 of A
		return c.PushInstr(BIT, StaticParam(3), A)
	case 0x60:
		// BIT 4, B
		// Test bit 4 of B
		return c.PushInstr(BIT, StaticParam(4), B)
	case 0x61:
		// BIT 4, C
		// Test bit 4 of C
		return c.PushInstr(BIT, StaticParam(4), C)
	case 0x62:
		// BIT 4, D
		// Test bit 4 of D
		return c.PushInstr(BIT, StaticParam(4), D)
	case 0x63:
		// BIT 4, E
		// Test bit 4 of E
		return c.PushInstr(BIT, StaticParam(4), E)
	case 0x64:
		// BIT 4, H
		// Test bit 4 of H
		return c.PushInstr(BIT, StaticParam(4), H)
	case 0x65:
		// BIT 4, L
		// Test bit 4 of L
		return c.PushInstr(BIT, StaticParam(4), L)
	case 0x66:
		// BIT 4, (HL)
		// Test bit 4 of value pointed by HL
		return c.PushInstr(BIT, StaticParam(4), Indirect{HL})
	case 0x67:
		// BIT 4, A
		// Test bit 4 of A
		return c.PushInstr(BIT, StaticParam(4), A)
	case 0x68:
		// BIT 5, B
		// Test bit 5 of B
		return c.PushInstr(BIT, StaticParam(5), B)
	case 0x69:
		// BIT 5, C
		// Test bit 5 of C
		return c.PushInstr(BIT, StaticParam(5), C)
	case 0x6A:
		// BIT 5, D
		// Test bit 5 of D
		return c.PushInstr(BIT, StaticParam(5), D)
	case 0x6B:
		// BIT 5, E
		// Test bit 5 of E
		return c.PushInstr(BIT, StaticParam(5), E)
	case 0x6C:
		// BIT 5, H
		// Test bit 5 of H
		return c.PushInstr(BIT, StaticParam(5), H)
	case 0x6D:
		// BIT 5, L
		// Test bit 5 of L
		return c.PushInstr(BIT, StaticParam(5), L)
	case 0x6E:
		// BIT 5, (HL)
		// Test bit 5 of value pointed by HL
		return c.PushInstr(BIT, StaticParam(5), Indirect{HL})
	case 0x6F:
		// BIT 5, A
		// Test bit 5 of A
		return c.PushInstr(BIT, StaticParam(5), A)
	case 0x70:
		// BIT 6, B
		// Test bit 6 of B
		return c.PushInstr(BIT, StaticParam(6), B)
	case 0x71:
		// BIT 6, C
		// Test bit 6 of C
		return c.PushInstr(BIT, StaticParam(6), C)
	case 0x72:
		// BIT 6, D
		// Test bit 6 of D
		return c.PushInstr(BIT, StaticParam(6), D)
	case 0x73:
		// BIT 6, E
		// Test bit 6 of E
		return c.PushInstr(BIT, StaticParam(6), E)
	case 0x74:
		// BIT 6, H
		// Test bit 6 of H
		return c.PushInstr(BIT, StaticParam(6), H)
	case 0x75:
		// BIT 6, L
		// Test bit 6 of L
		return c.PushInstr(BIT, StaticParam(6), L)
	case 0x76:
		// BIT 6, (HL)
		// Test bit 6 of value pointed by HL
		return c.PushInstr(BIT, StaticParam(6), Indirect{HL})
	case 0x77:
		// BIT 6, A
		// Test bit 6 of A
		return c.PushInstr(BIT, StaticParam(6), A)
	case 0x78:
		// BIT 7, B
		// Test bit 7 of B
		return c.PushInstr(BIT, StaticParam(7), B)
	case 0x79:
		// BIT 7, C
		// Test bit 7 of C
		return c.PushInstr(BIT, StaticParam(7), C)
	case 0x7A:
		// BIT 7, D
		// Test bit 7 of D
		return c.PushInstr(BIT, StaticParam(7), D)
	case 0x7B:
		// BIT 7, E
		// Test bit 7 of E
		return c.PushInstr(BIT, StaticParam(7), E)
	case 0x7C:
		// BIT 7, H
		// Test bit 7 of H
		return c.PushInstr(BIT, StaticParam(7), H)
	case 0x7D:
		// BIT 7, L
		// Test bit 7 of L
		return c.PushInstr(BIT, StaticParam(7), L)
	case 0x7E:
		// BIT 7, (HL)
		// Test bit 7 of value pointed by HL
		return c.PushInstr(BIT, StaticParam(7), Indirect{HL})
	case 0x7F:
		// BIT 7, A
		// Test bit 7 of A
		return c.PushInstr(BIT, StaticParam(7), A)
	case 0x80:
		// RES 0, B
		// Clear (reset) bit 0 of B
		return c.PushInstr(RES, StaticParam(0), B)
	case 0x81:
		// RES 0, C
		// Clear (reset) bit 0 of C
		return c.PushInstr(RES, StaticParam(0), C)
	case 0x82:
		// RES 0, D
		// Clear (reset) bit 0 of D
		return c.PushInstr(RES, StaticParam(0), D)
	case 0x83:
		// RES 0, E
		// Clear (reset) bit 0 of E
		return c.PushInstr(RES, StaticParam(0), E)
	case 0x84:
		// RES 0, H
		// Clear (reset) bit 0 of H
		return c.PushInstr(RES, StaticParam(0), H)
	case 0x85:
		// RES 0, L
		// Clear (reset) bit 0 of L
		return c.PushInstr(RES, StaticParam(0), L)
	case 0x86:
		// RES 0, (HL)
		// Clear (reset) bit 0 of value pointed by HL
		return c.PushInstr(RES, StaticParam(0), Indirect{HL})
	case 0x87:
		// RES 0, A
		// Clear (reset) bit 0 of A
		return c.PushInstr(RES, StaticParam(0), A)
	case 0x88:
		// RES 1, B
		// Clear (reset) bit 1 of B
		return c.PushInstr(RES, StaticParam(1), B)
	case 0x89:
		// RES 1, C
		// Clear (reset) bit 1 of C
		return c.PushInstr(RES, StaticParam(1), C)
	case 0x8A:
		// RES 1, D
		// Clear (reset) bit 1 of D
		return c.PushInstr(RES, StaticParam(1), D)
	case 0x8B:
		// RES 1, E
		// Clear (reset) bit 1 of E
		return c.PushInstr(RES, StaticParam(1), E)
	case 0x8C:
		// RES 1, H
		// Clear (reset) bit 1 of H
		return c.PushInstr(RES, StaticParam(1), H)
	case 0x8D:
		// RES 1, L
		// Clear (reset) bit 1 of L
		return c.PushInstr(RES, StaticParam(1), L)
	case 0x8E:
		// RES 1, (HL)
		// Clear (reset) bit 1 of value pointed by HL
		return c.PushInstr(RES, StaticParam(1), Indirect{HL})
	case 0x8F:
		// RES 1, A
		// Clear (reset) bit 1 of A
		return c.PushInstr(RES, StaticParam(1), A)
	case 0x90:
		// RES 2, B
		// Clear (reset) bit 2 of B
		return c.PushInstr(RES, StaticParam(2), B)
	case 0x91:
		// RES 2, C
		// Clear (reset) bit 2 of C
		return c.PushInstr(RES, StaticParam(2), C)
	case 0x92:
		// RES 2, D
		// Clear (reset) bit 2 of D
		return c.PushInstr(RES, StaticParam(2), D)
	case 0x93:
		// RES 2, E
		// Clear (reset) bit 2 of E
		return c.PushInstr(RES, StaticParam(2), E)
	case 0x94:
		// RES 2, H
		// Clear (reset) bit 2 of H
		return c.PushInstr(RES, StaticParam(2), H)
	case 0x95:
		// RES 2, L
		// Clear (reset) bit 2 of L
		return c.PushInstr(RES, StaticParam(2), L)
	case 0x96:
		// RES 2, (HL)
		// Clear (reset) bit 2 of value pointed by HL
		return c.PushInstr(RES, StaticParam(2), Indirect{HL})
	case 0x97:
		// RES 2, A
		// Clear (reset) bit 2 of A
		return c.PushInstr(RES, StaticParam(2), A)
	case 0x98:
		// RES 3, B
		// Clear (reset) bit 3 of B
		return c.PushInstr(RES, StaticParam(3), B)
	case 0x99:
		// RES 3, C
		// Clear (reset) bit 3 of C
		return c.PushInstr(RES, StaticParam(3), C)
	case 0x9A:
		// RES 3, D
		// Clear (reset) bit 3 of D
		return c.PushInstr(RES, StaticParam(3), D)
	case 0x9B:
		// RES 3, E
		// Clear (reset) bit 3 of E
		return c.PushInstr(RES, StaticParam(3), E)
	case 0x9C:
		// RES 3, H
		// Clear (reset) bit 3 of H
		return c.PushInstr(RES, StaticParam(3), H)
	case 0x9D:
		// RES 3, L
		// Clear (reset) bit 3 of L
		return c.PushInstr(RES, StaticParam(3), L)
	case 0x9E:
		// RES 3, (HL)
		// Clear (reset) bit 3 of value pointed by HL
		return c.PushInstr(RES, StaticParam(3), Indirect{HL})
	case 0x9F:
		// RES 3, A
		// Clear (reset) bit 3 of A
		return c.PushInstr(RES, StaticParam(3), A)
	case 0xA0:
		// RES 4, B
		// Clear (reset) bit 4 of B
		return c.PushInstr(RES, StaticParam(4), B)
	case 0xA1:
		// RES 4, C
		// Clear (reset) bit 4 of C
		return c.PushInstr(RES, StaticParam(4), C)
	case 0xA2:
		// RES 4, D
		// Clear (reset) bit 4 of D
		return c.PushInstr(RES, StaticParam(4), D)
	case 0xA3:
		// RES 4, E
		// Clear (reset) bit 4 of E
		return c.PushInstr(RES, StaticParam(4), E)
	case 0xA4:
		// RES 4, H
		// Clear (reset) bit 4 of H
		return c.PushInstr(RES, StaticParam(4), H)
	case 0xA5:
		// RES 4, L
		// Clear (reset) bit 4 of L
		return c.PushInstr(RES, StaticParam(4), L)
	case 0xA6:
		// RES 4, (HL)
		// Clear (reset) bit 4 of value pointed by HL
		return c.PushInstr(RES, StaticParam(4), Indirect{HL})
	case 0xA7:
		// RES 4, A
		// Clear (reset) bit 4 of A
		return c.PushInstr(RES, StaticParam(4), A)
	case 0xA8:
		// RES 5, B
		// Clear (reset) bit 5 of B
		return c.PushInstr(RES, StaticParam(5), B)
	case 0xA9:
		// RES 5, C
		// Clear (reset) bit 5 of C
		return c.PushInstr(RES, StaticParam(5), C)
	case 0xAA:
		// RES 5, D
		// Clear (reset) bit 5 of D
		return c.PushInstr(RES, StaticParam(5), D)
	case 0xAB:
		// RES 5, E
		// Clear (reset) bit 5 of E
		return c.PushInstr(RES, StaticParam(5), E)
	case 0xAC:
		// RES 5, H
		// Clear (reset) bit 5 of H
		return c.PushInstr(RES, StaticParam(5), H)
	case 0xAD:
		// RES 5, L
		// Clear (reset) bit 5 of L
		return c.PushInstr(RES, StaticParam(5), L)
	case 0xAE:
		// RES 5, (HL)
		// Clear (reset) bit 5 of value pointed by HL
		return c.PushInstr(RES, StaticParam(5), Indirect{HL})
	case 0xAF:
		// RES 5, A
		// Clear (reset) bit 5 of A
		return c.PushInstr(RES, StaticParam(5), A)
	case 0xB0:
		// RES 6, B
		// Clear (reset) bit 6 of B
		return c.PushInstr(RES, StaticParam(6), B)
	case 0xB1:
		// RES 6, C
		// Clear (reset) bit 6 of C
		return c.PushInstr(RES, StaticParam(6), C)
	case 0xB2:
		// RES 6, D
		// Clear (reset) bit 6 of D
		return c.PushInstr(RES, StaticParam(6), D)
	case 0xB3:
		// RES 6, E
		// Clear (reset) bit 6 of E
		return c.PushInstr(RES, StaticParam(6), E)
	case 0xB4:
		// RES 6, H
		// Clear (reset) bit 6 of H
		return c.PushInstr(RES, StaticParam(6), H)
	case 0xB5:
		// RES 6, L
		// Clear (reset) bit 6 of L
		return c.PushInstr(RES, StaticParam(6), L)
	case 0xB6:
		// RES 6, (HL)
		// Clear (reset) bit 6 of value pointed by HL
		return c.PushInstr(RES, StaticParam(6), Indirect{HL})
	case 0xB7:
		// RES 6, A
		// Clear (reset) bit 6 of A
		return c.PushInstr(RES, StaticParam(6), A)
	case 0xB8:
		// RES 7, B
		// Clear (reset) bit 7 of B
		return c.PushInstr(RES, StaticParam(7), B)
	case 0xB9:
		// RES 7, C
		// Clear (reset) bit 7 of C
		return c.PushInstr(RES, StaticParam(7), C)
	case 0xBA:
		// RES 7, D
		// Clear (reset) bit 7 of D
		return c.PushInstr(RES, StaticParam(7), D)
	case 0xBB:
		// RES 7, E
		// Clear (reset) bit 7 of E
		return c.PushInstr(RES, StaticParam(7), E)
	case 0xBC:
		// RES 7, H
		// Clear (reset) bit 7 of H
		return c.PushInstr(RES, StaticParam(7), H)
	case 0xBD:
		// RES 7, L
		// Clear (reset) bit 7 of L
		return c.PushInstr(RES, StaticParam(7), L)
	case 0xBE:
		// RES 7, (HL)
		// Clear (reset) bit 7 of value pointed by HL
		return c.PushInstr(RES, StaticParam(7), Indirect{HL})
	case 0xBF:
		// RES 7, A
		// Clear (reset) bit 7 of A
		return c.PushInstr(RES, StaticParam(7), A)
	case 0xC0:
		// SET 0, B
		// Set bit 0 of B
		return c.PushInstr(SET, StaticParam(0), B)
	case 0xC1:
		// SET 0, C
		// Set bit 0 of C
		return c.PushInstr(SET, StaticParam(0), C)
	case 0xC2:
		// SET 0, D
		// Set bit 0 of D
		return c.PushInstr(SET, StaticParam(0), D)
	case 0xC3:
		// SET 0, E
		// Set bit 0 of E
		return c.PushInstr(SET, StaticParam(0), E)
	case 0xC4:
		// SET 0, H
		// Set bit 0 of H
		return c.PushInstr(SET, StaticParam(0), H)
	case 0xC5:
		// SET 0, L
		// Set bit 0 of L
		return c.PushInstr(SET, StaticParam(0), L)
	case 0xC6:
		// SET 0, (HL)
		// Set bit 0 of value pointed by HL
		return c.PushInstr(SET, StaticParam(0), Indirect{HL})
	case 0xC7:
		// SET 0, A
		// Set bit 0 of A
		return c.PushInstr(SET, StaticParam(0), A)
	case 0xC8:
		// SET 1, B
		// Set bit 1 of B
		return c.PushInstr(SET, StaticParam(1), B)
	case 0xC9:
		// SET 1, C
		// Set bit 1 of C
		return c.PushInstr(SET, StaticParam(1), C)
	case 0xCA:
		// SET 1, D
		// Set bit 1 of D
		return c.PushInstr(SET, StaticParam(1), D)
	case 0xCB:
		// SET 1, E
		// Set bit 1 of E
		return c.PushInstr(SET, StaticParam(1), E)
	case 0xCC:
		// SET 1, H
		// Set bit 1 of H
		return c.PushInstr(SET, StaticParam(1), H)
	case 0xCD:
		// SET 1, L
		// Set bit 1 of L
		return c.PushInstr(SET, StaticParam(1), L)
	case 0xCE:
		// SET 1, (HL)
		// Set bit 1 of value pointed by HL
		return c.PushInstr(SET, StaticParam(1), Indirect{HL})
	case 0xCF:
		// SET 1, A
		// Set bit 1 of A
		return c.PushInstr(SET, StaticParam(1), A)
	case 0xD0:
		// SET 2, B
		// Set bit 2 of B
		return c.PushInstr(SET, StaticParam(2), B)
	case 0xD1:
		// SET 2, C
		// Set bit 2 of C
		return c.PushInstr(SET, StaticParam(2), C)
	case 0xD2:
		// SET 2, D
		// Set bit 2 of D
		return c.PushInstr(SET, StaticParam(2), D)
	case 0xD3:
		// SET 2, E
		// Set bit 2 of E
		return c.PushInstr(SET, StaticParam(2), E)
	case 0xD4:
		// SET 2, H
		// Set bit 2 of H
		return c.PushInstr(SET, StaticParam(2), H)
	case 0xD5:
		// SET 2, L
		// Set bit 2 of L
		return c.PushInstr(SET, StaticParam(2), L)
	case 0xD6:
		// SET 2, (HL)
		// Set bit 2 of value pointed by HL
		return c.PushInstr(SET, StaticParam(2), Indirect{HL})
	case 0xD7:
		// SET 2, A
		// Set bit 2 of A
		return c.PushInstr(SET, StaticParam(2), A)
	case 0xD8:
		// SET 3, B
		// Set bit 3 of B
		return c.PushInstr(SET, StaticParam(3), B)
	case 0xD9:
		// SET 3, C
		// Set bit 3 of C
		return c.PushInstr(SET, StaticParam(3), C)
	case 0xDA:
		// SET 3, D
		// Set bit 3 of D
		return c.PushInstr(SET, StaticParam(3), D)
	case 0xDB:
		// SET 3, E
		// Set bit 3 of E
		return c.PushInstr(SET, StaticParam(3), E)
	case 0xDC:
		// SET 3, H
		// Set bit 3 of H
		return c.PushInstr(SET, StaticParam(3), H)
	case 0xDD:
		// SET 3, L
		// Set bit 3 of L
		return c.PushInstr(SET, StaticParam(3), L)
	case 0xDE:
		// SET 3, (HL)
		// Set bit 3 of value pointed by HL
		return c.PushInstr(SET, StaticParam(3), Indirect{HL})
	case 0xDF:
		// SET 3, A
		// Set bit 3 of A
		return c.PushInstr(SET, StaticParam(3), A)
	case 0xE0:
		// SET 4, B
		// Set bit 4 of B
		return c.PushInstr(SET, StaticParam(4), B)
	case 0xE1:
		// SET 4, C
		// Set bit 4 of C
		return c.PushInstr(SET, StaticParam(4), C)
	case 0xE2:
		// SET 4, D
		// Set bit 4 of D
		return c.PushInstr(SET, StaticParam(4), D)
	case 0xE3:
		// SET 4, E
		// Set bit 4 of E
		return c.PushInstr(SET, StaticParam(4), E)
	case 0xE4:
		// SET 4, H
		// Set bit 4 of H
		return c.PushInstr(SET, StaticParam(4), H)
	case 0xE5:
		// SET 4, L
		// Set bit 4 of L
		return c.PushInstr(SET, StaticParam(4), L)
	case 0xE6:
		// SET 4, (HL)
		// Set bit 4 of value pointed by HL
		return c.PushInstr(SET, StaticParam(4), Indirect{HL})
	case 0xE7:
		// SET 4, A
		// Set bit 4 of A
		return c.PushInstr(SET, StaticParam(4), A)
	case 0xE8:
		// SET 5, B
		// Set bit 5 of B
		return c.PushInstr(SET, StaticParam(5), B)
	case 0xE9:
		// SET 5, C
		// Set bit 5 of C
		return c.PushInstr(SET, StaticParam(5), C)
	case 0xEA:
		// SET 5, D
		// Set bit 5 of D
		return c.PushInstr(SET, StaticParam(5), D)
	case 0xEB:
		// SET 5, E
		// Set bit 5 of E
		return c.PushInstr(SET, StaticParam(5), E)
	case 0xEC:
		// SET 5, H
		// Set bit 5 of H
		return c.PushInstr(SET, StaticParam(5), H)
	case 0xED:
		// SET 5, L
		// Set bit 5 of L
		return c.PushInstr(SET, StaticParam(5), L)
	case 0xEE:
		// SET 5, (HL)
		// Set bit 5 of value pointed by HL
		return c.PushInstr(SET, StaticParam(5), Indirect{HL})
	case 0xEF:
		// SET 5, A
		// Set bit 5 of A
		return c.PushInstr(SET, StaticParam(5), A)
	case 0xF0:
		// SET 6, B
		// Set bit 6 of B
		return c.PushInstr(SET, StaticParam(6), B)
	case 0xF1:
		// SET 6, C
		// Set bit 6 of C
		return c.PushInstr(SET, StaticParam(6), C)
	case 0xF2:
		// SET 6, D
		// Set bit 6 of D
		return c.PushInstr(SET, StaticParam(6), D)
	case 0xF3:
		// SET 6, E
		// Set bit 6 of E
		return c.PushInstr(SET, StaticParam(6), E)
	case 0xF4:
		// SET 6, H
		// Set bit 6 of H
		return c.PushInstr(SET, StaticParam(6), H)
	case 0xF5:
		// SET 6, L
		// Set bit 6 of L
		return c.PushInstr(SET, StaticParam(6), L)
	case 0xF6:
		// SET 6, (HL)
		// Set bit 6 of value pointed by HL
		return c.PushInstr(SET, StaticParam(6), Indirect{HL})
	case 0xF7:
		// SET 6, A
		// Set bit 6 of A
		return c.PushInstr(SET, StaticParam(6), A)
	case 0xF8:
		// SET 7, B
		// Set bit 7 of B
		return c.PushInstr(SET, StaticParam(7), B)
	case 0xF9:
		// SET 7, C
		// Set bit 7 of C
		return c.PushInstr(SET, StaticParam(7), C)
	case 0xFA:
		// SET 7, D
		// Set bit 7 of D
		return c.PushInstr(SET, StaticParam(7), D)
	case 0xFB:
		// SET 7, E
		// Set bit 7 of E
		return c.PushInstr(SET, StaticParam(7), E)
	case 0xFC:
		// SET 7, H
		// Set bit 7 of H
		return c.PushInstr(SET, StaticParam(7), H)
	case 0xFD:
		// SET 7, L
		// Set bit 7 of L
		return c.PushInstr(SET, StaticParam(7), L)
	case 0xFE:
		// SET 7, (HL)
		// Set bit 7 of value pointed by HL
		return c.PushInstr(SET, StaticParam(7), Indirect{HL})
	case 0xFF:
		// SET 7, A
		// Set bit 7 of A
		return c.PushInstr(SET, StaticParam(7), A)
	}

	panic("undefined opcode")
}

func CompileOpcode(c *Compiler) error {
	var bytebuf [1]byte
	_, err := c.Input.Read(bytebuf[:])
	if err != nil {
		return err
	}

	pos, err := c.Input.Seek(0, 1)
	if err != nil {
		return err
	}
	fmt.Fprintf(os.Stderr, " opcode: 0x%08x -> 0x%02x\n", pos-1, bytebuf[0])

	switch bytebuf[0] {
	case 0x00:
		// NOP
		// No Operation
		return compileInstrNOP(c)
	case 0x01:
		// LD BC, nn
		// Load 16-bit immediate into BC
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		return compileInstrLD(c, CompositeReg{B, C},
			Immediate16(uint16(operands[1])<<8|uint16(operands[0])))
	case 0x02:
		// LD (BC), A
		// Save A to address pointed by BC
		return c.PushInstr(LD, Indirect{BC}, A)
	case 0x03:
		// INC BC
		// Increment 16-bit BC
		return c.PushInstr(INC, CompositeReg{B, C})
	case 0x04:
		// INC B
		// Increment B
		return c.PushInstr(INC, B)
	case 0x05:
		// DEC B
		// Decrement B
		return c.PushInstr(DEC, B)
	case 0x06:
		// LD B, n
		// Load 8-bit immediate into B
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(LD, B, Immediate8(operands[0]))
	case 0x07:
		// RLC A
		// Rotate A left with carry
		return c.PushInstr(RLC, A)
	case 0x08:
		// LD (nn), SP
		// Save SP to given address
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		return c.PushInstr(LD, Immediate8(uint16(operands[1])<<8|uint16(operands[0])), SP)
	case 0x09:
		// ADD HL, BC
		// Add 16-bit BC to HL
		return c.PushInstr(ADD, CompositeReg{H, L}, CompositeReg{B, C})
	case 0x0A:
		// LD A, (BC)
		// Load A from address pointed to by BC
		return c.PushInstr(LD, A, Indirect{BC})
	case 0x0B:
		// DEC BC
		// Decrement 16-bit BC
		return c.PushInstr(DEC, CompositeReg{B, C})
	case 0x0C:
		// INC C
		// Increment C
		return c.PushInstr(INC, C)
	case 0x0D:
		// DEC C
		// Decrement C
		return c.PushInstr(DEC, C)
	case 0x0E:
		// LD C, n
		// Load 8-bit immediate into C
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(LD, C, Immediate8(operands[0]))
	case 0x0F:
		// RRC A
		// Rotate A right with carry
		return c.PushInstr(RRC, A)
	case 0x10:
		// STOP
		// Stop processor
		return compileInstrSTOP(c)
	case 0x11:
		// LD DE, nn
		// Load 16-bit immediate into DE
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		return c.PushInstr(LD, DE, Immediate16(uint16(operands[1])<<8|uint16(operands[0])))
	case 0x12:
		// LD (DE), A
		// Save A to address pointed by DE
		return c.PushInstr(LD, Indirect{DE}, A)
	case 0x13:
		// INC DE
		// Increment 16-bit DE
		return c.PushInstr(INC, CompositeReg{D, E})
	case 0x14:
		// INC D
		// Increment D
		return c.PushInstr(INC, D)
	case 0x15:
		// DEC D
		// Decrement D
		return c.PushInstr(DEC, D)
	case 0x16:
		// LD D, n
		// Load 8-bit immediate into D
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(LD, D, Immediate8(operands[0]))
	case 0x17:
		// RL A
		// Rotate A left
		return c.PushInstr(RL, A)
	case 0x18:
		// JR n
		// Relative jump by signed immediate
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(JR, Immediate8(operands[0]))
	case 0x19:
		// ADD HL, DE
		// Add 16-bit DE to HL
		return c.PushInstr(ADD, CompositeReg{H, L}, CompositeReg{D, E})
	case 0x1A:
		// LD A, (DE)
		// Load A from address pointed to by DE
		return c.PushInstr(LD, A, Indirect{DE})
	case 0x1B:
		// DEC DE
		// Decrement 16-bit DE
		return c.PushInstr(DEC, CompositeReg{D, E})
	case 0x1C:
		// INC E
		// Increment E
		return c.PushInstr(INC, E)
	case 0x1D:
		// DEC E
		// Decrement E
		return c.PushInstr(DEC, E)
	case 0x1E:
		// LD E, n
		// Load 8-bit immediate into E
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(LD, E, Immediate8(operands[0]))
	case 0x1F:
		// RR A
		// Rotate A right
		return c.PushInstr(RR, A)
	case 0x20:
		// JR NZ, n
		// Relative jump by signed immediate if last result was not zero
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(JR, IfNZ, Immediate8(operands[0]))
	case 0x21:
		// LD HL, nn
		// Load 16-bit immediate into HL
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		return c.PushInstr(LD, HL, Immediate16(uint16(operands[1])<<8|uint16(operands[0])))
	case 0x22:
		// LDI (HL), A
		// Save A to address pointed by HL, and increment HL
		return c.PushInstr(LDI, Indirect{HL}, A)
	case 0x23:
		// INC HL
		// Increment 16-bit HL
		return c.PushInstr(INC, HL)
	case 0x24:
		// INC H
		// Increment H
		return c.PushInstr(INC, H)
	case 0x25:
		// DEC H
		// Decrement H
		return c.PushInstr(DEC, H)
	case 0x26:
		// LD H, n
		// Load 8-bit immediate into H
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(LD, H, Immediate8(operands[0]))
	case 0x27:
		// DAA
		// Adjust A for BCD addition
		return compileInstrDAA(c)
	case 0x28:
		// JR Z, n
		// Relative jump by signed immediate if last result was zero
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(JR, IfZ, Immediate8(operands[0]))
	case 0x29:
		// ADD HL, HL
		// Add 16-bit HL to HL
		return c.PushInstr(ADD, HL, HL)
	case 0x2A:
		// LDI A, (HL)
		// Load A from address pointed to by HL, and increment HL
		return c.PushInstr(LDI, A, Indirect{HL})
	case 0x2B:
		// DEC HL
		// Decrement 16-bit HL
		return c.PushInstr(DEC, HL)
	case 0x2C:
		// INC L
		// Increment L
		return c.PushInstr(INC, L)
	case 0x2D:
		// DEC L
		// Decrement L
		return c.PushInstr(DEC, L)
	case 0x2E:
		// LD L, n
		// Load 8-bit immediate into L
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(LD, L, Immediate8(operands[0]))
	case 0x2F:
		// CPL
		// Complement (logical NOT) on A
		return compileInstrCPL(c)
	case 0x30:
		// JR NC, n
		// Relative jump by signed immediate if last result caused no carry
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(JR, IfNC, Immediate8(operands[0]))
	case 0x31:
		// LD SP, nn
		// Load 16-bit immediate into SP
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		return c.PushInstr(LD, SP, Immediate16(uint16(operands[1])<<8|uint16(operands[0])))
	case 0x32:
		// LDD (HL), A
		// Save A to address pointed by HL, and decrement HL
		return c.PushInstr(LDD, Indirect{HL}, A)
	case 0x33:
		// INC SP
		// Increment 16-bit HL
		return c.PushInstr(INC, SP)
	case 0x34:
		// INC (HL)
		// Increment value pointed by HL
		return c.PushInstr(INC, Indirect{HL})
	case 0x35:
		// DEC (HL)
		// Decrement value pointed by HL
		return c.PushInstr(DEC, Indirect{HL})
	case 0x36:
		// LD (HL), n
		// Load 8-bit immediate into address pointed by HL
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(LD, Indirect{HL}, Immediate8(operands[0]))
	case 0x37:
		// SCF
		// Set carry flag
		return compileInstrSCF(c)
	case 0x38:
		// JR C, n
		// Relative jump by signed immediate if last result caused carry
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(JR, IfC, Immediate8(operands[0]))
	case 0x39:
		// ADD HL, SP
		// Add 16-bit SP to HL
		return c.PushInstr(ADD, HL, SP)
	case 0x3A:
		// LDD A, (HL)
		// Load A from address pointed to by HL, and decrement HL
		return c.PushInstr(LDD, A, Indirect{HL})
	case 0x3B:
		// DEC SP
		// Decrement 16-bit SP
		return c.PushInstr(DEC, SP)
	case 0x3C:
		// INC A
		// Increment A
		return c.PushInstr(INC, A)
	case 0x3D:
		// DEC A
		// Decrement A
		return c.PushInstr(DEC, A)
	case 0x3E:
		// LD A, n
		// Load 8-bit immediate into A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(LD, A, Immediate8(operands[0]))
	case 0x3F:
		// CCF
		// Clear carry flag
		return compileInstrCCF(c)
	case 0x40:
		// LD B, B
		// Copy B to B
		return c.PushInstr(LD, B, B)
	case 0x41:
		// LD B, C
		// Copy C to B
		return c.PushInstr(LD, B, C)
	case 0x42:
		// LD B, D
		// Copy D to B
		return c.PushInstr(LD, B, D)
	case 0x43:
		// LD B, E
		// Copy E to B
		return c.PushInstr(LD, B, E)
	case 0x44:
		// LD B, H
		// Copy H to B
		return c.PushInstr(LD, B, H)
	case 0x45:
		// LD B, L
		// Copy L to B
		return c.PushInstr(LD, B, L)
	case 0x46:
		// LD B, (HL)
		// Copy value pointed by HL to B
		return c.PushInstr(LD, B, Indirect{HL})
	case 0x47:
		// LD B, A
		// Copy A to B
		return c.PushInstr(LD, B, A)
	case 0x48:
		// LD C, B
		// Copy B to C
		return c.PushInstr(LD, C, B)
	case 0x49:
		// LD C, C
		// Copy C to C
		return c.PushInstr(LD, C, C)
	case 0x4A:
		// LD C, D
		// Copy D to C
		return c.PushInstr(LD, C, D)
	case 0x4B:
		// LD C, E
		// Copy E to C
		return c.PushInstr(LD, C, E)
	case 0x4C:
		// LD C, H
		// Copy H to C
		return c.PushInstr(LD, C, H)
	case 0x4D:
		// LD C, L
		// Copy L to C
		return c.PushInstr(LD, C, L)
	case 0x4E:
		// LD C, (HL)
		// Copy value pointed by HL to C
		return c.PushInstr(LD, C, Indirect{HL})
	case 0x4F:
		// LD C, A
		// Copy A to C
		return c.PushInstr(LD, C, A)
	case 0x50:
		// LD D, B
		// Copy B to D
		return c.PushInstr(LD, D, B)
	case 0x51:
		// LD D, C
		// Copy C to D
		return c.PushInstr(LD, D, C)
	case 0x52:
		// LD D, D
		// Copy D to D
		return c.PushInstr(LD, D, D)
	case 0x53:
		// LD D, E
		// Copy E to D
		return c.PushInstr(LD, D, E)
	case 0x54:
		// LD D, H
		// Copy H to D
		return c.PushInstr(LD, D, H)
	case 0x55:
		// LD D, L
		// Copy L to D
		return c.PushInstr(LD, D, L)
	case 0x56:
		// LD D, (HL)
		// Copy value pointed by HL to D
		return c.PushInstr(LD, D, Indirect{HL})
	case 0x57:
		// LD D, A
		// Copy A to D
		return c.PushInstr(LD, D, A)
	case 0x58:
		// LD E, B
		// Copy B to E
		return c.PushInstr(LD, E, B)
	case 0x59:
		// LD E, C
		// Copy C to E
		return c.PushInstr(LD, E, C)
	case 0x5A:
		// LD E, D
		// Copy D to E
		return c.PushInstr(LD, E, D)
	case 0x5B:
		// LD E, E
		// Copy E to E
		return c.PushInstr(LD, E, E)
	case 0x5C:
		// LD E, H
		// Copy H to E
		return c.PushInstr(LD, E, H)
	case 0x5D:
		// LD E, L
		// Copy L to E
		return c.PushInstr(LD, E, L)
	case 0x5E:
		// LD E, (HL)
		// Copy value pointed by HL to E
		return c.PushInstr(LD, E, Indirect{HL})
	case 0x5F:
		// LD E, A
		// Copy A to E
		return c.PushInstr(LD, E, A)
	case 0x60:
		// LD H, B
		// Copy B to H
		return c.PushInstr(LD, H, B)
	case 0x61:
		// LD H, C
		// Copy C to H
		return c.PushInstr(LD, H, C)
	case 0x62:
		// LD H, D
		// Copy D to H
		return c.PushInstr(LD, H, D)
	case 0x63:
		// LD H, E
		// Copy E to H
		return c.PushInstr(LD, H, E)
	case 0x64:
		// LD H, H
		// Copy H to H
		return c.PushInstr(LD, H, H)
	case 0x65:
		// LD H, L
		// Copy L to H
		return c.PushInstr(LD, H, L)
	case 0x66:
		// LD H, (HL)
		// Copy value pointed by HL to H
		return c.PushInstr(LD, H, Indirect{HL})
	case 0x67:
		// LD H, A
		// Copy A to H
		return c.PushInstr(LD, H, A)
	case 0x68:
		// LD L, B
		// Copy B to L
		return c.PushInstr(LD, L, B)
	case 0x69:
		// LD L, C
		// Copy C to L
		return c.PushInstr(LD, L, C)
	case 0x6A:
		// LD L, D
		// Copy D to L
		return c.PushInstr(LD, L, D)
	case 0x6B:
		// LD L, E
		// Copy E to L
		return c.PushInstr(LD, L, E)
	case 0x6C:
		// LD L, H
		// Copy H to L
		return c.PushInstr(LD, L, H)
	case 0x6D:
		// LD L, L
		// Copy L to L
		return c.PushInstr(LD, L, L)
	case 0x6E:
		// LD L, (HL)
		// Copy value pointed by HL to L
		return c.PushInstr(LD, L, Indirect{HL})
	case 0x6F:
		// LD L, A
		// Copy A to L
		return c.PushInstr(LD, L, A)
	case 0x70:
		// LD (HL), B
		// Copy B to address pointed by HL
		return c.PushInstr(LD, Indirect{HL}, B)
	case 0x71:
		// LD (HL), C
		// Copy C to address pointed by HL
		return c.PushInstr(LD, Indirect{HL}, C)
	case 0x72:
		// LD (HL), D
		// Copy D to address pointed by HL
		return c.PushInstr(LD, Indirect{HL}, D)
	case 0x73:
		// LD (HL), E
		// Copy E to address pointed by HL
		return c.PushInstr(LD, Indirect{HL}, E)
	case 0x74:
		// LD (HL), H
		// Copy H to address pointed by HL
		return c.PushInstr(LD, Indirect{HL}, H)
	case 0x75:
		// LD (HL), L
		// Copy L to address pointed by HL
		return c.PushInstr(LD, Indirect{HL}, L)
	case 0x76:
		// HALT
		// Halt processor
		return compileInstrHALT(c)
	case 0x77:
		// LD (HL), A
		// Copy A to address pointed by HL
		return c.PushInstr(LD, Indirect{HL}, A)
	case 0x78:
		// LD A, B
		// Copy B to A
		return c.PushInstr(LD, A, B)
	case 0x79:
		// LD A, C
		// Copy C to A
		return c.PushInstr(LD, A, C)
	case 0x7A:
		// LD A, D
		// Copy D to A
		return c.PushInstr(LD, A, D)
	case 0x7B:
		// LD A, E
		// Copy E to A
		return c.PushInstr(LD, A, E)
	case 0x7C:
		// LD A, H
		// Copy H to A
		return c.PushInstr(LD, A, H)
	case 0x7D:
		// LD A, L
		// Copy L to A
		return c.PushInstr(LD, A, L)
	case 0x7E:
		// LD A, (HL)
		// Copy value pointed by HL to A
		return c.PushInstr(LD, A, Indirect{HL})
	case 0x7F:
		// LD A, A
		// Copy A to A
		return c.PushInstr(LD, A, A)
	case 0x80:
		// ADD A, B
		// Add B to A
		return c.PushInstr(ADD, A, B)
	case 0x81:
		// ADD A, C
		// Add C to A
		return c.PushInstr(ADD, A, C)
	case 0x82:
		// ADD A, D
		// Add D to A
		return c.PushInstr(ADD, A, D)
	case 0x83:
		// ADD A, E
		// Add E to A
		return c.PushInstr(ADD, A, E)
	case 0x84:
		// ADD A, H
		// Add H to A
		return c.PushInstr(ADD, A, H)
	case 0x85:
		// ADD A, L
		// Add L to A
		return c.PushInstr(ADD, A, L)
	case 0x86:
		// ADD A, (HL)
		// Add value pointed by HL to A
		return c.PushInstr(ADD, A, Indirect{HL})
	case 0x87:
		// ADD A, A
		// Add A to A
		return c.PushInstr(ADD, A, A)
	case 0x88:
		// ADC A, B
		// Add B and carry flag to A
		return c.PushInstr(ADC, A, B)
	case 0x89:
		// ADC A, C
		// Add C and carry flag to A
		return c.PushInstr(ADC, A, C)
	case 0x8A:
		// ADC A, D
		// Add D and carry flag to A
		return c.PushInstr(ADC, A, D)
	case 0x8B:
		// ADC A, E
		// Add E and carry flag to A
		return c.PushInstr(ADC, A, E)
	case 0x8C:
		// ADC A, H
		// Add H and carry flag to A
		return c.PushInstr(ADC, A, H)
	case 0x8D:
		// ADC A, L
		// Add and carry flag L to A
		return c.PushInstr(ADC, A, L)
	case 0x8E:
		// ADC A, (HL)
		// Add value pointed by HL and carry flag to A
		return c.PushInstr(ADC, A, Indirect{HL})
	case 0x8F:
		// ADC A, A
		// Add A and carry flag to A
		return c.PushInstr(ADC, A, A)
	case 0x90:
		// SUB A, B
		// Subtract B from A
		return c.PushInstr(SUB, A, B)
	case 0x91:
		// SUB A, C
		// Subtract C from A
		return c.PushInstr(SUB, A, C)
	case 0x92:
		// SUB A, D
		// Subtract D from A
		return c.PushInstr(SUB, A, D)
	case 0x93:
		// SUB A, E
		// Subtract E from A
		return c.PushInstr(SUB, A, E)
	case 0x94:
		// SUB A, H
		// Subtract H from A
		return c.PushInstr(SUB, A, H)
	case 0x95:
		// SUB A, L
		// Subtract L from A
		return c.PushInstr(SUB, A, L)
	case 0x96:
		// SUB A, (HL)
		// Subtract value pointed by HL from A
		return c.PushInstr(SUB, A, Indirect{HL})
	case 0x97:
		// SUB A, A
		// Subtract A from A
		return c.PushInstr(SUB, A, A)
	case 0x98:
		// SBC A, B
		// Subtract B and carry flag from A
		return c.PushInstr(SBC, A, B)
	case 0x99:
		// SBC A, C
		// Subtract C and carry flag from A
		return c.PushInstr(SBC, A, C)
	case 0x9A:
		// SBC A, D
		// Subtract D and carry flag from A
		return c.PushInstr(SBC, A, D)
	case 0x9B:
		// SBC A, E
		// Subtract E and carry flag from A
		return c.PushInstr(SBC, A, E)
	case 0x9C:
		// SBC A, H
		// Subtract H and carry flag from A
		return c.PushInstr(SBC, A, H)
	case 0x9D:
		// SBC A, L
		// Subtract and carry flag L from A
		return c.PushInstr(SBC, A, L)
	case 0x9E:
		// SBC A, (HL)
		// Subtract value pointed by HL and carry flag from A
		return c.PushInstr(SBC, A, Indirect{HL})
	case 0x9F:
		// SBC A, A
		// Subtract A and carry flag from A
		return c.PushInstr(SBC, A, A)
	case 0xA0:
		// AND B
		// Logical AND B against A
		return c.PushInstr(AND, B)
	case 0xA1:
		// AND C
		// Logical AND C against A
		return c.PushInstr(AND, C)
	case 0xA2:
		// AND D
		// Logical AND D against A
		return c.PushInstr(AND, D)
	case 0xA3:
		// AND E
		// Logical AND E against A
		return c.PushInstr(AND, E)
	case 0xA4:
		// AND H
		// Logical AND H against A
		return c.PushInstr(AND, H)
	case 0xA5:
		// AND L
		// Logical AND L against A
		return c.PushInstr(AND, L)
	case 0xA6:
		// AND (HL)
		// Logical AND value pointed by HL against A
		return c.PushInstr(AND, Indirect{HL})
	case 0xA7:
		// AND A
		// Logical AND A against A
		return c.PushInstr(AND, A)
	case 0xA8:
		// XOR B
		// Logical XOR B against A
		return c.PushInstr(XOR, B)
	case 0xA9:
		// XOR C
		// Logical XOR C against A
		return c.PushInstr(XOR, C)
	case 0xAA:
		// XOR D
		// Logical XOR D against A
		return c.PushInstr(XOR, D)
	case 0xAB:
		// XOR E
		// Logical XOR E against A
		return c.PushInstr(XOR, E)
	case 0xAC:
		// XOR H
		// Logical XOR H against A
		return c.PushInstr(XOR, H)
	case 0xAD:
		// XOR L
		// Logical XOR L against A
		return c.PushInstr(XOR, L)
	case 0xAE:
		// XOR (HL)
		// Logical XOR value pointed by HL against A
		return c.PushInstr(XOR, Indirect{HL})
	case 0xAF:
		// XOR A
		// Logical XOR A against A
		return c.PushInstr(XOR, A)
	case 0xB0:
		// OR B
		// Logical OR B against A
		return c.PushInstr(OR, B)
	case 0xB1:
		// OR C
		// Logical OR C against A
		return c.PushInstr(OR, C)
	case 0xB2:
		// OR D
		// Logical OR D against A
		return c.PushInstr(OR, D)
	case 0xB3:
		// OR E
		// Logical OR E against A
		return c.PushInstr(OR, E)
	case 0xB4:
		// OR H
		// Logical OR H against A
		return c.PushInstr(OR, H)
	case 0xB5:
		// OR L
		// Logical OR L against A
		return c.PushInstr(OR, L)
	case 0xB6:
		// OR (HL)
		// Logical OR value pointed by HL against A
		return c.PushInstr(OR, Indirect{HL})
	case 0xB7:
		// OR A
		// Logical OR A against A
		return c.PushInstr(OR, A)
	case 0xB8:
		// CP B
		// Compare B against A
		return c.PushInstr(CP, B)
	case 0xB9:
		// CP C
		// Compare C against A
		return c.PushInstr(CP, C)
	case 0xBA:
		// CP D
		// Compare D against A
		return c.PushInstr(CP, D)
	case 0xBB:
		// CP E
		// Compare E against A
		return c.PushInstr(CP, E)
	case 0xBC:
		// CP H
		// Compare H against A
		return c.PushInstr(CP, H)
	case 0xBD:
		// CP L
		// Compare L against A
		return c.PushInstr(CP, L)
	case 0xBE:
		// CP (HL)
		// Compare value pointed by HL against A
		return c.PushInstr(CP, Indirect{HL})
	case 0xBF:
		// CP A
		// Compare A against A
		return c.PushInstr(CP, A)
	case 0xC0:
		// RET NZ
		// Return if last result was not zero
		return c.PushInstr(RET, IfNZ)
	case 0xC1:
		// POP BC
		// Pop 16-bit value from stack into BC
		return c.PushInstr(POP, BC)
	case 0xC2:
		// JP NZ, nn
		// Absolute jump to 16-bit location if last result was not zero
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		return c.PushInstr(JP, IfNZ, Immediate16(uint16(operands[1])<<8|uint16(operands[0])))
	case 0xC3:
		// JP nn
		// Absolute jump to 16-bit location
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		return c.PushInstr(JP, Immediate16(uint16(operands[1])<<8|uint16(operands[0])))
	case 0xC4:
		// CALL NZ, nn
		// Call routine at 16-bit location if last result was not zero
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		return c.PushInstr(CALL, IfNZ, Immediate16(uint16(operands[1])<<8|uint16(operands[0])))
	case 0xC5:
		// PUSH BC
		// Push 16-bit BC onto stack
		return c.PushInstr(PUSH, BC)
	case 0xC6:
		// ADD A, n
		// Add 8-bit immediate to A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(ADD, A, Immediate8(operands[0]))
	case 0xC7:
		// RST 0
		// Call routine at address 0000h
		return c.PushInstr(RST, StaticParam(0))
	case 0xC8:
		// RET Z
		// Return if last result was zero
		return c.PushInstr(RET, IfZ)
	case 0xC9:
		// RET
		// Return to calling routine
		return compileInstrRET(c)
	case 0xCA:
		// JP Z, nn
		// Absolute jump to 16-bit location if last result was zero
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		return c.PushInstr(JP, IfZ, Immediate16(uint16(operands[1])<<8|uint16(operands[0])))
	case 0xCB:
		return a.compileExtCB(c)
	case 0xCC:
		// CALL Z, nn
		// Call routine at 16-bit location if last result was zero
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		return c.PushInstr(CALL, IfZ, Immediate16(uint16(operands[1])<<8|uint16(operands[0])))
	case 0xCD:
		// CALL nn
		// Call routine at 16-bit location
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		return c.PushInstr(CALL, Immediate16(uint16(operands[1])<<8|uint16(operands[0])))
	case 0xCE:
		// ADC A, n
		// Add 8-bit immediate and carry to A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(ADC, A, Immediate8(operands[0]))
	case 0xCF:
		// RST 8
		// Call routine at address 0008h
		return c.PushInstr(RST, StaticParam(8))
	case 0xD0:
		// RET NC
		// Return if last result caused no carry
		return c.PushInstr(RET, IfNC)
	case 0xD1:
		// POP DE
		// Pop 16-bit value from stack into DE
		return c.PushInstr(POP, DE)
	case 0xD2:
		// JP NC, nn
		// Absolute jump to 16-bit location if last result caused no carry
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		return c.PushInstr(JP, IfNC, Immediate16(uint16(operands[1])<<8|uint16(operands[0])))
	case 0xD3: // XX
		break
	case 0xD4:
		// CALL NC, nn
		// Call routine at 16-bit location if last result caused no carry
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		return c.PushInstr(CALL, IfNC, Immediate16(uint16(operands[1])<<8|uint16(operands[0])))
	case 0xD5:
		// PUSH DE
		// Push 16-bit DE onto stack
		return c.PushInstr(PUSH, DE)
	case 0xD6:
		// SUB A, n
		// Subtract 8-bit immediate from A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(SUB, A, Immediate8(operands[0]))
	case 0xD7:
		// RST 10
		// Call routine at address 0010h
		return c.PushInstr(RST, StaticParam(10))
	case 0xD8:
		// RET C
		// Return if last result caused carry
		return c.PushInstr(RET, C)
	case 0xD9:
		// RETI
		// Enable interrupts and return to calling routine
		return compileInstrRETI(c)
	case 0xDA:
		// JP C, nn
		// Absolute jump to 16-bit location if last result caused carry
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		return c.PushInstr(JP, C, Immediate16(uint16(operands[1])<<8|uint16(operands[0])))
	case 0xDB: // XX
		break
	case 0xDC:
		// CALL C, nn
		// Call routine at 16-bit location if last result caused carry
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		return c.PushInstr(CALL, C, Immediate16(uint16(operands[1])<<8|uint16(operands[0])))
	case 0xDD: // XX
		break
	case 0xDE:
		// SBC A, n
		// Subtract 8-bit immediate and carry from A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(SBC, A, Immediate8(operands[0]))
	case 0xDF:
		// RST 18
		// Call routine at address 0018h
		return c.PushInstr(RST, StaticParam(18))
	case 0xE0:
		// LDH (n), A
		// Save A at address pointed to by (FF00h + 8-bit immediate)
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(LDH, Immediate8(operands[0]), A)
	case 0xE1:
		// POP HL
		// Pop 16-bit value from stack into HL
		return c.PushInstr(POP, HL)
	case 0xE2:
		// LDH (C), A
		// Save A at address pointed to by (FF00h + C)
		return c.PushInstr(LDH, Indirect{C}, A)
	case 0xE3: // XX
	case 0xE4: // XX
		break
	case 0xE5:
		// PUSH HL
		// Push 16-bit HL onto stack
		return c.PushInstr(PUSH, HL)
	case 0xE6:
		// AND n
		// Logical AND 8-bit immediate against A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(AND, Immediate8(operands[0]))
	case 0xE7:
		// RST 20
		// Call routine at address 0020h
		return c.PushInstr(RST, StaticParam(20))
	case 0xE8:
		// ADD SP, d
		// Add signed 8-bit immediate to SP
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(ADD, SP, Immediate8(operands[0]))
	case 0xE9:
		// JP (HL)
		// Jump to 16-bit value pointed by HL
		return c.PushInstr(JP, Indirect{HL})
	case 0xEA:
		// LD (nn), A
		// Save A at given 16-bit address
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		return c.PushInstr(LD, Indirect{Immediate16(uint16(operands[1])<<8 | uint16(operands[0]))}, A)
	case 0xEB: // XX
	case 0xEC: // XX
	case 0xED: // XX
		break
	case 0xEE:
		// XOR n
		// Logical XOR 8-bit immediate against A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(XOR, Immediate8(operands[0]))
	case 0xEF:
		// RST 28
		// Call routine at address 0028h
		return c.PushInstr(RST, StaticParam(28))
	case 0xF0:
		// LDH A, (n)
		// Load A from address pointed to by (FF00h + 8-bit immediate)
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(LDH, A, Immediate8(operands[0]))
	case 0xF1:
		// POP AF
		// Pop 16-bit value from stack into AF
		return c.PushInstr(POP, AF)
	case 0xF2:
		return c.PushInstr(LD, A, Indirect{C})
	case 0xF3:
		// DI
		// DIsable interrupts
		return compileInstrDI(c)
	case 0xF4: // XX
		break
	case 0xF5:
		// PUSH AF
		// Push 16-bit AF onto stack
		return c.PushInstr(PUSH, AF)
	case 0xF6:
		// OR n
		// Logical OR 8-bit immediate against A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(OR, Immediate8(operands[0]))
	case 0xF7:
		// RST 30
		// Call routine at address 0030h
		return c.PushInstr(RST, StaticParam(30))
	case 0xF8:
		// LDHL SP, d
		// Add signed 8-bit immediate to SP and save result in HL
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(LDHL, SP, Immediate8(operands[0]))
	case 0xF9:
		// LD SP, HL
		// Copy HL to SP
		return c.PushInstr(LD, SP, HL)
	case 0xFA:
		// LD A, (nn)
		// Load A from given 16-bit address
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		return c.PushInstr(LD, A, Indirect{Immediate16(uint16(operands[1])<<8 | uint16(operands[0]))})
	case 0xFB:
		// EI
		// Enable interrupts
		return compileInstrEI(c)
	case 0xFC: // XX
	case 0xFD: // XX
		break
	case 0xFE:
		// CP n
		// Compare 8-bit immediate against A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		return c.PushInstr(CP, Immediate8(operands[0]))
	case 0xFF:
		// RST 38
		// Call routine at address 0038h
		return c.PushInstr(RST, StaticParam(38))
	}

	return InvalidOpcode(bytebuf[0])
}
