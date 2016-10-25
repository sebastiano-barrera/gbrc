package parser

// Machine-generated part of the parser.
//
// The main item of this file is the implementation of the
// `DecodeOpcode` method of `parser`.
//
// The implementation is based on a couple of big switch statements
// that translate a CPU opcode to a call to the PushInstr method of; return nil
// the passed parser instance.

import (
	"fmt"
	"io"
	"os"
)

//
// Errors
//
type (
	InvalidOpcode  uint8
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

func (ro ReadOnlyWrite) Error() string {
	return fmt.Sprintf("place is read-only: %v", ro.place)
}

func (invOp InvalidOperand) Error() string {
	return fmt.Sprintf("invalid operand for %s: %v (should be %s)",
		invOp.instruction, invOp.operand, invOp.shouldBe)
}

func compileExtCB(c *parser) error {
	var bytebuf [1]byte
	_, err := c.input.Read(bytebuf[:])
	if err != nil {
		return err
	}

	switch bytebuf[0] {
	case 0x00:
		// RLC B
		// Rotate B left with carry
		c.PushInstr(RLC, B)
		return nil
	case 0x01:
		// RLC C
		// Rotate C left with carry
		c.PushInstr(RLC, C)
		return nil
	case 0x02:
		// RLC D
		// Rotate D left with carry
		c.PushInstr(RLC, D)
		return nil
	case 0x03:
		// RLC E
		// Rotate E left with carry
		c.PushInstr(RLC, E)
		return nil
	case 0x04:
		// RLC H
		// Rotate H left with carry
		c.PushInstr(RLC, H)
		return nil
	case 0x05:
		// RLC L
		// Rotate L left with carry
		c.PushInstr(RLC, L)
		return nil
	case 0x06:
		// RLC (HL)
		// Rotate value pointed by HL left with carry
		c.PushInstr(RLC, Indirect{HL})
		return nil
	case 0x07:
		// RLC A
		// Rotate A left with carry
		c.PushInstr(RLC, A)
		return nil
	case 0x08:
		// RRC B
		// Rotate B right with carry
		c.PushInstr(RRC, B)
		return nil
	case 0x09:
		// RRC C
		// Rotate C right with carry
		c.PushInstr(RRC, C)
		return nil
	case 0x0A:
		// RRC D
		// Rotate D right with carry
		c.PushInstr(RRC, D)
		return nil
	case 0x0B:
		// RRC E
		// Rotate E right with carry
		c.PushInstr(RRC, E)
		return nil
	case 0x0C:
		// RRC H
		// Rotate H right with carry
		c.PushInstr(RRC, H)
		return nil
	case 0x0D:
		// RRC L
		// Rotate L right with carry
		c.PushInstr(RRC, L)
		return nil
	case 0x0E:
		// RRC (HL)
		// Rotate value pointed by HL right with carry
		c.PushInstr(RRC, Indirect{HL})
		return nil
	case 0x0F:
		// RRC A
		// Rotate A right with carry
		c.PushInstr(RRC, A)
		return nil
	case 0x10:
		// RL B
		// Rotate B left
		c.PushInstr(RL, B)
		return nil
	case 0x11:
		// RL C
		// Rotate C left
		c.PushInstr(RL, C)
		return nil
	case 0x12:
		// RL D
		// Rotate D left
		c.PushInstr(RL, D)
		return nil
	case 0x13:
		// RL E
		// Rotate E left
		c.PushInstr(RL, E)
		return nil
	case 0x14:
		// RL H
		// Rotate H left
		c.PushInstr(RL, H)
		return nil
	case 0x15:
		// RL L
		// Rotate L left
		c.PushInstr(RL, L)
		return nil
	case 0x16:
		// RL (HL)
		// Rotate value pointed by HL left
		c.PushInstr(RL, Indirect{HL})
		return nil
	case 0x17:
		// RL A
		// Rotate A left
		c.PushInstr(RL, A)
		return nil
	case 0x18:
		// RR B
		// Rotate B right
		c.PushInstr(RR, B)
		return nil
	case 0x19:
		// RR C
		// Rotate C right
		c.PushInstr(RR, C)
		return nil
	case 0x1A:
		// RR D
		// Rotate D right
		c.PushInstr(RR, D)
		return nil
	case 0x1B:
		// RR E
		// Rotate E right
		c.PushInstr(RR, E)
		return nil
	case 0x1C:
		// RR H
		// Rotate H right
		c.PushInstr(RR, H)
		return nil
	case 0x1D:
		// RR L
		// Rotate L right
		c.PushInstr(RR, L)
		return nil
	case 0x1E:
		// RR (HL)
		// Rotate value pointed by HL right
		c.PushInstr(RR, Indirect{HL})
		return nil
	case 0x1F:
		// RR A
		// Rotate A right
		c.PushInstr(RR, A)
		return nil
	case 0x20:
		// SLA B
		// Shift B left preserving sign
		c.PushInstr(SLA, B)
		return nil
	case 0x21:
		// SLA C
		// Shift C left preserving sign
		c.PushInstr(SLA, C)
		return nil
	case 0x22:
		// SLA D
		// Shift D left preserving sign
		c.PushInstr(SLA, D)
		return nil
	case 0x23:
		// SLA E
		// Shift E left preserving sign
		c.PushInstr(SLA, E)
		return nil
	case 0x24:
		// SLA H
		// Shift H left preserving sign
		c.PushInstr(SLA, H)
		return nil
	case 0x25:
		// SLA L
		// Shift L left preserving sign
		c.PushInstr(SLA, L)
		return nil
	case 0x26:
		// SLA (HL)
		// Shift value pointed by HL left preserving sign
		c.PushInstr(SLA, Indirect{HL})
		return nil
	case 0x27:
		// SLA A
		// Shift A left preserving sign
		c.PushInstr(SLA, A)
		return nil
	case 0x28:
		// SRA B
		// Shift B right preserving sign
		c.PushInstr(SRA, B)
		return nil
	case 0x29:
		// SRA C
		// Shift C right preserving sign
		c.PushInstr(SRA, C)
		return nil
	case 0x2A:
		// SRA D
		// Shift D right preserving sign
		c.PushInstr(SRA, D)
		return nil
	case 0x2B:
		// SRA E
		// Shift E right preserving sign
		c.PushInstr(SRA, E)
		return nil
	case 0x2C:
		// SRA H
		// Shift H right preserving sign
		c.PushInstr(SRA, H)
		return nil
	case 0x2D:
		// SRA L
		// Shift L right preserving sign
		c.PushInstr(SRA, L)
		return nil
	case 0x2E:
		// SRA (HL)
		// Shift value pointed by HL right preserving sign
		c.PushInstr(SRA, Indirect{HL})
		return nil
	case 0x2F:
		// SRA A
		// Shift A right preserving sign
		c.PushInstr(SRA, A)
		return nil
	case 0x30:
		// SWAP B
		// Swap nybbles in B
		c.PushInstr(SWAP, B)
		return nil
	case 0x31:
		// SWAP C
		// Swap nybbles in C
		c.PushInstr(SWAP, C)
		return nil
	case 0x32:
		// SWAP D
		// Swap nybbles in D
		c.PushInstr(SWAP, D)
		return nil
	case 0x33:
		// SWAP E
		// Swap nybbles in E
		c.PushInstr(SWAP, E)
		return nil
	case 0x34:
		// SWAP H
		// Swap nybbles in H
		c.PushInstr(SWAP, H)
		return nil
	case 0x35:
		// SWAP L
		// Swap nybbles in L
		c.PushInstr(SWAP, L)
		return nil
	case 0x36:
		// SWAP (HL)
		// Swap nybbles in value pointed by HL
		c.PushInstr(SWAP, Indirect{HL})
		return nil
	case 0x37:
		// SWAP A
		// Swap nybbles in A
		c.PushInstr(SWAP, A)
		return nil
	case 0x38:
		// SRL B
		// Shift B right
		c.PushInstr(SRL, B)
		return nil
	case 0x39:
		// SRL C
		// Shift C right
		c.PushInstr(SRL, C)
		return nil
	case 0x3A:
		// SRL D
		// Shift D right
		c.PushInstr(SRL, D)
		return nil
	case 0x3B:
		// SRL E
		// Shift E right
		c.PushInstr(SRL, E)
		return nil
	case 0x3C:
		// SRL H
		// Shift H right
		c.PushInstr(SRL, H)
		return nil
	case 0x3D:
		// SRL L
		// Shift L right
		c.PushInstr(SRL, L)
		return nil
	case 0x3E:
		// SRL (HL)
		// Shift value pointed by HL right
		c.PushInstr(SRL, Indirect{HL})
		return nil
	case 0x3F:
		// SRL A
		// Shift A right
		c.PushInstr(SRL, A)
		return nil
	case 0x40:
		// BIT 0, B
		// Test bit 0 of B
		c.PushInstr(BIT, StaticParam(0), B)
		return nil
	case 0x41:
		// BIT 0, C
		// Test bit 0 of C
		c.PushInstr(BIT, StaticParam(0), C)
		return nil
	case 0x42:
		// BIT 0, D
		// Test bit 0 of D
		c.PushInstr(BIT, StaticParam(0), D)
		return nil
	case 0x43:
		// BIT 0, E
		// Test bit 0 of E
		c.PushInstr(BIT, StaticParam(0), E)
		return nil
	case 0x44:
		// BIT 0, H
		// Test bit 0 of H
		c.PushInstr(BIT, StaticParam(0), H)
		return nil
	case 0x45:
		// BIT 0, L
		// Test bit 0 of L
		c.PushInstr(BIT, StaticParam(0), L)
		return nil
	case 0x46:
		// BIT 0, (HL)
		// Test bit 0 of value pointed by HL
		c.PushInstr(BIT, StaticParam(0), Indirect{HL})
		return nil
	case 0x47:
		// BIT 0, A
		// Test bit 0 of A
		c.PushInstr(BIT, StaticParam(0), A)
		return nil
	case 0x48:
		// BIT 1, B
		// Test bit 1 of B
		c.PushInstr(BIT, StaticParam(1), B)
		return nil
	case 0x49:
		// BIT 1, C
		// Test bit 1 of C
		c.PushInstr(BIT, StaticParam(1), C)
		return nil
	case 0x4A:
		// BIT 1, D
		// Test bit 1 of D
		c.PushInstr(BIT, StaticParam(1), D)
		return nil
	case 0x4B:
		// BIT 1, E
		// Test bit 1 of E
		c.PushInstr(BIT, StaticParam(1), E)
		return nil
	case 0x4C:
		// BIT 1, H
		// Test bit 1 of H
		c.PushInstr(BIT, StaticParam(1), H)
		return nil
	case 0x4D:
		// BIT 1, L
		// Test bit 1 of L
		c.PushInstr(BIT, StaticParam(1), L)
		return nil
	case 0x4E:
		// BIT 1, (HL)
		// Test bit 1 of value pointed by HL
		c.PushInstr(BIT, StaticParam(1), Indirect{HL})
		return nil
	case 0x4F:
		// BIT 1, A
		// Test bit 1 of A
		c.PushInstr(BIT, StaticParam(1), A)
		return nil
	case 0x50:
		// BIT 2, B
		// Test bit 2 of B
		c.PushInstr(BIT, StaticParam(2), B)
		return nil
	case 0x51:
		// BIT 2, C
		// Test bit 2 of C
		c.PushInstr(BIT, StaticParam(2), C)
		return nil
	case 0x52:
		// BIT 2, D
		// Test bit 2 of D
		c.PushInstr(BIT, StaticParam(2), D)
		return nil
	case 0x53:
		// BIT 2, E
		// Test bit 2 of E
		c.PushInstr(BIT, StaticParam(2), E)
		return nil
	case 0x54:
		// BIT 2, H
		// Test bit 2 of H
		c.PushInstr(BIT, StaticParam(2), H)
		return nil
	case 0x55:
		// BIT 2, L
		// Test bit 2 of L
		c.PushInstr(BIT, StaticParam(2), L)
		return nil
	case 0x56:
		// BIT 2, (HL)
		// Test bit 2 of value pointed by HL
		c.PushInstr(BIT, StaticParam(2), Indirect{HL})
		return nil
	case 0x57:
		// BIT 2, A
		// Test bit 2 of A
		c.PushInstr(BIT, StaticParam(2), A)
		return nil
	case 0x58:
		// BIT 3, B
		// Test bit 3 of B
		c.PushInstr(BIT, StaticParam(3), B)
		return nil
	case 0x59:
		// BIT 3, C
		// Test bit 3 of C
		c.PushInstr(BIT, StaticParam(3), C)
		return nil
	case 0x5A:
		// BIT 3, D
		// Test bit 3 of D
		c.PushInstr(BIT, StaticParam(3), D)
		return nil
	case 0x5B:
		// BIT 3, E
		// Test bit 3 of E
		c.PushInstr(BIT, StaticParam(3), E)
		return nil
	case 0x5C:
		// BIT 3, H
		// Test bit 3 of H
		c.PushInstr(BIT, StaticParam(3), H)
		return nil
	case 0x5D:
		// BIT 3, L
		// Test bit 3 of L
		c.PushInstr(BIT, StaticParam(3), L)
		return nil
	case 0x5E:
		// BIT 3, (HL)
		// Test bit 3 of value pointed by HL
		c.PushInstr(BIT, StaticParam(3), Indirect{HL})
		return nil
	case 0x5F:
		// BIT 3, A
		// Test bit 3 of A
		c.PushInstr(BIT, StaticParam(3), A)
		return nil
	case 0x60:
		// BIT 4, B
		// Test bit 4 of B
		c.PushInstr(BIT, StaticParam(4), B)
		return nil
	case 0x61:
		// BIT 4, C
		// Test bit 4 of C
		c.PushInstr(BIT, StaticParam(4), C)
		return nil
	case 0x62:
		// BIT 4, D
		// Test bit 4 of D
		c.PushInstr(BIT, StaticParam(4), D)
		return nil
	case 0x63:
		// BIT 4, E
		// Test bit 4 of E
		c.PushInstr(BIT, StaticParam(4), E)
		return nil
	case 0x64:
		// BIT 4, H
		// Test bit 4 of H
		c.PushInstr(BIT, StaticParam(4), H)
		return nil
	case 0x65:
		// BIT 4, L
		// Test bit 4 of L
		c.PushInstr(BIT, StaticParam(4), L)
		return nil
	case 0x66:
		// BIT 4, (HL)
		// Test bit 4 of value pointed by HL
		c.PushInstr(BIT, StaticParam(4), Indirect{HL})
		return nil
	case 0x67:
		// BIT 4, A
		// Test bit 4 of A
		c.PushInstr(BIT, StaticParam(4), A)
		return nil
	case 0x68:
		// BIT 5, B
		// Test bit 5 of B
		c.PushInstr(BIT, StaticParam(5), B)
		return nil
	case 0x69:
		// BIT 5, C
		// Test bit 5 of C
		c.PushInstr(BIT, StaticParam(5), C)
		return nil
	case 0x6A:
		// BIT 5, D
		// Test bit 5 of D
		c.PushInstr(BIT, StaticParam(5), D)
		return nil
	case 0x6B:
		// BIT 5, E
		// Test bit 5 of E
		c.PushInstr(BIT, StaticParam(5), E)
		return nil
	case 0x6C:
		// BIT 5, H
		// Test bit 5 of H
		c.PushInstr(BIT, StaticParam(5), H)
		return nil
	case 0x6D:
		// BIT 5, L
		// Test bit 5 of L
		c.PushInstr(BIT, StaticParam(5), L)
		return nil
	case 0x6E:
		// BIT 5, (HL)
		// Test bit 5 of value pointed by HL
		c.PushInstr(BIT, StaticParam(5), Indirect{HL})
		return nil
	case 0x6F:
		// BIT 5, A
		// Test bit 5 of A
		c.PushInstr(BIT, StaticParam(5), A)
		return nil
	case 0x70:
		// BIT 6, B
		// Test bit 6 of B
		c.PushInstr(BIT, StaticParam(6), B)
		return nil
	case 0x71:
		// BIT 6, C
		// Test bit 6 of C
		c.PushInstr(BIT, StaticParam(6), C)
		return nil
	case 0x72:
		// BIT 6, D
		// Test bit 6 of D
		c.PushInstr(BIT, StaticParam(6), D)
		return nil
	case 0x73:
		// BIT 6, E
		// Test bit 6 of E
		c.PushInstr(BIT, StaticParam(6), E)
		return nil
	case 0x74:
		// BIT 6, H
		// Test bit 6 of H
		c.PushInstr(BIT, StaticParam(6), H)
		return nil
	case 0x75:
		// BIT 6, L
		// Test bit 6 of L
		c.PushInstr(BIT, StaticParam(6), L)
		return nil
	case 0x76:
		// BIT 6, (HL)
		// Test bit 6 of value pointed by HL
		c.PushInstr(BIT, StaticParam(6), Indirect{HL})
		return nil
	case 0x77:
		// BIT 6, A
		// Test bit 6 of A
		c.PushInstr(BIT, StaticParam(6), A)
		return nil
	case 0x78:
		// BIT 7, B
		// Test bit 7 of B
		c.PushInstr(BIT, StaticParam(7), B)
		return nil
	case 0x79:
		// BIT 7, C
		// Test bit 7 of C
		c.PushInstr(BIT, StaticParam(7), C)
		return nil
	case 0x7A:
		// BIT 7, D
		// Test bit 7 of D
		c.PushInstr(BIT, StaticParam(7), D)
		return nil
	case 0x7B:
		// BIT 7, E
		// Test bit 7 of E
		c.PushInstr(BIT, StaticParam(7), E)
		return nil
	case 0x7C:
		// BIT 7, H
		// Test bit 7 of H
		c.PushInstr(BIT, StaticParam(7), H)
		return nil
	case 0x7D:
		// BIT 7, L
		// Test bit 7 of L
		c.PushInstr(BIT, StaticParam(7), L)
		return nil
	case 0x7E:
		// BIT 7, (HL)
		// Test bit 7 of value pointed by HL
		c.PushInstr(BIT, StaticParam(7), Indirect{HL})
		return nil
	case 0x7F:
		// BIT 7, A
		// Test bit 7 of A
		c.PushInstr(BIT, StaticParam(7), A)
		return nil
	case 0x80:
		// RES 0, B
		// Clear (reset) bit 0 of B
		c.PushInstr(RES, StaticParam(0), B)
		return nil
	case 0x81:
		// RES 0, C
		// Clear (reset) bit 0 of C
		c.PushInstr(RES, StaticParam(0), C)
		return nil
	case 0x82:
		// RES 0, D
		// Clear (reset) bit 0 of D
		c.PushInstr(RES, StaticParam(0), D)
		return nil
	case 0x83:
		// RES 0, E
		// Clear (reset) bit 0 of E
		c.PushInstr(RES, StaticParam(0), E)
		return nil
	case 0x84:
		// RES 0, H
		// Clear (reset) bit 0 of H
		c.PushInstr(RES, StaticParam(0), H)
		return nil
	case 0x85:
		// RES 0, L
		// Clear (reset) bit 0 of L
		c.PushInstr(RES, StaticParam(0), L)
		return nil
	case 0x86:
		// RES 0, (HL)
		// Clear (reset) bit 0 of value pointed by HL
		c.PushInstr(RES, StaticParam(0), Indirect{HL})
		return nil
	case 0x87:
		// RES 0, A
		// Clear (reset) bit 0 of A
		c.PushInstr(RES, StaticParam(0), A)
		return nil
	case 0x88:
		// RES 1, B
		// Clear (reset) bit 1 of B
		c.PushInstr(RES, StaticParam(1), B)
		return nil
	case 0x89:
		// RES 1, C
		// Clear (reset) bit 1 of C
		c.PushInstr(RES, StaticParam(1), C)
		return nil
	case 0x8A:
		// RES 1, D
		// Clear (reset) bit 1 of D
		c.PushInstr(RES, StaticParam(1), D)
		return nil
	case 0x8B:
		// RES 1, E
		// Clear (reset) bit 1 of E
		c.PushInstr(RES, StaticParam(1), E)
		return nil
	case 0x8C:
		// RES 1, H
		// Clear (reset) bit 1 of H
		c.PushInstr(RES, StaticParam(1), H)
		return nil
	case 0x8D:
		// RES 1, L
		// Clear (reset) bit 1 of L
		c.PushInstr(RES, StaticParam(1), L)
		return nil
	case 0x8E:
		// RES 1, (HL)
		// Clear (reset) bit 1 of value pointed by HL
		c.PushInstr(RES, StaticParam(1), Indirect{HL})
		return nil
	case 0x8F:
		// RES 1, A
		// Clear (reset) bit 1 of A
		c.PushInstr(RES, StaticParam(1), A)
		return nil
	case 0x90:
		// RES 2, B
		// Clear (reset) bit 2 of B
		c.PushInstr(RES, StaticParam(2), B)
		return nil
	case 0x91:
		// RES 2, C
		// Clear (reset) bit 2 of C
		c.PushInstr(RES, StaticParam(2), C)
		return nil
	case 0x92:
		// RES 2, D
		// Clear (reset) bit 2 of D
		c.PushInstr(RES, StaticParam(2), D)
		return nil
	case 0x93:
		// RES 2, E
		// Clear (reset) bit 2 of E
		c.PushInstr(RES, StaticParam(2), E)
		return nil
	case 0x94:
		// RES 2, H
		// Clear (reset) bit 2 of H
		c.PushInstr(RES, StaticParam(2), H)
		return nil
	case 0x95:
		// RES 2, L
		// Clear (reset) bit 2 of L
		c.PushInstr(RES, StaticParam(2), L)
		return nil
	case 0x96:
		// RES 2, (HL)
		// Clear (reset) bit 2 of value pointed by HL
		c.PushInstr(RES, StaticParam(2), Indirect{HL})
		return nil
	case 0x97:
		// RES 2, A
		// Clear (reset) bit 2 of A
		c.PushInstr(RES, StaticParam(2), A)
		return nil
	case 0x98:
		// RES 3, B
		// Clear (reset) bit 3 of B
		c.PushInstr(RES, StaticParam(3), B)
		return nil
	case 0x99:
		// RES 3, C
		// Clear (reset) bit 3 of C
		c.PushInstr(RES, StaticParam(3), C)
		return nil
	case 0x9A:
		// RES 3, D
		// Clear (reset) bit 3 of D
		c.PushInstr(RES, StaticParam(3), D)
		return nil
	case 0x9B:
		// RES 3, E
		// Clear (reset) bit 3 of E
		c.PushInstr(RES, StaticParam(3), E)
		return nil
	case 0x9C:
		// RES 3, H
		// Clear (reset) bit 3 of H
		c.PushInstr(RES, StaticParam(3), H)
		return nil
	case 0x9D:
		// RES 3, L
		// Clear (reset) bit 3 of L
		c.PushInstr(RES, StaticParam(3), L)
		return nil
	case 0x9E:
		// RES 3, (HL)
		// Clear (reset) bit 3 of value pointed by HL
		c.PushInstr(RES, StaticParam(3), Indirect{HL})
		return nil
	case 0x9F:
		// RES 3, A
		// Clear (reset) bit 3 of A
		c.PushInstr(RES, StaticParam(3), A)
		return nil
	case 0xA0:
		// RES 4, B
		// Clear (reset) bit 4 of B
		c.PushInstr(RES, StaticParam(4), B)
		return nil
	case 0xA1:
		// RES 4, C
		// Clear (reset) bit 4 of C
		c.PushInstr(RES, StaticParam(4), C)
		return nil
	case 0xA2:
		// RES 4, D
		// Clear (reset) bit 4 of D
		c.PushInstr(RES, StaticParam(4), D)
		return nil
	case 0xA3:
		// RES 4, E
		// Clear (reset) bit 4 of E
		c.PushInstr(RES, StaticParam(4), E)
		return nil
	case 0xA4:
		// RES 4, H
		// Clear (reset) bit 4 of H
		c.PushInstr(RES, StaticParam(4), H)
		return nil
	case 0xA5:
		// RES 4, L
		// Clear (reset) bit 4 of L
		c.PushInstr(RES, StaticParam(4), L)
		return nil
	case 0xA6:
		// RES 4, (HL)
		// Clear (reset) bit 4 of value pointed by HL
		c.PushInstr(RES, StaticParam(4), Indirect{HL})
		return nil
	case 0xA7:
		// RES 4, A
		// Clear (reset) bit 4 of A
		c.PushInstr(RES, StaticParam(4), A)
		return nil
	case 0xA8:
		// RES 5, B
		// Clear (reset) bit 5 of B
		c.PushInstr(RES, StaticParam(5), B)
		return nil
	case 0xA9:
		// RES 5, C
		// Clear (reset) bit 5 of C
		c.PushInstr(RES, StaticParam(5), C)
		return nil
	case 0xAA:
		// RES 5, D
		// Clear (reset) bit 5 of D
		c.PushInstr(RES, StaticParam(5), D)
		return nil
	case 0xAB:
		// RES 5, E
		// Clear (reset) bit 5 of E
		c.PushInstr(RES, StaticParam(5), E)
		return nil
	case 0xAC:
		// RES 5, H
		// Clear (reset) bit 5 of H
		c.PushInstr(RES, StaticParam(5), H)
		return nil
	case 0xAD:
		// RES 5, L
		// Clear (reset) bit 5 of L
		c.PushInstr(RES, StaticParam(5), L)
		return nil
	case 0xAE:
		// RES 5, (HL)
		// Clear (reset) bit 5 of value pointed by HL
		c.PushInstr(RES, StaticParam(5), Indirect{HL})
		return nil
	case 0xAF:
		// RES 5, A
		// Clear (reset) bit 5 of A
		c.PushInstr(RES, StaticParam(5), A)
		return nil
	case 0xB0:
		// RES 6, B
		// Clear (reset) bit 6 of B
		c.PushInstr(RES, StaticParam(6), B)
		return nil
	case 0xB1:
		// RES 6, C
		// Clear (reset) bit 6 of C
		c.PushInstr(RES, StaticParam(6), C)
		return nil
	case 0xB2:
		// RES 6, D
		// Clear (reset) bit 6 of D
		c.PushInstr(RES, StaticParam(6), D)
		return nil
	case 0xB3:
		// RES 6, E
		// Clear (reset) bit 6 of E
		c.PushInstr(RES, StaticParam(6), E)
		return nil
	case 0xB4:
		// RES 6, H
		// Clear (reset) bit 6 of H
		c.PushInstr(RES, StaticParam(6), H)
		return nil
	case 0xB5:
		// RES 6, L
		// Clear (reset) bit 6 of L
		c.PushInstr(RES, StaticParam(6), L)
		return nil
	case 0xB6:
		// RES 6, (HL)
		// Clear (reset) bit 6 of value pointed by HL
		c.PushInstr(RES, StaticParam(6), Indirect{HL})
		return nil
	case 0xB7:
		// RES 6, A
		// Clear (reset) bit 6 of A
		c.PushInstr(RES, StaticParam(6), A)
		return nil
	case 0xB8:
		// RES 7, B
		// Clear (reset) bit 7 of B
		c.PushInstr(RES, StaticParam(7), B)
		return nil
	case 0xB9:
		// RES 7, C
		// Clear (reset) bit 7 of C
		c.PushInstr(RES, StaticParam(7), C)
		return nil
	case 0xBA:
		// RES 7, D
		// Clear (reset) bit 7 of D
		c.PushInstr(RES, StaticParam(7), D)
		return nil
	case 0xBB:
		// RES 7, E
		// Clear (reset) bit 7 of E
		c.PushInstr(RES, StaticParam(7), E)
		return nil
	case 0xBC:
		// RES 7, H
		// Clear (reset) bit 7 of H
		c.PushInstr(RES, StaticParam(7), H)
		return nil
	case 0xBD:
		// RES 7, L
		// Clear (reset) bit 7 of L
		c.PushInstr(RES, StaticParam(7), L)
		return nil
	case 0xBE:
		// RES 7, (HL)
		// Clear (reset) bit 7 of value pointed by HL
		c.PushInstr(RES, StaticParam(7), Indirect{HL})
		return nil
	case 0xBF:
		// RES 7, A
		// Clear (reset) bit 7 of A
		c.PushInstr(RES, StaticParam(7), A)
		return nil
	case 0xC0:
		// SET 0, B
		// Set bit 0 of B
		c.PushInstr(SET, StaticParam(0), B)
		return nil
	case 0xC1:
		// SET 0, C
		// Set bit 0 of C
		c.PushInstr(SET, StaticParam(0), C)
		return nil
	case 0xC2:
		// SET 0, D
		// Set bit 0 of D
		c.PushInstr(SET, StaticParam(0), D)
		return nil
	case 0xC3:
		// SET 0, E
		// Set bit 0 of E
		c.PushInstr(SET, StaticParam(0), E)
		return nil
	case 0xC4:
		// SET 0, H
		// Set bit 0 of H
		c.PushInstr(SET, StaticParam(0), H)
		return nil
	case 0xC5:
		// SET 0, L
		// Set bit 0 of L
		c.PushInstr(SET, StaticParam(0), L)
		return nil
	case 0xC6:
		// SET 0, (HL)
		// Set bit 0 of value pointed by HL
		c.PushInstr(SET, StaticParam(0), Indirect{HL})
		return nil
	case 0xC7:
		// SET 0, A
		// Set bit 0 of A
		c.PushInstr(SET, StaticParam(0), A)
		return nil
	case 0xC8:
		// SET 1, B
		// Set bit 1 of B
		c.PushInstr(SET, StaticParam(1), B)
		return nil
	case 0xC9:
		// SET 1, C
		// Set bit 1 of C
		c.PushInstr(SET, StaticParam(1), C)
		return nil
	case 0xCA:
		// SET 1, D
		// Set bit 1 of D
		c.PushInstr(SET, StaticParam(1), D)
		return nil
	case 0xCB:
		// SET 1, E
		// Set bit 1 of E
		c.PushInstr(SET, StaticParam(1), E)
		return nil
	case 0xCC:
		// SET 1, H
		// Set bit 1 of H
		c.PushInstr(SET, StaticParam(1), H)
		return nil
	case 0xCD:
		// SET 1, L
		// Set bit 1 of L
		c.PushInstr(SET, StaticParam(1), L)
		return nil
	case 0xCE:
		// SET 1, (HL)
		// Set bit 1 of value pointed by HL
		c.PushInstr(SET, StaticParam(1), Indirect{HL})
		return nil
	case 0xCF:
		// SET 1, A
		// Set bit 1 of A
		c.PushInstr(SET, StaticParam(1), A)
		return nil
	case 0xD0:
		// SET 2, B
		// Set bit 2 of B
		c.PushInstr(SET, StaticParam(2), B)
		return nil
	case 0xD1:
		// SET 2, C
		// Set bit 2 of C
		c.PushInstr(SET, StaticParam(2), C)
		return nil
	case 0xD2:
		// SET 2, D
		// Set bit 2 of D
		c.PushInstr(SET, StaticParam(2), D)
		return nil
	case 0xD3:
		// SET 2, E
		// Set bit 2 of E
		c.PushInstr(SET, StaticParam(2), E)
		return nil
	case 0xD4:
		// SET 2, H
		// Set bit 2 of H
		c.PushInstr(SET, StaticParam(2), H)
		return nil
	case 0xD5:
		// SET 2, L
		// Set bit 2 of L
		c.PushInstr(SET, StaticParam(2), L)
		return nil
	case 0xD6:
		// SET 2, (HL)
		// Set bit 2 of value pointed by HL
		c.PushInstr(SET, StaticParam(2), Indirect{HL})
		return nil
	case 0xD7:
		// SET 2, A
		// Set bit 2 of A
		c.PushInstr(SET, StaticParam(2), A)
		return nil
	case 0xD8:
		// SET 3, B
		// Set bit 3 of B
		c.PushInstr(SET, StaticParam(3), B)
		return nil
	case 0xD9:
		// SET 3, C
		// Set bit 3 of C
		c.PushInstr(SET, StaticParam(3), C)
		return nil
	case 0xDA:
		// SET 3, D
		// Set bit 3 of D
		c.PushInstr(SET, StaticParam(3), D)
		return nil
	case 0xDB:
		// SET 3, E
		// Set bit 3 of E
		c.PushInstr(SET, StaticParam(3), E)
		return nil
	case 0xDC:
		// SET 3, H
		// Set bit 3 of H
		c.PushInstr(SET, StaticParam(3), H)
		return nil
	case 0xDD:
		// SET 3, L
		// Set bit 3 of L
		c.PushInstr(SET, StaticParam(3), L)
		return nil
	case 0xDE:
		// SET 3, (HL)
		// Set bit 3 of value pointed by HL
		c.PushInstr(SET, StaticParam(3), Indirect{HL})
		return nil
	case 0xDF:
		// SET 3, A
		// Set bit 3 of A
		c.PushInstr(SET, StaticParam(3), A)
		return nil
	case 0xE0:
		// SET 4, B
		// Set bit 4 of B
		c.PushInstr(SET, StaticParam(4), B)
		return nil
	case 0xE1:
		// SET 4, C
		// Set bit 4 of C
		c.PushInstr(SET, StaticParam(4), C)
		return nil
	case 0xE2:
		// SET 4, D
		// Set bit 4 of D
		c.PushInstr(SET, StaticParam(4), D)
		return nil
	case 0xE3:
		// SET 4, E
		// Set bit 4 of E
		c.PushInstr(SET, StaticParam(4), E)
		return nil
	case 0xE4:
		// SET 4, H
		// Set bit 4 of H
		c.PushInstr(SET, StaticParam(4), H)
		return nil
	case 0xE5:
		// SET 4, L
		// Set bit 4 of L
		c.PushInstr(SET, StaticParam(4), L)
		return nil
	case 0xE6:
		// SET 4, (HL)
		// Set bit 4 of value pointed by HL
		c.PushInstr(SET, StaticParam(4), Indirect{HL})
		return nil
	case 0xE7:
		// SET 4, A
		// Set bit 4 of A
		c.PushInstr(SET, StaticParam(4), A)
		return nil
	case 0xE8:
		// SET 5, B
		// Set bit 5 of B
		c.PushInstr(SET, StaticParam(5), B)
		return nil
	case 0xE9:
		// SET 5, C
		// Set bit 5 of C
		c.PushInstr(SET, StaticParam(5), C)
		return nil
	case 0xEA:
		// SET 5, D
		// Set bit 5 of D
		c.PushInstr(SET, StaticParam(5), D)
		return nil
	case 0xEB:
		// SET 5, E
		// Set bit 5 of E
		c.PushInstr(SET, StaticParam(5), E)
		return nil
	case 0xEC:
		// SET 5, H
		// Set bit 5 of H
		c.PushInstr(SET, StaticParam(5), H)
		return nil
	case 0xED:
		// SET 5, L
		// Set bit 5 of L
		c.PushInstr(SET, StaticParam(5), L)
		return nil
	case 0xEE:
		// SET 5, (HL)
		// Set bit 5 of value pointed by HL
		c.PushInstr(SET, StaticParam(5), Indirect{HL})
		return nil
	case 0xEF:
		// SET 5, A
		// Set bit 5 of A
		c.PushInstr(SET, StaticParam(5), A)
		return nil
	case 0xF0:
		// SET 6, B
		// Set bit 6 of B
		c.PushInstr(SET, StaticParam(6), B)
		return nil
	case 0xF1:
		// SET 6, C
		// Set bit 6 of C
		c.PushInstr(SET, StaticParam(6), C)
		return nil
	case 0xF2:
		// SET 6, D
		// Set bit 6 of D
		c.PushInstr(SET, StaticParam(6), D)
		return nil
	case 0xF3:
		// SET 6, E
		// Set bit 6 of E
		c.PushInstr(SET, StaticParam(6), E)
		return nil
	case 0xF4:
		// SET 6, H
		// Set bit 6 of H
		c.PushInstr(SET, StaticParam(6), H)
		return nil
	case 0xF5:
		// SET 6, L
		// Set bit 6 of L
		c.PushInstr(SET, StaticParam(6), L)
		return nil
	case 0xF6:
		// SET 6, (HL)
		// Set bit 6 of value pointed by HL
		c.PushInstr(SET, StaticParam(6), Indirect{HL})
		return nil
	case 0xF7:
		// SET 6, A
		// Set bit 6 of A
		c.PushInstr(SET, StaticParam(6), A)
		return nil
	case 0xF8:
		// SET 7, B
		// Set bit 7 of B
		c.PushInstr(SET, StaticParam(7), B)
		return nil
	case 0xF9:
		// SET 7, C
		// Set bit 7 of C
		c.PushInstr(SET, StaticParam(7), C)
		return nil
	case 0xFA:
		// SET 7, D
		// Set bit 7 of D
		c.PushInstr(SET, StaticParam(7), D)
		return nil
	case 0xFB:
		// SET 7, E
		// Set bit 7 of E
		c.PushInstr(SET, StaticParam(7), E)
		return nil
	case 0xFC:
		// SET 7, H
		// Set bit 7 of H
		c.PushInstr(SET, StaticParam(7), H)
		return nil
	case 0xFD:
		// SET 7, L
		// Set bit 7 of L
		c.PushInstr(SET, StaticParam(7), L)
		return nil
	case 0xFE:
		// SET 7, (HL)
		// Set bit 7 of value pointed by HL
		c.PushInstr(SET, StaticParam(7), Indirect{HL})
		return nil
	case 0xFF:
		// SET 7, A
		// Set bit 7 of A
		c.PushInstr(SET, StaticParam(7), A)
		return nil
	}

	panic("undefined opcode")
}

func decodeOpcode(c *parser) error {
	var bytebuf [1]byte
	_, err := c.input.Read(bytebuf[:])
	if err != nil {
		return err
	}

	pos, err := c.input.Seek(0, 1)
	if err != nil {
		return err
	}
	fmt.Fprintf(os.Stderr, " opcode: 0x%08x -> 0x%02x\n", pos-1, bytebuf[0])

	switch bytebuf[0] {
	case 0x00:
		// NOP
		// No Operation
		c.PushInstr(NOP)
		return nil
	case 0x01:
		// LD BC, nn
		// Load 16-bit immediate into BC
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 2)
		if err != nil {
			return err
		}
		c.PushInstr(LD, CompositeReg{B, C}, Immediate16(uint16(operands[1])<<8|uint16(operands[0])))
		return nil
	case 0x02:
		// LD (BC), A
		// Save A to address pointed by BC
		c.PushInstr(LD, Indirect{BC}, A)
		return nil
	case 0x03:
		// INC BC
		// Increment 16-bit BC
		c.PushInstr(INC, CompositeReg{B, C})
		return nil
	case 0x04:
		// INC B
		// Increment B
		c.PushInstr(INC, B)
		return nil
	case 0x05:
		// DEC B
		// Decrement B
		c.PushInstr(DEC, B)
		return nil
	case 0x06:
		// LD B, n
		// Load 8-bit immediate into B
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(LD, B, Immediate8(operands[0]))
		return nil
	case 0x07:
		// RLC A
		// Rotate A left with carry
		c.PushInstr(RLC, A)
		return nil
	case 0x08:
		// LD (nn), SP
		// Save SP to given address
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 2)
		if err != nil {
			return err
		}
		c.PushInstr(LD, Immediate8(uint16(operands[1])<<8|uint16(operands[0])), SP)
		return nil
	case 0x09:
		// ADD HL, BC
		// Add 16-bit BC to HL
		c.PushInstr(ADD, CompositeReg{H, L}, CompositeReg{B, C})
		return nil
	case 0x0A:
		// LD A, (BC)
		// Load A from address pointed to by BC
		c.PushInstr(LD, A, Indirect{BC})
		return nil
	case 0x0B:
		// DEC BC
		// Decrement 16-bit BC
		c.PushInstr(DEC, CompositeReg{B, C})
		return nil
	case 0x0C:
		// INC C
		// Increment C
		c.PushInstr(INC, C)
		return nil
	case 0x0D:
		// DEC C
		// Decrement C
		c.PushInstr(DEC, C)
		return nil
	case 0x0E:
		// LD C, n
		// Load 8-bit immediate into C
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(LD, C, Immediate8(operands[0]))
		return nil
	case 0x0F:
		// RRC A
		// Rotate A right with carry
		c.PushInstr(RRC, A)
		return nil
	case 0x10:
		// STOP
		// Stop processor
		c.PushInstr(STOP)
		return nil
	case 0x11:
		// LD DE, nn
		// Load 16-bit immediate into DE
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 2)
		if err != nil {
			return err
		}
		c.PushInstr(LD, DE, Immediate16(uint16(operands[1])<<8|uint16(operands[0])))
		return nil
	case 0x12:
		// LD (DE), A
		// Save A to address pointed by DE
		c.PushInstr(LD, Indirect{DE}, A)
		return nil
	case 0x13:
		// INC DE
		// Increment 16-bit DE
		c.PushInstr(INC, CompositeReg{D, E})
		return nil
	case 0x14:
		// INC D
		// Increment D
		c.PushInstr(INC, D)
		return nil
	case 0x15:
		// DEC D
		// Decrement D
		c.PushInstr(DEC, D)
		return nil
	case 0x16:
		// LD D, n
		// Load 8-bit immediate into D
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(LD, D, Immediate8(operands[0]))
		return nil
	case 0x17:
		// RL A
		// Rotate A left
		c.PushInstr(RL, A)
		return nil
	case 0x18:
		// JR n
		// Relative jump by signed immediate
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(JR, Immediate8(operands[0]))
		return nil
	case 0x19:
		// ADD HL, DE
		// Add 16-bit DE to HL
		c.PushInstr(ADD, CompositeReg{H, L}, CompositeReg{D, E})
		return nil
	case 0x1A:
		// LD A, (DE)
		// Load A from address pointed to by DE
		c.PushInstr(LD, A, Indirect{DE})
		return nil
	case 0x1B:
		// DEC DE
		// Decrement 16-bit DE
		c.PushInstr(DEC, CompositeReg{D, E})
		return nil
	case 0x1C:
		// INC E
		// Increment E
		c.PushInstr(INC, E)
		return nil
	case 0x1D:
		// DEC E
		// Decrement E
		c.PushInstr(DEC, E)
		return nil
	case 0x1E:
		// LD E, n
		// Load 8-bit immediate into E
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(LD, E, Immediate8(operands[0]))
		return nil
	case 0x1F:
		// RR A
		// Rotate A right
		c.PushInstr(RR, A)
		return nil
	case 0x20:
		// JR NZ, n
		// Relative jump by signed immediate if last result was not zero
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(JR, CondNonZero, Immediate8(operands[0]))
		return nil
	case 0x21:
		// LD HL, nn
		// Load 16-bit immediate into HL
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 2)
		if err != nil {
			return err
		}
		c.PushInstr(LD, HL, Immediate16(uint16(operands[1])<<8|uint16(operands[0])))
		return nil
	case 0x22:
		// LDI (HL), A
		// Save A to address pointed by HL, and increment HL
		c.PushInstr(LDI, Indirect{HL}, A)
		return nil
	case 0x23:
		// INC HL
		// Increment 16-bit HL
		c.PushInstr(INC, HL)
		return nil
	case 0x24:
		// INC H
		// Increment H
		c.PushInstr(INC, H)
		return nil
	case 0x25:
		// DEC H
		// Decrement H
		c.PushInstr(DEC, H)
		return nil
	case 0x26:
		// LD H, n
		// Load 8-bit immediate into H
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(LD, H, Immediate8(operands[0]))
		return nil
	case 0x27:
		// DAA
		// Adjust A for BCD addition
		c.PushInstr(DAA)
		return nil
	case 0x28:
		// JR Z, n
		// Relative jump by signed immediate if last result was zero
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(JR, CondZero, Immediate8(operands[0]))
		return nil
	case 0x29:
		// ADD HL, HL
		// Add 16-bit HL to HL
		c.PushInstr(ADD, HL, HL)
		return nil
	case 0x2A:
		// LDI A, (HL)
		// Load A from address pointed to by HL, and increment HL
		c.PushInstr(LDI, A, Indirect{HL})
		return nil
	case 0x2B:
		// DEC HL
		// Decrement 16-bit HL
		c.PushInstr(DEC, HL)
		return nil
	case 0x2C:
		// INC L
		// Increment L
		c.PushInstr(INC, L)
		return nil
	case 0x2D:
		// DEC L
		// Decrement L
		c.PushInstr(DEC, L)
		return nil
	case 0x2E:
		// LD L, n
		// Load 8-bit immediate into L
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(LD, L, Immediate8(operands[0]))
		return nil
	case 0x2F:
		// CPL
		// Complement (logical NOT) on A
		c.PushInstr(CPL)
		return nil
	case 0x30:
		// JR NC, n
		// Relative jump by signed immediate if last result caused no carry
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(JR, CondNonCarry, Immediate8(operands[0]))
		return nil
	case 0x31:
		// LD SP, nn
		// Load 16-bit immediate into SP
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 2)
		if err != nil {
			return err
		}
		c.PushInstr(LD, SP, Immediate16(uint16(operands[1])<<8|uint16(operands[0])))
		return nil
	case 0x32:
		// LDD (HL), A
		// Save A to address pointed by HL, and decrement HL
		c.PushInstr(LDD, Indirect{HL}, A)
		return nil
	case 0x33:
		// INC SP
		// Increment 16-bit HL
		c.PushInstr(INC, SP)
		return nil
	case 0x34:
		// INC (HL)
		// Increment value pointed by HL
		c.PushInstr(INC, Indirect{HL})
		return nil
	case 0x35:
		// DEC (HL)
		// Decrement value pointed by HL
		c.PushInstr(DEC, Indirect{HL})
		return nil
	case 0x36:
		// LD (HL), n
		// Load 8-bit immediate into address pointed by HL
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(LD, Indirect{HL}, Immediate8(operands[0]))
		return nil
	case 0x37:
		// SCF
		// Set carry flag
		c.PushInstr(SCF)
		return nil
	case 0x38:
		// JR C, n
		// Relative jump by signed immediate if last result caused carry
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(JR, CondCarry, Immediate8(operands[0]))
		return nil
	case 0x39:
		// ADD HL, SP
		// Add 16-bit SP to HL
		c.PushInstr(ADD, HL, SP)
		return nil
	case 0x3A:
		// LDD A, (HL)
		// Load A from address pointed to by HL, and decrement HL
		c.PushInstr(LDD, A, Indirect{HL})
		return nil
	case 0x3B:
		// DEC SP
		// Decrement 16-bit SP
		c.PushInstr(DEC, SP)
		return nil
	case 0x3C:
		// INC A
		// Increment A
		c.PushInstr(INC, A)
		return nil
	case 0x3D:
		// DEC A
		// Decrement A
		c.PushInstr(DEC, A)
		return nil
	case 0x3E:
		// LD A, n
		// Load 8-bit immediate into A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(LD, A, Immediate8(operands[0]))
		return nil
	case 0x3F:
		// CCF
		// Clear carry flag
		c.PushInstr(CCF)
		return nil
	case 0x40:
		// LD B, B
		// Copy B to B
		c.PushInstr(LD, B, B)
		return nil
	case 0x41:
		// LD B, C
		// Copy C to B
		c.PushInstr(LD, B, C)
		return nil
	case 0x42:
		// LD B, D
		// Copy D to B
		c.PushInstr(LD, B, D)
		return nil
	case 0x43:
		// LD B, E
		// Copy E to B
		c.PushInstr(LD, B, E)
		return nil
	case 0x44:
		// LD B, H
		// Copy H to B
		c.PushInstr(LD, B, H)
		return nil
	case 0x45:
		// LD B, L
		// Copy L to B
		c.PushInstr(LD, B, L)
		return nil
	case 0x46:
		// LD B, (HL)
		// Copy value pointed by HL to B
		c.PushInstr(LD, B, Indirect{HL})
		return nil
	case 0x47:
		// LD B, A
		// Copy A to B
		c.PushInstr(LD, B, A)
		return nil
	case 0x48:
		// LD C, B
		// Copy B to C
		c.PushInstr(LD, C, B)
		return nil
	case 0x49:
		// LD C, C
		// Copy C to C
		c.PushInstr(LD, C, C)
		return nil
	case 0x4A:
		// LD C, D
		// Copy D to C
		c.PushInstr(LD, C, D)
		return nil
	case 0x4B:
		// LD C, E
		// Copy E to C
		c.PushInstr(LD, C, E)
		return nil
	case 0x4C:
		// LD C, H
		// Copy H to C
		c.PushInstr(LD, C, H)
		return nil
	case 0x4D:
		// LD C, L
		// Copy L to C
		c.PushInstr(LD, C, L)
		return nil
	case 0x4E:
		// LD C, (HL)
		// Copy value pointed by HL to C
		c.PushInstr(LD, C, Indirect{HL})
		return nil
	case 0x4F:
		// LD C, A
		// Copy A to C
		c.PushInstr(LD, C, A)
		return nil
	case 0x50:
		// LD D, B
		// Copy B to D
		c.PushInstr(LD, D, B)
		return nil
	case 0x51:
		// LD D, C
		// Copy C to D
		c.PushInstr(LD, D, C)
		return nil
	case 0x52:
		// LD D, D
		// Copy D to D
		c.PushInstr(LD, D, D)
		return nil
	case 0x53:
		// LD D, E
		// Copy E to D
		c.PushInstr(LD, D, E)
		return nil
	case 0x54:
		// LD D, H
		// Copy H to D
		c.PushInstr(LD, D, H)
		return nil
	case 0x55:
		// LD D, L
		// Copy L to D
		c.PushInstr(LD, D, L)
		return nil
	case 0x56:
		// LD D, (HL)
		// Copy value pointed by HL to D
		c.PushInstr(LD, D, Indirect{HL})
		return nil
	case 0x57:
		// LD D, A
		// Copy A to D
		c.PushInstr(LD, D, A)
		return nil
	case 0x58:
		// LD E, B
		// Copy B to E
		c.PushInstr(LD, E, B)
		return nil
	case 0x59:
		// LD E, C
		// Copy C to E
		c.PushInstr(LD, E, C)
		return nil
	case 0x5A:
		// LD E, D
		// Copy D to E
		c.PushInstr(LD, E, D)
		return nil
	case 0x5B:
		// LD E, E
		// Copy E to E
		c.PushInstr(LD, E, E)
		return nil
	case 0x5C:
		// LD E, H
		// Copy H to E
		c.PushInstr(LD, E, H)
		return nil
	case 0x5D:
		// LD E, L
		// Copy L to E
		c.PushInstr(LD, E, L)
		return nil
	case 0x5E:
		// LD E, (HL)
		// Copy value pointed by HL to E
		c.PushInstr(LD, E, Indirect{HL})
		return nil
	case 0x5F:
		// LD E, A
		// Copy A to E
		c.PushInstr(LD, E, A)
		return nil
	case 0x60:
		// LD H, B
		// Copy B to H
		c.PushInstr(LD, H, B)
		return nil
	case 0x61:
		// LD H, C
		// Copy C to H
		c.PushInstr(LD, H, C)
		return nil
	case 0x62:
		// LD H, D
		// Copy D to H
		c.PushInstr(LD, H, D)
		return nil
	case 0x63:
		// LD H, E
		// Copy E to H
		c.PushInstr(LD, H, E)
		return nil
	case 0x64:
		// LD H, H
		// Copy H to H
		c.PushInstr(LD, H, H)
		return nil
	case 0x65:
		// LD H, L
		// Copy L to H
		c.PushInstr(LD, H, L)
		return nil
	case 0x66:
		// LD H, (HL)
		// Copy value pointed by HL to H
		c.PushInstr(LD, H, Indirect{HL})
		return nil
	case 0x67:
		// LD H, A
		// Copy A to H
		c.PushInstr(LD, H, A)
		return nil
	case 0x68:
		// LD L, B
		// Copy B to L
		c.PushInstr(LD, L, B)
		return nil
	case 0x69:
		// LD L, C
		// Copy C to L
		c.PushInstr(LD, L, C)
		return nil
	case 0x6A:
		// LD L, D
		// Copy D to L
		c.PushInstr(LD, L, D)
		return nil
	case 0x6B:
		// LD L, E
		// Copy E to L
		c.PushInstr(LD, L, E)
		return nil
	case 0x6C:
		// LD L, H
		// Copy H to L
		c.PushInstr(LD, L, H)
		return nil
	case 0x6D:
		// LD L, L
		// Copy L to L
		c.PushInstr(LD, L, L)
		return nil
	case 0x6E:
		// LD L, (HL)
		// Copy value pointed by HL to L
		c.PushInstr(LD, L, Indirect{HL})
		return nil
	case 0x6F:
		// LD L, A
		// Copy A to L
		c.PushInstr(LD, L, A)
		return nil
	case 0x70:
		// LD (HL), B
		// Copy B to address pointed by HL
		c.PushInstr(LD, Indirect{HL}, B)
		return nil
	case 0x71:
		// LD (HL), C
		// Copy C to address pointed by HL
		c.PushInstr(LD, Indirect{HL}, C)
		return nil
	case 0x72:
		// LD (HL), D
		// Copy D to address pointed by HL
		c.PushInstr(LD, Indirect{HL}, D)
		return nil
	case 0x73:
		// LD (HL), E
		// Copy E to address pointed by HL
		c.PushInstr(LD, Indirect{HL}, E)
		return nil
	case 0x74:
		// LD (HL), H
		// Copy H to address pointed by HL
		c.PushInstr(LD, Indirect{HL}, H)
		return nil
	case 0x75:
		// LD (HL), L
		// Copy L to address pointed by HL
		c.PushInstr(LD, Indirect{HL}, L)
		return nil
	case 0x76:
		// HALT
		// Halt processor
		c.PushInstr(HALT)
		return nil
	case 0x77:
		// LD (HL), A
		// Copy A to address pointed by HL
		c.PushInstr(LD, Indirect{HL}, A)
		return nil
	case 0x78:
		// LD A, B
		// Copy B to A
		c.PushInstr(LD, A, B)
		return nil
	case 0x79:
		// LD A, C
		// Copy C to A
		c.PushInstr(LD, A, C)
		return nil
	case 0x7A:
		// LD A, D
		// Copy D to A
		c.PushInstr(LD, A, D)
		return nil
	case 0x7B:
		// LD A, E
		// Copy E to A
		c.PushInstr(LD, A, E)
		return nil
	case 0x7C:
		// LD A, H
		// Copy H to A
		c.PushInstr(LD, A, H)
		return nil
	case 0x7D:
		// LD A, L
		// Copy L to A
		c.PushInstr(LD, A, L)
		return nil
	case 0x7E:
		// LD A, (HL)
		// Copy value pointed by HL to A
		c.PushInstr(LD, A, Indirect{HL})
		return nil
	case 0x7F:
		// LD A, A
		// Copy A to A
		c.PushInstr(LD, A, A)
		return nil
	case 0x80:
		// ADD A, B
		// Add B to A
		c.PushInstr(ADD, A, B)
		return nil
	case 0x81:
		// ADD A, C
		// Add C to A
		c.PushInstr(ADD, A, C)
		return nil
	case 0x82:
		// ADD A, D
		// Add D to A
		c.PushInstr(ADD, A, D)
		return nil
	case 0x83:
		// ADD A, E
		// Add E to A
		c.PushInstr(ADD, A, E)
		return nil
	case 0x84:
		// ADD A, H
		// Add H to A
		c.PushInstr(ADD, A, H)
		return nil
	case 0x85:
		// ADD A, L
		// Add L to A
		c.PushInstr(ADD, A, L)
		return nil
	case 0x86:
		// ADD A, (HL)
		// Add value pointed by HL to A
		c.PushInstr(ADD, A, Indirect{HL})
		return nil
	case 0x87:
		// ADD A, A
		// Add A to A
		c.PushInstr(ADD, A, A)
		return nil
	case 0x88:
		// ADC A, B
		// Add B and carry flag to A
		c.PushInstr(ADC, A, B)
		return nil
	case 0x89:
		// ADC A, C
		// Add C and carry flag to A
		c.PushInstr(ADC, A, C)
		return nil
	case 0x8A:
		// ADC A, D
		// Add D and carry flag to A
		c.PushInstr(ADC, A, D)
		return nil
	case 0x8B:
		// ADC A, E
		// Add E and carry flag to A
		c.PushInstr(ADC, A, E)
		return nil
	case 0x8C:
		// ADC A, H
		// Add H and carry flag to A
		c.PushInstr(ADC, A, H)
		return nil
	case 0x8D:
		// ADC A, L
		// Add and carry flag L to A
		c.PushInstr(ADC, A, L)
		return nil
	case 0x8E:
		// ADC A, (HL)
		// Add value pointed by HL and carry flag to A
		c.PushInstr(ADC, A, Indirect{HL})
		return nil
	case 0x8F:
		// ADC A, A
		// Add A and carry flag to A
		c.PushInstr(ADC, A, A)
		return nil
	case 0x90:
		// SUB A, B
		// Subtract B from A
		c.PushInstr(SUB, A, B)
		return nil
	case 0x91:
		// SUB A, C
		// Subtract C from A
		c.PushInstr(SUB, A, C)
		return nil
	case 0x92:
		// SUB A, D
		// Subtract D from A
		c.PushInstr(SUB, A, D)
		return nil
	case 0x93:
		// SUB A, E
		// Subtract E from A
		c.PushInstr(SUB, A, E)
		return nil
	case 0x94:
		// SUB A, H
		// Subtract H from A
		c.PushInstr(SUB, A, H)
		return nil
	case 0x95:
		// SUB A, L
		// Subtract L from A
		c.PushInstr(SUB, A, L)
		return nil
	case 0x96:
		// SUB A, (HL)
		// Subtract value pointed by HL from A
		c.PushInstr(SUB, A, Indirect{HL})
		return nil
	case 0x97:
		// SUB A, A
		// Subtract A from A
		c.PushInstr(SUB, A, A)
		return nil
	case 0x98:
		// SBC A, B
		// Subtract B and carry flag from A
		c.PushInstr(SBC, A, B)
		return nil
	case 0x99:
		// SBC A, C
		// Subtract C and carry flag from A
		c.PushInstr(SBC, A, C)
		return nil
	case 0x9A:
		// SBC A, D
		// Subtract D and carry flag from A
		c.PushInstr(SBC, A, D)
		return nil
	case 0x9B:
		// SBC A, E
		// Subtract E and carry flag from A
		c.PushInstr(SBC, A, E)
		return nil
	case 0x9C:
		// SBC A, H
		// Subtract H and carry flag from A
		c.PushInstr(SBC, A, H)
		return nil
	case 0x9D:
		// SBC A, L
		// Subtract and carry flag L from A
		c.PushInstr(SBC, A, L)
		return nil
	case 0x9E:
		// SBC A, (HL)
		// Subtract value pointed by HL and carry flag from A
		c.PushInstr(SBC, A, Indirect{HL})
		return nil
	case 0x9F:
		// SBC A, A
		// Subtract A and carry flag from A
		c.PushInstr(SBC, A, A)
		return nil
	case 0xA0:
		// AND B
		// Logical AND B against A
		c.PushInstr(AND, B)
		return nil
	case 0xA1:
		// AND C
		// Logical AND C against A
		c.PushInstr(AND, C)
		return nil
	case 0xA2:
		// AND D
		// Logical AND D against A
		c.PushInstr(AND, D)
		return nil
	case 0xA3:
		// AND E
		// Logical AND E against A
		c.PushInstr(AND, E)
		return nil
	case 0xA4:
		// AND H
		// Logical AND H against A
		c.PushInstr(AND, H)
		return nil
	case 0xA5:
		// AND L
		// Logical AND L against A
		c.PushInstr(AND, L)
		return nil
	case 0xA6:
		// AND (HL)
		// Logical AND value pointed by HL against A
		c.PushInstr(AND, Indirect{HL})
		return nil
	case 0xA7:
		// AND A
		// Logical AND A against A
		c.PushInstr(AND, A)
		return nil
	case 0xA8:
		// XOR B
		// Logical XOR B against A
		c.PushInstr(XOR, B)
		return nil
	case 0xA9:
		// XOR C
		// Logical XOR C against A
		c.PushInstr(XOR, C)
		return nil
	case 0xAA:
		// XOR D
		// Logical XOR D against A
		c.PushInstr(XOR, D)
		return nil
	case 0xAB:
		// XOR E
		// Logical XOR E against A
		c.PushInstr(XOR, E)
		return nil
	case 0xAC:
		// XOR H
		// Logical XOR H against A
		c.PushInstr(XOR, H)
		return nil
	case 0xAD:
		// XOR L
		// Logical XOR L against A
		c.PushInstr(XOR, L)
		return nil
	case 0xAE:
		// XOR (HL)
		// Logical XOR value pointed by HL against A
		c.PushInstr(XOR, Indirect{HL})
		return nil
	case 0xAF:
		// XOR A
		// Logical XOR A against A
		c.PushInstr(XOR, A)
		return nil
	case 0xB0:
		// OR B
		// Logical OR B against A
		c.PushInstr(OR, B)
		return nil
	case 0xB1:
		// OR C
		// Logical OR C against A
		c.PushInstr(OR, C)
		return nil
	case 0xB2:
		// OR D
		// Logical OR D against A
		c.PushInstr(OR, D)
		return nil
	case 0xB3:
		// OR E
		// Logical OR E against A
		c.PushInstr(OR, E)
		return nil
	case 0xB4:
		// OR H
		// Logical OR H against A
		c.PushInstr(OR, H)
		return nil
	case 0xB5:
		// OR L
		// Logical OR L against A
		c.PushInstr(OR, L)
		return nil
	case 0xB6:
		// OR (HL)
		// Logical OR value pointed by HL against A
		c.PushInstr(OR, Indirect{HL})
		return nil
	case 0xB7:
		// OR A
		// Logical OR A against A
		c.PushInstr(OR, A)
		return nil
	case 0xB8:
		// CP B
		// Compare B against A
		c.PushInstr(CP, B)
		return nil
	case 0xB9:
		// CP C
		// Compare C against A
		c.PushInstr(CP, C)
		return nil
	case 0xBA:
		// CP D
		// Compare D against A
		c.PushInstr(CP, D)
		return nil
	case 0xBB:
		// CP E
		// Compare E against A
		c.PushInstr(CP, E)
		return nil
	case 0xBC:
		// CP H
		// Compare H against A
		c.PushInstr(CP, H)
		return nil
	case 0xBD:
		// CP L
		// Compare L against A
		c.PushInstr(CP, L)
		return nil
	case 0xBE:
		// CP (HL)
		// Compare value pointed by HL against A
		c.PushInstr(CP, Indirect{HL})
		return nil
	case 0xBF:
		// CP A
		// Compare A against A
		c.PushInstr(CP, A)
		return nil
	case 0xC0:
		// RET NZ
		// Return if last result was not zero
		c.PlaceRet( CondNonZero)
		return nil
	case 0xC1:
		// POP BC
		// Pop 16-bit value from stack into BC
		c.PushInstr(POP, BC)
		return nil
	case 0xC2:
		// JP NZ, nn
		// Absolute jump to 16-bit location if last result was not zero
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 2)
		if err != nil {
			return err
		}
		c.PlaceJump(CondNonZero, Address(Immediate16(uint16(operands[1])<<8|uint16(operands[0]))))
		return nil
	case 0xC3:
		// JP nn
		// Absolute jump to 16-bit location
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 2)
		if err != nil {
			return err
		}
		c.PlaceJump(CondAlways, Address(Immediate16(uint16(operands[1])<<8|uint16(operands[0]))))
		return nil
	case 0xC4:
		// CALL NZ, nn
		// Call routine at 16-bit location if last result was not zero
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 2)
		if err != nil {
			return err
		}
		c.PlaceCall(CondNonZero, Address(Immediate16(uint16(operands[1])<<8|uint16(operands[0]))))
		return nil
	case 0xC5:
		// PUSH BC
		// Push 16-bit BC onto stack
		c.PushInstr(PUSH, BC)
		return nil
	case 0xC6:
		// ADD A, n
		// Add 8-bit immediate to A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(ADD, A, Immediate8(operands[0]))
		return nil
	case 0xC7:
		// RST 0
		// Call routine at address 0000h
		c.PushInstr(RST, StaticParam(0))
		return nil
	case 0xC8:
		// RET Z
		// Return if last result was zero
		c.PlaceRet( CondZero)
		return nil
	case 0xC9:
		// RET
		// Return to calling routine
		c.PlaceRet(CondAlways)
		return nil
	case 0xCA:
		// JP Z, nn
		// Absolute jump to 16-bit location if last result was zero
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 2)
		if err != nil {
			return err
		}
		c.PlaceJump(CondZero, Address(Immediate16(uint16(operands[1])<<8|uint16(operands[0]))))
		return nil
	case 0xCB:
		return compileExtCB(c)
	case 0xCC:
		// CALL Z, nn
		// Call routine at 16-bit location if last result was zero
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 2)
		if err != nil {
			return err
		}
		c.PlaceCall(CondZero, Address(Immediate16(uint16(operands[1])<<8|uint16(operands[0]))))
		return nil
	case 0xCD:
		// CALL nn
		// Call routine at 16-bit location
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 2)
		if err != nil {
			return err
		}
		c.PlaceCall(CondAlways, Address(Immediate16(uint16(operands[1])<<8|uint16(operands[0]))))
		return nil
	case 0xCE:
		// ADC A, n
		// Add 8-bit immediate and carry to A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(ADC, A, Immediate8(operands[0]))
		return nil
	case 0xCF:
		// RST 8
		// Call routine at address 0008h
		c.PushInstr(RST, StaticParam(8))
		return nil
	case 0xD0:
		// RET NC
		// Return if last result caused no carry
		c.PlaceRet( CondNonCarry)
		return nil
	case 0xD1:
		// POP DE
		// Pop 16-bit value from stack into DE
		c.PushInstr(POP, DE)
		return nil
	case 0xD2:
		// JP NC, nn
		// Absolute jump to 16-bit location if last result caused no carry
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 2)
		if err != nil {
			return err
		}
		c.PlaceJump(CondNonCarry, Address(Immediate16(uint16(operands[1])<<8|uint16(operands[0]))))
		return nil
	case 0xD3: // XX
		break
	case 0xD4:
		// CALL NC, nn
		// Call routine at 16-bit location if last result caused no carry
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 2)
		if err != nil {
			return err
		}
		c.PlaceCall(CondNonCarry, Address(Immediate16(uint16(operands[1])<<8|uint16(operands[0]))))
		return nil
	case 0xD5:
		// PUSH DE
		// Push 16-bit DE onto stack
		c.PushInstr(PUSH, DE)
		return nil
	case 0xD6:
		// SUB A, n
		// Subtract 8-bit immediate from A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(SUB, A, Immediate8(operands[0]))
		return nil
	case 0xD7:
		// RST 10
		// Call routine at address 0010h
		c.PushInstr(RST, StaticParam(10))
		return nil
	case 0xD8:
		// RET C
		// Return if last result caused carry
		c.PlaceRet(CondCarry)
		return nil
	case 0xD9:
		// RETI
		// Enable interrupts and return to calling routine
		c.PushInstr(RETI)
		return nil
	case 0xDA:
		// JP C, nn
		// Absolute jump to 16-bit location if last result caused carry
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 2)
		if err != nil {
			return err
		}
		c.PlaceJump(CondCarry, Address(Immediate16(uint16(operands[1])<<8|uint16(operands[0]))))
		return nil
	case 0xDB: // XX
		break
	case 0xDC:
		// CALL C, nn
		// Call routine at 16-bit location if last result caused carry
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 2)
		if err != nil {
			return err
		}
		c.PlaceCall(CondCarry, Address(Immediate16(uint16(operands[1])<<8|uint16(operands[0]))))
		return nil
	case 0xDD: // XX
		break
	case 0xDE:
		// SBC A, n
		// Subtract 8-bit immediate and carry from A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(SBC, A, Immediate8(operands[0]))
		return nil
	case 0xDF:
		// RST 18
		// Call routine at address 0018h
		c.PushInstr(RST, StaticParam(18))
		return nil
	case 0xE0:
		// LDH (n), A
		// Save A at address pointed to by (FF00h + 8-bit immediate)
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(LDH, Immediate8(operands[0]), A)
		return nil
	case 0xE1:
		// POP HL
		// Pop 16-bit value from stack into HL
		c.PushInstr(POP, HL)
		return nil
	case 0xE2:
		// LDH (C), A
		// Save A at address pointed to by (FF00h + C)
		c.PushInstr(LDH, Indirect{C}, A)
		return nil
	case 0xE3: // XX
	case 0xE4: // XX
		break
	case 0xE5:
		// PUSH HL
		// Push 16-bit HL onto stack
		c.PushInstr(PUSH, HL)
		return nil
	case 0xE6:
		// AND n
		// Logical AND 8-bit immediate against A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(AND, Immediate8(operands[0]))
		return nil
	case 0xE7:
		// RST 20
		// Call routine at address 0020h
		c.PushInstr(RST, StaticParam(20))
		return nil
	case 0xE8:
		// ADD SP, d
		// Add signed 8-bit immediate to SP
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(ADD, SP, Immediate8(operands[0]))
		return nil
	case 0xE9:
		// JP (HL)
		// Jump to 16-bit value pointed by HL
		c.PlaceDynamicJump()
		return nil
	case 0xEA:
		// LD (nn), A
		// Save A at given 16-bit address
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 2)
		if err != nil {
			return err
		}
		c.PushInstr(LD, Indirect{Immediate16(uint16(operands[1])<<8 | uint16(operands[0]))}, A)
		return nil
	case 0xEB: // XX
	case 0xEC: // XX
	case 0xED: // XX
		break
	case 0xEE:
		// XOR n
		// Logical XOR 8-bit immediate against A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(XOR, Immediate8(operands[0]))
		return nil
	case 0xEF:
		// RST 28
		// Call routine at address 0028h
		c.PushInstr(RST, StaticParam(28))
		return nil
	case 0xF0:
		// LDH A, (n)
		// Load A from address pointed to by (FF00h + 8-bit immediate)
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(LDH, A, Immediate8(operands[0]))
		return nil
	case 0xF1:
		// POP AF
		// Pop 16-bit value from stack into AF
		c.PushInstr(POP, AF)
		return nil
	case 0xF2:
		c.PushInstr(LD, A, Indirect{C})
		return nil
	case 0xF3:
		// DI
		// DIsable interrupts
		c.PushInstr(DI)
		return nil
	case 0xF4: // XX
		break
	case 0xF5:
		// PUSH AF
		// Push 16-bit AF onto stack
		c.PushInstr(PUSH, AF)
		return nil
	case 0xF6:
		// OR n
		// Logical OR 8-bit immediate against A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(OR, Immediate8(operands[0]))
		return nil
	case 0xF7:
		// RST 30
		// Call routine at address 0030h
		c.PushInstr(RST, StaticParam(30))
		return nil
	case 0xF8:
		// LDHL SP, d
		// Add signed 8-bit immediate to SP and save result in HL
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(LDHL, SP, Immediate8(operands[0]))
		return nil
	case 0xF9:
		// LD SP, HL
		// Copy HL to SP
		c.PushInstr(LD, SP, HL)
		return nil
	case 0xFA:
		// LD A, (nn)
		// Load A from given 16-bit address
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 2)
		if err != nil {
			return err
		}
		c.PushInstr(LD, A, Indirect{Immediate16(uint16(operands[1])<<8 | uint16(operands[0]))})
		return nil
	case 0xFB:
		// EI
		// Enable interrupts
		c.PushInstr(EI)
		return nil
	case 0xFC: // XX
	case 0xFD: // XX
		break
	case 0xFE:
		// CP n
		// Compare 8-bit immediate against A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.input, operands[:], 1)
		if err != nil {
			return err
		}
		c.PushInstr(CP, Immediate8(operands[0]))
		return nil
	case 0xFF:
		// RST 38
		// Call routine at address 0038h
		c.PushInstr(RST, StaticParam(38))
		return nil
	}

	return InvalidOpcode(bytebuf[0])
}
