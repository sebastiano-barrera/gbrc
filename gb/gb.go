package gb

import (
	"errors"
	"fmt"
	"gbrc/compiler"
	"io"
)

// The type that ties all pieces together
type Arch struct{}

// Operand types
type Register8 int
type Register16 int
type IndirectReg Register16
type CompositeReg struct{ lo, hi Register8 }
type IndirectComposite CompositeReg
type Immediate16 uint16
type Immediate8 uint16

const (
	A Register8 = iota
	F
	B
	C
	D
	E
	H
	L

	SP Register16 = iota
	PC
)

// Represents an expression in the compiled code
type Expr struct {
	Format    string
	Terms     []Place
	ValueSize uint
}

func MkExpr(size uint, format string, args ...Place) *Expr {
	return &Expr{format, args, size}
}

// The general interface for representing accesses and mutations to
// the machine state in the compiled code.  For any Place p,
// p.String() must return the text of an *expression* that returns, in
// the compiled code, the value of the represented place;
// p.Setter(value) must return the *statement* that sets the place to
// the given value, checking that the size is correct by using the
// value's Size() method. (If the size is wrong, errSizeMismatch
// should be returned).
type Place interface {
	fmt.Stringer
	Setter(value Place) (string, error)
	Size() uint // In bytes
}

var (
	errReadOnly     = errors.New("compiler error: place is read-only")
	errSizeMismatch = errors.New("compiler error: assignment size mismatch")
)

//
// Implementations of Place for all the operand types
//

func (imm Immediate8) String() string {
	return fmt.Sprintf("0x%02x", uint16(imm))
}

func (imm Immediate8) Setter(value Place) (string, error) {
	return "", errReadOnly
}

func (imm Immediate8) Size() uint { return 1 }


func (imm Immediate16) String() string {
	return fmt.Sprintf("0x%04x", uint16(imm))
}

func (imm Immediate16) Setter(value Place) (string, error) {
	return "", errReadOnly
}

func (imm Immediate16) Size() uint { return 2 }


func (reg Register8) String() string {
	return fmt.Sprintf("m.Reg8[%d]", int(reg))
}

func (reg Register8) Setter(value Place) (string, error) {
	if value.Size() != reg.Size() {
		return "", errSizeMismatch
	}
	return fmt.Sprintf("m.Reg8[%d] = %v\n", int(reg), value), nil
}

func (reg Register8) Size() uint { return 1 }


func (reg Register16) String() string {
	return fmt.Sprintf("m.Reg16[%d]", int(reg))
}

func (reg Register16) Setter(value Place) (string, error) {
	if value.Size() != reg.Size() {
		return "", errSizeMismatch
	}
	return fmt.Sprintf("m.Reg16[%d] = %v\n", int(reg), value), nil
}

func (reg Register16) Size() uint { return 16 }


// type IndirectReg Register16
func (reg IndirectReg) String() string {
	return fmt.Sprintf("m.MemRead(%s)", Register16(reg))
}

func (reg IndirectReg) Setter(value Place) (string, error) {
	if value.Size() != 1 {
		return "", errSizeMismatch
	}
	stmt := fmt.Sprintf("m.MemWrite(%s, %s)\n", Register16(reg), value)
	return stmt, nil
}

func (reg IndirectReg) Size() uint { return 1 }


// type CompositeRegister struct{ lo, hi Register }
func (reg CompositeReg) String() string {
	return fmt.Sprintf("(uint16(%s) << 8 | uint16(%s))",
		reg.hi, reg.lo)
}

func (reg CompositeReg) Setter(value Place) (string, error) {
	if value.Size() != 2 {
		return "", errSizeMismatch
	}
	setterHi, err := reg.hi.Setter(MkExpr(1, "uint8(val >> 8)"))
	if err != nil {
		return "", err
	}
	setterLo, err := reg.lo.Setter(MkExpr(1, "uint8(val & 0xFF)"))
	if err != nil {
		return "", err
	}

	const tmpl = `
func(val uint16) {\n
	%s
	%s
}(%s)\n`
	stmt := fmt.Sprintf(tmpl, setterHi, setterLo, value)
	return stmt, nil
}

func (reg CompositeReg) Size() uint { return 2 }


// type IndirectComposite CompositeRegister
func (reg IndirectComposite) String() string {
	return fmt.Sprintf("m.MemRead(%s)", CompositeReg(reg))
}

func (reg IndirectComposite) Setter(value Place) (string, error) {
	if value.Size() != 2 {
		return "", errSizeMismatch
	}
	stmt := fmt.Sprintf("m.MemWrite(%s, %s)\n", CompositeReg(reg), value)
	return stmt, nil
}

func (reg IndirectComposite) Size() uint { return 2 }


func (expr *Expr) String() string {
	terms := make([]interface{}, len(expr.Terms))
	for i, term := range expr.Terms {
		terms[i] = term.(interface{})
	}
	return fmt.Sprintf(expr.Format, terms...)
}

func (expr *Expr) Setter(value Place) (string, error) {
	return "", errReadOnly
}

func (expr *Expr) Size() uint { return expr.ValueSize }


func compileInstrSLA(c *compiler.Compiler, operands ...Place) error {
	// SLA E	- Shift E left preserving sign
	// SLA L	- Shift L left preserving sign
	// SLA (HL)	- Shift value pointed by HL left preserving sign
	// SLA A	- Shift A left preserving sign
	// SLA C	- Shift C left preserving sign
	// SLA H	- Shift H left preserving sign
	// SLA D	- Shift D left preserving sign
	// SLA B	- Shift B left preserving sign
	shiftExpr := MkExpr(operands[0].Size(), "%s << 1", operands[0])
	instr, err := operands[0].Setter(shiftExpr)
	if err != nil {
		return err
	}
	c.PushInstr(instr)
	return nil
}

func compileInstrSRA(c *compiler.Compiler, operands ...Place) error {
	// SRA B	- Shift B right preserving sign
	// SRA (HL)	- Shift value pointed by HL right preserving sign
	// SRA C	- Shift C right preserving sign
	// SRA A	- Shift A right preserving sign
	// SRA D	- Shift D right preserving sign
	// SRA H	- Shift H right preserving sign
	// SRA E	- Shift E right preserving sign
	// SRA L	- Shift L right preserving sign
}

func compileInstrHALT(c *compiler.Compiler, operands ...Place) error {
	// HALT 	- Halt processor
}

func compileInstrRL(c *compiler.Compiler, operands ...Place) error {
	// RL L	- Rotate L left
	// RL (HL)	- Rotate value pointed by HL left
	// RL C	- Rotate C left
	// RL E	- Rotate E left
	// RL A	- Rotate A left
	// RL A	- Rotate A left
	// RL D	- Rotate D left
	// RL B	- Rotate B left
	// RL H	- Rotate H left
}

func compileInstrLDI(c *compiler.Compiler, operands ...Place) error {
	// LDI A, (HL)	- Load A from address pointed to by HL, and increment HL
	// LDI (HL), A	- Save A to address pointed by HL, and increment HL

	instr, err := operands[0].Setter(operands[1])
	if err != nil {
		return err
	}
	c.PushInstr(instr)

	incExpr := MkExpr(operands[0].Size(), "%s + 1", operands[0])
	instr, err = operands[0].Setter(incExpr)
	if err != nil {
		return err
	}
	c.PushInstr(instr)

	return nil
}

func compileInstrSTOP(c *compiler.Compiler, operands ...Place) error {
	// STOP 	- Stop processor
}

func compileInstrADD(c *compiler.Compiler, operands ...Place) error {
	// ADD SP, d	- Add signed 8-bit immediate to SP
	// ADD A, n	- Add 8-bit immediate to A
	// ADD HL, SP	- Add 16-bit SP to HL
	// ADD A, E	- Add E to A
	// ADD A, C	- Add C to A
	// ADD A, H	- Add H to A
	// ADD HL, DE	- Add 16-bit DE to HL
	// ADD A, B	- Add B to A
	// ADD HL, BC	- Add 16-bit BC to HL
	// ADD A, A	- Add A to A
	// ADD A, L	- Add L to A
	// ADD HL, HL	- Add 16-bit HL to HL
	// ADD A, (HL)	- Add value pointed by HL to A
	// ADD A, D	- Add D to A
}

func compileInstrRST(c *compiler.Compiler, operands ...Place) error {
	// RST 18	- Call routine at address 0018h
	// RST 28	- Call routine at address 0028h
	// RST 30	- Call routine at address 0030h
	// RST 10	- Call routine at address 0010h
	// RST 8	- Call routine at address 0008h
	// RST 20	- Call routine at address 0020h
	// RST 38	- Call routine at address 0038h
	// RST 0	- Call routine at address 0000h
}

func compileInstrSRL(c *compiler.Compiler, operands ...Place) error {
	// SRL E	- Shift E right
	// SRL H	- Shift H right
	// SRL B	- Shift B right
	// SRL A	- Shift A right
	// SRL (HL)	- Shift value pointed by HL right
	// SRL C	- Shift C right
	// SRL D	- Shift D right
	// SRL L	- Shift L right
}

func compileInstrSUB(c *compiler.Compiler, operands ...Place) error {
	// SUB A, A	- Subtract A from A
	// SUB A, n	- Subtract 8-bit immediate from A
	// SUB A, C	- Subtract C from A
	// SUB A, E	- Subtract E from A
	// SUB A, (HL)	- Subtract value pointed by HL from A
	// SUB A, D	- Subtract D from A
	// SUB A, B	- Subtract B from A
	// SUB A, L	- Subtract L from A
	// SUB A, H	- Subtract H from A
}

func compileInstrLDHL(c *compiler.Compiler, operands ...Place) error {
	// LDHL SP, d	- Add signed 8-bit immediate to SP and save result in HL

	instr, err := CompositeReg{H, L}.Setter(
		MkExpr(2, "%s + %s", SP, operands[1]))
	if err != nil {
		return err
	}
	c.PushInstr(instr)
	return nil
}

func compileInstrJP(c *compiler.Compiler, operands ...Place) error {
	// JP Z, nn	- Absolute jump to 16-bit location if last result was zero
	// JP C, nn	- Absolute jump to 16-bit location if last result caused carry
	// JP NZ, nn	- Absolute jump to 16-bit location if last result was not zero
	// JP NC, nn	- Absolute jump to 16-bit location if last result caused no carry
	// JP nn	- Absolute jump to 16-bit location
	// JP (HL)	- Jump to 16-bit value pointed by HL
}

func compileInstrDI(c *compiler.Compiler, operands ...Place) error {
	// DI 	- DIsable interrupts
}

func compileInstrINC(c *compiler.Compiler, operands ...Place) error {
	// INC L	- Increment L
	// INC C	- Increment C
	// INC E	- Increment E
	// INC DE	- Increment 16-bit DE
	// INC H	- Increment H
	// INC SP	- Increment 16-bit HL
	// INC BC	- Increment 16-bit BC
	// INC (HL)	- Increment value pointed by HL
	// INC B	- Increment B
	// INC D	- Increment D
	// INC HL	- Increment 16-bit HL
	// INC A	- Increment A
}

func compileInstrCP(c *compiler.Compiler, operands ...Place) error {
	// CP (HL)	- Compare value pointed by HL against A
	// CP B	- Compare B against A
	// CP H	- Compare H against A
	// CP E	- Compare E against A
	// CP C	- Compare C against A
	// CP A	- Compare A against A
	// CP L	- Compare L against A
	// CP n	- Compare 8-bit immediate against A
	// CP D	- Compare D against A
}

func compileInstrRET(c *compiler.Compiler, operands ...Place) error {
	// RET NZ	- Return if last result was not zero
	// RET NC	- Return if last result caused no carry
	// RET 	- Return to calling routine
	// RET C	- Return if last result caused carry
	// RET Z	- Return if last result was zero
}

func compileInstrRES(c *compiler.Compiler, operands ...Place) error {
	// RES 6, C	- Clear (reset) bit 6 of C
	// RES 0, A	- Clear (reset) bit 0 of A
	// RES 4, L	- Clear (reset) bit 4 of L
	// RES 6, (HL)	- Clear (reset) bit 6 of value pointed by HL
	// RES 7, A	- Clear (reset) bit 7 of A
	// RES 3, E	- Clear (reset) bit 3 of E
	// RES 3, A	- Clear (reset) bit 3 of A
	// RES 4, C	- Clear (reset) bit 4 of C
	// RES 1, A	- Clear (reset) bit 1 of A
	// RES 1, C	- Clear (reset) bit 1 of C
	// RES 5, (HL)	- Clear (reset) bit 5 of value pointed by HL
	// RES 4, D	- Clear (reset) bit 4 of D
	// RES 1, E	- Clear (reset) bit 1 of E
	// RES 7, (HL)	- Clear (reset) bit 7 of value pointed by HL
	// RES 6, B	- Clear (reset) bit 6 of B
	// RES 0, (HL)	- Clear (reset) bit 0 of value pointed by HL
	// RES 7, D	- Clear (reset) bit 7 of D
	// RES 2, A	- Clear (reset) bit 2 of A
	// RES 3, H	- Clear (reset) bit 3 of H
	// RES 1, (HL)	- Clear (reset) bit 1 of value pointed by HL
	// RES 3, C	- Clear (reset) bit 3 of C
	// RES 5, A	- Clear (reset) bit 5 of A
	// RES 2, H	- Clear (reset) bit 2 of H
	// RES 7, B	- Clear (reset) bit 7 of B
	// RES 2, L	- Clear (reset) bit 2 of L
	// RES 5, B	- Clear (reset) bit 5 of B
	// RES 3, D	- Clear (reset) bit 3 of D
	// RES 7, L	- Clear (reset) bit 7 of L
	// RES 7, C	- Clear (reset) bit 7 of C
	// RES 6, D	- Clear (reset) bit 6 of D
	// RES 4, B	- Clear (reset) bit 4 of B
	// RES 5, H	- Clear (reset) bit 5 of H
	// RES 6, A	- Clear (reset) bit 6 of A
	// RES 3, (HL)	- Clear (reset) bit 3 of value pointed by HL
	// RES 0, L	- Clear (reset) bit 0 of L
	// RES 5, E	- Clear (reset) bit 5 of E
	// RES 0, E	- Clear (reset) bit 0 of E
	// RES 0, C	- Clear (reset) bit 0 of C
	// RES 1, B	- Clear (reset) bit 1 of B
	// RES 0, D	- Clear (reset) bit 0 of D
	// RES 6, E	- Clear (reset) bit 6 of E
	// RES 6, L	- Clear (reset) bit 6 of L
	// RES 2, D	- Clear (reset) bit 2 of D
	// RES 4, (HL)	- Clear (reset) bit 4 of value pointed by HL
	// RES 1, D	- Clear (reset) bit 1 of D
	// RES 4, E	- Clear (reset) bit 4 of E
	// RES 2, (HL)	- Clear (reset) bit 2 of value pointed by HL
	// RES 2, E	- Clear (reset) bit 2 of E
	// RES 4, H	- Clear (reset) bit 4 of H
	// RES 2, B	- Clear (reset) bit 2 of B
	// RES 3, L	- Clear (reset) bit 3 of L
	// RES 1, H	- Clear (reset) bit 1 of H
	// RES 5, L	- Clear (reset) bit 5 of L
	// RES 7, E	- Clear (reset) bit 7 of E
	// RES 2, C	- Clear (reset) bit 2 of C
	// RES 3, B	- Clear (reset) bit 3 of B
	// RES 7, H	- Clear (reset) bit 7 of H
	// RES 5, D	- Clear (reset) bit 5 of D
	// RES 5, C	- Clear (reset) bit 5 of C
	// RES 0, B	- Clear (reset) bit 0 of B
	// RES 1, L	- Clear (reset) bit 1 of L
	// RES 4, A	- Clear (reset) bit 4 of A
	// RES 0, H	- Clear (reset) bit 0 of H
	// RES 6, H	- Clear (reset) bit 6 of H
}

func compileInstrCCF(c *compiler.Compiler, operands ...Place) error {
	// CCF 	- Clear carry flag
}

func compileInstrRETI(c *compiler.Compiler, operands ...Place) error {
	// RETI 	- Enable interrupts and return to calling routine
}

func compileInstrNOP(c *compiler.Compiler, operands ...Place) error {
	// NOP 	- No Operation
}

func compileInstrBIT(c *compiler.Compiler, operands ...Place) error {
	// BIT 0, (HL)	- Test bit 0 of value pointed by HL
	// BIT 1, L	- Test bit 1 of L
	// BIT 0, H	- Test bit 0 of H
	// BIT 6, (HL)	- Test bit 6 of value pointed by HL
	// BIT 5, H	- Test bit 5 of H
	// BIT 0, L	- Test bit 0 of L
	// BIT 5, C	- Test bit 5 of C
	// BIT 0, C	- Test bit 0 of C
	// BIT 1, C	- Test bit 1 of C
	// BIT 4, (HL)	- Test bit 4 of value pointed by HL
	// BIT 1, A	- Test bit 1 of A
	// BIT 2, C	- Test bit 2 of C
	// BIT 2, (HL)	- Test bit 2 of value pointed by HL
	// BIT 2, D	- Test bit 2 of D
	// BIT 3, D	- Test bit 3 of D
	// BIT 5, D	- Test bit 5 of D
	// BIT 4, H	- Test bit 4 of H
	// BIT 1, H	- Test bit 1 of H
	// BIT 7, B	- Test bit 7 of B
	// BIT 3, L	- Test bit 3 of L
	// BIT 1, B	- Test bit 1 of B
	// BIT 7, E	- Test bit 7 of E
	// BIT 7, (HL)	- Test bit 7 of value pointed by HL
	// BIT 2, H	- Test bit 2 of H
	// BIT 3, E	- Test bit 3 of E
	// BIT 3, C	- Test bit 3 of C
	// BIT 0, A	- Test bit 0 of A
	// BIT 3, A	- Test bit 3 of A
	// BIT 4, C	- Test bit 4 of C
	// BIT 4, B	- Test bit 4 of B
	// BIT 6, D	- Test bit 6 of D
	// BIT 7, C	- Test bit 7 of C
	// BIT 4, A	- Test bit 4 of A
	// BIT 6, A	- Test bit 6 of A
	// BIT 1, D	- Test bit 1 of D
	// BIT 5, E	- Test bit 5 of E
	// BIT 5, (HL)	- Test bit 5 of value pointed by HL
	// BIT 2, E	- Test bit 2 of E
	// BIT 2, B	- Test bit 2 of B
	// BIT 6, H	- Test bit 6 of H
	// BIT 5, A	- Test bit 5 of A
	// BIT 7, A	- Test bit 7 of A
	// BIT 4, D	- Test bit 4 of D
	// BIT 1, E	- Test bit 1 of E
	// BIT 3, H	- Test bit 3 of H
	// BIT 7, H	- Test bit 7 of H
	// BIT 7, D	- Test bit 7 of D
	// BIT 6, C	- Test bit 6 of C
	// BIT 0, D	- Test bit 0 of D
	// BIT 4, E	- Test bit 4 of E
	// BIT 5, B	- Test bit 5 of B
	// BIT 5, L	- Test bit 5 of L
	// BIT 1, (HL)	- Test bit 1 of value pointed by HL
	// BIT 6, E	- Test bit 6 of E
	// BIT 4, L	- Test bit 4 of L
	// BIT 6, L	- Test bit 6 of L
	// BIT 6, B	- Test bit 6 of B
	// BIT 3, B	- Test bit 3 of B
	// BIT 2, L	- Test bit 2 of L
	// BIT 0, E	- Test bit 0 of E
	// BIT 0, B	- Test bit 0 of B
	// BIT 3, (HL)	- Test bit 3 of value pointed by HL
	// BIT 2, A	- Test bit 2 of A
	// BIT 7, L	- Test bit 7 of L
}

func compileInstrAND(c *compiler.Compiler, operands ...Place) error {
	// AND L	- Logical AND L against A
	// AND H	- Logical AND H against A
	// AND C	- Logical AND C against A
	// AND n	- Logical AND 8-bit immediate against A
	// AND E	- Logical AND E against A
	// AND B	- Logical AND B against A
	// AND A	- Logical AND A against A
	// AND D	- Logical AND D against A
	// AND (HL)	- Logical AND value pointed by HL against A
}

func compileInstrLDH(c *compiler.Compiler, operands ...Place) error {
	// LDH (n), A	- Save A at address pointed to by (FF00h + 8-bit immediate)
	// LDH A, (n)	- Load A from address pointed to by (FF00h + 8-bit immediate)
	// LDH (C), A	- Save A at address pointed to by (FF00h + C)
}

func compileInstrLD(c *compiler.Compiler, operands ...Place) error {
	// LD (nn), SP	- Save SP to given address
	// LD D, D	- Copy D to D
	// LD H, L	- Copy L to H
	// LD B, E	- Copy E to B
	// LD (HL), E	- Copy E to address pointed by HL
	// LD A, B	- Copy B to A
	// LD (HL), B	- Copy B to address pointed by HL
	// LD A, n	- Load 8-bit immediate into A
	// LD B, D	- Copy D to B
	// LD E, (HL)	- Copy value pointed by HL to E
	// LD C, L	- Copy L to C
	// LD L, H	- Copy H to L
	// LD B, H	- Copy H to B
	// LD L, E	- Copy E to L
	// LD L, B	- Copy B to L
	// LD (HL), L	- Copy L to address pointed by HL
	// LD E, B	- Copy B to E
	// LD H, n	- Load 8-bit immediate into H
	// LD B, B	- Copy B to B
	// LD E, A	- Copy A to E
	// LD A, L	- Copy L to A
	// LD (HL), H	- Copy H to address pointed by HL
	// LD L, C	- Copy C to L
	// LD D, C	- Copy C to D
	// LD L, L	- Copy L to L
	// LD D, L	- Copy L to D
	// LD L, D	- Copy D to L
	// LD (DE), A	- Save A to address pointed by DE
	// LD B, (HL)	- Copy value pointed by HL to B
	// LD H, E	- Copy E to H
	// LD SP, nn	- Load 16-bit immediate into SP
	// LD D, H	- Copy H to D
	// LD C, A	- Copy A to C
	// LD C, B	- Copy B to C
	// LD (HL), A	- Copy A to address pointed by HL
	// LD HL, nn	- Load 16-bit immediate into HL
	// LD A, (nn)	- Load A from given 16-bit address
	// LD (HL), C	- Copy C to address pointed by HL
	// LD D, (HL)	- Copy value pointed by HL to D
	// LD H, A	- Copy A to H
	// LD D, n	- Load 8-bit immediate into D
	// LD (BC), A	- Save A to address pointed by BC
	// LD A, (BC)	- Load A from address pointed to by BC
	// LD A, E	- Copy E to A
	// LD H, D	- Copy D to H
	// LD A, (HL)	- Copy value pointed by HL to A
	// LD E, n	- Load 8-bit immediate into E
	// LD B, C	- Copy C to B
	// LD (HL), n	- Load 8-bit immediate into address pointed by HL
	// LD B, L	- Copy L to B
	// LD H, H	- Copy H to H
	// LD A, A	- Copy A to A
	// LD A, C	- Copy C to A
	// LD C, C	- Copy C to C
	// LD C, D	- Copy D to C
	// LD A, H	- Copy H to A
	// LD DE, nn	- Load 16-bit immediate into DE
	// LD C, E	- Copy E to C
	// LD (HL), D	- Copy D to address pointed by HL
	// LD L, (HL)	- Copy value pointed by HL to L
	// LD B, n	- Load 8-bit immediate into B
	// LD D, E	- Copy E to D
	// LD E, H	- Copy H to E
	// LD E, L	- Copy L to E
	// LD C, H	- Copy H to C
	// LD H, (HL)	- Copy value pointed by HL to H
	// LD E, C	- Copy C to E
	// LD H, C	- Copy C to H
	// LD SP, HL	- Copy HL to SP
	// LD E, E	- Copy E to E
	// LD C, n	- Load 8-bit immediate into C
	// LD L, A	- Copy A to L
	// LD D, A	- Copy A to D
	// LD E, D	- Copy D to E
	// LD A, D	- Copy D to A
	// LD (nn), A	- Save A at given 16-bit address
	// LD D, B	- Copy B to D
	// LD A, (DE)	- Load A from address pointed to by DE
	// LD B, A	- Copy A to B
	// LD C, (HL)	- Copy value pointed by HL to C
	// LD L, n	- Load 8-bit immediate into L
	// LD BC, nn	- Load 16-bit immediate into BC
	// LD H, B	- Copy B to H
}

func compileInstrSCF(c *compiler.Compiler, operands ...Place) error {
	// SCF 	- Set carry flag
}

func compileInstrRLC(c *compiler.Compiler, operands ...Place) error {
	// RLC H	- Rotate H left with carry
	// RLC A	- Rotate A left with carry
	// RLC E	- Rotate E left with carry
	// RLC A	- Rotate A left with carry
	// RLC (HL)	- Rotate value pointed by HL left with carry
	// RLC L	- Rotate L left with carry
	// RLC C	- Rotate C left with carry
	// RLC D	- Rotate D left with carry
	// RLC B	- Rotate B left with carry
}

func compileInstrSWAP(c *compiler.Compiler, operands ...Place) error {
	// SWAP E	- Swap nybbles in E
	// SWAP B	- Swap nybbles in B
	// SWAP H	- Swap nybbles in H
	// SWAP L	- Swap nybbles in L
	// SWAP (HL)	- Swap nybbles in value pointed by HL
	// SWAP C	- Swap nybbles in C
	// SWAP A	- Swap nybbles in A
	// SWAP D	- Swap nybbles in D
}

func compileInstrCPL(c *compiler.Compiler, operands ...Place) error {
	// CPL 	- Complement (logical NOT) on A
}

func compileInstrDEC(c *compiler.Compiler, operands ...Place) error {
	// DEC SP	- Decrement 16-bit SP
	// DEC A	- Decrement A
	// DEC C	- Decrement C
	// DEC L	- Decrement L
	// DEC E	- Decrement E
	// DEC (HL)	- Decrement value pointed by HL
	// DEC B	- Decrement B
	// DEC DE	- Decrement 16-bit DE
	// DEC HL	- Decrement 16-bit HL
	// DEC BC	- Decrement 16-bit BC
	// DEC D	- Decrement D
	// DEC H	- Decrement H
}

func compileInstrCALL(c *compiler.Compiler, operands ...Place) error {
	// CALL Z, nn	- Call routine at 16-bit location if last result was zero
	// CALL C, nn	- Call routine at 16-bit location if last result caused carry
	// CALL NZ, nn	- Call routine at 16-bit location if last result was not zero
	// CALL nn	- Call routine at 16-bit location
	// CALL NC, nn	- Call routine at 16-bit location if last result caused no carry
}

func compileInstrSBC(c *compiler.Compiler, operands ...Place) error {
	// SBC A, D	- Subtract D and carry flag from A
	// SBC A, C	- Subtract C and carry flag from A
	// SBC A, n	- Subtract 8-bit immediate and carry from A
	// SBC A, H	- Subtract H and carry flag from A
	// SBC A, E	- Subtract E and carry flag from A
	// SBC A, (HL)	- Subtract value pointed by HL and carry flag from A
	// SBC A, A	- Subtract A and carry flag from A
	// SBC A, B	- Subtract B and carry flag from A
	// SBC A, L	- Subtract and carry flag L from A
}

func compileInstrDAA(c *compiler.Compiler, operands ...Place) error {
	// DAA 	- Adjust A for BCD addition
}

func compileInstrRRC(c *compiler.Compiler, operands ...Place) error {
	// RRC E	- Rotate E right with carry
	// RRC L	- Rotate L right with carry
	// RRC A	- Rotate A right with carry
	// RRC H	- Rotate H right with carry
	// RRC B	- Rotate B right with carry
	// RRC A	- Rotate A right with carry
	// RRC C	- Rotate C right with carry
	// RRC D	- Rotate D right with carry
	// RRC (HL)	- Rotate value pointed by HL right with carry
}

func compileInstrSET(c *compiler.Compiler, operands ...Place) error {
	// SET 0, A	- Set bit 0 of A
	// SET 5, L	- Set bit 5 of L
	// SET 6, C	- Set bit 6 of C
	// SET 0, C	- Set bit 0 of C
	// SET 5, B	- Set bit 5 of B
	// SET 7, D	- Set bit 7 of D
	// SET 7, (HL)	- Set bit 7 of value pointed by HL
	// SET 2, E	- Set bit 2 of E
	// SET 3, L	- Set bit 3 of L
	// SET 1, E	- Set bit 1 of E
	// SET 3, A	- Set bit 3 of A
	// SET 6, D	- Set bit 6 of D
	// SET 5, (HL)	- Set bit 5 of value pointed by HL
	// SET 2, H	- Set bit 2 of H
	// SET 1, A	- Set bit 1 of A
	// SET 4, H	- Set bit 4 of H
	// SET 5, H	- Set bit 5 of H
	// SET 0, E	- Set bit 0 of E
	// SET 0, L	- Set bit 0 of L
	// SET 1, L	- Set bit 1 of L
	// SET 2, B	- Set bit 2 of B
	// SET 6, L	- Set bit 6 of L
	// SET 2, L	- Set bit 2 of L
	// SET 7, B	- Set bit 7 of B
	// SET 5, A	- Set bit 5 of A
	// SET 6, H	- Set bit 6 of H
	// SET 6, B	- Set bit 6 of B
	// SET 3, E	- Set bit 3 of E
	// SET 6, A	- Set bit 6 of A
	// SET 2, (HL)	- Set bit 2 of value pointed by HL
	// SET 7, A	- Set bit 7 of A
	// SET 4, L	- Set bit 4 of L
	// SET 7, C	- Set bit 7 of C
	// SET 7, E	- Set bit 7 of E
	// SET 5, C	- Set bit 5 of C
	// SET 4, (HL)	- Set bit 4 of value pointed by HL
	// SET 6, (HL)	- Set bit 6 of value pointed by HL
	// SET 0, (HL)	- Set bit 0 of value pointed by HL
	// SET 1, C	- Set bit 1 of C
	// SET 4, D	- Set bit 4 of D
	// SET 1, H	- Set bit 1 of H
	// SET 5, E	- Set bit 5 of E
	// SET 4, E	- Set bit 4 of E
	// SET 4, C	- Set bit 4 of C
	// SET 0, D	- Set bit 0 of D
	// SET 2, D	- Set bit 2 of D
	// SET 3, C	- Set bit 3 of C
	// SET 4, A	- Set bit 4 of A
	// SET 1, D	- Set bit 1 of D
	// SET 7, H	- Set bit 7 of H
	// SET 1, (HL)	- Set bit 1 of value pointed by HL
	// SET 7, L	- Set bit 7 of L
	// SET 2, C	- Set bit 2 of C
	// SET 5, D	- Set bit 5 of D
	// SET 3, (HL)	- Set bit 3 of value pointed by HL
	// SET 1, B	- Set bit 1 of B
	// SET 3, B	- Set bit 3 of B
	// SET 0, B	- Set bit 0 of B
	// SET 6, E	- Set bit 6 of E
	// SET 3, D	- Set bit 3 of D
	// SET 3, H	- Set bit 3 of H
	// SET 0, H	- Set bit 0 of H
	// SET 2, A	- Set bit 2 of A
	// SET 4, B	- Set bit 4 of B
}

func compileInstrJR(c *compiler.Compiler, operands ...Place) error {
	// JR NC, n	- Relative jump by signed immediate if last result caused no carry
	// JR Z, n	- Relative jump by signed immediate if last result was zero
	// JR n	- Relative jump by signed immediate
	// JR C, n	- Relative jump by signed immediate if last result caused carry
	// JR NZ, n	- Relative jump by signed immediate if last result was not zero
}

func compileInstrRR(c *compiler.Compiler, operands ...Place) error {
	// RR A	- Rotate A right
	// RR B	- Rotate B right
	// RR A	- Rotate A right
	// RR C	- Rotate C right
	// RR L	- Rotate L right
	// RR E	- Rotate E right
	// RR D	- Rotate D right
	// RR H	- Rotate H right
	// RR (HL)	- Rotate value pointed by HL right
}

func compileInstrLDD(c *compiler.Compiler, operands ...Place) error {
	// LDD A, (HL)	- Load A from address pointed to by HL, and decrement HL
	// LDD (HL), A	- Save A to address pointed by HL, and decrement HL
}

func compileInstrADC(c *compiler.Compiler, operands ...Place) error {
	// ADC A, A	- Add A and carry flag to A
	// ADC A, n	- Add 8-bit immediate and carry to A
	// ADC A, C	- Add C and carry flag to A
	// ADC A, H	- Add H and carry flag to A
	// ADC A, L	- Add and carry flag L to A
	// ADC A, (HL)	- Add value pointed by HL and carry flag to A
	// ADC A, D	- Add D and carry flag to A
	// ADC A, E	- Add E and carry flag to A
	// ADC A, B	- Add B and carry flag to A
}

func compileInstrXOR(c *compiler.Compiler, operands ...Place) error {
	// XOR H	- Logical XOR H against A
	// XOR E	- Logical XOR E against A
	// XOR (HL)	- Logical XOR value pointed by HL against A
	// XOR L	- Logical XOR L against A
	// XOR B	- Logical XOR B against A
	// XOR n	- Logical XOR 8-bit immediate against A
	// XOR A	- Logical XOR A against A
	// XOR D	- Logical XOR D against A
	// XOR C	- Logical XOR C against A
}

func compileInstrPUSH(c *compiler.Compiler, operands ...Place) error {
	// PUSH AF	- Push 16-bit AF onto stack
	// PUSH DE	- Push 16-bit DE onto stack
	// PUSH HL	- Push 16-bit HL onto stack
	// PUSH BC	- Push 16-bit BC onto stack
}

func compileInstrPOP(c *compiler.Compiler, operands ...Place) error {
	// POP AF	- Pop 16-bit value from stack into AF
	// POP DE	- Pop 16-bit value from stack into DE
	// POP BC	- Pop 16-bit value from stack into BC
	// POP HL	- Pop 16-bit value from stack into HL
}

func compileInstrEI(c *compiler.Compiler, operands ...Place) error {
	// EI 	- Enable interrupts
}

func compileInstrXX(c *compiler.Compiler, operands ...Place) error {
	// XX 	- Operation removed in this CPU
	// XX 	- Operation removed in this CPU
	// XX 	- Operation removed in this CPU
	// XX 	- Operation removed in this CPU
	// XX 	- Operation removed in this CPU
	// XX 	- Operation removed in this CPU
	// XX 	- Operation removed in this CPU
	// XX 	- Operation removed in this CPU
	// XX 	- Operation removed in this CPU
	// XX 	- Operation removed in this CPU
	// XX 	- Operation removed in this CPU
	// XX 	- Operation removed in this CPU
}

func compileInstrOR(c *compiler.Compiler, operands ...Place) error {
	// OR n	- Logical OR 8-bit immediate against A
	// OR A	- Logical OR A against A
	// OR E	- Logical OR E against A
	// OR D	- Logical OR D against A
	// OR C	- Logical OR C against A
	// OR L	- Logical OR L against A
	// OR H	- Logical OR H against A
	// OR (HL)	- Logical OR value pointed by HL against A
	// OR B	- Logical OR B against A
}

func (a Arch) compileExtCB(c *compiler.Compiler) error {
	var bytebuf [1]byte
	_, err := c.Input.Read(bytebuf[:])
	if err != nil {
		return err
	}

	switch bytebuf[0] {
	case 0x00:
		// RLC B
		// Rotate B left with carry
		compileInstrRLC(B)
	case 0x01:
		// RLC C
		// Rotate C left with carry
		compileInstrRLC(C)
	case 0x02:
		// RLC D
		// Rotate D left with carry
		compileInstrRLC(D)
	case 0x03:
		// RLC E
		// Rotate E left with carry
		compileInstrRLC(E)
	case 0x04:
		// RLC H
		// Rotate H left with carry
		compileInstrRLC(H)
	case 0x05:
		// RLC L
		// Rotate L left with carry
		compileInstrRLC(L)
	case 0x06:
		// RLC (HL)
		// Rotate value pointed by HL left with carry
		compileInstrRLC(IndirectRegister((HL)))
	case 0x07:
		// RLC A
		// Rotate A left with carry
		compileInstrRLC(A)
	case 0x08:
		// RRC B
		// Rotate B right with carry
		compileInstrRRC(B)
	case 0x09:
		// RRC C
		// Rotate C right with carry
		compileInstrRRC(C)
	case 0x0A:
		// RRC D
		// Rotate D right with carry
		compileInstrRRC(D)
	case 0x0B:
		// RRC E
		// Rotate E right with carry
		compileInstrRRC(E)
	case 0x0C:
		// RRC H
		// Rotate H right with carry
		compileInstrRRC(H)
	case 0x0D:
		// RRC L
		// Rotate L right with carry
		compileInstrRRC(L)
	case 0x0E:
		// RRC (HL)
		// Rotate value pointed by HL right with carry
		compileInstrRRC(IndirectRegister((HL)))
	case 0x0F:
		// RRC A
		// Rotate A right with carry
		compileInstrRRC(A)
	case 0x10:
		// RL B
		// Rotate B left
		compileInstrRL(B)
	case 0x11:
		// RL C
		// Rotate C left
		compileInstrRL(C)
	case 0x12:
		// RL D
		// Rotate D left
		compileInstrRL(D)
	case 0x13:
		// RL E
		// Rotate E left
		compileInstrRL(E)
	case 0x14:
		// RL H
		// Rotate H left
		compileInstrRL(H)
	case 0x15:
		// RL L
		// Rotate L left
		compileInstrRL(L)
	case 0x16:
		// RL (HL)
		// Rotate value pointed by HL left
		compileInstrRL(IndirectRegister((HL)))
	case 0x17:
		// RL A
		// Rotate A left
		compileInstrRL(A)
	case 0x18:
		// RR B
		// Rotate B right
		compileInstrRR(B)
	case 0x19:
		// RR C
		// Rotate C right
		compileInstrRR(C)
	case 0x1A:
		// RR D
		// Rotate D right
		compileInstrRR(D)
	case 0x1B:
		// RR E
		// Rotate E right
		compileInstrRR(E)
	case 0x1C:
		// RR H
		// Rotate H right
		compileInstrRR(H)
	case 0x1D:
		// RR L
		// Rotate L right
		compileInstrRR(L)
	case 0x1E:
		// RR (HL)
		// Rotate value pointed by HL right
		compileInstrRR(IndirectRegister((HL)))
	case 0x1F:
		// RR A
		// Rotate A right
		compileInstrRR(A)
	case 0x20:
		// SLA B
		// Shift B left preserving sign
		compileInstrSLA(B)
	case 0x21:
		// SLA C
		// Shift C left preserving sign
		compileInstrSLA(C)
	case 0x22:
		// SLA D
		// Shift D left preserving sign
		compileInstrSLA(D)
	case 0x23:
		// SLA E
		// Shift E left preserving sign
		compileInstrSLA(E)
	case 0x24:
		// SLA H
		// Shift H left preserving sign
		compileInstrSLA(H)
	case 0x25:
		// SLA L
		// Shift L left preserving sign
		compileInstrSLA(L)
	case 0x26:
		// SLA (HL)
		// Shift value pointed by HL left preserving sign
		compileInstrSLA(IndirectRegister((HL)))
	case 0x27:
		// SLA A
		// Shift A left preserving sign
		compileInstrSLA(A)
	case 0x28:
		// SRA B
		// Shift B right preserving sign
		compileInstrSRA(B)
	case 0x29:
		// SRA C
		// Shift C right preserving sign
		compileInstrSRA(C)
	case 0x2A:
		// SRA D
		// Shift D right preserving sign
		compileInstrSRA(D)
	case 0x2B:
		// SRA E
		// Shift E right preserving sign
		compileInstrSRA(E)
	case 0x2C:
		// SRA H
		// Shift H right preserving sign
		compileInstrSRA(H)
	case 0x2D:
		// SRA L
		// Shift L right preserving sign
		compileInstrSRA(L)
	case 0x2E:
		// SRA (HL)
		// Shift value pointed by HL right preserving sign
		compileInstrSRA(IndirectRegister((HL)))
	case 0x2F:
		// SRA A
		// Shift A right preserving sign
		compileInstrSRA(A)
	case 0x30:
		// SWAP B
		// Swap nybbles in B
		compileInstrSWAP(B)
	case 0x31:
		// SWAP C
		// Swap nybbles in C
		compileInstrSWAP(C)
	case 0x32:
		// SWAP D
		// Swap nybbles in D
		compileInstrSWAP(D)
	case 0x33:
		// SWAP E
		// Swap nybbles in E
		compileInstrSWAP(E)
	case 0x34:
		// SWAP H
		// Swap nybbles in H
		compileInstrSWAP(H)
	case 0x35:
		// SWAP L
		// Swap nybbles in L
		compileInstrSWAP(L)
	case 0x36:
		// SWAP (HL)
		// Swap nybbles in value pointed by HL
		compileInstrSWAP(IndirectRegister((HL)))
	case 0x37:
		// SWAP A
		// Swap nybbles in A
		compileInstrSWAP(A)
	case 0x38:
		// SRL B
		// Shift B right
		compileInstrSRL(B)
	case 0x39:
		// SRL C
		// Shift C right
		compileInstrSRL(C)
	case 0x3A:
		// SRL D
		// Shift D right
		compileInstrSRL(D)
	case 0x3B:
		// SRL E
		// Shift E right
		compileInstrSRL(E)
	case 0x3C:
		// SRL H
		// Shift H right
		compileInstrSRL(H)
	case 0x3D:
		// SRL L
		// Shift L right
		compileInstrSRL(L)
	case 0x3E:
		// SRL (HL)
		// Shift value pointed by HL right
		compileInstrSRL(IndirectRegister((HL)))
	case 0x3F:
		// SRL A
		// Shift A right
		compileInstrSRL(A)
	case 0x40:
		// BIT 0, B
		// Test bit 0 of B
		compileInstrBIT(StaticParam(0), B)
	case 0x41:
		// BIT 0, C
		// Test bit 0 of C
		compileInstrBIT(StaticParam(0), C)
	case 0x42:
		// BIT 0, D
		// Test bit 0 of D
		compileInstrBIT(StaticParam(0), D)
	case 0x43:
		// BIT 0, E
		// Test bit 0 of E
		compileInstrBIT(StaticParam(0), E)
	case 0x44:
		// BIT 0, H
		// Test bit 0 of H
		compileInstrBIT(StaticParam(0), H)
	case 0x45:
		// BIT 0, L
		// Test bit 0 of L
		compileInstrBIT(StaticParam(0), L)
	case 0x46:
		// BIT 0, (HL)
		// Test bit 0 of value pointed by HL
		compileInstrBIT(StaticParam(0), IndirectRegister((HL)))
	case 0x47:
		// BIT 0, A
		// Test bit 0 of A
		compileInstrBIT(StaticParam(0), A)
	case 0x48:
		// BIT 1, B
		// Test bit 1 of B
		compileInstrBIT(StaticParam(1), B)
	case 0x49:
		// BIT 1, C
		// Test bit 1 of C
		compileInstrBIT(StaticParam(1), C)
	case 0x4A:
		// BIT 1, D
		// Test bit 1 of D
		compileInstrBIT(StaticParam(1), D)
	case 0x4B:
		// BIT 1, E
		// Test bit 1 of E
		compileInstrBIT(StaticParam(1), E)
	case 0x4C:
		// BIT 1, H
		// Test bit 1 of H
		compileInstrBIT(StaticParam(1), H)
	case 0x4D:
		// BIT 1, L
		// Test bit 1 of L
		compileInstrBIT(StaticParam(1), L)
	case 0x4E:
		// BIT 1, (HL)
		// Test bit 1 of value pointed by HL
		compileInstrBIT(StaticParam(1), IndirectRegister((HL)))
	case 0x4F:
		// BIT 1, A
		// Test bit 1 of A
		compileInstrBIT(StaticParam(1), A)
	case 0x50:
		// BIT 2, B
		// Test bit 2 of B
		compileInstrBIT(StaticParam(2), B)
	case 0x51:
		// BIT 2, C
		// Test bit 2 of C
		compileInstrBIT(StaticParam(2), C)
	case 0x52:
		// BIT 2, D
		// Test bit 2 of D
		compileInstrBIT(StaticParam(2), D)
	case 0x53:
		// BIT 2, E
		// Test bit 2 of E
		compileInstrBIT(StaticParam(2), E)
	case 0x54:
		// BIT 2, H
		// Test bit 2 of H
		compileInstrBIT(StaticParam(2), H)
	case 0x55:
		// BIT 2, L
		// Test bit 2 of L
		compileInstrBIT(StaticParam(2), L)
	case 0x56:
		// BIT 2, (HL)
		// Test bit 2 of value pointed by HL
		compileInstrBIT(StaticParam(2), IndirectRegister((HL)))
	case 0x57:
		// BIT 2, A
		// Test bit 2 of A
		compileInstrBIT(StaticParam(2), A)
	case 0x58:
		// BIT 3, B
		// Test bit 3 of B
		compileInstrBIT(StaticParam(3), B)
	case 0x59:
		// BIT 3, C
		// Test bit 3 of C
		compileInstrBIT(StaticParam(3), C)
	case 0x5A:
		// BIT 3, D
		// Test bit 3 of D
		compileInstrBIT(StaticParam(3), D)
	case 0x5B:
		// BIT 3, E
		// Test bit 3 of E
		compileInstrBIT(StaticParam(3), E)
	case 0x5C:
		// BIT 3, H
		// Test bit 3 of H
		compileInstrBIT(StaticParam(3), H)
	case 0x5D:
		// BIT 3, L
		// Test bit 3 of L
		compileInstrBIT(StaticParam(3), L)
	case 0x5E:
		// BIT 3, (HL)
		// Test bit 3 of value pointed by HL
		compileInstrBIT(StaticParam(3), IndirectRegister((HL)))
	case 0x5F:
		// BIT 3, A
		// Test bit 3 of A
		compileInstrBIT(StaticParam(3), A)
	case 0x60:
		// BIT 4, B
		// Test bit 4 of B
		compileInstrBIT(StaticParam(4), B)
	case 0x61:
		// BIT 4, C
		// Test bit 4 of C
		compileInstrBIT(StaticParam(4), C)
	case 0x62:
		// BIT 4, D
		// Test bit 4 of D
		compileInstrBIT(StaticParam(4), D)
	case 0x63:
		// BIT 4, E
		// Test bit 4 of E
		compileInstrBIT(StaticParam(4), E)
	case 0x64:
		// BIT 4, H
		// Test bit 4 of H
		compileInstrBIT(StaticParam(4), H)
	case 0x65:
		// BIT 4, L
		// Test bit 4 of L
		compileInstrBIT(StaticParam(4), L)
	case 0x66:
		// BIT 4, (HL)
		// Test bit 4 of value pointed by HL
		compileInstrBIT(StaticParam(4), IndirectRegister((HL)))
	case 0x67:
		// BIT 4, A
		// Test bit 4 of A
		compileInstrBIT(StaticParam(4), A)
	case 0x68:
		// BIT 5, B
		// Test bit 5 of B
		compileInstrBIT(StaticParam(5), B)
	case 0x69:
		// BIT 5, C
		// Test bit 5 of C
		compileInstrBIT(StaticParam(5), C)
	case 0x6A:
		// BIT 5, D
		// Test bit 5 of D
		compileInstrBIT(StaticParam(5), D)
	case 0x6B:
		// BIT 5, E
		// Test bit 5 of E
		compileInstrBIT(StaticParam(5), E)
	case 0x6C:
		// BIT 5, H
		// Test bit 5 of H
		compileInstrBIT(StaticParam(5), H)
	case 0x6D:
		// BIT 5, L
		// Test bit 5 of L
		compileInstrBIT(StaticParam(5), L)
	case 0x6E:
		// BIT 5, (HL)
		// Test bit 5 of value pointed by HL
		compileInstrBIT(StaticParam(5), IndirectRegister((HL)))
	case 0x6F:
		// BIT 5, A
		// Test bit 5 of A
		compileInstrBIT(StaticParam(5), A)
	case 0x70:
		// BIT 6, B
		// Test bit 6 of B
		compileInstrBIT(StaticParam(6), B)
	case 0x71:
		// BIT 6, C
		// Test bit 6 of C
		compileInstrBIT(StaticParam(6), C)
	case 0x72:
		// BIT 6, D
		// Test bit 6 of D
		compileInstrBIT(StaticParam(6), D)
	case 0x73:
		// BIT 6, E
		// Test bit 6 of E
		compileInstrBIT(StaticParam(6), E)
	case 0x74:
		// BIT 6, H
		// Test bit 6 of H
		compileInstrBIT(StaticParam(6), H)
	case 0x75:
		// BIT 6, L
		// Test bit 6 of L
		compileInstrBIT(StaticParam(6), L)
	case 0x76:
		// BIT 6, (HL)
		// Test bit 6 of value pointed by HL
		compileInstrBIT(StaticParam(6), IndirectRegister((HL)))
	case 0x77:
		// BIT 6, A
		// Test bit 6 of A
		compileInstrBIT(StaticParam(6), A)
	case 0x78:
		// BIT 7, B
		// Test bit 7 of B
		compileInstrBIT(StaticParam(7), B)
	case 0x79:
		// BIT 7, C
		// Test bit 7 of C
		compileInstrBIT(StaticParam(7), C)
	case 0x7A:
		// BIT 7, D
		// Test bit 7 of D
		compileInstrBIT(StaticParam(7), D)
	case 0x7B:
		// BIT 7, E
		// Test bit 7 of E
		compileInstrBIT(StaticParam(7), E)
	case 0x7C:
		// BIT 7, H
		// Test bit 7 of H
		compileInstrBIT(StaticParam(7), H)
	case 0x7D:
		// BIT 7, L
		// Test bit 7 of L
		compileInstrBIT(StaticParam(7), L)
	case 0x7E:
		// BIT 7, (HL)
		// Test bit 7 of value pointed by HL
		compileInstrBIT(StaticParam(7), IndirectRegister((HL)))
	case 0x7F:
		// BIT 7, A
		// Test bit 7 of A
		compileInstrBIT(StaticParam(7), A)
	case 0x80:
		// RES 0, B
		// Clear (reset) bit 0 of B
		compileInstrRES(StaticParam(0), B)
	case 0x81:
		// RES 0, C
		// Clear (reset) bit 0 of C
		compileInstrRES(StaticParam(0), C)
	case 0x82:
		// RES 0, D
		// Clear (reset) bit 0 of D
		compileInstrRES(StaticParam(0), D)
	case 0x83:
		// RES 0, E
		// Clear (reset) bit 0 of E
		compileInstrRES(StaticParam(0), E)
	case 0x84:
		// RES 0, H
		// Clear (reset) bit 0 of H
		compileInstrRES(StaticParam(0), H)
	case 0x85:
		// RES 0, L
		// Clear (reset) bit 0 of L
		compileInstrRES(StaticParam(0), L)
	case 0x86:
		// RES 0, (HL)
		// Clear (reset) bit 0 of value pointed by HL
		compileInstrRES(StaticParam(0), IndirectRegister((HL)))
	case 0x87:
		// RES 0, A
		// Clear (reset) bit 0 of A
		compileInstrRES(StaticParam(0), A)
	case 0x88:
		// RES 1, B
		// Clear (reset) bit 1 of B
		compileInstrRES(StaticParam(1), B)
	case 0x89:
		// RES 1, C
		// Clear (reset) bit 1 of C
		compileInstrRES(StaticParam(1), C)
	case 0x8A:
		// RES 1, D
		// Clear (reset) bit 1 of D
		compileInstrRES(StaticParam(1), D)
	case 0x8B:
		// RES 1, E
		// Clear (reset) bit 1 of E
		compileInstrRES(StaticParam(1), E)
	case 0x8C:
		// RES 1, H
		// Clear (reset) bit 1 of H
		compileInstrRES(StaticParam(1), H)
	case 0x8D:
		// RES 1, L
		// Clear (reset) bit 1 of L
		compileInstrRES(StaticParam(1), L)
	case 0x8E:
		// RES 1, (HL)
		// Clear (reset) bit 1 of value pointed by HL
		compileInstrRES(StaticParam(1), IndirectRegister((HL)))
	case 0x8F:
		// RES 1, A
		// Clear (reset) bit 1 of A
		compileInstrRES(StaticParam(1), A)
	case 0x90:
		// RES 2, B
		// Clear (reset) bit 2 of B
		compileInstrRES(StaticParam(2), B)
	case 0x91:
		// RES 2, C
		// Clear (reset) bit 2 of C
		compileInstrRES(StaticParam(2), C)
	case 0x92:
		// RES 2, D
		// Clear (reset) bit 2 of D
		compileInstrRES(StaticParam(2), D)
	case 0x93:
		// RES 2, E
		// Clear (reset) bit 2 of E
		compileInstrRES(StaticParam(2), E)
	case 0x94:
		// RES 2, H
		// Clear (reset) bit 2 of H
		compileInstrRES(StaticParam(2), H)
	case 0x95:
		// RES 2, L
		// Clear (reset) bit 2 of L
		compileInstrRES(StaticParam(2), L)
	case 0x96:
		// RES 2, (HL)
		// Clear (reset) bit 2 of value pointed by HL
		compileInstrRES(StaticParam(2), IndirectRegister((HL)))
	case 0x97:
		// RES 2, A
		// Clear (reset) bit 2 of A
		compileInstrRES(StaticParam(2), A)
	case 0x98:
		// RES 3, B
		// Clear (reset) bit 3 of B
		compileInstrRES(StaticParam(3), B)
	case 0x99:
		// RES 3, C
		// Clear (reset) bit 3 of C
		compileInstrRES(StaticParam(3), C)
	case 0x9A:
		// RES 3, D
		// Clear (reset) bit 3 of D
		compileInstrRES(StaticParam(3), D)
	case 0x9B:
		// RES 3, E
		// Clear (reset) bit 3 of E
		compileInstrRES(StaticParam(3), E)
	case 0x9C:
		// RES 3, H
		// Clear (reset) bit 3 of H
		compileInstrRES(StaticParam(3), H)
	case 0x9D:
		// RES 3, L
		// Clear (reset) bit 3 of L
		compileInstrRES(StaticParam(3), L)
	case 0x9E:
		// RES 3, (HL)
		// Clear (reset) bit 3 of value pointed by HL
		compileInstrRES(StaticParam(3), IndirectRegister((HL)))
	case 0x9F:
		// RES 3, A
		// Clear (reset) bit 3 of A
		compileInstrRES(StaticParam(3), A)
	case 0xA0:
		// RES 4, B
		// Clear (reset) bit 4 of B
		compileInstrRES(StaticParam(4), B)
	case 0xA1:
		// RES 4, C
		// Clear (reset) bit 4 of C
		compileInstrRES(StaticParam(4), C)
	case 0xA2:
		// RES 4, D
		// Clear (reset) bit 4 of D
		compileInstrRES(StaticParam(4), D)
	case 0xA3:
		// RES 4, E
		// Clear (reset) bit 4 of E
		compileInstrRES(StaticParam(4), E)
	case 0xA4:
		// RES 4, H
		// Clear (reset) bit 4 of H
		compileInstrRES(StaticParam(4), H)
	case 0xA5:
		// RES 4, L
		// Clear (reset) bit 4 of L
		compileInstrRES(StaticParam(4), L)
	case 0xA6:
		// RES 4, (HL)
		// Clear (reset) bit 4 of value pointed by HL
		compileInstrRES(StaticParam(4), IndirectRegister((HL)))
	case 0xA7:
		// RES 4, A
		// Clear (reset) bit 4 of A
		compileInstrRES(StaticParam(4), A)
	case 0xA8:
		// RES 5, B
		// Clear (reset) bit 5 of B
		compileInstrRES(StaticParam(5), B)
	case 0xA9:
		// RES 5, C
		// Clear (reset) bit 5 of C
		compileInstrRES(StaticParam(5), C)
	case 0xAA:
		// RES 5, D
		// Clear (reset) bit 5 of D
		compileInstrRES(StaticParam(5), D)
	case 0xAB:
		// RES 5, E
		// Clear (reset) bit 5 of E
		compileInstrRES(StaticParam(5), E)
	case 0xAC:
		// RES 5, H
		// Clear (reset) bit 5 of H
		compileInstrRES(StaticParam(5), H)
	case 0xAD:
		// RES 5, L
		// Clear (reset) bit 5 of L
		compileInstrRES(StaticParam(5), L)
	case 0xAE:
		// RES 5, (HL)
		// Clear (reset) bit 5 of value pointed by HL
		compileInstrRES(StaticParam(5), IndirectRegister((HL)))
	case 0xAF:
		// RES 5, A
		// Clear (reset) bit 5 of A
		compileInstrRES(StaticParam(5), A)
	case 0xB0:
		// RES 6, B
		// Clear (reset) bit 6 of B
		compileInstrRES(StaticParam(6), B)
	case 0xB1:
		// RES 6, C
		// Clear (reset) bit 6 of C
		compileInstrRES(StaticParam(6), C)
	case 0xB2:
		// RES 6, D
		// Clear (reset) bit 6 of D
		compileInstrRES(StaticParam(6), D)
	case 0xB3:
		// RES 6, E
		// Clear (reset) bit 6 of E
		compileInstrRES(StaticParam(6), E)
	case 0xB4:
		// RES 6, H
		// Clear (reset) bit 6 of H
		compileInstrRES(StaticParam(6), H)
	case 0xB5:
		// RES 6, L
		// Clear (reset) bit 6 of L
		compileInstrRES(StaticParam(6), L)
	case 0xB6:
		// RES 6, (HL)
		// Clear (reset) bit 6 of value pointed by HL
		compileInstrRES(StaticParam(6), IndirectRegister((HL)))
	case 0xB7:
		// RES 6, A
		// Clear (reset) bit 6 of A
		compileInstrRES(StaticParam(6), A)
	case 0xB8:
		// RES 7, B
		// Clear (reset) bit 7 of B
		compileInstrRES(StaticParam(7), B)
	case 0xB9:
		// RES 7, C
		// Clear (reset) bit 7 of C
		compileInstrRES(StaticParam(7), C)
	case 0xBA:
		// RES 7, D
		// Clear (reset) bit 7 of D
		compileInstrRES(StaticParam(7), D)
	case 0xBB:
		// RES 7, E
		// Clear (reset) bit 7 of E
		compileInstrRES(StaticParam(7), E)
	case 0xBC:
		// RES 7, H
		// Clear (reset) bit 7 of H
		compileInstrRES(StaticParam(7), H)
	case 0xBD:
		// RES 7, L
		// Clear (reset) bit 7 of L
		compileInstrRES(StaticParam(7), L)
	case 0xBE:
		// RES 7, (HL)
		// Clear (reset) bit 7 of value pointed by HL
		compileInstrRES(StaticParam(7), IndirectRegister((HL)))
	case 0xBF:
		// RES 7, A
		// Clear (reset) bit 7 of A
		compileInstrRES(StaticParam(7), A)
	case 0xC0:
		// SET 0, B
		// Set bit 0 of B
		compileInstrSET(StaticParam(0), B)
	case 0xC1:
		// SET 0, C
		// Set bit 0 of C
		compileInstrSET(StaticParam(0), C)
	case 0xC2:
		// SET 0, D
		// Set bit 0 of D
		compileInstrSET(StaticParam(0), D)
	case 0xC3:
		// SET 0, E
		// Set bit 0 of E
		compileInstrSET(StaticParam(0), E)
	case 0xC4:
		// SET 0, H
		// Set bit 0 of H
		compileInstrSET(StaticParam(0), H)
	case 0xC5:
		// SET 0, L
		// Set bit 0 of L
		compileInstrSET(StaticParam(0), L)
	case 0xC6:
		// SET 0, (HL)
		// Set bit 0 of value pointed by HL
		compileInstrSET(StaticParam(0), IndirectRegister((HL)))
	case 0xC7:
		// SET 0, A
		// Set bit 0 of A
		compileInstrSET(StaticParam(0), A)
	case 0xC8:
		// SET 1, B
		// Set bit 1 of B
		compileInstrSET(StaticParam(1), B)
	case 0xC9:
		// SET 1, C
		// Set bit 1 of C
		compileInstrSET(StaticParam(1), C)
	case 0xCA:
		// SET 1, D
		// Set bit 1 of D
		compileInstrSET(StaticParam(1), D)
	case 0xCB:
		// SET 1, E
		// Set bit 1 of E
		compileInstrSET(StaticParam(1), E)
	case 0xCC:
		// SET 1, H
		// Set bit 1 of H
		compileInstrSET(StaticParam(1), H)
	case 0xCD:
		// SET 1, L
		// Set bit 1 of L
		compileInstrSET(StaticParam(1), L)
	case 0xCE:
		// SET 1, (HL)
		// Set bit 1 of value pointed by HL
		compileInstrSET(StaticParam(1), IndirectRegister((HL)))
	case 0xCF:
		// SET 1, A
		// Set bit 1 of A
		compileInstrSET(StaticParam(1), A)
	case 0xD0:
		// SET 2, B
		// Set bit 2 of B
		compileInstrSET(StaticParam(2), B)
	case 0xD1:
		// SET 2, C
		// Set bit 2 of C
		compileInstrSET(StaticParam(2), C)
	case 0xD2:
		// SET 2, D
		// Set bit 2 of D
		compileInstrSET(StaticParam(2), D)
	case 0xD3:
		// SET 2, E
		// Set bit 2 of E
		compileInstrSET(StaticParam(2), E)
	case 0xD4:
		// SET 2, H
		// Set bit 2 of H
		compileInstrSET(StaticParam(2), H)
	case 0xD5:
		// SET 2, L
		// Set bit 2 of L
		compileInstrSET(StaticParam(2), L)
	case 0xD6:
		// SET 2, (HL)
		// Set bit 2 of value pointed by HL
		compileInstrSET(StaticParam(2), IndirectRegister((HL)))
	case 0xD7:
		// SET 2, A
		// Set bit 2 of A
		compileInstrSET(StaticParam(2), A)
	case 0xD8:
		// SET 3, B
		// Set bit 3 of B
		compileInstrSET(StaticParam(3), B)
	case 0xD9:
		// SET 3, C
		// Set bit 3 of C
		compileInstrSET(StaticParam(3), C)
	case 0xDA:
		// SET 3, D
		// Set bit 3 of D
		compileInstrSET(StaticParam(3), D)
	case 0xDB:
		// SET 3, E
		// Set bit 3 of E
		compileInstrSET(StaticParam(3), E)
	case 0xDC:
		// SET 3, H
		// Set bit 3 of H
		compileInstrSET(StaticParam(3), H)
	case 0xDD:
		// SET 3, L
		// Set bit 3 of L
		compileInstrSET(StaticParam(3), L)
	case 0xDE:
		// SET 3, (HL)
		// Set bit 3 of value pointed by HL
		compileInstrSET(StaticParam(3), IndirectRegister((HL)))
	case 0xDF:
		// SET 3, A
		// Set bit 3 of A
		compileInstrSET(StaticParam(3), A)
	case 0xE0:
		// SET 4, B
		// Set bit 4 of B
		compileInstrSET(StaticParam(4), B)
	case 0xE1:
		// SET 4, C
		// Set bit 4 of C
		compileInstrSET(StaticParam(4), C)
	case 0xE2:
		// SET 4, D
		// Set bit 4 of D
		compileInstrSET(StaticParam(4), D)
	case 0xE3:
		// SET 4, E
		// Set bit 4 of E
		compileInstrSET(StaticParam(4), E)
	case 0xE4:
		// SET 4, H
		// Set bit 4 of H
		compileInstrSET(StaticParam(4), H)
	case 0xE5:
		// SET 4, L
		// Set bit 4 of L
		compileInstrSET(StaticParam(4), L)
	case 0xE6:
		// SET 4, (HL)
		// Set bit 4 of value pointed by HL
		compileInstrSET(StaticParam(4), IndirectRegister((HL)))
	case 0xE7:
		// SET 4, A
		// Set bit 4 of A
		compileInstrSET(StaticParam(4), A)
	case 0xE8:
		// SET 5, B
		// Set bit 5 of B
		compileInstrSET(StaticParam(5), B)
	case 0xE9:
		// SET 5, C
		// Set bit 5 of C
		compileInstrSET(StaticParam(5), C)
	case 0xEA:
		// SET 5, D
		// Set bit 5 of D
		compileInstrSET(StaticParam(5), D)
	case 0xEB:
		// SET 5, E
		// Set bit 5 of E
		compileInstrSET(StaticParam(5), E)
	case 0xEC:
		// SET 5, H
		// Set bit 5 of H
		compileInstrSET(StaticParam(5), H)
	case 0xED:
		// SET 5, L
		// Set bit 5 of L
		compileInstrSET(StaticParam(5), L)
	case 0xEE:
		// SET 5, (HL)
		// Set bit 5 of value pointed by HL
		compileInstrSET(StaticParam(5), IndirectRegister((HL)))
	case 0xEF:
		// SET 5, A
		// Set bit 5 of A
		compileInstrSET(StaticParam(5), A)
	case 0xF0:
		// SET 6, B
		// Set bit 6 of B
		compileInstrSET(StaticParam(6), B)
	case 0xF1:
		// SET 6, C
		// Set bit 6 of C
		compileInstrSET(StaticParam(6), C)
	case 0xF2:
		// SET 6, D
		// Set bit 6 of D
		compileInstrSET(StaticParam(6), D)
	case 0xF3:
		// SET 6, E
		// Set bit 6 of E
		compileInstrSET(StaticParam(6), E)
	case 0xF4:
		// SET 6, H
		// Set bit 6 of H
		compileInstrSET(StaticParam(6), H)
	case 0xF5:
		// SET 6, L
		// Set bit 6 of L
		compileInstrSET(StaticParam(6), L)
	case 0xF6:
		// SET 6, (HL)
		// Set bit 6 of value pointed by HL
		compileInstrSET(StaticParam(6), IndirectRegister((HL)))
	case 0xF7:
		// SET 6, A
		// Set bit 6 of A
		compileInstrSET(StaticParam(6), A)
	case 0xF8:
		// SET 7, B
		// Set bit 7 of B
		compileInstrSET(StaticParam(7), B)
	case 0xF9:
		// SET 7, C
		// Set bit 7 of C
		compileInstrSET(StaticParam(7), C)
	case 0xFA:
		// SET 7, D
		// Set bit 7 of D
		compileInstrSET(StaticParam(7), D)
	case 0xFB:
		// SET 7, E
		// Set bit 7 of E
		compileInstrSET(StaticParam(7), E)
	case 0xFC:
		// SET 7, H
		// Set bit 7 of H
		compileInstrSET(StaticParam(7), H)
	case 0xFD:
		// SET 7, L
		// Set bit 7 of L
		compileInstrSET(StaticParam(7), L)
	case 0xFE:
		// SET 7, (HL)
		// Set bit 7 of value pointed by HL
		compileInstrSET(StaticParam(7), IndirectRegister((HL)))
	case 0xFF:
		// SET 7, A
		// Set bit 7 of A
		compileInstrSET(StaticParam(7), A)
	}
}

func (a Arch) compileExtDirect(c *compiler.Compiler) error {
	var bytebuf [1]byte
	_, err := c.Input.Read(bytebuf[:])
	if err != nil {
		return err
	}

	switch bytebuf[0] {
	case 0x00:
		// NOP
		// No Operation
		compileInstrNOP()
	case 0x01:
		// LD BC, nn
		// Load 16-bit immediate into BC
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		compileInstrLD(CompositeRegister{B, C}, uint16(operands[1])<<8|uint16(operands[0]))
	case 0x02:
		// LD (BC), A
		// Save A to address pointed by BC
		compileInstrLD(IndirectRegister((BC)), A)
	case 0x03:
		// INC BC
		// Increment 16-bit BC
		compileInstrINC(CompositeRegister{B, C})
	case 0x04:
		// INC B
		// Increment B
		compileInstrINC(B)
	case 0x05:
		// DEC B
		// Decrement B
		compileInstrDEC(B)
	case 0x06:
		// LD B, n
		// Load 8-bit immediate into B
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrLD(B, operands[0])
	case 0x07:
		// RLC A
		// Rotate A left with carry
		compileInstrRLC(A)
	case 0x08:
		// LD (nn), SP
		// Save SP to given address
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		compileInstrLD(uint16(operands[1])<<8|uint16(operands[0]), SP)
	case 0x09:
		// ADD HL, BC
		// Add 16-bit BC to HL
		compileInstrADD(CompositeRegister{H, L}, CompositeRegister{B, C})
	case 0x0A:
		// LD A, (BC)
		// Load A from address pointed to by BC
		compileInstrLD(A, IndirectRegister((BC)))
	case 0x0B:
		// DEC BC
		// Decrement 16-bit BC
		compileInstrDEC(CompositeRegister{B, C})
	case 0x0C:
		// INC C
		// Increment C
		compileInstrINC(C)
	case 0x0D:
		// DEC C
		// Decrement C
		compileInstrDEC(C)
	case 0x0E:
		// LD C, n
		// Load 8-bit immediate into C
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrLD(C, operands[0])
	case 0x0F:
		// RRC A
		// Rotate A right with carry
		compileInstrRRC(A)
	case 0x10:
		// STOP
		// Stop processor
		compileInstrSTOP()
	case 0x11:
		// LD DE, nn
		// Load 16-bit immediate into DE
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		compileInstrLD(CompositeRegister{D, E}, uint16(operands[1])<<8|uint16(operands[0]))
	case 0x12:
		// LD (DE), A
		// Save A to address pointed by DE
		compileInstrLD(IndirectRegister((DE)), A)
	case 0x13:
		// INC DE
		// Increment 16-bit DE
		compileInstrINC(CompositeRegister{D, E})
	case 0x14:
		// INC D
		// Increment D
		compileInstrINC(D)
	case 0x15:
		// DEC D
		// Decrement D
		compileInstrDEC(D)
	case 0x16:
		// LD D, n
		// Load 8-bit immediate into D
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrLD(D, operands[0])
	case 0x17:
		// RL A
		// Rotate A left
		compileInstrRL(A)
	case 0x18:
		// JR n
		// Relative jump by signed immediate
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrJR(operands[0])
	case 0x19:
		// ADD HL, DE
		// Add 16-bit DE to HL
		compileInstrADD(CompositeRegister{H, L}, CompositeRegister{D, E})
	case 0x1A:
		// LD A, (DE)
		// Load A from address pointed to by DE
		compileInstrLD(A, IndirectRegister((DE)))
	case 0x1B:
		// DEC DE
		// Decrement 16-bit DE
		compileInstrDEC(CompositeRegister{D, E})
	case 0x1C:
		// INC E
		// Increment E
		compileInstrINC(E)
	case 0x1D:
		// DEC E
		// Decrement E
		compileInstrDEC(E)
	case 0x1E:
		// LD E, n
		// Load 8-bit immediate into E
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrLD(E, operands[0])
	case 0x1F:
		// RR A
		// Rotate A right
		compileInstrRR(A)
	case 0x20:
		// JR NZ, n
		// Relative jump by signed immediate if last result was not zero
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrJR(Condition(NZ), operands[0])
	case 0x21:
		// LD HL, nn
		// Load 16-bit immediate into HL
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		compileInstrLD(CompositeRegister{H, L}, uint16(operands[1])<<8|uint16(operands[0]))
	case 0x22:
		// LDI (HL), A
		// Save A to address pointed by HL, and increment HL
		compileInstrLDI(IndirectRegister((HL)), A)
	case 0x23:
		// INC HL
		// Increment 16-bit HL
		compileInstrINC(CompositeRegister{H, L})
	case 0x24:
		// INC H
		// Increment H
		compileInstrINC(H)
	case 0x25:
		// DEC H
		// Decrement H
		compileInstrDEC(H)
	case 0x26:
		// LD H, n
		// Load 8-bit immediate into H
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrLD(H, operands[0])
	case 0x27:
		// DAA
		// Adjust A for BCD addition
		compileInstrDAA()
	case 0x28:
		// JR Z, n
		// Relative jump by signed immediate if last result was zero
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrJR(Condition(Z), operands[0])
	case 0x29:
		// ADD HL, HL
		// Add 16-bit HL to HL
		compileInstrADD(CompositeRegister{H, L}, CompositeRegister{H, L})
	case 0x2A:
		// LDI A, (HL)
		// Load A from address pointed to by HL, and increment HL
		compileInstrLDI(A, IndirectRegister((HL)))
	case 0x2B:
		// DEC HL
		// Decrement 16-bit HL
		compileInstrDEC(CompositeRegister{H, L})
	case 0x2C:
		// INC L
		// Increment L
		compileInstrINC(L)
	case 0x2D:
		// DEC L
		// Decrement L
		compileInstrDEC(L)
	case 0x2E:
		// LD L, n
		// Load 8-bit immediate into L
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrLD(L, operands[0])
	case 0x2F:
		// CPL
		// Complement (logical NOT) on A
		compileInstrCPL()
	case 0x30:
		// JR NC, n
		// Relative jump by signed immediate if last result caused no carry
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrJR(Condition(NC), operands[0])
	case 0x31:
		// LD SP, nn
		// Load 16-bit immediate into SP
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		compileInstrLD(SP, uint16(operands[1])<<8|uint16(operands[0]))
	case 0x32:
		// LDD (HL), A
		// Save A to address pointed by HL, and decrement HL
		compileInstrLDD(IndirectRegister((HL)), A)
	case 0x33:
		// INC SP
		// Increment 16-bit HL
		compileInstrINC(SP)
	case 0x34:
		// INC (HL)
		// Increment value pointed by HL
		compileInstrINC(IndirectRegister((HL)))
	case 0x35:
		// DEC (HL)
		// Decrement value pointed by HL
		compileInstrDEC(IndirectRegister((HL)))
	case 0x36:
		// LD (HL), n
		// Load 8-bit immediate into address pointed by HL
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrLD(IndirectRegister((HL)), operands[0])
	case 0x37:
		// SCF
		// Set carry flag
		compileInstrSCF()
	case 0x38:
		// JR C, n
		// Relative jump by signed immediate if last result caused carry
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrJR(C, operands[0])
	case 0x39:
		// ADD HL, SP
		// Add 16-bit SP to HL
		compileInstrADD(CompositeRegister{H, L}, SP)
	case 0x3A:
		// LDD A, (HL)
		// Load A from address pointed to by HL, and decrement HL
		compileInstrLDD(A, IndirectRegister((HL)))
	case 0x3B:
		// DEC SP
		// Decrement 16-bit SP
		compileInstrDEC(SP)
	case 0x3C:
		// INC A
		// Increment A
		compileInstrINC(A)
	case 0x3D:
		// DEC A
		// Decrement A
		compileInstrDEC(A)
	case 0x3E:
		// LD A, n
		// Load 8-bit immediate into A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrLD(A, operands[0])
	case 0x3F:
		// CCF
		// Clear carry flag
		compileInstrCCF()
	case 0x40:
		// LD B, B
		// Copy B to B
		compileInstrLD(B, B)
	case 0x41:
		// LD B, C
		// Copy C to B
		compileInstrLD(B, C)
	case 0x42:
		// LD B, D
		// Copy D to B
		compileInstrLD(B, D)
	case 0x43:
		// LD B, E
		// Copy E to B
		compileInstrLD(B, E)
	case 0x44:
		// LD B, H
		// Copy H to B
		compileInstrLD(B, H)
	case 0x45:
		// LD B, L
		// Copy L to B
		compileInstrLD(B, L)
	case 0x46:
		// LD B, (HL)
		// Copy value pointed by HL to B
		compileInstrLD(B, IndirectRegister((HL)))
	case 0x47:
		// LD B, A
		// Copy A to B
		compileInstrLD(B, A)
	case 0x48:
		// LD C, B
		// Copy B to C
		compileInstrLD(C, B)
	case 0x49:
		// LD C, C
		// Copy C to C
		compileInstrLD(C, C)
	case 0x4A:
		// LD C, D
		// Copy D to C
		compileInstrLD(C, D)
	case 0x4B:
		// LD C, E
		// Copy E to C
		compileInstrLD(C, E)
	case 0x4C:
		// LD C, H
		// Copy H to C
		compileInstrLD(C, H)
	case 0x4D:
		// LD C, L
		// Copy L to C
		compileInstrLD(C, L)
	case 0x4E:
		// LD C, (HL)
		// Copy value pointed by HL to C
		compileInstrLD(C, IndirectRegister((HL)))
	case 0x4F:
		// LD C, A
		// Copy A to C
		compileInstrLD(C, A)
	case 0x50:
		// LD D, B
		// Copy B to D
		compileInstrLD(D, B)
	case 0x51:
		// LD D, C
		// Copy C to D
		compileInstrLD(D, C)
	case 0x52:
		// LD D, D
		// Copy D to D
		compileInstrLD(D, D)
	case 0x53:
		// LD D, E
		// Copy E to D
		compileInstrLD(D, E)
	case 0x54:
		// LD D, H
		// Copy H to D
		compileInstrLD(D, H)
	case 0x55:
		// LD D, L
		// Copy L to D
		compileInstrLD(D, L)
	case 0x56:
		// LD D, (HL)
		// Copy value pointed by HL to D
		compileInstrLD(D, IndirectRegister((HL)))
	case 0x57:
		// LD D, A
		// Copy A to D
		compileInstrLD(D, A)
	case 0x58:
		// LD E, B
		// Copy B to E
		compileInstrLD(E, B)
	case 0x59:
		// LD E, C
		// Copy C to E
		compileInstrLD(E, C)
	case 0x5A:
		// LD E, D
		// Copy D to E
		compileInstrLD(E, D)
	case 0x5B:
		// LD E, E
		// Copy E to E
		compileInstrLD(E, E)
	case 0x5C:
		// LD E, H
		// Copy H to E
		compileInstrLD(E, H)
	case 0x5D:
		// LD E, L
		// Copy L to E
		compileInstrLD(E, L)
	case 0x5E:
		// LD E, (HL)
		// Copy value pointed by HL to E
		compileInstrLD(E, IndirectRegister((HL)))
	case 0x5F:
		// LD E, A
		// Copy A to E
		compileInstrLD(E, A)
	case 0x60:
		// LD H, B
		// Copy B to H
		compileInstrLD(H, B)
	case 0x61:
		// LD H, C
		// Copy C to H
		compileInstrLD(H, C)
	case 0x62:
		// LD H, D
		// Copy D to H
		compileInstrLD(H, D)
	case 0x63:
		// LD H, E
		// Copy E to H
		compileInstrLD(H, E)
	case 0x64:
		// LD H, H
		// Copy H to H
		compileInstrLD(H, H)
	case 0x65:
		// LD H, L
		// Copy L to H
		compileInstrLD(H, L)
	case 0x66:
		// LD H, (HL)
		// Copy value pointed by HL to H
		compileInstrLD(H, IndirectRegister((HL)))
	case 0x67:
		// LD H, A
		// Copy A to H
		compileInstrLD(H, A)
	case 0x68:
		// LD L, B
		// Copy B to L
		compileInstrLD(L, B)
	case 0x69:
		// LD L, C
		// Copy C to L
		compileInstrLD(L, C)
	case 0x6A:
		// LD L, D
		// Copy D to L
		compileInstrLD(L, D)
	case 0x6B:
		// LD L, E
		// Copy E to L
		compileInstrLD(L, E)
	case 0x6C:
		// LD L, H
		// Copy H to L
		compileInstrLD(L, H)
	case 0x6D:
		// LD L, L
		// Copy L to L
		compileInstrLD(L, L)
	case 0x6E:
		// LD L, (HL)
		// Copy value pointed by HL to L
		compileInstrLD(L, IndirectRegister((HL)))
	case 0x6F:
		// LD L, A
		// Copy A to L
		compileInstrLD(L, A)
	case 0x70:
		// LD (HL), B
		// Copy B to address pointed by HL
		compileInstrLD(IndirectRegister((HL)), B)
	case 0x71:
		// LD (HL), C
		// Copy C to address pointed by HL
		compileInstrLD(IndirectRegister((HL)), C)
	case 0x72:
		// LD (HL), D
		// Copy D to address pointed by HL
		compileInstrLD(IndirectRegister((HL)), D)
	case 0x73:
		// LD (HL), E
		// Copy E to address pointed by HL
		compileInstrLD(IndirectRegister((HL)), E)
	case 0x74:
		// LD (HL), H
		// Copy H to address pointed by HL
		compileInstrLD(IndirectRegister((HL)), H)
	case 0x75:
		// LD (HL), L
		// Copy L to address pointed by HL
		compileInstrLD(IndirectRegister((HL)), L)
	case 0x76:
		// HALT
		// Halt processor
		compileInstrHALT()
	case 0x77:
		// LD (HL), A
		// Copy A to address pointed by HL
		compileInstrLD(IndirectRegister((HL)), A)
	case 0x78:
		// LD A, B
		// Copy B to A
		compileInstrLD(A, B)
	case 0x79:
		// LD A, C
		// Copy C to A
		compileInstrLD(A, C)
	case 0x7A:
		// LD A, D
		// Copy D to A
		compileInstrLD(A, D)
	case 0x7B:
		// LD A, E
		// Copy E to A
		compileInstrLD(A, E)
	case 0x7C:
		// LD A, H
		// Copy H to A
		compileInstrLD(A, H)
	case 0x7D:
		// LD A, L
		// Copy L to A
		compileInstrLD(A, L)
	case 0x7E:
		// LD A, (HL)
		// Copy value pointed by HL to A
		compileInstrLD(A, IndirectRegister((HL)))
	case 0x7F:
		// LD A, A
		// Copy A to A
		compileInstrLD(A, A)
	case 0x80:
		// ADD A, B
		// Add B to A
		compileInstrADD(A, B)
	case 0x81:
		// ADD A, C
		// Add C to A
		compileInstrADD(A, C)
	case 0x82:
		// ADD A, D
		// Add D to A
		compileInstrADD(A, D)
	case 0x83:
		// ADD A, E
		// Add E to A
		compileInstrADD(A, E)
	case 0x84:
		// ADD A, H
		// Add H to A
		compileInstrADD(A, H)
	case 0x85:
		// ADD A, L
		// Add L to A
		compileInstrADD(A, L)
	case 0x86:
		// ADD A, (HL)
		// Add value pointed by HL to A
		compileInstrADD(A, IndirectRegister((HL)))
	case 0x87:
		// ADD A, A
		// Add A to A
		compileInstrADD(A, A)
	case 0x88:
		// ADC A, B
		// Add B and carry flag to A
		compileInstrADC(A, B)
	case 0x89:
		// ADC A, C
		// Add C and carry flag to A
		compileInstrADC(A, C)
	case 0x8A:
		// ADC A, D
		// Add D and carry flag to A
		compileInstrADC(A, D)
	case 0x8B:
		// ADC A, E
		// Add E and carry flag to A
		compileInstrADC(A, E)
	case 0x8C:
		// ADC A, H
		// Add H and carry flag to A
		compileInstrADC(A, H)
	case 0x8D:
		// ADC A, L
		// Add and carry flag L to A
		compileInstrADC(A, L)
	case 0x8E:
		// ADC A, (HL)
		// Add value pointed by HL and carry flag to A
		compileInstrADC(A, IndirectRegister((HL)))
	case 0x8F:
		// ADC A, A
		// Add A and carry flag to A
		compileInstrADC(A, A)
	case 0x90:
		// SUB A, B
		// Subtract B from A
		compileInstrSUB(A, B)
	case 0x91:
		// SUB A, C
		// Subtract C from A
		compileInstrSUB(A, C)
	case 0x92:
		// SUB A, D
		// Subtract D from A
		compileInstrSUB(A, D)
	case 0x93:
		// SUB A, E
		// Subtract E from A
		compileInstrSUB(A, E)
	case 0x94:
		// SUB A, H
		// Subtract H from A
		compileInstrSUB(A, H)
	case 0x95:
		// SUB A, L
		// Subtract L from A
		compileInstrSUB(A, L)
	case 0x96:
		// SUB A, (HL)
		// Subtract value pointed by HL from A
		compileInstrSUB(A, IndirectRegister((HL)))
	case 0x97:
		// SUB A, A
		// Subtract A from A
		compileInstrSUB(A, A)
	case 0x98:
		// SBC A, B
		// Subtract B and carry flag from A
		compileInstrSBC(A, B)
	case 0x99:
		// SBC A, C
		// Subtract C and carry flag from A
		compileInstrSBC(A, C)
	case 0x9A:
		// SBC A, D
		// Subtract D and carry flag from A
		compileInstrSBC(A, D)
	case 0x9B:
		// SBC A, E
		// Subtract E and carry flag from A
		compileInstrSBC(A, E)
	case 0x9C:
		// SBC A, H
		// Subtract H and carry flag from A
		compileInstrSBC(A, H)
	case 0x9D:
		// SBC A, L
		// Subtract and carry flag L from A
		compileInstrSBC(A, L)
	case 0x9E:
		// SBC A, (HL)
		// Subtract value pointed by HL and carry flag from A
		compileInstrSBC(A, IndirectRegister((HL)))
	case 0x9F:
		// SBC A, A
		// Subtract A and carry flag from A
		compileInstrSBC(A, A)
	case 0xA0:
		// AND B
		// Logical AND B against A
		compileInstrAND(B)
	case 0xA1:
		// AND C
		// Logical AND C against A
		compileInstrAND(C)
	case 0xA2:
		// AND D
		// Logical AND D against A
		compileInstrAND(D)
	case 0xA3:
		// AND E
		// Logical AND E against A
		compileInstrAND(E)
	case 0xA4:
		// AND H
		// Logical AND H against A
		compileInstrAND(H)
	case 0xA5:
		// AND L
		// Logical AND L against A
		compileInstrAND(L)
	case 0xA6:
		// AND (HL)
		// Logical AND value pointed by HL against A
		compileInstrAND(IndirectRegister((HL)))
	case 0xA7:
		// AND A
		// Logical AND A against A
		compileInstrAND(A)
	case 0xA8:
		// XOR B
		// Logical XOR B against A
		compileInstrXOR(B)
	case 0xA9:
		// XOR C
		// Logical XOR C against A
		compileInstrXOR(C)
	case 0xAA:
		// XOR D
		// Logical XOR D against A
		compileInstrXOR(D)
	case 0xAB:
		// XOR E
		// Logical XOR E against A
		compileInstrXOR(E)
	case 0xAC:
		// XOR H
		// Logical XOR H against A
		compileInstrXOR(H)
	case 0xAD:
		// XOR L
		// Logical XOR L against A
		compileInstrXOR(L)
	case 0xAE:
		// XOR (HL)
		// Logical XOR value pointed by HL against A
		compileInstrXOR(IndirectRegister((HL)))
	case 0xAF:
		// XOR A
		// Logical XOR A against A
		compileInstrXOR(A)
	case 0xB0:
		// OR B
		// Logical OR B against A
		compileInstrOR(B)
	case 0xB1:
		// OR C
		// Logical OR C against A
		compileInstrOR(C)
	case 0xB2:
		// OR D
		// Logical OR D against A
		compileInstrOR(D)
	case 0xB3:
		// OR E
		// Logical OR E against A
		compileInstrOR(E)
	case 0xB4:
		// OR H
		// Logical OR H against A
		compileInstrOR(H)
	case 0xB5:
		// OR L
		// Logical OR L against A
		compileInstrOR(L)
	case 0xB6:
		// OR (HL)
		// Logical OR value pointed by HL against A
		compileInstrOR(IndirectRegister((HL)))
	case 0xB7:
		// OR A
		// Logical OR A against A
		compileInstrOR(A)
	case 0xB8:
		// CP B
		// Compare B against A
		compileInstrCP(B)
	case 0xB9:
		// CP C
		// Compare C against A
		compileInstrCP(C)
	case 0xBA:
		// CP D
		// Compare D against A
		compileInstrCP(D)
	case 0xBB:
		// CP E
		// Compare E against A
		compileInstrCP(E)
	case 0xBC:
		// CP H
		// Compare H against A
		compileInstrCP(H)
	case 0xBD:
		// CP L
		// Compare L against A
		compileInstrCP(L)
	case 0xBE:
		// CP (HL)
		// Compare value pointed by HL against A
		compileInstrCP(IndirectRegister((HL)))
	case 0xBF:
		// CP A
		// Compare A against A
		compileInstrCP(A)
	case 0xC0:
		// RET NZ
		// Return if last result was not zero
		compileInstrRET(Condition(NZ))
	case 0xC1:
		// POP BC
		// Pop 16-bit value from stack into BC
		compileInstrPOP(CompositeRegister{B, C})
	case 0xC2:
		// JP NZ, nn
		// Absolute jump to 16-bit location if last result was not zero
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		compileInstrJP(Condition(NZ), uint16(operands[1])<<8|uint16(operands[0]))
	case 0xC3:
		// JP nn
		// Absolute jump to 16-bit location
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		compileInstrJP(uint16(operands[1])<<8 | uint16(operands[0]))
	case 0xC4:
		// CALL NZ, nn
		// Call routine at 16-bit location if last result was not zero
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		compileInstrCALL(Condition(NZ), uint16(operands[1])<<8|uint16(operands[0]))
	case 0xC5:
		// PUSH BC
		// Push 16-bit BC onto stack
		compileInstrPUSH(CompositeRegister{B, C})
	case 0xC6:
		// ADD A, n
		// Add 8-bit immediate to A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrADD(A, operands[0])
	case 0xC7:
		// RST 0
		// Call routine at address 0000h
		compileInstrRST(StaticParam(0))
	case 0xC8:
		// RET Z
		// Return if last result was zero
		compileInstrRET(Condition(Z))
	case 0xC9:
		// RET
		// Return to calling routine
		compileInstrRET()
	case 0xCA:
		// JP Z, nn
		// Absolute jump to 16-bit location if last result was zero
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		compileInstrJP(Condition(Z), uint16(operands[1])<<8|uint16(operands[0]))
	case 0xCB:
		return compileExtCB(c)
	case 0xCC:
		// CALL Z, nn
		// Call routine at 16-bit location if last result was zero
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		compileInstrCALL(Condition(Z), uint16(operands[1])<<8|uint16(operands[0]))
	case 0xCD:
		// CALL nn
		// Call routine at 16-bit location
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		compileInstrCALL(uint16(operands[1])<<8 | uint16(operands[0]))
	case 0xCE:
		// ADC A, n
		// Add 8-bit immediate and carry to A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrADC(A, operands[0])
	case 0xCF:
		// RST 8
		// Call routine at address 0008h
		compileInstrRST(StaticParam(8))
	case 0xD0:
		// RET NC
		// Return if last result caused no carry
		compileInstrRET(Condition(NC))
	case 0xD1:
		// POP DE
		// Pop 16-bit value from stack into DE
		compileInstrPOP(CompositeRegister{D, E})
	case 0xD2:
		// JP NC, nn
		// Absolute jump to 16-bit location if last result caused no carry
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		compileInstrJP(Condition(NC), uint16(operands[1])<<8|uint16(operands[0]))
	case 0xD3:
		// XX
		// Operation removed in this CPU
		compileInstrXX()
	case 0xD4:
		// CALL NC, nn
		// Call routine at 16-bit location if last result caused no carry
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		compileInstrCALL(Condition(NC), uint16(operands[1])<<8|uint16(operands[0]))
	case 0xD5:
		// PUSH DE
		// Push 16-bit DE onto stack
		compileInstrPUSH(CompositeRegister{D, E})
	case 0xD6:
		// SUB A, n
		// Subtract 8-bit immediate from A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrSUB(A, operands[0])
	case 0xD7:
		// RST 10
		// Call routine at address 0010h
		compileInstrRST(StaticParam(10))
	case 0xD8:
		// RET C
		// Return if last result caused carry
		compileInstrRET(C)
	case 0xD9:
		// RETI
		// Enable interrupts and return to calling routine
		compileInstrRETI()
	case 0xDA:
		// JP C, nn
		// Absolute jump to 16-bit location if last result caused carry
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		compileInstrJP(C, uint16(operands[1])<<8|uint16(operands[0]))
	case 0xDB:
		// XX
		// Operation removed in this CPU
		compileInstrXX()
	case 0xDC:
		// CALL C, nn
		// Call routine at 16-bit location if last result caused carry
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		compileInstrCALL(C, uint16(operands[1])<<8|uint16(operands[0]))
	case 0xDD:
		// XX
		// Operation removed in this CPU
		compileInstrXX()
	case 0xDE:
		// SBC A, n
		// Subtract 8-bit immediate and carry from A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrSBC(A, operands[0])
	case 0xDF:
		// RST 18
		// Call routine at address 0018h
		compileInstrRST(StaticParam(18))
	case 0xE0:
		// LDH (n), A
		// Save A at address pointed to by (FF00h + 8-bit immediate)
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrLDH(operands[0], A)
	case 0xE1:
		// POP HL
		// Pop 16-bit value from stack into HL
		compileInstrPOP(CompositeRegister{H, L})
	case 0xE2:
		// LDH (C), A
		// Save A at address pointed to by (FF00h + C)
		compileInstrLDH(IndirectRegister((C)), A)
	case 0xE3:
		// XX
		// Operation removed in this CPU
		compileInstrXX()
	case 0xE4:
		// XX
		// Operation removed in this CPU
		compileInstrXX()
	case 0xE5:
		// PUSH HL
		// Push 16-bit HL onto stack
		compileInstrPUSH(CompositeRegister{H, L})
	case 0xE6:
		// AND n
		// Logical AND 8-bit immediate against A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrAND(operands[0])
	case 0xE7:
		// RST 20
		// Call routine at address 0020h
		compileInstrRST(StaticParam(20))
	case 0xE8:
		// ADD SP, d
		// Add signed 8-bit immediate to SP
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrADD(SP, operands[0])
	case 0xE9:
		// JP (HL)
		// Jump to 16-bit value pointed by HL
		compileInstrJP(IndirectRegister((HL)))
	case 0xEA:
		// LD (nn), A
		// Save A at given 16-bit address
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		compileInstrLD(uint16(operands[1])<<8|uint16(operands[0]), A)
	case 0xEB:
		// XX
		// Operation removed in this CPU
		compileInstrXX()
	case 0xEC:
		// XX
		// Operation removed in this CPU
		compileInstrXX()
	case 0xED:
		// XX
		// Operation removed in this CPU
		compileInstrXX()
	case 0xEE:
		// XOR n
		// Logical XOR 8-bit immediate against A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrXOR(operands[0])
	case 0xEF:
		// RST 28
		// Call routine at address 0028h
		compileInstrRST(StaticParam(28))
	case 0xF0:
		// LDH A, (n)
		// Load A from address pointed to by (FF00h + 8-bit immediate)
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrLDH(A, operands[0])
	case 0xF1:
		// POP AF
		// Pop 16-bit value from stack into AF
		compileInstrPOP(CompositeRegister{A, F})
	case 0xF2:
		// XX
		// Operation removed in this CPU
		compileInstrXX()
	case 0xF3:
		// DI
		// DIsable interrupts
		compileInstrDI()
	case 0xF4:
		// XX
		// Operation removed in this CPU
		compileInstrXX()
	case 0xF5:
		// PUSH AF
		// Push 16-bit AF onto stack
		compileInstrPUSH(CompositeRegister{A, F})
	case 0xF6:
		// OR n
		// Logical OR 8-bit immediate against A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrOR(operands[0])
	case 0xF7:
		// RST 30
		// Call routine at address 0030h
		compileInstrRST(StaticParam(30))
	case 0xF8:
		// LDHL SP, d
		// Add signed 8-bit immediate to SP and save result in HL
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrLDHL(SP, operands[0])
	case 0xF9:
		// LD SP, HL
		// Copy HL to SP
		compileInstrLD(SP, CompositeRegister{H, L})
	case 0xFA:
		// LD A, (nn)
		// Load A from given 16-bit address
		var operands [2]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 2)
		if err != nil {
			return err
		}
		compileInstrLD(A, Immediate16(uint16(operands[1])<<8|uint16(operands[0])))
	case 0xFB:
		// EI
		// Enable interrupts
		compileInstrEI()
	case 0xFC:
		// XX
		// Operation removed in this CPU
		compileInstrXX()
	case 0xFD:
		// XX
		// Operation removed in this CPU
		compileInstrXX()
	case 0xFE:
		// CP n
		// Compare 8-bit immediate against A
		var operands [1]uint8
		_, err = io.ReadAtLeast(c.Input, operands[:], 1)
		if err != nil {
			return err
		}
		compileInstrCP(operands[0])
	case 0xFF:
		// RST 38
		// Call routine at address 0038h
		compileInstrRST(StaticParam(38))
	}
}
