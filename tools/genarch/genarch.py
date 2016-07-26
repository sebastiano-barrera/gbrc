"""Generate the skeleton of a Gameboy disassembler, in Go, starting
from JSON opcode data, provided from STDIN.
"""

import json
import sys
import re
import itertools
from collections import defaultdict
from abc import ABCMeta, abstractmethod, abstractproperty


def lower_keys(d):
	return {k.lower(): v for k, v in d.items()}


class FormatError(Exception):
	pass


class Operand(metaclass=ABCMeta):
	def __init__(self, spec):
		self.spec = spec

	@classmethod
	def from_spec(cls, spec):
		constructors = [
			Register8,
			Register16,
			StaticParam,
			Condition,
			IndirectRegister,
			lambda spec: Immediate(spec, size=1),
			lambda spec: Immediate(spec, size=2),
		]

		for cons in constructors:
			try:
				return cons(spec)
			except FormatError:
				pass

		raise FormatError('invalid spec for operand: {}'.format(spec))

	def _format_assert(self, spec, cond):
		if not cond:
			raise FormatError('invalid spec for {}: {}'.format(
				self.__class__.__name__, spec))

	def __str__(self):
		return "{}({})".format(
			self.__class__.__name__,
			", ".join(
				"{}={}".format(k, repr(v))
				for k, v in self.__dict__.items()
				if not k.startswith('_')))

	def generate_reader(self):
		yield ''

	@abstractmethod
	def generate_arg(self):
		pass


class Register8(Operand):
	VALID_REGS = 'A F B C D E H L SP'.split()
	def __init__(self, spec):
		super().__init__(spec)
		self._format_assert(spec, spec in self.VALID_REGS)

	def generate_arg(self):
		return 'Register({})'.format(self.spec)


class Register16(Register8):
	VALID_REGS = 'AF BC DE HL'.split()

	def generate_arg(self):
		return 'CompositeRegister{{{}, {}}}'.format(self.spec[0], self.spec[1])


class IndirectRegister(Operand):
	VALID = '(HL) (BC) (DE) (C)'.split()
	def __init__(self, spec):
		super().__init__(spec)
		self._format_assert(spec, spec in self.VALID)

	def generate_arg(self):
		return 'IndirectRegister({})'.format(self.spec)


class StaticParam(Operand):
	VALID_RE = re.compile(r'^[0-9]+$')
	def __init__(self, spec):
		super().__init__(spec)
		self._format_assert(spec, self.VALID_RE.match(spec))
		self.value = int(spec)

	def generate_arg(self):
		return 'StaticParam({})'.format(self.value)


class Condition(Operand):
	VALID = 'Z NZ C NC'.split()
	def __init__(self, spec):
		super().__init__(spec)
		self._format_assert(spec, spec in self.VALID)

	def generate_arg(self):
		return 'Condition({})'.format(self.spec)


class Immediate(Operand):
	def __init__(self, spec, size):
		assert size == 1 or size == 2
		super().__init__(spec)

		self.size = size

		if size == 1:
			if spec == 'n' or spec == 'd':
				self.indirect = False
			elif spec == '(n)':
				self.indirect = True
			else:
				raise FormatError('invalid imm8 operand: {}'.format(spec))
		elif size == 2:
			if spec == 'nn':
				self.indirect = False
			elif spec == '(nn)':
				self.indirect = True
			else:
				raise FormatError('invalid imm16 operand: {}'.format(spec))

	def generate_reader(self):
		yield '''var operands [{}]uint8
	_, err = io.ReadAtLeast(c.Input, operands[:], {})
	if err != nil {{
		return err
	}}
		'''.format(self.size, self.size)

	def generate_arg(self):
		if self.size == 1:
			return 'operands[0]'
		elif self.size == 2:
			return 'uint16(operands[1]) << 8 | uint16(operands[0])'


class Instruction:
	def __init__(self, name):
		self.name = name
		self.opcodes = set()

	_INSTRUCTIONS = {}

	@classmethod
	def get(cls, name):
		if name not in cls._INSTRUCTIONS:
			cls._INSTRUCTIONS[name] = cls(name)
		return cls._INSTRUCTIONS[name]

	@classmethod
	def instances(cls):
		return cls._INSTRUCTIONS.values()

	@property
	def func_name(self):
		return 'compileInstr{}'.format(self.name)

	def generate_compiler(self):
		yield "func {}(c *compiler.Compiler, operands... []Operand) error {{\n".format(self.func_name)
		for opcode in self.opcodes:
			yield "// {} {}\t- {}\n".format(self.name,
											", ".join(op.spec for op in opcode.operands),
											opcode.desc)
		yield "}\n\n"


class Opcode:
	def __init__(self, instr, operands, desc=''):
		self.instr = Instruction.get(instr)
		self.operands = tuple(Operand.from_spec(operand) for operand in operands)
		self.desc = desc
		self.instr.opcodes.add(self)

	@classmethod
	def from_data(cls, data):
		return Opcode(**lower_keys(data))

	def generate_decoder(self):
		yield '// {} {}\n'.format(self.instr.name,
								  ", ".join(operand.spec for operand in self.operands))
		yield '// {}\n'.format(self.desc)

		for operand in self.operands:
			yield from operand.generate_reader()

		args = ", ".join(operand.generate_arg() for operand in self.operands)
		yield "{}({})".format(self.func_name, args)

	@property
	def func_name(self):
		return self.instr.func_name


class OpcodePlane(Opcode):
	def __init__(self, func_name):
		self._func_name = func_name
		self.opcodes = {}
		self.extensions = set()

	@property
	def func_name(self):
		return self._func_name

	@classmethod
	def from_data(cls, data, plane_name, func_name=None):
		if func_name is None:
			func_name = 'compileExt{}'.format(plane_name)

		ret = OpcodePlane(func_name=func_name)
		plane_data = data[plane_name]

		for code, instr_data in plane_data.items():
			num_code = int(code, 16)  # convert from hex string
			if instr_data['Instr'] == 'Ext':
				ret.extensions.add(num_code)
				ret.opcodes[num_code] = OpcodePlane.from_data(data, plane_name=code)
			else:
				ret.opcodes[num_code] = Opcode.from_data(instr_data)

		return ret

	def generate(self):
		for ext_code in self.extensions:
			yield from self.opcodes[ext_code].generate()

		yield 'func (a Arch) {}(c *compiler.Compiler) error {{'.format(self.func_name)
		yield '''
			var bytebuf [1]byte
			_, err := c.Input.Read(bytebuf[:])
			if err != nil {
					return err
			}

			switch bytebuf[0] {'''

		for code, instr in self.opcodes.items():
			yield '\ncase 0x{:02X}:\n'.format(code)
			yield from instr.generate_decoder()

		yield '}\n'
		yield '}\n\n'

	def generate_decoder(self):
		yield "return {}(c)".format(self.func_name)


class Compiler:
	def __init__(self, root_plane, pkg_name='arch'):
		self.pkg_name = pkg_name
		self.root_plane = root_plane

	def generate(self):
		yield '''package {pkg_name}

import (
	"io"
	"gbrc/compiler"
)

type Arch struct{{}}
type Register int
type IndirectRegister Register
type CompositeRegister struct {{lo, hi Register}}

const (
	A Register = iota
	B
	C
	D
	E
	F

	H
	L
)

type Operand interface {{
	GenGetter(c *compiler.Compiler) error
        GenSetter(c *compiler.Compiler, value Operand) error
}}

		'''.format(pkg_name=self.pkg_name)

		for instr in Instruction.instances():
			yield from instr.generate_compiler()

		yield from self.root_plane.generate()



def main():
	opcodes_data = json.load(sys.stdin)
	root_plane = OpcodePlane.from_data(opcodes_data, 'Direct')
	compiler = Compiler(root_plane, pkg_name='gb')

	for fragment in compiler.generate():
		print(fragment, end='')


if __name__ == '__main__':
	main()
