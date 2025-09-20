package tac

import "../parser"
import "../scanner"
import "../sema"

import "core:fmt"

// plan:
// walk ast to generate cfg of basic blocks
// intern strings
// lowering sema types into tac type

SignedInteger :: struct {
	bit_width: int,
}

TacType :: struct {
	signed: SignedInteger,
}

TacKind :: enum {
	add,
}

TacInst :: struct {
	kind: TacKind,
	data: struct #raw_union {
		a: f32,
		b: int,
	},
}

StringIndex :: distinct u32
