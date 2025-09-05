package scanner

import "core:fmt"

TokenKind :: enum u8 {
	Null = 0,
	Eof,
	Error,

	// comments
	LineComment,
	BlockComment,

	// literals
	Identifier,
	String,
	Real,
	Integer,

	// delimiters & punctuation
	Comma, // ,
	Period, // .
	Colon, // :
	Semicolon, // ;
	LParen, // (
	RParen, // )
	LBracket, // [
	RBracket, // ]
	LBrace, // {
	RBrace, // }
	Deref, // .*

	// assignment & comparison operators
	Const, // ::
	Var, // :=
	Assign, // =
	Equal, // ==
	NotEqual, // !=
	Less, // <
	LessEqual, // <=
	Greater, // >
	GreaterEqual, // >=

	// logical operators
	Or, // ||
	And, // &&
	Not, // !

	// arithmetic operators
	Plus, // +
	Minus, // -
	Mul, // *
	Div, // /
	Mod, // %
	PlusEqual, // +=
	MinusEqual, // -=
	MulEqual, // *=
	DivEqual, // /=
	ModEqual, // %=

	// bitwise
	AmpersandEqual, // &=
	PipeEqual, // |=
	TildeEqual, // ~=
	LShiftEqual, // <<=
	RShiftEqual, // >>=
	Hash, // #
	Dollar, // $
	Tilde, // ~
	Ampersand, // & (Bitwise AND / Address-of)
	Pipe, // | (Bitwise OR)
	Hat, // ^ (Bitwise XOR)
	LShift, // <<
	RShift, // >>

	// keywords
	Module,
	Return,
	Loop,
	If,
	Else,
	Break,
	Continue,
	Struct,
	True,
	False,

	// type keywords
	Void,
	F32,
	I32,
	Bool,
}

TokenIndex :: distinct u32
INVALID_TOKEN :: max(TokenIndex)

Token :: struct {
	kind:  TokenKind,
	start: TokenIndex,
	end:   TokenIndex,
}

Scanner :: struct {
	source:   []u8,
	keywords: map[string]TokenKind,
	start:    TokenIndex,
	cursor:   TokenIndex,
}

make_keywords :: proc() -> map[string]TokenKind {
	keywords := make(map[string]TokenKind)

	keywords["module"] = .Module
	keywords["return"] = .Return
	keywords["loop"] = .Loop
	keywords["if"] = .If
	keywords["else"] = .Else
	keywords["break"] = .Break
	keywords["continue"] = .Continue
	keywords["struct"] = .Struct
	keywords["true"] = .True
	keywords["false"] = .False
	keywords["Void"] = .Void // maybe change to ()
	keywords["F32"] = .F32
	keywords["I32"] = .I32
	keywords["Bool"] = .Bool

	return keywords
}

make_scanner :: proc(source: []u8, start: TokenIndex = 0, cursor: TokenIndex = 0) -> Scanner {
	keywords := make_keywords()

	return Scanner{source, keywords, start, cursor}
}

make_token :: proc(t: ^Scanner, kind: TokenKind) -> Token {
	return Token{kind, t.start, t.cursor}
}

peek :: proc(t: ^Scanner) -> u8 {
	if is_at_end(t) {
		return 0
	}

	return t.source[t.cursor]
}

peek_next :: proc(t: ^Scanner) -> u8 {
	if is_at_end(t) {
		return 0
	}

	return t.source[t.cursor + 1]
}

advance :: proc(t: ^Scanner) -> u8 {
	t.cursor += 1
	return t.source[t.cursor - 1]
}

is_at_end :: proc(t: ^Scanner) -> bool {
	return t.cursor >= TokenIndex(len(t.source))
}

skip_whitespace :: proc(t: ^Scanner) {
	for {
		r := peek(t)
		switch r {
		case ' ', '\r', '\t', '\n':
			advance(t)
		case:
			return
		}
	}
}

line_comment :: proc(t: ^Scanner) -> Token {
	for peek(t) != '\n' && !is_at_end(t) {
		advance(t)
	}

	return make_token(t, .LineComment)
}

block_comment :: proc(t: ^Scanner) -> Token {
	for !(peek(t) == '*' && peek_next(t) == '/') && !is_at_end(t) {
		advance(t)
	}
	advance(t)
	advance(t)

	return make_token(t, .BlockComment)
}

is_alpha :: proc(r: u8) -> bool {
	return (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || r == '_'
}

is_digit :: proc(r: u8) -> bool {
	return r >= '0' && r <= '9'
}

identifier_type :: proc(t: ^Scanner) -> TokenKind {
	ident := string(t.source[t.start:t.cursor])

	keyword, is_keyword := t.keywords[ident]
	if is_keyword {
		return keyword
	}

	return .Identifier
}

identifier :: proc(t: ^Scanner) -> Token {
	for is_alpha(peek(t)) || is_digit(peek(t)) {
		advance(t)
	}

	return make_token(t, identifier_type(t))
}

number :: proc(t: ^Scanner) -> Token {
	is_integer := true

	for is_digit(peek(t)) {
		advance(t)
	}

	if peek(t) == '.' && is_digit(peek_next(t)) {
		is_integer = false
		advance(t)

		for is_digit(peek(t)) {
			advance(t)
		}
	}

	if is_integer {
		return make_token(t, .Integer)
	}

	return make_token(t, .Real)
}

next_token :: proc(t: ^Scanner) -> Token {
	skip_whitespace(t)

	t.start = t.cursor

	if is_at_end(t) {
		return make_token(t, .Eof)
	}

	r := advance(t)

	if is_alpha(r) {
		return identifier(t)
	}
	if is_digit(r) {
		return number(t)
	}

	switch r {
	case '"':
		advance(t)
		for peek(t) != '"' {
			advance(t)
		}
        advance(t)
		return make_token(t, .String)
	case '!':
		if peek(t) == '=' {
			advance(t)
			return make_token(t, .NotEqual)
		}
		return make_token(t, .Not)
	case '#':
		return make_token(t, .Hash)
	case '$':
		return make_token(t, .Dollar)
	case '%':
		if peek(t) == '=' {
			advance(t)
			return make_token(t, .ModEqual)
		}
		return make_token(t, .Mod)
	case '&':
		if peek(t) == '&' {
			advance(t)
			return make_token(t, .And)
		} else if peek(t) == '=' {
			advance(t)
			return make_token(t, .AmpersandEqual)
		}
		return make_token(t, .Ampersand)
	case '(':
		return make_token(t, .LParen)
	case ')':
		return make_token(t, .RParen)
	case '*':
		if peek(t) == '=' {
			advance(t)
			return make_token(t, .MulEqual)
		}
		return make_token(t, .Mul)
	case '+':
		if peek(t) == '=' {
			advance(t)
			return make_token(t, .PlusEqual)
		}
		return make_token(t, .Plus)
	case ',':
		return make_token(t, .Comma)
	case '-':
		if peek(t) == '=' {
			advance(t)
			return make_token(t, .MinusEqual)
		}
		return make_token(t, .Minus)
	case '.':
		if peek(t) == '*' {
			advance(t)
			return make_token(t, .Deref)
		}
		return make_token(t, .Period)
	case '/':
		if peek(t) == '/' {
			return line_comment(t)
		}
		if peek(t) == '*' {
			return block_comment(t)
		}
		if peek(t) == '=' {
			advance(t)
			return make_token(t, .DivEqual)
		}
		return make_token(t, .Div)
	case ':':
		if peek(t) == ':' {
			advance(t)
			return make_token(t, .Const)
		} else if peek(t) == '=' {
			advance(t)
			return make_token(t, .Var)
		} else {
			return make_token(t, .Colon)
		}
	case ';':
		return make_token(t, .Semicolon)
	case '=':
		if peek(t) == '=' {
			advance(t)
			return make_token(t, .Equal)
		}
		return make_token(t, .Assign)
	case '<':
		if peek(t) == '<' {
			advance(t)
			if peek(t) == '=' {
				advance(t)
				return make_token(t, .LShiftEqual)
			}
			return make_token(t, .LShift)
		}
		if peek(t) == '=' {
			advance(t)
			return make_token(t, .LessEqual)
		}
		return make_token(t, .Less)
	case '>':
		if peek(t) == '>' {
			advance(t)
			if peek(t) == '=' {
				advance(t)
				return make_token(t, .RShiftEqual)
			}
			return make_token(t, .RShift)
		}
		if peek(t) == '=' {
			advance(t)
			return make_token(t, .GreaterEqual)
		}
		return make_token(t, .Greater)
	case '[':
		return make_token(t, .LBracket)
	case ']':
		return make_token(t, .RBracket)
	case '^':
		return make_token(t, .Hat)
	case '{':
		return make_token(t, .LBrace)
	case '}':
		return make_token(t, .RBrace)
	case '~':
		if peek(t) == '=' {
			advance(t)
			return make_token(t, .TildeEqual)
		}
		return make_token(t, .Tilde)
	case '|':
		if peek(t) == '=' {
			advance(t)
			return make_token(t, .PipeEqual)
		}
		return make_token(t, .Pipe)
	}

	return make_token(t, .Error)
}

consume_all :: proc(t: ^Scanner) -> [dynamic]Token {
	list: [dynamic]Token

	for tok := next_token(t); tok.kind != .Eof; tok = next_token(t) {
		append(&list, tok)
	}
	append(&list, make_token(t, .Eof))

	return list
}
