package lexer

import "core:fmt"
import "core:strconv"

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
	StructLit,

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
	Arrow, // ->

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
	Power, // **
	PlusEqual, // +=
	MinusEqual, // -=
	MulEqual, // *=
	DivEqual, // /=
	ModEqual, // %=
	PowerEqual, // **=

	// bitwise
	Tilde, // ~
	Ampersand, // &
	Pipe, // |
	Hat, // ^
	TildeEqual, // ~=
	AmpersandEqual, // &=
	PipeEqual, // |=
	HatEqual, // ^=
	LShiftEqual, // <<=
	RShiftEqual, // >>=
	Hash, // #
	Dollar, // $
	LShift, // <<
	RShift, // >>

	// keywords
	Import,
	Return,
	Loop,
	If,
	Else,
	Break,
	Continue,
	Struct,
	Union,
	Enum,
	True,
	False,
}

TokenIndex :: distinct u32
INVALID_TOKEN :: max(TokenIndex)

Token :: struct {
	kind:  TokenKind,
	start: TokenIndex,
	end:   TokenIndex,
}

ScannerFlag :: bit_set[enum {
	ScanComments,
}]

Scanner :: struct {
	source:   []u8,
	tokens:   [dynamic]Token,
	keywords: map[string]TokenKind,
	start:    TokenIndex,
	cursor:   TokenIndex,
	flags:    ScannerFlag,
}

make_keywords :: proc() -> map[string]TokenKind {
	keywords := make(map[string]TokenKind)

	keywords["import"] = .Import
	keywords["return"] = .Return
	keywords["loop"] = .Loop
	keywords["if"] = .If
	keywords["else"] = .Else
	keywords["break"] = .Break
	keywords["continue"] = .Continue
	keywords["struct"] = .Struct
	keywords["union"] = .Union
	keywords["enum"] = .Enum
	keywords["true"] = .True
	keywords["false"] = .False

	return keywords
}

make_scanner :: proc(source: []u8, start: TokenIndex = 0, cursor: TokenIndex = 0) -> Scanner {
	keywords := make_keywords()
	tokens := make([dynamic]Token)

	return Scanner{source, tokens, keywords, start, cursor, {}}
}

make_token :: proc(s: ^Scanner, kind: TokenKind) -> Token {
	return Token{kind, s.start, s.cursor}
}

get_position :: proc(source: []u8, offset: TokenIndex) -> (int, int) {
	current_line := 1
	current_column := 1
	offset := int(offset)

	for i := 0; i < offset && i < len(source); i += 1 {
		if source[i] == '\n' {
			current_line += 1
			current_column = 1
		} else {
			current_column += 1
		}
	}

	return current_line, current_column
}

format_token_location :: proc(source: []u8, token: Token) -> string {
	line, column := get_position(source, token.start)
	return fmt.tprintf("line %d, column %d", line, column)
}

peek :: proc(s: ^Scanner) -> u8 {
	if is_at_end(s) {
		return 0
	}

	return s.source[s.cursor]
}

peek_next :: proc(s: ^Scanner) -> u8 {
	if is_at_end(s) {
		return 0
	}

	return s.source[s.cursor + 1]
}

advance :: proc(s: ^Scanner) -> u8 {
	s.cursor += 1
	return s.source[s.cursor - 1]
}

is_at_end :: proc(s: ^Scanner) -> bool {
	return s.cursor >= TokenIndex(len(s.source))
}

skip_whitespace :: proc(s: ^Scanner) {
	for {
		r := peek(s)
		switch r {
		case ' ', '\r', '\t', '\n':
			advance(s)
		case:
			return
		}
	}
}

line_comment :: proc(s: ^Scanner) -> Token {
	for peek(s) != '\n' && !is_at_end(s) {
		advance(s)
	}

	return make_token(s, .LineComment)
}

block_comment :: proc(s: ^Scanner) -> Token {
	for !(peek(s) == '*' && peek_next(s) == '/') && !is_at_end(s) {
		advance(s)
	}
	advance(s)
	advance(s)

	return make_token(s, .BlockComment)
}

is_alpha :: proc(r: u8) -> bool {
	return (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || r == '_'
}

is_digit :: proc(r: u8) -> bool {
	return r >= '0' && r <= '9'
}

identifier_type :: proc(s: ^Scanner) -> TokenKind {
	ident := string(s.source[s.start:s.cursor])

	keyword, is_keyword := s.keywords[ident]
	if is_keyword {
		return keyword
	}

	return .Identifier
}

identifier :: proc(s: ^Scanner) -> Token {
	for is_alpha(peek(s)) || is_digit(peek(s)) {
		advance(s)
	}

	return make_token(s, identifier_type(s))
}

number :: proc(s: ^Scanner) -> Token {
	is_integer := true

	for is_digit(peek(s)) {
		advance(s)
	}

	if peek(s) == '.' && is_digit(peek_next(s)) {
		is_integer = false
		advance(s)

		for is_digit(peek(s)) {
			advance(s)
		}
	}

	if is_integer {
		return make_token(s, .Integer)
	}

	return make_token(s, .Real)
}

next_token :: proc(s: ^Scanner) -> Token {
	skip_whitespace(s)

	s.start = s.cursor

	if is_at_end(s) {
		return make_token(s, .Eof)
	}

	r := advance(s)

	if is_alpha(r) {
		return identifier(s)
	}
	if is_digit(r) {
		return number(s)
	}

	switch r {
	case '"':
		// note: right now it also keeps the '"' character, starting and ending the string literal
		for peek(s) != '"' {
			advance(s)
		}
		advance(s)
		return make_token(s, .String)
	case '!':
		if peek(s) == '=' {
			advance(s)
			return make_token(s, .NotEqual)
		}
		return make_token(s, .Not)
	case '#':
		return make_token(s, .Hash)
	case '$':
		return make_token(s, .Dollar)
	case '%':
		if peek(s) == '=' {
			advance(s)
			return make_token(s, .ModEqual)
		}
		return make_token(s, .Mod)
	case '&':
		if peek(s) == '&' {
			advance(s)
			return make_token(s, .And)
		} else if peek(s) == '=' {
			advance(s)
			return make_token(s, .AmpersandEqual)
		}
		return make_token(s, .Ampersand)
	case '(':
		return make_token(s, .LParen)
	case ')':
		return make_token(s, .RParen)
	case '*':
		if peek(s) == '=' {
			advance(s)
			return make_token(s, .MulEqual)
		}
		if peek(s) == '*' {
			advance(s)
			if peek(s) == '=' {
				advance(s)
				return make_token(s, .PowerEqual)
			}
			return make_token(s, .Power)
		}
		return make_token(s, .Mul)
	case '+':
		if peek(s) == '=' {
			advance(s)
			return make_token(s, .PlusEqual)
		}
		return make_token(s, .Plus)
	case ',':
		return make_token(s, .Comma)
	case '-':
		if peek(s) == '=' {
			advance(s)
			return make_token(s, .MinusEqual)
		}
		if peek(s) == '>' {
			advance(s)
			return make_token(s, .Arrow)
		}
		return make_token(s, .Minus)
	case '.':
		if peek(s) == '*' {
			advance(s)
			return make_token(s, .Deref)
		}
		return make_token(s, .Period)
	case '/':
		if peek(s) == '/' {
			line := line_comment(s)
			if .ScanComments in s.flags {
				return line
			} else {
				return next_token(s)
			}
		}
		if peek(s) == '*' {
			block := block_comment(s)
			if .ScanComments in s.flags {
				return block
			} else {
				return next_token(s)
			}
		}
		if peek(s) == '=' {
			advance(s)
			return make_token(s, .DivEqual)
		}
		return make_token(s, .Div)
	case ':':
		if peek(s) == ':' {
			advance(s)
			return make_token(s, .Const)
		} else if peek(s) == '=' {
			advance(s)
			return make_token(s, .Var)
		} else {
			return make_token(s, .Colon)
		}
	case ';':
		return make_token(s, .Semicolon)
	case '=':
		if peek(s) == '=' {
			advance(s)
			return make_token(s, .Equal)
		}
		return make_token(s, .Assign)
	case '<':
		if peek(s) == '<' {
			advance(s)
			if peek(s) == '=' {
				advance(s)
				return make_token(s, .LShiftEqual)
			}
			return make_token(s, .LShift)
		}
		if peek(s) == '=' {
			advance(s)
			return make_token(s, .LessEqual)
		}
		return make_token(s, .Less)
	case '>':
		if peek(s) == '>' {
			advance(s)
			if peek(s) == '=' {
				advance(s)
				return make_token(s, .RShiftEqual)
			}
			return make_token(s, .RShift)
		}
		if peek(s) == '=' {
			advance(s)
			return make_token(s, .GreaterEqual)
		}
		return make_token(s, .Greater)
	case '[':
		return make_token(s, .LBracket)
	case ']':
		return make_token(s, .RBracket)
	case '^':
		if peek(s) == '=' {
			advance(s)
			return make_token(s, .HatEqual)
		}
		return make_token(s, .Hat)
	case '{':
		return make_token(s, .LBrace)
	case '}':
		return make_token(s, .RBrace)
	case '~':
		if peek(s) == '=' {
			advance(s)
			return make_token(s, .TildeEqual)
		}
		return make_token(s, .Tilde)
	case '|':
		if peek(s) == '|' {
			advance(s)
			return make_token(s, .Or)
		}
		if peek(s) == '=' {
			advance(s)
			return make_token(s, .PipeEqual)
		}
		return make_token(s, .Pipe)
	}

	return make_token(s, .Error)
}

consume_all :: proc(s: ^Scanner) -> [dynamic]Token {
	for tok := next_token(s); tok.kind != .Eof; tok = next_token(s) {
		append(&s.tokens, tok)
	}
	append(&s.tokens, make_token(s, .Eof))

	return s.tokens
}
