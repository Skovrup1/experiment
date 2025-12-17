package lexer

TokenKind :: enum u8 {
	Invalid,
	Eof,
	//
	Identifier,
	Integer,
	Float,
	String,
	//
	Plus, // +
	Minus, // -
	Asterisk, // *
	Slash, // /
	Percent, // %
	Equal, // =
	ColonEqual, // :=
	ColonColon, // ::
	EqualEqual, // ==
	NotEqual, // !=
	Less, // <
	LessEqual, // <=
	Greater, // >
	GreaterEqual, // >=
	LeftParen, // (
	RightParen, // )
	LeftBrace, // {
	RightBrace, // }
	Comma, // ,
	Semicolon, // ;
	Colon, // :
	Dot, // .
	Arrow, // ->
	//
	If,
	Else,
	For,
	Return,
	True,
	False,
}

Token :: struct {
	kind:  TokenKind,
	start: TokenIndex,
	end:   TokenIndex,
}

TokenIndex :: distinct u32
INVALID_TOKEN := max(TokenIndex)

LexerError :: struct {
	message:  string,
	position: TokenIndex,
}

Scanner :: struct {
	source:   string,
	tokens:   [dynamic]Token,
	errors:   [dynamic]LexerError,
	keywords: map[string]TokenKind,
	//
	start:    TokenIndex,
	cursor:   TokenIndex,
}

make_keywords :: proc() -> map[string]TokenKind {
	keywords := make(map[string]TokenKind)

	keywords["if"] = .If
	keywords["else"] = .Else
	keywords["for"] = .For
	keywords["return"] = .Return
	keywords["true"] = .True
	keywords["false"] = .False

	return keywords
}

make_scanner :: proc(source: string) -> Scanner {
	tokens := make([dynamic]Token)
	errors := make([dynamic]LexerError)
	keywords := make_keywords()
	start :: 0
	current :: 0
	return Scanner{source, tokens, errors, keywords, start, current}
}

is_at_end :: proc(s: ^Scanner) -> bool {
	return s.cursor >= TokenIndex(len(s.source))
}

peek :: proc(s: ^Scanner) -> byte {
	if is_at_end(s) {
		return 0
	}

	return s.source[s.cursor]
}

peek_next :: proc(s: ^Scanner) -> byte {
	if s.cursor + 1 >= TokenIndex(len(s.source)) {
		return 0
	}

	return s.source[s.cursor + 1]
}

advance :: proc(s: ^Scanner) -> byte {
	char := peek(s)
	s.cursor += 1
	return char
}

add_token :: proc(s: ^Scanner, kind: TokenKind) {
	token := Token{kind, s.start, s.cursor}
	append(&s.tokens, token)
}

add_synthetic_token :: proc(s: ^Scanner, kind: TokenKind, pos: TokenIndex) {
	token := Token{kind, pos, pos}
	append(&s.tokens, token)
}

add_error :: proc(s: ^Scanner, message: string, position: TokenIndex) {
	error := LexerError{message, position}
	append(&s.errors, error)
}

skip :: proc(s: ^Scanner) {
	for {
		char := peek(s)
		switch char {
		case ' ', '\t', '\r', '\n':
			advance(s)
		case:
			return
		}
	}
}

is_digit :: proc(char: byte) -> bool {
	return char >= '0' && char <= '9'
}

is_alpha :: proc(char: byte) -> bool {
	return (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') || (char == '_')
}

number :: proc(s: ^Scanner) {
	is_integer := true

	for is_digit(peek(s)) {
		advance(s)
	}

	if peek(s) == '.' {
		is_integer = false
		advance(s)

		for is_digit(peek(s)) {
			advance(s)
		}
	}

	if is_integer {
		add_token(s, .Integer)
	} else {
		add_token(s, .Float)
	}
}

identifier :: proc(s: ^Scanner) {
	for is_alpha(peek(s)) || is_digit(peek(s)) {
		advance(s)
	}

	str := string(s.source[s.start:s.cursor])

	if kind, ok := s.keywords[str]; ok {
		add_token(s, kind)
	} else {
		add_token(s, .Identifier)
	}
}

scan_token :: proc(s: ^Scanner) {
	skip(s)

	s.start = s.cursor

	if is_at_end(s) {
		add_token(s, .Eof)
		return
	}

	char := advance(s)

	switch char {
	case '+':
		add_token(s, .Plus)
	case '-':
		if peek(s) == '>' {
			advance(s)
			add_token(s, .Arrow)
		} else {
			add_token(s, .Minus)
		}
	case '*':
		add_token(s, .Asterisk)
	case '/':
		add_token(s, .Slash)
	case '%':
		add_token(s, .Percent)
	case '=':
		if peek(s) == '=' {
			advance(s)
			add_token(s, .EqualEqual)
		} else {
			add_token(s, .Equal)
		}
	case '!':
		if peek(s) == '=' {
			advance(s)
			add_token(s, .NotEqual)
		}
	case '<':
		if peek(s) == '=' {
			advance(s)
			add_token(s, .LessEqual)
		} else {
			add_token(s, .Less)
		}
	case '>':
		if peek(s) == '=' {
			advance(s)
			add_token(s, .GreaterEqual)
		} else {
			add_token(s, .Greater)
		}
	case '(':
		add_token(s, .LeftParen)
	case ')':
		add_token(s, .RightParen)
	case '{':
		add_token(s, .LeftBrace)
	case '}':
		add_token(s, .RightBrace)
	case ',':
		add_token(s, .Comma)
	case ';':
		add_token(s, .Semicolon)
	case ':':
		if peek(s) == '=' {
			advance(s)
			add_token(s, .ColonEqual)
		} else if peek(s) == ':' {
			advance(s)
			add_token(s, .ColonColon)
		} else {
			add_token(s, .Colon)
		}
	case '.':
		add_token(s, .Dot)
	case:
		if is_digit(char) {
			number(s)
		} else if is_alpha(char) {
			identifier(s)
		} else {
			add_token(s, .Invalid)
			add_error(s, "unexpected character", s.start)
		}
	}
}

scan_tokens :: proc(s: ^Scanner) -> [dynamic]Token {
	if is_at_end(s) { 	// guards againt empty input
		add_token(s, .Eof)
	}

	for !is_at_end(s) {
		scan_token(s)
	}

	return s.tokens
}

token_text :: proc(source: string, token: Token) -> string {
	return source[token.start:token.end]
}
