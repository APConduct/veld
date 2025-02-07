import tables     # Hash tables for keywords
import strutils   # String utilities
# import unicode    # Unicode support

type TokenKind* = enum
  # Basic tokens
  TkEof,      # End of file
  TkError,    # For Error Handling
  TkNumber,   # Numeric literals

  # Literals
  TkIdentifier    # variable names, function names, etc.
  TkString        # string literals

  # Keywords
  TkFunc,   # func
  TkLet,    # let
  TkVar,    # var
  TkSome,   # some
  TkEnd,    # end
  TkStruct, # struct
  TkImpl,  # impl
  TkWhere,  # where
  TkIn,     # in

  # Symbols
  TkEqual,    # =
  TkComma,    # ,
  TkColon,    # :
  TkDot,      # .
  TkArrow,    # ->
  TkLParen,   # (
  TkRParen,   # )
  TkLAngle,   # <
  TkRAngle,   # >
  TkLBracket, # [
  TkRBracket, # ]
  TkLBrace,   # {
  TkRBrace,   # }

  # Token object to store information about each token
type Token* = object
  kind*: TokenKind
  lexeme*: string # The actual text of the token
  line*: int      # Line number of the token for Error Reporting
  col*: int       # Column number for Error Reporting

# Lexer object to handle Tokenization
type Lexer* = object
 source*: string    # The source code
 start*: int        # Start of the current token
 current*: int      # Current position
 line*: int         # Current line
 col*: int          # Current column

# Initialize a new lexer
proc new_lexer*(source: string): Lexer =
  result = Lexer(
    source: source,
    start: 0,
    current: 0,
    line: 1,
    col: 1
  )

# Helper function to check if we're at the end of the source
proc is_at_end(self: Lexer): bool =
  self.current >= self.source.len

proc peek(self: Lexer): char =
  if self.is_at_end(): '\0'
  else: self.source[self.current]

proc next_peek(self: Lexer): char =
  if self.current + 1 >= self.source.len: '\0'
  else: self.source[self.current + 1]

proc adance(self: var Lexer): char =
  result = self.source[self.current]
  inc self.current
  inc self.col

# Helper function to create a token
proc make_token(self: Lexer, kind: TokenKind): Token =
  Token(
    kind: kind,
    lexeme: self.source[self.start..<self.current],
    line: self.line,
    col: self.col - (self.current - self.start)
  )

proc match(self: var Lexer, expected: char): bool =
  if self.is_at_end(): return false
  if self.source[self.current] != expected: return false
  inc self.current
  inc self.col
  true

# Keywords lookup table
const keywords = {
  "func": TkFunc,
  "let": TkLet,
  "var": TkVar,
  "some": TkSome,
  "end": TkEnd,
  "struct": TkStruct,
  "impl": TkImpl,
  "where": TkWhere,
  "in": TkIn
}.to_table

proc identifier_or_keyword(self: var Lexer): Token =
  while is_alpha_numeric(self.peek()) or self.peek() == '_':
    discard self.adance()
  let text = self.source[self.start..<self.current]
  let kind = if keywords.has_key(text): keywords[text] else: TkIdentifier
  self.make_token(kind)


proc error_token(self: Lexer, message: string): Token =
  Token(kind: TkError, lexeme: message, line: self.line, col: self.col)

# Scan string literals
proc scan_string(self: var Lexer): Token =
  while self.peek() != '"' and not self.is_at_end():
    if self.peek() == '\n':
      inc self.line
      self.col = 1
    discard self.adance()

  if self.is_at_end():
    return self.error_token("Unterminated string.")

  # Consume the closing quote
  discard self.adance()
  self.make_token(TkString)

# Scan Numbers (integers and floats)
proc scan_number (self: var Lexer): Token =
  while is_digit(self.peek()):
    discard self.adance()

  # check for decimal part
  if self.peek() == '.' and is_digit(self.next_peek()):
    # consume the decimal point
    discard self.adance()
    # consume the digits after the decimal point
    while is_digit(self.peek()):
      discard self.adance()
  return self.make_token(TkNumber)

# Scan identifiers and keywords
proc scan_identifier(self: var Lexer): Token =
  while is_alpha_numeric(self.peek()) or self.peek() == '_':
    discard self.adance()

  let text = self.source[self.start..<self.current]
  let kind = if keywords.has_key(text): keywords[text] else: TkIdentifier
  return self.make_token(kind)

# Skip whitespace and comments
proc skip_whitespace(self: var Lexer) =
  while true:
    let c = self.peek()
    case c
    of ' ', '\r', '\t':
      discard self.adance()
    of '\n':
      inc self.line
      self.col = 1
      discard self.adance()
    of '-': # handle comments
      if self.peek() == '-':
      # Comment goes until end of line
        while self.peek() != '\n' and not self.is_at_end():
          discard self.adance()
      else: return
    else: return

# Main scanning function
proc scan_token*(self: var Lexer): Token =
  self.skip_whitespace()
  self.start = self.current

  if self.is_at_end():
    return self.make_token(TkEof)

  let c = self.adance()

  # Single character tokens
  case c
  of '(': return self.make_token(TkLParen)
  of ')': return self.make_token(TkRParen)
  of '{': return self.make_token(TkLBrace)
  of '}': return self.make_token(TkRBrace)
  of '[': return self.make_token(TkLBracket)
  of ']': return self.make_token(TkRBracket)
  of '<': return self.make_token(TkLAngle)
  of '>': return self.make_token(TkRAngle)
  of ',': return self.make_token(TkComma)
  of '.': return self.make_token(TkDot)
  of ':': return self.make_token(TkColon)
  of '=': return self.make_token(TkEqual)

  # String literals
  of '"': result = self.scan_string()

  # Numbers
  of '0'..'9': result = self.scan_number()

  # Identifiers and keywords
  of 'a'..'z', 'A'..'Z', '_': result = self.scan_identifier()

  # Arrow
  of '-':
    if self.match('>'):
      result = self.make_token(TkArrow)
    else:
      result = self.error_token("Unexpected character.")
  else:
    result = self.error_token("Unexpected character.")
