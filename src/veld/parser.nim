import lexer
import error
import ast

type
  Parser* = ref object
    lexer*: Lexer
    current*: Token
    previous*: Token
    error_reporter*: ErrorReporter
    had_error*: bool
    panic_mode*: bool

proc new_parser*(source: string): Parser =
  result = Parser(
    lexer: new_lexer(source),
    error_reporter: new_error_reporter(),
    had_error: false,
    panic_mode: false
  )
  # Prime the parser with the first token
  result.current = result.lexer.scan_token()

proc error_at_current*(self: Parser, message: string) =
  if self.panic_mode: return
  self.panic_mode = true

  let error = Error(
    kind: ErrorSyntax,
    level: LevelError,
    message: message,
    line: self.current.line,
    col: self.current.col,
  )
  self.error_reporter.report(error)
  self.had_error = true

proc advance*(self: var Parser) =
  self.previous = self.current

  while true:
    self.current = self.lexer.scan_token()
    if self.current.kind != TkError: break

    self.error_at_current(self.current.lexeme)

proc consume*(self: var Parser, kind: TokenKind, message: string) =
  if self.current.kind == kind:
    self.advance()
    return

  self.error_at_current(message)

proc check*(self: Parser, kind: TokenKind): bool =
  self.current.kind == kind

proc match*(self: var Parser, kind: TokenKind): bool =
  if not self.check(kind): return false
  self.advance()
  true

# Parsing functions for different constructs
proc parse_struct*(self: var Parser): Node =
  # struct Name<T> ... end
  self.consume(TkStruct, "Expect 'struct' keyword  before struct definition.")
  self.consume(TkIdentifier, "Expected struct name.")
  let name = self.previous.lexeme

  var generic_params: seq[Node] = @[]
  if self.match(TkLAngle):
    while not self.check(TkRAngle):
      # Parse generic parameter
      self.consume(TkIdentifier, "Expected generic parameter name.")
