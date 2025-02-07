type
  ErrorKind* = enum
    ErrorLexical,   # Lexical error (e.g., invalid character)
    ErrorSyntax,    # Syntax error (unexpected token, missing delimiter,...)
    ErrorSemantic,  # Semantic error (e.g., type mismatch)

  ErrorLevel* = enum
    LevelError,     # Error that prevents further compilation
    LevelWarning,   # Warning that does not prevent compilation
    LevelNote,      # Informational note

  Error* = object
    kind*: ErrorKind
    level*: ErrorLevel
    message*: string
    line*: int
    col*: int
    file*: string   # Source file name

  ErrorReporter* = ref object
    errors*: seq[Error]
    warnings*: seq[Error]
    has_error*: bool

proc new_error_reporter*(): ErrorReporter =
  ErrorReporter(
    errors: @[],
    warnings: @[],
    has_error: false
  )

proc report*(self: ErrorReporter, error: Error) =
  case error.level:
  of LevelError:
    self.errors.add(error)
  of LevelWarning:
    self.warnings.add(error)
  of LevelNote:
    echo "[Note] ", error.message

proc format_error*(error: Error): string =
  let prefix = case error.level:
    of LevelError: "Error"
    of LevelWarning: "Warning"
    of LevelNote: "Note"
  return prefix & " at " & error.file & ":" & $error.line & ":" & $error.col & ": " & error.message
