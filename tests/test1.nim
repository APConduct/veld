# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest
import ../src/veld/lexer


# Test program
proc test_lexer() =
  let source = """
    --| This is a comment
    struct Point<T>
      x: T
      y: T
    end

    let p = some {
      x: int = 10,
      y: int = 20.5
    }

    impl<T> to_string for Point<T> where T: Display
      func to_string -> string
        -- Implementation
      end
    end
  """

  var lexer = new_lexer(source)
  while true:
    let token = lexer.scan_token()
    echo "Line ", token.line, " Col ", token.col, ": ", token.kind, " '", token.lexeme, "'"
    if token.kind == TkEof:
      break

test "lexer test":
  test_lexer()
