import strutils


type TokenKind = enum
  TkIdentifier, TkString, TkNumber, TkOperator, TkKeyword, TkComment, TkWhitespace
  TkStruct, TkKind, TkImpl, TkFunc, TkSome,
  TkVar, TkLet, TkArrow,
  TkIf, TkEnd, TkLParen, TkRParen,
  TkLBrace, TkRBrace, TkLAngle, TkRAngle,
  TkLBracket, TkRBracket,
  TkColon, TkComma, TkDot, TkIn, TkWhere, TkEof

  TkElse, TkThen, TkElseIf, TkWhile, TkFor, TkDo, TkReturn,
  TkBreak, TkContinue,

  TkError,
  TkEqual, TkOf

type Token = object
  kind: TokenKind
  lexeme: string
  line: int
  col: int

type NodeKind = enum
  # Expression nodes
  NkIdentifier, NkLiteral, NkCall, NkMethodCall,
  NkArrayAccess, NkGenericExpr,
  # Type nodes
  NkTypeAnnotation, NkGenericType, NkArrayType,
  NkTupleType,
  # Statement/Declaration nodes
  NkStruct, NkImpl, NkFunc, NkSomeObj, NkSomeKind,
  SkTrait,
  NkKind,
  # Patterm  matching
  NkPattern, NkMatch,




type Node = ref object
  case kind: NodeKind
  of NkCall:
    callee: Node
    args: seq[Node]
  of NkIdentifier:
    name: string
  of NkLiteral:
    value: string
    literal_type: TokenKind
  of NkGenericExpr:
    base_expr: Node
    type_args: seq[Node]
    constraints: seq[Node] # where clauses
  of NkArrayAccess:
    array: Node
    index: Node
  of NkSomeObj:
    fields: seq[tuple[name: string, typ: Node, value: Node]]
  of NkSomeKind:
    methods: seq[Node]
  of NkImpl:
    target: Node
    trait: Node
    type_params: seq[string]
    where_clause: seq[Node]
    mthds: seq[Node]
  of NkMethodCall:
    receiver: Node
    methds: string
    method_args: seq[Node]
  of NkStruct:
    struct_name: string
    struct_fields: seq[tuple[name: string, typ: Node, mutable: bool]]
    generic_params: seq[string]
  of NkTupleType:
    types: seq[Node]
  of NkGenericType:
    base_type: Node
    generic_type_args: seq[Node]
  of NkArrayType:
    array_base_type: Node
    element_type: Node
    size: int
  of NkFunc:
    func_name: string
    params: seq[tuple[name: string, typ: Node]]
    return_type: Node
    body: seq[Node]

  of NkPattern:
    pattern_name: string
    pattern_fields: seq[string]
    range_start, range_end: Node
    collection: Node # for collection patterns
  else:
    discard

type Lexer = object
  source: string
  current: int
  start: int
  line: int
  col: int


proc new_lexer(source: string): Lexer = Lexer(source: source, current: 0, start: 0, line: 1, col: 1)

proc advance(self: var Lexer): char =
  result = if self.current < self.source.len: self.source[self.current] else: '\0'
  inc self.current
  inc self.col

proc peek(self: Lexer): char =
  if self.current >= self.source.len: '\0'
  else: self.source[self.current]

proc next_peek(self: Lexer): char =
  if self.current + 1 >= self.source.len: '\0'
  else: self.source[self.current + 1]

proc is_at_end(self: Lexer): bool =
  self.current >= self.source.len

proc scan_token(self: var Lexer): Token =
  proc make_token(kind: TokenKind): Token =
    Token(
      kind: kind,
      lexeme: self.source[self.start..<self.current],
      line: self.line,
      col: self.col - (self.current - self.start)
    )
  while not self.is_at_end():
    let c = self.peek()
    case c
    of ' ', '\t', '\r':
      discard self.advance()
      self.start = self.current
    of '\n':
      inc self.line
      self.col = 0
      discard self.advance()
      self.start = self.current
    else: break

  self.start = self.current
  if self.is_at_end(): return make_token(TkEof)

  let c = self.advance()
  case c
  of '(':
    result = make_token(TkLParen)
  of ')':
    result = make_token(TkRParen)
  of ':':
    result = make_token(TkColon)
  of ',':
    result = make_token(TkComma)
  of '.':
    result = make_token(TkDot)
  of '-':
    if self.peek() == '>' and self.advance() == '>':
      result = make_token(TkArrow)
    else:
      # handke error
      # result = make_token(TkOperator)
      discard
  of 'a'..'z', 'A'..'Z', '_':
    while self.peek().isAlphaNumeric() or self.peek() == '_':
      discard self.advance()

    let lexeme = self.source[self.start..<self.current]
    result = case lexeme
    of "struct": make_token(TkStruct)
    of "kind": make_token(TkKind)
    of "impl": make_token(TkImpl)
    of "func": make_token(TkFunc)
    of "some": make_token(TkSome)
    of "var": make_token(TkVar)
    of "let": make_token(TkLet)
    of "end": make_token(TkEnd)
    of "if": make_token(TkIf)
    of "else": make_token(TkElse)
    of "then": make_token(TkThen)
    of "else if": make_token(TkElseIf)
    of "while": make_token(TkWhile)
    of "for": make_token(TkFor)
    of "do": make_token(TkDo)
    else: make_token(TkIdentifier)
  of '=':
    result = make_token(TkEqual)
  of '{':
    result = make_token(TkLBrace)
  of '}':
    result = make_token(TkRBrace)
  of '[':
    result = make_token(TkLBracket)
  of ']':
    result = make_token(TkRBracket)
  of '<':
    result = make_token(TkLAngle)
  of '>':
    result = make_token(TkRAngle)
  of '0'..'9':
    while self.peek().isDigit():
      discard self.advance()

    # if self.peek() == '.' and self.next_peek().isDigit():
    #  discard self.advance()
    #  while self.peek().isDigit():
    #    discard self.advance()

    result = make_token(TkNumber)
  else:
    # handle error
    result = make_token(TkError)

type Parser = object
  lexer: Lexer
  current: Token
  prev: Token
  had_error: bool

type ParseError = object of CatchableError

proc error(self: var Parser, message: string) =
  self.had_error = true
  raise newException(ParseError,
    "Error at line " & ", column " & $self.current.col &
    ": " & message)





proc new_parser(source: string): Parser =
  result = Parser(lexer: new_lexer(source))
  result.current = result.lexer.scan_token()

proc advance(self: var Parser) =
  self.prev = self.current
  while true:
    self.current = self.lexer.scan_token()
    if self.current.kind != TkError: break
      # handle error
      #
proc consume(self: var Parser, kind: TokenKind, message: string) =
  if self.current.kind == kind:
    self.advance()
  else:
    self.error(message)
    self.had_error = true




proc check(self: Parser, kind: TokenKind): bool =
  self.current.kind == kind

proc match(self: var Parser, kind: TokenKind): bool =
  if self.check(kind):
    self.advance()
    return true
  return false

proc parse_expression(self: var Parser): Node =
  # This is a basic implementation,
  # Expand this to handle all expression types for Veld
  if self.match(TkIdentifier):
    result = Node(kind: NkIdentifier, name: self.prev.lexeme)
  elif self.match(TkNumber):
    result = Node(kind: NkLiteral, value: self.prev.lexeme, literal_type: TkNumber)
  elif self.match(TkString):
    result = Node(kind: NkLiteral, value: self.prev.lexeme, literal_type: TkString)
  else:
    # Handle error: expected expression
    self.had_error = true
    return nil

proc parse_type(self: var Parser): Node =
  # Parse a basic type or start a complex type
  var base_type: Node

  if self.match(TkIdentifier):
    base_type = Node(kind: NkIdentifier, name: self.prev.lexeme)
  elif self.match(TkLParen):
    # parse tuple type
    var types: seq[Node] = @[]
    while not self.check(TkRParen):
      types.add(self.parse_type())
      if self.match(TkComma): continue
      else: break
    self.consume(TkRParen, "Expect ')' after tuple type")
    base_type = Node(kind: NkTupleType, types: types)
  else:
    # Handle err: expected type
    self.had_error = true
    return nil

  # Check for generic type parameters
  if self.match(TkLAngle):
    var type_args: seq[Node] = @[]
    while not self.check(TkRAngle):
      type_args.add(self.parse_type())
      if self.match(TkComma): continue
      else: break
    self.consume(TkRAngle, "Expect '>' after generic type parameters")
    result = Node(kind: NkGenericType, base_type: base_type, generic_type_args: type_args)
  # Check for array type
  elif self.match(TkLBracket):
    let size_expr =  self.parse_expression()
    let size = try: parseInt(size_expr.value)
      except: 0 # handle error
    self.consume(TkRBracket, "Expect ']' after array size")
    result = Node(kind: NkArrayType, array_base_type: base_type, element_type: base_type, size: size)
  else:
    result = base_type



    proc parse_method(self: var Parser): Node =
      # parse method definition
      self.consume(TkFunc, "Expect 'func' keyword")
      var method_name_token = self.current
      self.consume(TkIdentifier, "Expect method name")

      # Parse parameter list
      self.consume(TkLParen, "Expect '(' after struct name")
      var params: seq[tuple[name: string, typ: Node]] = @[]

      while not self.check(TkRParen):
        var param_name_token = self.current
        self.consume(TkIdentifier, "Expect parameter name")
        self.consume(TkColon, "Expect ':' after parameter name")
        let param_type = self.parse_type()
        params.add((name: param_name_token.lexeme, typ: param_type))

        if self.match(TkComma): continue
        else: break

      self.consume(TkRParen, "Expect ')' after parameters")

      # Parse return type
      var return_type: Node = nil
      if self.match(TkArrow):
        return_type = self.parse_type()

      # Parse method body
      var body: seq[Node] = @[]
      while not self.check(TkEnd):
        body.add(self.parse_expression())

      self.consume(TkEnd, "Expect 'end' after method body")

      result = Node(kind: NkFunc, func_name: method_name_token.lexeme, params: params, return_type: return_type, body: body)

proc parse_range_or_collection_pattern(self: var Parser): Node =
  let start = self.parse_expression()
  if self.match(TkDot):
    # Range pattern
    self.consume(TkDot, "Expect '..' in range pattern")
    let expr_end = self.parse_expression()
    result = Node(kind: NkPattern, pattern_name: "range", range_start: start, range_end: expr_end)
  else:
    # Collection pattern
    result = Node(kind: NkPattern, pattern_name: "collection", collection: start)


    proc parse_struct(self: var Parser): Node {.used.} =
          # parse struct definition
          self.consume(TkStruct, "Expect 'struct' keyword")
          let name_token = self.current
          self.consume(TkIdentifier, "Expect struct name")

          var generic_params: seq[string] = @[]
          if self.match(TkLBracket):
            while not self.check(TkRBracket):
              let param_token = self.current
              self.consume(TkIdentifier, "Expect generic parameter name")
              generic_params.add(param_token.lexeme)
              if self.match(TkComma): continue
            self.consume(TkRBracket, "Expect ']' after generic parameters")

          var fields: seq[tuple[name: string, typ: Node, mutable: bool]] = @[]

          while not self.check(TkEnd):
            let is_mutable = self.match(TkVar)
            let field_token = self.current
            self.consume(TkIdentifier, "Expect field name")
            self.consume(TkColon, "Expect ':' after field name")
            let field_type = self.parse_type()
            fields.add((name: field_token.lexeme, typ: field_type, mutable: is_mutable))

          self.consume(TkEnd, "Expect 'end' after struct definition")

          result = Node(kind: NkStruct, struct_name: name_token.lexeme, struct_fields: fields, generic_params: generic_params)

          # Remove result.params assignment since result is now properly initialized
          # Remove result.constraints assignment for the same reason

          if self.match(TkLAngle):
            while not self.check(TkRAngle):
              var param_token = self.current
              self.consume(TkIdentifier, "Expect type parameter name")
              if self.check(TkComma): self.advance()
            self.advance() # Consume TkRAngle

            if self.match(TkWhere):
              while true:
                self.consume(TkColon, "Expect : after type parameter")
                if not self.match(TkComma): break

                proc parse_some_object(self: var Parser): Node {.used.} =
                  self.consume(TkSome, "Expect 'some' keyword")
                  self.consume(TkLBrace, "Expect '{' after 'some'")
                  var fields: seq[tuple[name: string, typ: Node, value: Node]] = @[]

                  while not self.check(TkRBrace):
                    var name_token = self.current
                    self.consume(TkIdentifier, "Expect field name")
                    var typ: Node = nil
                    var value: Node = nil

                    if self.match(TkColon):
                      typ = self.parse_type()

                    if self.match(TkEqual):
                      value = self.parse_expression()

                    fields.add((name: name_token.lexeme, typ: typ, value: value))

                    if self.check(TkComma): self.advance()

                  self.consume(TkRBrace, "Expect '}' after object fields")

                  result = Node(kind: NkSomeObj, fields: fields)

                  proc parse_pattern(self: var Parser): Node {.used.} =
                    if self.match(TkLBrace):
                      # Destructuring pattern
                      var fields: seq[string] = @[]
                      while not self.check(TkRBrace):
                        let identToken = self.current
                        self.consume(TkIdentifier, "Expect field name")
                        fields.add(identToken.lexeme)
                        if self.match(TkComma): discard
                      self.consume(TkRBrace, "Expect '}' after destructuring pattern")
                      result = Node(kind: NkPattern, pattern_name: "destructure",
                        pattern_fields: fields,
                        range_start: nil,
                        range_end: nil,
                        collection: nil)
                    elif self.match(TkIdentifier):
                      if self.match(TkIn):
                        # Range or Collection Pattern
                        result = self.parse_range_or_collection_pattern()
                      else:
                        # Simple Identifier Pattern
                        result = Node(kind: NkPattern,
                          pattern_name: self.prev.lexeme,
                          pattern_fields: @[],
                          range_start: nil,
                          range_end: nil,
                          collection: nil)


                        proc parse_impl(self: var Parser): Node {.used.} =
                          self.consume(TkImpl, "Expect implm keyword")
                          var type_params: seq[string] = @[]
                          var constraints: seq[Node] = @[]

                          if self.match(TkLAngle):
                            while not self.check(TkRAngle):
                              let param_token = self.current
                              self.consume(TkIdentifier, "Expect type parameter name")
                              type_params.add(param_token.lexeme)
                              if self.match(TkComma): continue
                              else: break

                            self.consume(TkRAngle, "Expect '>' after type parameters")

                            if self.match(TkWhere):
                              while true:
                                let constraint = self.parse_type()
                                constraints.add(constraint)
                                if not self.match(TkComma): break

                          let trait = self.parse_type()
                          self.consume(TkFor, "Expect 'for' after trait")

                          let target = self.parse_type()

                          var methods: seq[Node] = @[]
                          while not self.check(TkEnd):
                            # For now, let's just parse one method
                            # We'll need to add proper method parsing later
                            let meth = self.parse_type() # Placeholder
                            methods.add(meth)

                          self.consume(TkEnd, "Expect 'end' after impl block")

                          result = Node(kind: NkImpl, trait: trait, target: target, type_params: type_params, where_clause: constraints, mthds: methods)
