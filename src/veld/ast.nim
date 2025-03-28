type
  NodeKind* = enum
    # Declarations
    NodeStruct
    NodeImpl
    NodeFunc
    NodeLet
    NodeVar

    # Expressions
    NodeIdentifier
    NodeNumber
    NodeString
    NodeCall
    NodeBinary
    NodeUnary

    # types
    NodeTypeRef
    NodeGenericType
    NodeWhereClause
    NodeArrayType

    NodeOptionalType

  Node* = ref object
    case kind*: NodeKind
    of NodeTypeRef:
      type_name*: string
      type_args*: seq[Node]
    of NodeGenericType:
      base_type*: Node
      params*: seq[Node]
    of NodeArrayType:
      element_type*: Node
      dimensions*: int
    of NodeOptionalType:
      base_of_type*: Node
    of NodeStruct:
      struct_name*: string
      generic_params*: seq[string]
      struct_fields*: seq[Node]
    of NodeImpl:
      impl_type*: Node
      impl_methods*: seq[Node]
      impl_for*: Node
      where_clause*: Node
    of NodeFunc:
      func_name*: string
      func_params*: seq[Node]
      func_return_type*: Node
      func_body*: seq[Node]
    of NodeLet, NodeVar:
      name*: string
      type_annotation*: Node
      initializer*: Node
    else:
      discard
