module Language.Sml.Ast.Core.Lit.Character where

import           Text.Printf

data Character
  = Char Char
  | Newline
  | VerticalTab
  | FormFeed
  | CarriageReturn
  | Control Char
  | IntEscape Int
  | HexEscape Int
  | DoubleQuote
  | Backslash
  | Ignore [Char]
  deriving (Eq, Ord, Show)

instance ToString [Character] where
  toString = foldMap
    (\case
      Char c         -> [c]
      Newline        -> "\\n"
      VerticalTab    -> "\\v"
      FormFeed       -> "\\f"
      CarriageReturn -> "\\r"
      Control   c    -> printf "\\^%c" c
      IntEscape i    -> printf "\\%03d" i
      HexEscape x    -> printf "\\%04x" x
      DoubleQuote    -> "\""
      Backslash      -> "\\\\"
      Ignore s       -> printf "\\%s\\" s
    )

instance ToText [Character] where
  toText = toText . toString
