{-# LANGUAGE AllowAmbiguousTypes #-}

module Parser.Internal.Reserved where

import qualified Relude.Extra.Enum             as Enum

class Reserved a where
  text :: a -> Text

  -- | List of characters which cannot appear immediately
  --   after the reserved symbol
  disallowedFollowingChars :: [Char]

data ReservedWord
    = Abstype
    | And
    | Andalso
    | As
    | Case
    | Datatype
    | Do
    | Else
    | End
    | Exception
    | Fn
    | Fun
    | Handle
    | If
    | In
    | Infix
    | Infixr
    | Let
    | Local
    | Nonfix
    | Of
    | Op
    | Open
    | Orelse
    | Raise
    | Rec
    | Then
    | Type
    | Val
    | While
    | With
    | Withtype
    deriving (Bounded, Enum, Show)

data ReservedOp
    = Colon
    | Comma
    | Dotdotdot
    | Equal
    | Lbrace
    | Lbracket
    | Lparen
    | Narrowarrow
    | Octothorpe
    | Pipe
    | Rbrace
    | Rbracket
    | Rparen
    | Semicolon
    | Underscore
    | Widearrow
    deriving (Bounded, Enum, Show)

instance Reserved ReservedWord where
  text = \case
    Abstype   -> "abstype"
    And       -> "and"
    Andalso   -> "andalso"
    As        -> "as"
    Case      -> "case"
    Datatype  -> "datatype"
    Do        -> "do"
    Else      -> "else"
    End       -> "end"
    Exception -> "exception"
    Fn        -> "fn"
    Fun       -> "fun"
    Handle    -> "handle"
    If        -> "if"
    In        -> "in"
    Infix     -> "infix"
    Infixr    -> "infixr"
    Let       -> "let"
    Local     -> "local"
    Nonfix    -> "nonfix"
    Of        -> "of"
    Op        -> "op"
    Open      -> "open"
    Orelse    -> "orelse"
    Raise     -> "raise"
    Rec       -> "rec"
    Then      -> "then"
    Type      -> "type"
    Val       -> "val"
    While     -> "while"
    With      -> "with"
    Withtype  -> "withtype"

  disallowedFollowingChars =
    concatMap (toString . text) (Enum.universe @ReservedWord)

instance Reserved ReservedOp where
  text = \case
    Colon       -> ":"
    Comma       -> ","
    Dotdotdot   -> "..."
    Equal       -> "="
    Lbrace      -> "{"
    Lbracket    -> "["
    Lparen      -> "("
    Narrowarrow -> "->"
    Octothorpe  -> "#"
    Pipe        -> "|"
    Rbrace      -> "}"
    Rbracket    -> "]"
    Rparen      -> ")"
    Semicolon   -> ";"
    Underscore  -> "_"
    Widearrow   -> "=>"

  disallowedFollowingChars = []

reservedTokens :: [Text]
reservedTokens = reservedWords ++ reservedOps
 where
  reservedWords = map text (Enum.universe @ReservedWord)
  reservedOps   = map text (Enum.universe @ReservedOp)
