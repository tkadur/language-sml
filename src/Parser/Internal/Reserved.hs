module Parser.Internal.Reserved where

class Reserved a where
  text :: a -> Text

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
    | With
    | Withtype
    | While
    deriving (Show, Enum)

data ReservedOp
    = Lparen
    | Rparen
    | Lbracket
    | Rbracket
    | Lbrace
    | Rbrace
    | Comma
    | Colon
    | Semicolon
    | Dotdotdot
    | Underscore
    | Equal
    | Widearrow
    | Narrowarrow
    | Octothorpe
    deriving (Show, Enum)

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
    With      -> "with"
    Withtype  -> "withtype"
    While     -> "while"

instance Reserved ReservedOp where
  text = \case
    Lparen      -> "("
    Rparen      -> ")"
    Lbracket    -> "["
    Rbracket    -> "]"
    Lbrace      -> "{"
    Rbrace      -> "}"
    Comma       -> ","
    Colon       -> ":"
    Semicolon   -> ";"
    Dotdotdot   -> "..."
    Underscore  -> "_"
    Equal       -> "="
    Widearrow   -> "=>"
    Narrowarrow -> "->"
    Octothorpe  -> "#"

reservedTokens :: [Text]
reservedTokens = reservedWords ++ reservedOps
 where
  reservedWords = text <$> enumFrom (toEnum 0 :: ReservedWord)
  reservedOps   = text <$> enumFrom (toEnum 0 :: ReservedOp)
