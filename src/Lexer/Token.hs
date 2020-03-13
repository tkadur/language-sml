module Lexer.Token where

import           Data.Scientific                ( Scientific )

data Token
    = Eof
    -- Literals
    | Int Integer
    | Hex Integer
    | Word Integer
    | HexWord Integer
    | Real Scientific
    -- Misc. symbols
    | Dot
    -- Identifiers
    | Alphanumeric Text
    | Symbolic Text
    -- Core reserved words
    | Abstype
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
    -- Module reserved words
    | Eqtype
    | Functor
    | Include
    | Sharing
    | Sig
    | Signature
    | Struct
    | Structure
    | Where
    -- Core reserved symbols
    | Colon
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
    -- Module reserved symbols
    | ColonGt
    deriving (Eq, Ord, Show)
