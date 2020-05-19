module Language.Sml.Lexer.Token where

import           Data.Scientific                ( Scientific )

import           Language.Sml.Ast.Core.Lit.Character
                                                ( Character )
import           Language.Sml.Common.Positive   ( Positive )

data Token
    = Eof
    -- Numeric literals
    | Int Integer
    | Hex Integer
    | Word Positive
    | HexWord Positive
    | Real Scientific
    -- Char/string literals
    | Character [Character]
    | String [Character]
    -- Misc. symbols
    | Dot
    -- Identifiers
    | Alphanumeric Text
    | Symbolic Text
    | Tick -- For type variables
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
