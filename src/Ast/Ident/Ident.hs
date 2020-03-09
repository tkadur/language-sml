module Ast.Ident.Ident
  ( Tagged()
  , Untagged()
  , TaggedView(..)
  , IdentType(..)
  , Identifier(..)
  , tag
  , untag
  , view
  , ident
  , alphanumeric
  , symbolic
  )
where

import           Data.Char                      ( isAlphaNum )
import           Text.Show

-- | Represents common operations over tagged and untagged identifiers
class Identifier a where
  name :: a -> Text
  identType :: a -> IdentType

data IdentType
  = Alphanumeric
  | Symbolic
  deriving (Eq, Generic, Ord, Show)
instance Hashable IdentType

-- | An identifier which is tagged statically with its identifier type.
newtype Tagged (t :: IdentType)
  = Tagged (TaggedView t)
  deriving (Eq, Ord, Hashable, Show)

data TaggedView (t :: IdentType) where
  Alphanum ::Text -> TaggedView 'Alphanumeric
  Symb ::Text -> TaggedView 'Symbolic

instance Identifier (Tagged t) where
  name      = name . untag
  identType = identType . untag

instance Eq (TaggedView t) where
  x1 == x2 = untag (unview x1) == untag (unview x2)

instance Ord (TaggedView t) where
  compare x1 x2 = compare (untag $ unview x1) (untag $ unview x2)

instance Hashable (TaggedView t) where
  hashWithSalt salt = hashWithSalt salt . untag . unview

instance Show (TaggedView t) where
  showsPrec prec = showsPrec prec . untag . unview

-- | An identifier which isn't statically tagged with its identifier type.
--   Despite the lack of a static tag, untagged identifiers can still be
--   queried for their identifier type at runtime using the 'identType'
--   function.
data Untagged = Untagged Text IdentType
  deriving(Eq, Generic, Ord, Show)
instance Hashable Untagged

instance Identifier Untagged where
  name (Untagged x _) = x
  identType (Untagged _ t) = t

tag :: Untagged -> Either (Tagged 'Alphanumeric) (Tagged 'Symbolic)
tag x = case identType x of
  Alphanumeric -> Left . Tagged $ Alphanum (name x)
  Symbolic     -> Right . Tagged $ Symb (name x)

untag :: Tagged t -> Untagged
untag t = case view t of
  Alphanum x -> Untagged x Alphanumeric
  Symb     x -> Untagged x Symbolic

view :: Tagged t -> TaggedView t
view (Tagged v) = v

unview :: TaggedView t -> Tagged t
unview = Tagged

ident :: Text -> Untagged
ident x = Untagged x t
 where
  t :: IdentType
  t = if all (\c -> c == '_' || isAlphaNum c) (toString x)
    then Alphanumeric
    else Symbolic

alphanumeric :: Text -> Tagged 'Alphanumeric
alphanumeric x = case tag $ ident x of
  Left t@(Tagged (Alphanum _)) -> t
  _ -> error "identifier is not alphanumeric"


symbolic :: Text -> Tagged 'Symbolic
symbolic x = case tag $ ident x of
  Right t@(Tagged (Symb _)) -> t
  _ -> error "identifier is not alphanumeric"
