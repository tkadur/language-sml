module Pretty.Internal.Basic
  ( Pretty(..)
  , Config(..)
  , Doc
  , DocOf
  , punctuate
  , nest
  , hang
  , grouped
  , hsep
  , vsep
  , fillSep
  , sep
  , hcat
  , vcat
  , fillCat
  , cat
  , parens
  , brackets
  , braces
  , line
  , line'
  , softline
  , softline'
  , hardline
  , semi
  , colon
  , comma
  , space
  , dot
  , equals
  , pipe
  )
where

import qualified Data.Text.Prettyprint.Doc     as Doc

import           Common.Marked                  ( Marked )
import qualified Common.Marked                 as Marked
import           Common.Positive                ( Positive )
import qualified Common.Positive               as Positive
import           Pretty.Comments                ( Comments )
import qualified Pretty.Comments               as Comments

data Config
  = Config
    { comments :: Comments
    }

type Doc ann = State Config (Doc.Doc ann)

type DocOf f ann = State Config (f (Doc.Doc ann))

type DocList ann = DocOf [] ann

instance IsString (Doc ann) where
  fromString = return . fromString

instance Semigroup (Doc ann) where
  doc1 <> doc2 = do
    d1 <- doc1
    d2 <- doc2
    return (d1 <> d2)

class Pretty a where
  pretty :: a -> Doc ann

instance (Pretty a) => Pretty (Marked a) where
  pretty marked = do
    config@Config {..} <- get
    let (past, comments') = Comments.split marked comments
    let pastComments      = Comments.toList past
    put (config { comments = comments' })

    let value = Marked.value marked
    case pastComments of
      [] -> pretty value
      _ ->
        let pastPretty = sep $ mapM pretty (Comments.toList past)
        in  sep $ sequence [pastPretty, pretty (Marked.value marked)]

instance Pretty Comments.Comment where
  pretty comment =
    cat $ sequence ["(*", pretty $ Comments.unComment comment, "*)"]

punctuate :: Doc ann -> DocList ann -> DocList ann
punctuate p xs = do
  p' <- p
  Doc.punctuate p' <$> xs

nest :: Int -> Doc ann -> Doc ann
nest i = adaptFunction (Doc.nest i)

hang :: Int -> Doc ann -> Doc ann
hang i = adaptFunction (Doc.hang i)

parens :: Doc ann -> Doc ann
parens = adaptFunction Doc.parens

brackets :: Doc ann -> Doc ann
brackets = adaptFunction Doc.brackets

braces :: Doc ann -> Doc ann
braces = adaptFunction Doc.braces

grouped :: Doc ann -> Doc ann
grouped = adaptFunction Doc.group

adaptFunction :: (Doc.Doc ann -> Doc.Doc ann) -> Doc ann -> Doc ann
adaptFunction = fmap

hsep :: DocList ann -> Doc ann
hsep = adaptConcat Doc.hsep

vsep :: DocList ann -> Doc ann
vsep = adaptConcat Doc.vsep

fillSep :: DocList ann -> Doc ann
fillSep = adaptConcat Doc.fillSep

sep :: DocList ann -> Doc ann
sep = adaptConcat Doc.sep

hcat :: DocList ann -> Doc ann
hcat = adaptConcat Doc.hcat

vcat :: DocList ann -> Doc ann
vcat = adaptConcat Doc.vcat

fillCat :: DocList ann -> Doc ann
fillCat = adaptConcat Doc.fillCat

cat :: DocList ann -> Doc ann
cat = adaptConcat Doc.cat

adaptConcat :: ([Doc.Doc ann] -> Doc.Doc ann) -> DocOf [] ann -> Doc ann
adaptConcat f docs = f <$> docs

line :: Doc ann
line = adapt Doc.line

line' :: Doc ann
line' = adapt Doc.line'

softline :: Doc ann
softline = adapt Doc.softline

softline' :: Doc ann
softline' = adapt Doc.softline'

hardline :: Doc ann
hardline = adapt Doc.hardline

semi :: Doc ann
semi = adapt Doc.semi

colon :: Doc ann
colon = adapt Doc.colon

comma :: Doc ann
comma = adapt Doc.comma

space :: Doc ann
space = adapt Doc.space

dot :: Doc ann
dot = adapt Doc.dot

equals :: Doc ann
equals = adapt Doc.equals

pipe :: Doc ann
pipe = adapt Doc.pipe

adapt :: Doc.Doc ann -> Doc ann
adapt = return

instance Pretty Positive where
  pretty = return . Doc.pretty . (Positive.unPositive @Integer)

instance Pretty Bool where
  pretty = return . Doc.pretty

instance Pretty Char where
  pretty = return . Doc.pretty

instance Pretty Double where
  pretty = return . Doc.pretty

instance Pretty Float where
  pretty = return . Doc.pretty

instance Pretty Int where
  pretty = return . Doc.pretty

instance Pretty Integer where
  pretty = return . Doc.pretty

instance Pretty Natural where
  pretty = return . Doc.pretty

instance Pretty () where
  pretty = return . Doc.pretty

instance Pretty Text where
  pretty = return . Doc.pretty

instance Pretty String where
  pretty = return . Doc.pretty
