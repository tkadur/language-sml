module Pretty.Internal.Basic
  ( Pretty(..)
  , Doc
  , DocOf
  , PrecAssoc(..)
  , evalDocState
  , getIndent
  , maybeExprParen
  , maybeTypParen
  , getExprPrecAssoc
  , setExprPrecAssoc
  , resetExprPrecAssoc
  , getTypPrecAssoc
  , setTypPrecAssoc
  , resetTypPrecAssoc
  , encloseSep
  , record
  , list
  , tupled
  , punctuate
  , nest
  , hang
  , align
  , grouped
  , flatAlt
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
  , colon
  , space
  , dot
  , equals
  , pipe
  , (<+>)
  )
where

import qualified Data.Text.Prettyprint.Doc     as Doc

import           Ast.Associativity              ( Associativity )
import           Common.Marked                  ( Marked )
import qualified Common.Marked                 as Marked
import           Common.Positive                ( Positive )
import qualified Common.Positive               as Positive
import           Pretty.Comments                ( Comments )
import qualified Pretty.Comments               as Comments

data Config
  = Config
    { comments :: Comments
    , indent :: Int
    , exprPrecAssoc :: Maybe PrecAssoc
    , typPrecAssoc :: Maybe PrecAssoc
    }
  deriving (Show)

-- Information needed to properly parenthesize based on precedence/associativity
data PrecAssoc
  = PrecAssoc
    { precedence :: Int
    , associativity :: Associativity
    , direction :: Associativity
    }
  deriving (Show)

newtype DocState doc = DocState (State Config doc )
  deriving (Functor, Applicative, Monad, MonadState Config)

evalDocState :: Int -> Comments -> Doc ann -> Doc.Doc ann
evalDocState indent comments (DocState docState) = evalState
  docState
  (Config { comments, indent, exprPrecAssoc = Nothing, typPrecAssoc = Nothing })

type Doc ann = DocState (Doc.Doc ann)

type DocOf f ann = DocState (f (Doc.Doc ann))

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

instance (Pretty a, Show a) => Pretty (Marked a) where
  pretty marked = do
    cfg@Config {..} <- get
    let (past, comments') = Comments.split marked comments
    let pastComments      = Comments.toList past
    put (cfg { comments = comments' })

    let value = Marked.value marked
    case pastComments of
      [] -> pretty value
      _ ->
        let pastPretty = sep $ mapM pretty (Comments.toList past)
        in  sep $ sequence [pastPretty, pretty (Marked.value marked)]

instance Pretty Comments.Comment where
  pretty comment =
    cat $ sequence ["(*", pretty $ Comments.unComment comment, "*)"]

getIndent :: DocState Int
getIndent = do
  Config {..} <- get
  return indent

-- | @maybeExprParen prevPrecAssoc doc = doc'@
--   where @doc'@ might be parenthesized based on
--   @prevPrecAssoc@ and the current value of @exprPrecAssoc@.
maybeExprParen :: Maybe PrecAssoc -> Doc ann -> Doc ann
maybeExprParen = maybeParen getExprPrecAssoc

-- | @maybeTypParen prevPrecAssoc doc = doc'@
--   where @doc'@ might be parenthesized based on
--   @prevPrecAssoc@ and the current value of @typPrecAssoc@.
maybeTypParen :: Maybe PrecAssoc -> Doc ann -> Doc ann
maybeTypParen = maybeParen getTypPrecAssoc


maybeParen :: DocState (Maybe PrecAssoc)
           -> Maybe PrecAssoc
           -> Doc ann
           -> Doc ann
maybeParen getPrecAssoc prevPrecAssoc doc = do
  currPrecAssoc <- getPrecAssoc
  case (prevPrecAssoc, currPrecAssoc) of
    (Nothing, _      ) -> doc
    (_      , Nothing) -> error "No precAssoc is currently set"
    (Just prev, Just curr) ->
      case
          ( compare (precedence prev) (precedence curr)
          , associativity prev == associativity curr
          , direction prev == associativity prev
          )
        of
          -- If prev is lower precedence, we don't need parens
          (LT, _, _) -> doc
          -- If prev is higher precedence, we need parens
          (GT, _, _) -> parens doc
          -- If both are the same precedence, both have the same associativity,
          -- and the associativity is the same direction prev went, we don't
          -- need parens
          (EQ, True, True) -> doc
          -- Otherwise, we need parens
          _ -> parens doc

getExprPrecAssoc :: DocState (Maybe PrecAssoc)
getExprPrecAssoc = do
  Config {..} <- get
  return exprPrecAssoc

setExprPrecAssoc :: PrecAssoc -> DocState ()
setExprPrecAssoc precAssoc =
  modify $ \cfg -> cfg { exprPrecAssoc = Just precAssoc }

resetExprPrecAssoc :: DocState ()
resetExprPrecAssoc = modify $ \cfg -> cfg { exprPrecAssoc = Nothing }

getTypPrecAssoc :: DocState (Maybe PrecAssoc)
getTypPrecAssoc = do
  Config {..} <- get
  return typPrecAssoc

setTypPrecAssoc :: PrecAssoc -> DocState ()
setTypPrecAssoc precAssoc =
  modify $ \cfg -> cfg { typPrecAssoc = Just precAssoc }

resetTypPrecAssoc :: DocState ()
resetTypPrecAssoc = modify $ \cfg -> cfg { typPrecAssoc = Nothing }

encloseSep :: Doc ann -> Doc ann -> Doc ann -> DocList ann -> Doc ann
encloseSep l r separator xs = do
  l'         <- l
  r'         <- r
  separator' <- separator
  xs'        <- xs
  return $ Doc.encloseSep l' r' separator' xs'

record :: DocList ann -> Doc ann
record = grouped . align . encloseSep open close separator
 where
  open      = "{ "
  close     = flatAlt "\n}" " }"
  separator = flatAlt ", " ", "

list :: DocList ann -> Doc ann
list = grouped . align . encloseSep open close separator
 where
  open      = flatAlt "[ " "["
  close     = flatAlt "\n]" "]"
  separator = flatAlt "\n, " ", "

tupled :: DocList ann -> Doc ann
tupled = grouped . align . encloseSep open close separator
 where
  open      = flatAlt "( " "("
  close     = flatAlt "\n)" ")"
  separator = flatAlt "\n, " ", "

punctuate :: Doc ann -> DocList ann -> DocList ann
punctuate p xs = do
  p' <- p
  Doc.punctuate p' <$> xs

nest :: Int -> Doc ann -> Doc ann
nest i = adaptFunction (Doc.nest i)

hang :: Int -> Doc ann -> Doc ann
hang i = adaptFunction (Doc.hang i)

align :: Doc ann -> Doc ann
align = adaptFunction Doc.align

parens :: Doc ann -> Doc ann
parens = adaptFunction Doc.parens

brackets :: Doc ann -> Doc ann
brackets = adaptFunction Doc.brackets

braces :: Doc ann -> Doc ann
braces = adaptFunction Doc.braces

grouped :: Doc ann -> Doc ann
grouped = adaptFunction Doc.group

flatAlt :: Doc ann -> Doc ann -> Doc ann
flatAlt x fallback = do
  x'        <- x
  fallback' <- fallback
  return $ Doc.flatAlt x' fallback'

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

adaptConcat :: ([Doc.Doc ann] -> Doc.Doc ann) -> DocList ann -> Doc ann
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

colon :: Doc ann
colon = adapt Doc.colon

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

(<+>) :: Doc ann -> Doc ann -> Doc ann
doc1 <+> doc2 = do
  doc1' <- doc1
  doc2' <- doc2
  return (doc1' Doc.<+> doc2')

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
