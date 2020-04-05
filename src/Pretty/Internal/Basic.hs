module Pretty.Internal.Basic
  ( Pretty(..)
  , Doc
  , DocOf
  , PrecAssoc(..)
  , evalDocState
  , getIndent
  , startsWith
  , endsWith
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
  , vsepHard
  , concatWith
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
  , emptyDoc
  )
where

import           Data.Foldable                  ( foldr1 )
import qualified Data.Text                     as Text
import qualified Data.Text.Prettyprint.Doc     as Doc

import           Ast.Associativity              ( Associativity )
import           Common.Marked                  ( Marked )
import qualified Common.Marked                 as Marked
import           Common.Position                ( Position )
import qualified Common.Position               as Position
import           Common.Positive                ( Positive )
import qualified Common.Positive               as Positive
import           Pretty.Comments                ( Comments )
import qualified Pretty.Comments               as Comments

data Config
  = Config
    { comments :: Comments
    , indent :: Int
    , positions :: [(Position, Position)]
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

newtype DocState doc = DocState { unDocState :: State Config doc }
  deriving (Functor, Applicative, Monad, MonadState Config)

evalDocState :: Int -> Comments -> Doc ann -> Doc.Doc ann
evalDocState indent comments docState = evalState
  (unDocState $ flushRemainingComments docState)
  (Config { comments
          , indent
          , positions     = []
          , exprPrecAssoc = Nothing
          , typPrecAssoc  = Nothing
          }
  )
 where
  -- TODO(tkadur): This acts weird if the last pretty-printed thing isn't marked
  -- (which it usually isn't). Should find a way to fix this
  flushRemainingComments :: Doc ann -> Doc ann
  flushRemainingComments doc =
    let remaining = do
          Config { comments = remainingComments } <- get
          pretty remainingComments
    in  sep $ sequence [doc, remaining]

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

instance Monoid (Doc ann) where
  mempty = return mempty

class Pretty a where
  pretty :: a -> Doc ann

instance (Pretty a, Show a) => Pretty (Marked a) where
  pretty marked = do
    (past, pastPretty) <- flushAndReturnCommentsBefore marked
    let lastComment = Comments.last past
    let value       = Marked.value marked

    setCurrentPosition (Marked.startPosition marked, Marked.endPosition marked)
    res <- case lastComment of
      Nothing -> pretty value
      Just comment ->
        -- Try to preserve line breaks between comments and other things
        let separator =
                if Position.line (Marked.endPosition $ Comments.unComment comment)
                     < Position.line (Marked.startPosition marked)
                  then vsep
                  else sep
        in  separator $ sequence [pastPretty, pretty value]
    resetCurrentPosition
    return res

instance Pretty Comments where
  -- Because comments are marked, pretty-printing the last one will
  -- automatically also pretty-print comments before it
  pretty comments = maybe emptyDoc pretty (Comments.last comments)
  -- pretty = sep . mapM pretty . Comments.toList

instance Pretty Comments.Comment where
  pretty comment = do
    -- Because comments are really just @Marked Text@ and we need to pretty-print
    -- comments differently than just plain text, we need to do this manually
    -- instead of deferring to the @Pretty (Marked a)@ instance.
    let markedComment = Comments.unComment comment

    setCurrentPosition
      (Marked.startPosition markedComment, Marked.endPosition markedComment)
    let removeBlankLines = dropWhile (== "")
    let body =
          markedComment
            |> Marked.value
            |> Text.splitOn "\n"
            -- Remove leading blank lines
            |> removeBlankLines
            -- Remove trailing blank lines
            |> reverse
            |> removeBlankLines
            |> reverse
            -- Pretty print each line, forcing newlines between them
            |> mapM pretty
            |> vsepHard
    res <- cat $ sequence ["(*", body, "*)"]
    resetCurrentPosition
    return res

flushAndReturnCommentsBefore :: Marked a -> DocState (Comments, Doc ann)
flushAndReturnCommentsBefore marked = do
  cfg@Config {..} <- get
  let (past, comments') = Comments.split marked comments
  put (cfg { comments = comments' })
  return (past, pretty past)

getIndent :: DocState Int
getIndent = do
  Config {..} <- get
  return indent

startsWith :: (Pretty a, Show a) => a -> DocState (Doc ann)
startsWith start = do
  currPos <- getCurrentPosition
  -- trace "startsWith: " $ traceShow start $ traceShow currPos $ trace "" $ return
  --   ()
  return $ case currPos of
    Nothing -> error "There's no current position"
    Just (startPos, _) -> pretty $ Marked.Marked
      { Marked.value         = start
      , Marked.startPosition = startPos
      , Marked.endPosition   = startPos
      }

endsWith :: (Pretty a, Show a) => a -> DocState (Doc ann)
endsWith end = do
  currPos <- getCurrentPosition
  -- trace "endsWith: " $ traceShow end $ traceShow currPos $ trace "" $ return ()
  return $ case currPos of
    Nothing          -> error "There's no current position"
    Just (_, endPos) -> pretty $ Marked.Marked { Marked.value         = end
                                               , Marked.startPosition = endPos
                                               , Marked.endPosition   = endPos
                                               }

-- | @maybeExprParen prevPrecAssoc doc = doc'@
--   where @doc'@ might be parenthesized based on
--   @prevPrecAssoc@ and the current value of @exprPrecAssoc@.
maybeExprParen :: Maybe PrecAssoc -> Doc ann -> Doc ann
maybeExprParen prevPrecAssoc doc =
  maybeParen getExprPrecAssoc prevPrecAssoc doc << resetExprPrecAssoc

-- | @maybeTypParen prevPrecAssoc doc = doc'@
--   where @doc'@ might be parenthesized based on
--   @prevPrecAssoc@ and the current value of @typPrecAssoc@.
maybeTypParen :: Maybe PrecAssoc -> Doc ann -> Doc ann
maybeTypParen prevPrecAssoc doc =
  maybeParen getTypPrecAssoc prevPrecAssoc doc << resetTypPrecAssoc

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
getExprPrecAssoc = exprPrecAssoc <$> get

setExprPrecAssoc :: PrecAssoc -> DocState ()
setExprPrecAssoc precAssoc =
  modify $ \cfg -> cfg { exprPrecAssoc = Just precAssoc }

resetExprPrecAssoc :: DocState ()
resetExprPrecAssoc = modify $ \cfg -> cfg { exprPrecAssoc = Nothing }

getTypPrecAssoc :: DocState (Maybe PrecAssoc)
getTypPrecAssoc = typPrecAssoc <$> get

setTypPrecAssoc :: PrecAssoc -> DocState ()
setTypPrecAssoc precAssoc =
  modify $ \cfg -> cfg { typPrecAssoc = Just precAssoc }

resetTypPrecAssoc :: DocState ()
resetTypPrecAssoc = modify $ \cfg -> cfg { typPrecAssoc = Nothing }

setCurrentPosition :: (Position, Position) -> DocState ()
setCurrentPosition position =
  modify $ \cfg@Config {..} -> cfg { positions = position : positions }

getCurrentPosition :: DocState (Maybe (Position, Position))
getCurrentPosition = head <<$>> nonEmpty <$> positions <$> get

resetCurrentPosition :: DocState ()
resetCurrentPosition =
  modify $ \cfg@Config {..} -> cfg { positions = drop 1 positions }

encloseSep :: Doc ann -> Doc ann -> Doc ann -> DocList ann -> Doc ann
encloseSep l r separator xs = do
  l'         <- l
  r'         <- r
  separator' <- separator
  xs'        <- xs
  return $ Doc.encloseSep l' r' separator' xs'

record :: DocList ann -> Doc ann
record docs = do
  docs' <- docs
  case docs' of
    [] -> "{}"
    _  -> grouped . align $ encloseSep open close separator docs
 where
  open      = join $ startsWith ("{ " :: Text)
  close     = join (endsWith ("" :: Text)) <> flatAlt "\n}" " }"
  separator = ", "

list :: DocList ann -> Doc ann
list docs = do
  docs' <- docs
  case docs' of
    [] -> "[]"
    _  -> grouped . align $ encloseSep open close separator docs
 where
  open      = join (startsWith ("" :: Text)) <> flatAlt "[ " "["
  close     = join (endsWith ("" :: Text)) <> flatAlt "\n]" "]"
  separator = ", "

tupled :: DocList ann -> Doc ann
tupled docs = do
  docs' <- docs
  case docs' of
    [] -> "()"
    _  -> grouped . align $ encloseSep open close separator docs
 where
  open      = join (startsWith ("" :: Text)) <> flatAlt "( " "("
  close     = join (endsWith ("" :: Text)) <> flatAlt "\n)" ")"
  separator = ", "

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

vsepHard :: DocList ann -> Doc ann
vsepHard = concatWith (\doc1 doc2 -> doc1 <> hardline <> doc2)

concatWith :: (Foldable t, Functor t)
           => (Doc ann -> Doc ann -> Doc ann)
           -> DocOf t ann
           -> Doc ann
concatWith f docs = do
  docs' <- docs
  if null docs' then emptyDoc else foldr1 f (return <$> docs')

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

emptyDoc :: Doc ann
emptyDoc = mempty

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
