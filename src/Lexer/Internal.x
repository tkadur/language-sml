{
module Lexer.Internal where

import qualified Data.List                     as List
import           Text.Printf
import           Text.Read                      ( read )

import qualified Common.Marked                 as Marked
import           Common.Marked                  ( Marked )
import qualified Common.Position               as Position
import           Common.Position                ( Position )

import           Ast.Lit.Character
import           Lexer.Token
}

%wrapper "monadUserState"

$prime = '
$underscore = _
$negation = \~

$digit = 0-9
$hexdigit = [0-9a-fA-F]

$alpha = [a-zA-Z]
$symbol = [ ! \% & \$ \# \+ \- \/ : \< = \> \? @ \\ \~ ` \^ \| \* ]

-- Char/string literal basics

-- SML has a different notion of "printable" than Alex
$char = [\33-\126 \ ] # \\
$control = \64-\95

@intescape = $digit $digit $digit
@hexescape = $hexdigit $hexdigit $hexdigit $hexdigit

@formatting = $white
@ignore = \\ @formatting+ \\

-- Numeric basics

@digits = $digit+
@hexdigits = $hexdigit+
@hexprefix = 0x
@wordprefix = 0w
@hexwordprefix = 0wx

-- More complex numeric stuff

-- Signed/unsigned integers
@uint = @digits
@sint = "~"? @uint

@exp = [eE] @sint

@alphanum = ($alpha | $digit)+
@symbols = $symbol+

@any = . | \n

tokens :-

  -- Whitespace
  <0> $white+                                    ;

  -- Numeric literals
  <0> @sint                                      { tokWith readNum Int                }
  <0> "~"? @hexprefix @hexdigits                 { tokWith readNum Hex                }
  <0> @sint "." @uint @exp?                      { tokWith readNum Real               }
  <0> @sint @exp                                 { tokWith readNum Real               }
  <0> @wordprefix @digits                        { tokWith readWord Word              }
  <0> @hexwordprefix @hexdigits                  { tokWith readWord HexWord           }

  -- Char/string literals
  <0>            \# \"                           { startChar                          }
  <0>            \"                              { startString                        }
  <state_string> \"                              { endString                          }

  <state_string> "\n"                            { extendString Newline               }
  <state_string> "\v"                            { extendString VerticalTab           }
  <state_string> "\f"                            { extendString FormFeed              }
  <state_string> "\r"                            { extendString CarriageReturn        }
  <state_string> \\ \"                           { extendString DoubleQuote           }
  <state_string> \\ \\                           { extendString Backslash             }

  <state_string> "\^" $control                   { extendStringWithControl            }
  <state_string> "\" @intescape                  { extendStringWithIntEscape          }
  <state_string> "\u" @hexescape                 { extendStringWithHexEscape          }
  <state_string> $char                           { extendStringWithChar               }

  <state_string> @ignore                         { extendStringWithIgnore             }

  -- Comments
                  "(*"                           { startComment                       }
  <state_comment> "*)"                           { endComment                         }
  -- We don't want this to get lexed as [Star, Rparen]
  <0>             "*)"                           { genericLexError                    }
  <state_comment> @any                           { extendComment                      }

  -- Misc. symbols
  <0> "."                                        { tok Dot                            }

  -- Core reserved words
  <0> "abstype"                                  { tok Abstype                        }
  <0> "and"                                      { tok And                            }
  <0> "andalso"                                  { tok Andalso                        }
  <0> "as"                                       { tok As                             }
  <0> "case"                                     { tok Case                           }
  <0> "datatype"                                 { tok Datatype                       }
  <0> "do"                                       { tok Do                             }
  <0> "else"                                     { tok Else                           }
  <0> "end"                                      { tok End                            }
  <0> "exception                                 { tok Exception                      }
  <0> "fn"                                       { tok Fn                             }
  <0> "fun"                                      { tok Fun                            }
  <0> "handle"                                   { tok Handle                         }
  <0> "if"                                       { tok If                             }
  <0> "in"                                       { tok In                             }
  <0> "infix"                                    { tok Infix                          }
  <0> "infixr"                                   { tok Infixr                         }
  <0> "let"                                      { tok Let                            }
  <0> "local"                                    { tok Local                          }
  <0> "nonfix"                                   { tok Nonfix                         }
  <0> "of"                                       { tok Of                             }
  <0> "op"                                       { tok Op                             }
  <0> "open"                                     { tok Open                           }
  <0> "orelse"                                   { tok Orelse                         }
  <0> "raise"                                    { tok Raise                          }
  <0> "rec"                                      { tok Rec                            }
  <0> "then"                                     { tok Then                           }
  <0> "type"                                     { tok Type                           }
  <0> "val"                                      { tok Val                            }
  <0> "while"                                    { tok While                          }
  <0> "with"                                     { tok With                           }
  <0> "withtype"                                 { tok Withtype                       }

  -- Module reserved words
  <0> "eqtype"                                   { tok Eqtype                          }
  <0> "functor"                                  { tok Functor                         }
  <0> "include"                                  { tok Include                         }
  <0> "sharing"                                  { tok Sharing                         }
  <0> "sig"                                      { tok Sig                             }
  <0> "signature"                                { tok Signature                       }
  <0> "struct"                                   { tok Struct                          }
  <0> "structure"                                { tok Structure                       }
  <0> "where"                                    { tok Where                           }

  -- Core reserved symbols
  <0> ":"                                        { tok Colon                          }
  <0> ","                                        { tok Comma                          }
  <0> "..."                                      { tok Dotdotdot                      }
  <0> "="                                        { tok Equal                          }
  <0> "{"                                        { tok Lbrace                         }
  <0> "["                                        { tok Lbracket                       }
  <0> "("                                        { tok Lparen                         }
  <0> "->"                                       { tok Narrowarrow                    }
  <0> "#"                                        { tok Octothorpe                     }
  <0> "|"                                        { tok Pipe                           }
  <0> "}"                                        { tok Rbrace                         }
  <0> "]"                                        { tok Rbracket                       }
  <0> ")"                                        { tok Rparen                         }
  <0> ";"                                        { tok Semicolon                      }
  <0> "_"                                        { tok Underscore                     }
  <0> "=>"                                       { tok Widearrow                      }

  -- Module reserved symbols
  <0> ":>"                                       { tok ColonGt                        }

  -- Identifiers
  <0> $alpha (@alphanum | $underscore | $prime)* { tokWith toText Alphanumeric        }
  <0> @symbols                                   { tokWith toText Symbolic            }
  <0> "'"                                        { tok Tick                           }

  -- Error reporting
  @any                                           { genericLexError                    }
{
type Comment = Text

data Mode = StringMode | CharMode

lex :: Alex ([Marked Comment], [Marked Token])
lex = do
  -- No vowels allowed
  tkn <- alexMonadScan
  case Marked.value tkn of
    Eof -> do
      cmnts <- getComments
      return (cmnts, [tkn])
    _ -> do
      (cmnts, tkns) <- lex
      return (cmnts, tkn : tkns)

posn :: AlexInput -> Alex Position
posn (p, _, _, _) = do
  file <- getFile
  return $ makePos file p
 where
  makePos :: FilePath -> AlexPosn -> Position
  makePos file (AlexPn _ line col) =
    Position.Position { Position.file, Position.line = toEnum line, Position.col = toEnum col }

str :: AlexInput -> String
str (_, _, _, s) = s

mark :: AlexInput -> Int -> a -> Alex (Marked a)
mark input len value = do
  startPosition <- posn input
  let endPosition = advance startPosition (take len $ str input)
  return
    $ Marked.Marked { Marked.value, Marked.startPosition, Marked.endPosition }

-- | Given the position at the start of a string, returns
--   the position at the end of that string
advance :: Position -> String -> Position
advance Position.Position {..} s = Position.Position { Position.file
                                                     , Position.line = line'
                                                     , Position.col  = col'
                                                     }
 where
  (line', col') = go line col s

  go ln cl cs = case cs of
    []         -> (ln, cl)
    '\n' : cs' -> go (ln + 1) 0 cs'
    _    : cs' -> go ln (cl + 1) cs'

tok :: Token -> AlexAction (Marked Token)
tok tkn input len = mark input len tkn

tokWith :: (String -> a) -> (a -> Token) -> AlexAction (Marked Token)
tokWith f t input len = mark input len (t . f . take len $ str input)

readNum :: (Num a, Read a) => String -> a
readNum = read . map
  (\case
  -- SML's negation symbol needs to be made normal before
  -- @read@ can understand lexed numbers
    '~' -> '-'
    c   -> c
  )

readWord :: String -> Integer
readWord = read . filter (/= 'w')

startString :: AlexAction (Marked Token)
startString input _ = do
  alexSetStartCode state_string
  position <- posn input
  initString StringMode position
  alexMonadScan

startChar :: AlexAction (Marked Token)
startChar input _ = do
  alexSetStartCode state_string
  position <- posn input
  initString CharMode position
  alexMonadScan

extendString :: Character -> AlexAction (Marked Token)
extendString c _ _ = extendStringWith c

extendStringWith :: Character -> Alex (Marked Token)
extendStringWith c = do
  addToString c
  alexMonadScan

extendStringWithChar :: AlexAction (Marked Token)
extendStringWithChar input _ =
  input
    |> str
    -- Take char
    |> List.head
    |> Char
    |> extendStringWith

extendStringWithControl :: AlexAction (Marked Token)
extendStringWithControl input _ =
  input
    |> str
    -- Drop "\^"
    |> drop 2
    -- Take control char
    |> List.head
    |> Control
    |> extendStringWith

extendStringWithIntEscape :: AlexAction (Marked Token)
extendStringWithIntEscape input _ =
  input
    |> str
    -- Drop "\"
    |> drop 1
    -- Take escape code
    |> take 3
    |> read
    |> IntEscape
    |> extendStringWith

extendStringWithHexEscape :: AlexAction (Marked Token)
extendStringWithHexEscape input _ =
  input
    |> str
    -- Drop "\u"
    |> drop 2
    -- Take escape code
    |> take 4
    |> ("0x" ++)
    |> read
    |> HexEscape
    |> extendStringWith

extendStringWithIgnore :: AlexAction (Marked Token)
extendStringWithIgnore input len =
  input
    |> str
    -- Drop "\"
    |> drop 1
    -- Take text between backlashes
    |> take (len - 2)
    |> Ignore
    |> extendStringWith

endString :: AlexAction (Marked Token)
endString _ _ = do
  alexSetStartCode 0
  string <- getString
  clearString
  return string

startComment :: AlexAction (Marked Token)
startComment input len = do
  alexSetStartCode state_comment
  incrCommentDepth
  depth <- getCommentDepth
  case depth of
    -- Switching from code to comment
    1 -> do
      position <- posn input
      initComment position
    -- Starting nested comment
    _ -> addToComment $ take len (str input)
  alexMonadScan

extendComment :: AlexAction (Marked Token)
extendComment input len = do
  addToComment $ take len (str input)
  alexMonadScan

endComment :: AlexAction (Marked Token)
endComment input len = do
  decrCommentDepth
  depth <- getCommentDepth
  case depth of
    -- Switching from comment to code
    0 -> do
      promoteComment
      alexSetStartCode 0
    -- Ending nested comment
    _ -> addToComment $ take len (str input)
  alexMonadScan

-- State management functions

getFile :: Alex FilePath
getFile = filepath <$> getUst

getComments :: Alex [Marked Comment]
getComments = comments <$> getUst

getCommentDepth :: Alex Int
getCommentDepth = commentDepth <$> getUst

incrCommentDepth :: Alex ()
incrCommentDepth = Alex $ \s ->
  let s' = updateUst s (\ust -> ust { commentDepth = commentDepth ust + 1 })
  in  Right (s', ())

decrCommentDepth :: Alex ()
decrCommentDepth = Alex $ \s ->
  let s' = updateUst s (\ust -> ust { commentDepth = commentDepth ust - 1 })
  in  Right (s', ())

-- Begins a new string/char
initString :: Mode -> Position -> Alex ()
initString mode position = Alex $ \s -> Right (initUst s, ())
 where
  initUst = initPos . initContents . initMode

  initPos s = updateUst s $ \ust -> case currStringPos ust of
    Just _  -> error "string position is already initialized"
    Nothing -> ust { currStringPos = Just position }

  initContents s = updateUst s $ \ust -> case currStringContents ust of
    Just _  -> error "string contents already already initialized"
    Nothing -> ust { currStringContents = Just [] }

  initMode s = updateUst s $ \ust -> case currStringMode ust of
    Just _  -> error "string mode already already initialized"
    Nothing -> ust { currStringMode = Just mode }

addToString :: Character -> Alex ()
addToString new = Alex $ \s ->
  let s' = updateUst s $ \ust@AlexUserState { currStringContents } ->
        case currStringContents of
          Nothing   -> error "no in-progress string to add to"
          Just curr -> ust { currStringContents = Just (new : curr) }
  in  Right (s', ())

-- Gets the currently lexed string
getString :: Alex (Marked Token)
getString = do
  AlexUserState {..} <- getUst

  let mode          = fromMaybe (error "no in-progress string") currStringMode
  let contents      = fromMaybe (error "no in-progress string") currStringContents
  let startPosition = fromMaybe (error "no in-progress string") currStringPos
  let endPosition   = advance startPosition (printf "\"%s\"" $ toString contents)

  value <- case mode of
    StringMode -> return $ String (reverse contents)
    CharMode ->
      -- Chars can have any number of ignores in them
      let notIgnore = \case
            Ignore _ -> False
            _        -> True
      in  case filter notIgnore contents of
            [_] -> return $ Character (reverse contents)
            _   -> lexError startPosition
                            (Just endPosition)
                            "character constant not length 1"

  return
    $ Marked.Marked { Marked.value, Marked.startPosition, Marked.endPosition }

-- Clears out lexed string storage
clearString :: Alex ()
clearString = Alex $ \s -> Right (updateUst s clear, ())
 where
  clear AlexUserState {..} = AlexUserState { filepath
                                           , currStringPos      = Nothing
                                           , currStringContents = Nothing
                                           , currStringMode     = Nothing
                                           , commentDepth
                                           , comments
                                           , currCommentPos
                                           , currCommentContents
                                           }

-- Begins a new comment
initComment :: Position -> Alex ()
initComment position = Alex $ \s -> Right (initUst s, ())
 where
  initUst = initPos . initContents

  initPos s = updateUst s $ \ust -> case currCommentPos ust of
    Just _  -> error "comment position is already initialized"
    Nothing -> ust { currCommentPos = Just position }

  initContents s = updateUst s $ \ust -> case currCommentContents ust of
    Just _  -> error "comment contents already already initialized"
    Nothing -> ust { currCommentContents = Just "" }

addToComment :: String -> Alex ()
addToComment new = Alex $ \s ->
  let s' = updateUst s $ \ust@AlexUserState { currCommentContents } ->
        case currCommentContents of
          Nothing   -> error "no in-progress comment to add ot"
          Just curr -> ust { currCommentContents = Just (curr <> toText new) }
  in  Right (s', ())

-- Promotes the currently in-progress comment to the main comment storage,
-- clearing out the in-progress spot for a new comment
promoteComment :: Alex ()
promoteComment = Alex $ \s ->
  let
    s' = updateUst s $ \AlexUserState {..} ->
      let
        value = fromMaybe
          (error "no in-progress comment contents to promote")
          currCommentContents

        startPosition = fromMaybe
          (error "no in-progress comment position to promote")
          currCommentPos

        -- Need to account for "(*" and "*)"
        endPosition = advance startPosition (printf "(*%s*)" (toString value))

        comment     = Marked.Marked { Marked.value
                                    , Marked.startPosition
                                    , Marked.endPosition
                                    }
      in
        AlexUserState { filepath
                      , currStringPos
                      , currStringContents
                      , currStringMode
                      , commentDepth
                      , comments           = comment : comments
                      , currCommentPos     = Nothing
                      , currCommentContents = Nothing
                      }
  in  Right (s', ())

updateUst :: AlexState -> (AlexUserState -> AlexUserState) -> AlexState
updateUst s@AlexState { alex_ust } f = s { alex_ust = f alex_ust }

getUst :: Alex AlexUserState
getUst = Alex $ \s@AlexState { alex_ust } -> Right (s, alex_ust)

-- | Reports a generic error at the current input position
genericLexError :: AlexAction a
genericLexError input _ = do
  position <- posn input
  lexError position Nothing "unexpected character"

-- | Reports an error at the given position
lexError :: Position -> Maybe Position -> String -> Alex a
lexError startPosition endPos message = case endPos of
  Nothing -> alexError $ printf "%s at %s" message (show @String startPosition)
  Just endPosition -> alexError $ printf "%s at %s-%s"
                                         message
                                         (show @String startPosition)
                                         (show @String endPosition)

-- Alex Plumbing

-- TODO(tkadur) Instead of many separate @Maybe@s for strings/comments
-- unify them into a single @Maybe@. Something something illegal states
-- unrepresentable.
data AlexUserState = AlexUserState
  { filepath :: FilePath

  -- Position of current string
  , currStringPos :: Maybe Position
  -- Contents lexed so far of current string (stored in reverse order)
  , currStringContents :: Maybe [Character]
  , currStringMode :: Maybe Mode

  , commentDepth :: Int
  , comments :: [Marked Comment]
  -- Position of current comment
  , currCommentPos :: Maybe Position
  -- Contents lexed so far of current comment
  , currCommentContents :: Maybe Text
  }

alexEOF :: Alex (Marked Token)
alexEOF = do
  input <- alexGetInput
  code  <- alexGetStartCode
  if code == 0
    then mark input 0 Eof
    else if code == state_comment
      then do
        position <- posn input
        lexError position Nothing "unclosed comment"
      else if code == state_string
        then do
          position <- posn input
          lexError position Nothing "unclosed char or string literal"
        else error $ "unknown start code " <> show code

-- We don't use the built-in @runAlex@ because it doesn't allow us to pass
-- a filename to the initial state. But this variable needs to exist or
-- @runAlex@ won't compile.
-- TODO(tkadur) Stop hacking around Alex wrappers and just manually write
-- in the needed code

alexInitUserState :: AlexUserState
alexInitUserState = error "use alexInitUserState' instead"

alexInitUserState' :: FilePath -> AlexUserState
alexInitUserState' file = AlexUserState { filepath           = file
                                        , currStringPos      = Nothing
                                        , currStringContents = Nothing
                                        , currStringMode     = Nothing
                                        , commentDepth       = 0
                                        , comments           = []
                                        , currCommentPos     = Nothing
                                        , currCommentContents = Nothing
                                        }

runAlex' :: FilePath -> String -> Alex a -> Either String a
runAlex' file input (Alex f) = snd <$> f initState
 where
  initState = AlexState { alex_pos   = alexStartPos
                        , alex_inp   = input
                        , alex_chr   = '\n'
                        , alex_bytes = []
                        , alex_ust   = alexInitUserState' file
                        , alex_scd   = 0
                        }
}