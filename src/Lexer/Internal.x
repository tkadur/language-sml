{
module Lexer.Internal where

import           Text.Read                      ( read )

import qualified Common.Marked                 as Marked
import           Common.Marked                  ( Marked )
import qualified Common.Position               as Position
import           Common.Position                ( Position )

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

  -- String literals

  -- Comments
                  "(*"                           { startComment                       }
  <state_comment> "*)"                           { endComment                         }
  <0>             "*)"                           { lexError                           }
  <state_comment> . | \n                         { extendComment                      }

  -- Misc. symbols
  <0> "."                                        { tok Dot                           }

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
  <0> $alpha (@alphanum | $underscore | $prime)* { tokWith toText Alphanum            }
  <0> @symbols                                   { tokWith toText Symbolic            }

  -- Error reporting
  .                                              { lexError                           }
{
type Comment = Text

lex :: Alex ([Marked Comment], [Marked Token])
lex = do
  t <- alexMonadScan
  case Marked.value t of
    Eof -> do
      -- No vowels allowed
      cmnts <- getComments
      return (cmnts, [t])
    _ -> do
      (cmnts, ts) <- lex
      return (cmnts, t:ts)

posn :: AlexInput -> AlexPosn
posn (p, _, _, _) = p

str :: AlexInput -> String
str (_, _, _, s) = s

makePos :: FilePath -> AlexPosn -> Position
makePos file (AlexPn _ line col) =
  Position.Position { Position.file, Position.line, Position.col }

getPosition :: AlexPosn -> Alex Position
getPosition pos = do
  file <- getFile
  return $ makePos file pos

mark :: AlexPosn -> a -> Alex (Marked a)
mark pos value = do
  position <- getPosition pos
  return $ Marked.Marked { Marked.value, Marked.position }

tok :: Token -> AlexAction (Marked Token)
tok tkn input _ = mark (posn input) tkn

tokWith :: (String -> a) -> (a -> Token) -> AlexAction (Marked Token)
tokWith f t input len = mark (posn input) (t . f . take len $ str input)

readNum :: (Num a, Read a) => String -> a
readNum = read . map (\case
  '~' -> '-'
  c -> c
  )

readWord :: String -> Integer
readWord = read . filter (/= 'w')

startComment :: AlexAction (Marked Token)
startComment input len = do
  alexSetStartCode state_comment
  incrCommentDepth
  depth <- getCommentDepth
  case depth of
    -- Switching from code to comment
    1 -> do
      position <- getPosition (posn input)
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
getFile = Alex $ \s@AlexState { alex_ust } -> Right (s, filepath alex_ust)

getComments :: Alex [Marked Comment]
getComments = Alex $ \s@AlexState { alex_ust } -> Right (s, comments alex_ust)

getCommentDepth :: Alex Int
getCommentDepth = Alex $ \s@AlexState { alex_ust } -> Right (s, commentDepth alex_ust)

incrCommentDepth :: Alex ()
incrCommentDepth = Alex $ \s ->
  let s' = updateUst s (\ust -> ust { commentDepth = commentDepth ust + 1 })
  in Right (s', ())

decrCommentDepth :: Alex ()
decrCommentDepth = Alex $ \s ->
  let s' = updateUst s (\ust -> ust { commentDepth = commentDepth ust - 1 })
  in Right (s', ())

-- Begins a new comment
initComment :: Position -> Alex ()
initComment position = Alex $ \s -> Right (initState s, ())
 where
  initState = initPosn . initContents

  initPosn s = updateUst s $ \ust -> case currCommentPosn ust of
    Just _  -> error "comment position is already initialized"
    Nothing -> ust { currCommentPosn = Just position }

  initContents s = updateUst s $ \ust -> case currComment ust of
    Just _  -> error "comment contents already already initialized"
    Nothing ->  ust { currComment = Just "" }

-- Promotes the currently in-progress comment to the main comment storage,
-- clearing out the in-progress spot for a new comment
promoteComment :: Alex ()
promoteComment = Alex $ \s ->
  let
    s' = updateUst s $ \AlexUserState {..} ->
      let
        position = case currCommentPosn of
          Nothing -> error "no in-progress comment posn to promote"
          Just curr -> curr

        value = case currComment of
          Nothing -> error "no in-progress comment contents to promote"
          Just curr -> curr

        comment = Marked.Marked { Marked.value , Marked.position }
      in
        AlexUserState
          { filepath
          , commentDepth
          , comments = comment : comments
          , currCommentPosn = Nothing
          , currComment = Nothing
          }
  in
    Right (s', ())

addToComment :: String -> Alex ()
addToComment new = Alex $ \s ->
  let
    s' = updateUst s $ \ust@AlexUserState { currComment } ->
      case currComment of
        Nothing -> error "no in-progress comment to add ot"
        Just curr -> ust { currComment = Just (curr <> toText new) }
  in
    Right (s', ())

updateUst :: AlexState -> (AlexUserState -> AlexUserState) -> AlexState
updateUst s@AlexState { alex_ust } f = s { alex_ust = f alex_ust }

-- Convenience
lexError :: AlexAction a
lexError input _ = do
  position <- getPosition (posn input)
  alexError $ "unexpected character at " ++ show position

-- Alex Plumbing

data AlexUserState = AlexUserState
  { filepath :: FilePath
  , commentDepth :: Int
  , comments :: [Marked Comment]
  -- Position of current comment
  , currCommentPosn :: Maybe Position
  -- Contents lexed so far of current comment
  , currComment :: Maybe Text
  }

alexEOF :: Alex (Marked Token)
alexEOF = do
  input <- alexGetInput
  let pos = posn input
  code <- alexGetStartCode
  if code == 0 then
    mark pos Eof
  else if code == state_comment then do
    position <- getPosition pos
    alexError $ "unclosed comment starting at " ++ show position
  else
    error $ "unknown start code " <> show code

-- We don't use the built-in @runAlex@ because it doesn't allow us to pass
-- a filename to the initial state. But this variable needs to exist or
-- @runAlex@ won't compile.
-- TODO(tkadur) Stop hacking around Alex wrappers and just manually write
-- in the needed code

alexInitUserState :: AlexUserState
alexInitUserState = error "use alexInitUserState' instead"

alexInitUserState' :: FilePath -> AlexUserState
alexInitUserState' file = AlexUserState
  { filepath = file
  , commentDepth = 0
  , comments = []
  , currCommentPosn = Nothing
  , currComment = Nothing
  }

runAlex' :: FilePath -> String -> Alex a -> Either String a
runAlex' file input (Alex f)
   = case f (AlexState {alex_pos = alexStartPos,
                        alex_inp = input,
                        alex_chr = '\n',
                        alex_bytes = [],
                        alex_ust = alexInitUserState' file,
                        alex_scd = 0}) of Left msg -> Left msg
                                          Right ( _, a ) -> Right a
}