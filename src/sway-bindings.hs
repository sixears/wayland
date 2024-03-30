{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}

import Debug.Trace  ( trace, traceShow )
import Base1
import Prelude  ( error )

-- base --------------------------------

import Data.Char         ( chr, isSpace, ord )
import Data.Foldable     ( concat )
import Data.Functor      ( (<$) )
import Data.Maybe        ( catMaybes )
import Data.Monoid       ( mempty )
import GHC.Num           ( subtract )
import System.IO.Unsafe  ( unsafePerformIO )
import System.Process    ( readProcess )
import Text.Read         ( read )

-- hgettext ----------------------------

import Text.I18N.GetText  ( getText )

-- parsers -----------------------------

import Text.Parser.Char         ( CharParsing, alphaNum, char, digit, hexDigit
                                , noneOf, notChar, octDigit, oneOf, satisfy
                                , satisfyRange, spaces, string )
import Text.Parser.Combinators  ( choice, count, optional, sepEndBy, try )
import Text.Parser.Token        ( TokenParsing, braces, someSpace, token )

-- text --------------------------------

import Data.Text     ( pack )
import Data.Text.IO  ( putStrLn )

-- trifecta ----------------------------

import Text.Trifecta.Parser  ( Parser, parseString )
import Text.Trifecta.Result  ( Result( Failure, Success ) )

--------------------------------------------------------------------------------

swaymsgPath ∷ String
swaymsgPath = "/run/current-system/sw/bin/swaymsg"

----------------------------------------

data InputType = Keyboard | TouchPad
  deriving Show

data InputCommands = InputId (String,String,String) | InputType InputType
  deriving Show

data AccelProfile = Adaptive | Flat
data Abled = Enabled | Disabled

ŧ ∷ TokenParsing η ⇒ 𝕊 → η 𝕊
ŧ = token ∘ string

ţ ∷ CharParsing η ⇒ α → 𝕊 → η α
ţ a s = a <$ string s

(Ⓣ) ∷ CharParsing η ⇒ α → 𝕊 → η α
(Ⓣ) a s = a <$ string s

upto ∷ ℕ → Parser a → Parser [a]
upto n p | n > 0 = (:) ⊳ try p ⊵ (upto (n-1) p ∤ return [])
          | otherwise = return []

upto1 ∷ ℕ → Parser a → Parser [a]
upto1 n p | n > 0 = (:) ⊳ p ⊵ upto (n-1) p
          | otherwise = return []

nonSpace ∷ CharParsing η ⇒ η 𝕊
nonSpace = many ∘ satisfy $ not ∘ isSpace

nonSpace' ∷ TokenParsing η ⇒ η 𝕊
nonSpace' = token nonSpace

abled ∷ Parser Abled
abled = choice [ Enabled Ⓣ "enabled", Disabled Ⓣ "disabled" ]

data InputSubCommands = XKBFile String
                      | IComment String
                      | AccelProfile AccelProfile
                      | DWT Abled
                      | ClickMethod ClickMethod

data ClickMethod = ClickNone | ButtonAreas | ClickFinger

clickMethod ∷ Parser ClickMethod
clickMethod = choice [ ClickNone Ⓣ "none"
                     , ButtonAreas Ⓣ "button_areas"
                     , ClickFinger Ⓣ "clickfinger"
                     ]

comment ∷ Parser String
comment = char '#' ⋫ many (noneOf "\n")


inputSubCommands ∷ Parser InputSubCommands
inputSubCommands =
  let accel_profile = choice [ Adaptive <$ string "adaptive"
                             , Flat <$ string "flat" ]
  in  choice [ XKBFile ⊳ (ŧ "xkb_file" ⋫ nonSpace)
             , AccelProfile ⊳ (ŧ "accel_profile" ⋫ accel_profile)
             , DWT ⊳ (ŧ "dwt" ⋫ abled)
             , IComment ⊳ comment
             , ClickMethod ⊳ (ŧ "click_method" ⋫ clickMethod)
             ]

input ∷ Parser InputCommands
input =
  let input_id = (string "type:" ⋫ choice [InputType Keyboard <$ string "keyboard"
                                          ,InputType TouchPad <$ string "touchpad"])
               ∤ (InputId ⊳ deviceIdentifier)
  in  ŧ "input" ⋫ (token input_id ⋪ braces (many $ token inputSubCommands))


deviceIdentifier ∷ Parser (String, String, String)
deviceIdentifier = (,,) ⊳
  (many digit ⋪ char ':') ⊵ (many digit ⋪ char ':') ⊵ many (alphaNum ∤ oneOf "/:_")

inputCommand ∷ Parser InputCommands
inputCommand = input

data Font = Pango 𝕊 | NonPango 𝕊
  deriving Show

font ∷ Parser Font
font = token $ choice [ Pango ⊳ try (string "pango:" ⋫ nonSpace)
                      , NonPango ⊳ nonSpace ]

data NormalOrInverse = Normal | Inverse
  deriving Show

data Clause = Comment      𝕊
            | InputCommand InputCommands
            | Font         Font
            | SetVariable  SetVariable
            | ExecAlways   Command
            | Output       𝕊 Output
            | BindSym      BindSym
            | FloatingModifier 𝕊 NormalOrInverse
  deriving Show

data BindSym = BindSym' 𝕊 𝕊
  deriving Show

normalOrInverse ∷ Parser NormalOrInverse
normalOrInverse = choice [ Normal Ⓣ "normal", Inverse Ⓣ "inverse" ]

floatingModifier ∷ Parser Clause
floatingModifier =
  FloatingModifier ⊳ (ŧ "floating_modifier" ⋫ nonSpace') ⊵ normalOrInverse

-- (shell parsing; note that sway just passes the whole line, including apparent
--  comments, to `sh`; thence, (ba)sh does any comment interpretation)

-- a '#', either starting a word or by itself, makes the rest of the line a
-- comment

-- command_comment ∷ Parser 𝕊 (𝕄 𝕊)
-- command_comment = many (noneOf "\n#") -- # in a command is okay, probably

data CW = C 𝕊 | W 𝕊

{- | Parse the rest of the line as a list of of words, much as bash would -}
restOfLineBash ∷ Parser ([𝕊], 𝕄 𝕊)
restOfLineBash =
  let {- bash definitions (from DEFINITIONS in the man page)

       blank  A space or tab.
       word   A sequence of characters considered as a single unit by the shell.
              Also known as a token.
       name   A word consisting only of alphanumeric characters and underscores,
              and beginning with an alphabetic character or  an  underscore.
              Also referred to as an identifier.
       metacharacter
              A character that, when unquoted, separates words.  One of the
              following: |  & ; ( ) < > space tab newline
       control operator
              A token that performs a control function.  It is one of the
              following symbols: || & && ; ;; ;& ;;& ( ) | |& <newline>

      -}

      blank ∷ Parser ℂ
      blank = oneOf " \t"

      blanks ∷ Parser 𝕊
      blanks = some blank

      metachars ∷ [ℂ]
      metachars = "|&;()<> \t\n"

      metachar ∷ Parser ℂ
      metachar = oneOf metachars

      unquoted_word ∷ Parser 𝕊
      unquoted_word = some (noneOf metachars)

      dq_chars ∷ Parser 𝕊
      dq_chars = choice [ some (noneOf "\\\"\n")
                        , pure ⊳ (char '\\' ⋫ char '\\')
                        , (:) ⊳ char '\\' ⊵ (pure ⊳ notChar '\n')
                        ]
      dquoted_word = char '"' ⋫ (ю ⊳ many dq_chars) ⋪ char '"'
      quoted_word  = char '\'' ⋫ many (notChar '\'') ⋪ char '\''
      dollar_quoted_word =
        let o_word_3      = (:) ⊳ oneOf "0123" ⊵ upto 2 octDigit
            o_8bit_string = try o_word_3 ∤ upto1 2 octDigit
            octal_8bit    = chr ∘ read ∘ ("0o" ⊕) ⊳ o_8bit_string
            read_hex      ∷ 𝕊 → ℂ
            read_hex      = chr ∘ read ∘ ("0x" ⊕)
            hex_8bit      = read_hex ⊳ upto1 2 hexDigit
            hex_16bit     = read_hex ⊳ upto1 4 hexDigit

            c_range a   z =
              let offset_ord = ord a - 1
              in  pure ∘ chr ∘ subtract offset_ord ∘ ord ⊳ satisfyRange a z

            chars =
              let nhex n = pure ∘ read_hex ⊳ upto1 n hexDigit
              in  choice [ some (noneOf "\'\\")
                         , char '\\' ⋫ choice [ char 'a' ⋫ pure "\BEL"
                                              , char 'b' ⋫ pure "\BS"
                                              , char 'e' ⋫ pure "\ESC"
                                              , char 'E' ⋫ pure "\ESC"
                                              , char 'f' ⋫ pure "\FF"
                                              , char 'n' ⋫ pure "\LF"
                                              , char 'r' ⋫ pure "\CR"
                                              , char 't' ⋫ pure "\HT"
                                              , char 'v' ⋫ pure "\VT"
                                              , pure ⊳ oneOf "'?\\\""
                                              , pure ⊳ octal_8bit
                                              , char 'x' ⋫ nhex 2
                                              , char 'u' ⋫ nhex 4
                                              , char 'U' ⋫ nhex 8
                                              , char 'c' ⋫ (c_range 'a' 'z' ∤
                                                            c_range 'A' 'Z')
                                              ]
                           ]
        in  string "$'" ⋫ (ю ⊳ many chars) ⋪ char '\''

      dollar_double_quoted_word =
        string "$\"" ⋫ (unsafePerformIO ∘ getText ⊳ dq_chars) ⋪ char '"'

-- this needs to interpolate quoted things, too
-- and then handle comments
      word ∷ Parser 𝕊
      word = concat ⊳ some (choice [ unquoted_word, dquoted_word, quoted_word
                          , dollar_quoted_word, dollar_double_quoted_word ])

      comment ∷ Parser 𝕊
      comment = char '#' ⋫ many (noneOf "#\n")

      next = C ⊳ comment ∤ W ⊳ word ⋪ spaces

      nn ∷ Parser ([𝕊],𝕄 𝕊)
      nn = do
        x ← next
        case x of
          W w → do
            (ws,c) ← nn
            traceShow ("W",w) $ return (w:ws,c)
          C c → traceShow ("C",c) $ return ([],𝕵 c)

      words_maybe_comment ∷ [CW] → ([𝕊], 𝕄 𝕊)
      words_maybe_comment (W w : xs) = first (w:) (words_maybe_comment xs)
      words_maybe_comment [C c]        = ([], 𝕵 c)
      words_maybe_comment []           = ([], 𝕹)
      words_maybe_comment (C c : xs) =
        error $ "non-terminating comment '" ⊕ c ⊕ "'"

  in -- many ∘ token $ choice [ dquoted_word, quoted_word , dollar_quoted_word, dollar_double_quoted_word]
    -- nn -- sepEndBy word someSpace
    words_maybe_comment ⊳ sepEndBy (C ⊳ comment ∤ W ⊳ word) someSpace
{- | Note that sway doesn't do inline comments; however, the exec cmdline is
     passed to 'sh', which does -}
bindsym ∷ Parser BindSym
bindsym = BindSym' ⊳ nonSpace' ⊵ (many (noneOf "\n"))

data SetVariable = SetV 𝕊 𝕊
  deriving Show

setVariable ∷ Parser SetVariable
setVariable = SetV ⊳ token (char '$' ⋫ nonSpace) ⊵ token (many (noneOf "\n"))

data Output = OutputBG 𝕊 𝕊 𝕊
  deriving Show

output ∷ Parser Output
output = choice [ OutputBG Ⓣ "bg" ⊵ nonSpace' ⊵ nonSpace' ⊵ nonSpace'
                ]

newtype Command = Command 𝕊
  deriving Show

command ∷ Parser Command
command = Command ⊳ many (noneOf "\n")

clause ∷ Parser Clause
clause =  choice [ Comment      ⊳ ((token (char '#') ⋫ many (noneOf "#\n")))
                 , InputCommand ⊳ inputCommand
                 , Font         ⊳ (ŧ "font" ⋫ font)
                 , SetVariable  ⊳ (ŧ "set" ⋫ setVariable)
                 , ExecAlways   ⊳ (ŧ "exec_always" ⋫ command)
                 , Output       ⊳ (ŧ "output" ⋫ token nonSpace) ⊵ output
                 , BindSym      ⊳ (ŧ "bindsym" ⋫ bindsym)
                 , floatingModifier
                 ]

main ∷ IO ()
main = do
  cfg ← readProcess swaymsgPath [ "-t", "get_config", "--pretty" ] ""

  let prsr = spaces ⋫ (many $ token clause)
  let r = parseString prsr mempty cfg -- " # foo\n # bar"
  case r of
    Success s → forM_ s (putStrLn ∘ pack ∘ show)
    Failure e → putStrLn ∘ pack $ show e

-- that's all, folks! ----------------------------------------------------------
