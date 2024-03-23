{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}

import Base1

-- base --------------------------------

import Data.Char       ( chr, isSpace )
import Data.Functor    ( (<$) )
import Data.Maybe      ( catMaybes )
import Data.Monoid     ( mempty )
import System.Process  ( readProcess )
import Text.Read       ( read )

-- parsers -----------------------------

import Text.Parser.Char         ( CharParsing, alphaNum, char, digit, hexDigit
                                , noneOf, notChar, octDigit, oneOf, satisfy
                                , spaces, string )
import Text.Parser.Combinators  ( choice, count, optional, try )
import Text.Parser.Token        ( TokenParsing, braces, token )

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

{- | Parse the rest of the line as a list of of words

-- a word is:
--  a quote-protected group (I'm unsure if \' protects an enclosed '; it is
--    difficult to test that on the comwand)
--  a double-quote protected group (\" protects an enclosed ")
--  a bareword not starting with a '#'
-}
restOfLine ∷ Parser [𝕊]
restOfLine =
  let dquoted_word =
        let dq_chars ∷ Parser 𝕊
            dq_chars = choice [ some (noneOf "\\\"\n")
                              , pure ⊳ (char '\\' ⋫ char '\\')
                              , (:) ⊳ char '\\' ⊵ (pure ⊳ notChar '\n')
                              ]
        in  char '"' ⋫ (ю ⊳ many dq_chars) ⋪ char '"'
      quoted_word  = char '\'' ⋫ many (notChar '\'') ⋪ char '\''
      dollar_quoted_word =
        let o_prefix      = oneOf "0123"
            o_word_3      = (\ a b c → [a,b,c]) ⊳ o_prefix ⊵ octDigit  ⊵ octDigit
            o_word_2_1    = (\ a mb → a : maybe [] pure mb) ⊳ octDigit ⊵ optional octDigit
            o_8bit_string = try o_word_3 ∤ o_word_2_1
            octal_8bit    = chr ∘ read ∘ ("0o" ⊕) ⊳ o_8bit_string

            x_word_2_1    = (\ a mb → a : maybe [] pure mb) ⊳ hexDigit ⊵ optional hexDigit
            x_8bit_string = x_word_2_1
            hex_8bit      = chr ∘ read ∘ ("0x" ⊕) ⊳ x_8bit_string

            chars =
              choice [ some (noneOf "\'\\")
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
                                          , char 'x' ⋫ (pure ⊳ hex_8bit)
                                          ]
                           ]
        in  string "$'" ⋫ (ю ⊳ many chars) ⋪ char '\''
  in  many ∘ token $ choice [ dquoted_word, quoted_word, dollar_quoted_word ]

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
