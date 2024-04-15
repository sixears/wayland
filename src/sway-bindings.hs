{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

import Debug.Trace  ( trace, traceShow )
import Base1
import Prelude  ( error )

-- base --------------------------------

import Control.Monad     ( foldM_ )
import Data.Char         ( chr, isAlpha, isSpace, ord )
import Data.Foldable     ( concat )
import Data.Functor      ( (<$) )
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
import Text.Parser.Combinators  ( choice, count, sepEndBy, try )
import Text.Parser.Token        ( TokenParsing, braces, token )

-- text --------------------------------

import Data.Text     ( pack )
import Data.Text.IO  ( putStrLn )

-- trifecta ----------------------------

import Text.Trifecta.Parser  ( Parser, parseString )
import Text.Trifecta.Result  ( Result( Failure, Success ) )

--------------------------------------------------------------------------------

class Parse α where
  parse ∷ Parser α
  tparse ∷ Parser α
  tparse = token parse

--------------------

instance Parse α ⇒ Parse [α] where
  parse = many tparse

--------------------

instance (Parse α, Parse β) ⇒ Parse (𝔼 α β) where
  parse = token $ choice [ 𝕷 ⊳ try parse, 𝕽 ⊳ parse ]

------------------------------------------------------------

{- parse between 0 and n instances of a thing -}
upto ∷ ℕ → Parser a → Parser [a]
upto n p | n > 0 = (:) ⊳ try p ⊵ (upto (n-1) p ∤ return [])
          | otherwise = return []

{- parse between 1 and n instances of a thing -}
upto1 ∷ ℕ → Parser a → Parser [a]
upto1 n p | n > 0 = (:) ⊳ p ⊵ upto (n-1) p
          | otherwise = return []

----------------------------------------

{- parse a character, and discard any whitespace after -}
ç ∷ TokenParsing η ⇒ ℂ → η ℂ
ç = token ∘ char

(↝) ∷ CharParsing φ ⇒ ℂ → α → φ α
c ↝ x = char c ⋫ pure x

(↬) ∷ CharParsing φ ⇒ ℂ → φ α → φ α
c ↬ x = char c ⋫ x

----------------------------------------

{- parse a string, and discard any whitespace after -}
ŧ ∷ TokenParsing η ⇒ 𝕊 → η 𝕊
ŧ = token ∘ string

----------------------------------------

þ ∷ Parse α ⇒ 𝕊 → Parser α
þ s = ŧ s ⋫ tparse

----------------------------------------

(⟹) ∷ CharParsing η ⇒ 𝕊 → α → η α
s ⟹ a = a <$ string s

----------------------------------------

namedParse ∷ TokenParsing η ⇒ (α → β) → 𝕊 → η α → η β
namedParse f s p = f ⊳ (ŧ s ⋫ p)

--------------------

(⇨) ∷ Parse α ⇒ 𝕊 → (α → β) → Parser β
s ⇨ f = namedParse f s tparse

----------------------------------------

ƕ ∷ Parse α ⇒ 𝕊 → Parser α
ƕ s = s ⇨ id

----------------------------------------

nonSpace ∷ CharParsing η ⇒ η 𝕊
nonSpace = many ∘ satisfy $ not ∘ isSpace

--------------------

nonSpace' ∷ TokenParsing η ⇒ η 𝕊
nonSpace' = token nonSpace

----------------------------------------

restOfLine ∷ CharParsing η ⇒ η 𝕊
restOfLine = many $ noneOf "\n"

----------------------------------------

restOfLine1 ∷ CharParsing η ⇒ η 𝕊
restOfLine1 = some $ noneOf "\n"

----------------------------------------

comment ∷ Parser 𝕊
comment = ç '#' ⋫ restOfLine

------------------------------------------------------------
--                         types                          --
------------------------------------------------------------

newtype Identifier = Identifier 𝕊
  deriving Show

instance Parse Identifier where
  parse = Identifier ⊳ some (satisfy (\ c → isAlpha c ∨ c ≡ '_'))

------------------------------------------------------------

data InputType = Keyboard | TouchPad
  deriving Show

instance Parse InputType where
  parse ∷ Parser InputType
  parse = choice [ "keyboard" ⟹ Keyboard,  "touchpad" ⟹ TouchPad ]

------------------------------------------------------------

data AccelProfile = Adaptive | Flat
  deriving Show

instance Parse AccelProfile where
  parse = choice [ "adaptive" ⟹ Adaptive, "flat" ⟹ Flat ]

------------------------------------------------------------

data ClickMethod = ButtonAreas | ClickNone | ClickFinger
  deriving Show

instance Parse ClickMethod where
  parse = choice [ "none"         ⟹ ClickNone
                 , "button_areas" ⟹ ButtonAreas
                 , "clickfinger"  ⟹ ClickFinger
                 ]

------------------------------------------------------------

data Abled = Enabled | Disabled
  deriving Show

instance Parse Abled where
  parse = choice [ "enabled" ⟹ Enabled, "disabled" ⟹ Disabled ]

------------------------------------------------------------

data InputSubCommand = XKBFile      𝕊
                     | IComment     Comment
                     | AccelProfile AccelProfile
                     | DWT          Abled
                     | ClickMethod  ClickMethod
  deriving Show

instance Parse InputSubCommand where
  parse =
    choice [ namedParse XKBFile "xkb_file" nonSpace
           , "accel_profile" ⇨ AccelProfile
           , "dwt"           ⇨ DWT
           , "click_method"  ⇨ ClickMethod
           , IComment        ⊳ parse
           ]

----------------------------------------

data InputSpecifier = InputId (𝕊,𝕊,𝕊) | InputType InputType
  deriving Show

instance Parse InputSpecifier where
  parse = let deviceIdentifier ∷ Parser (𝕊,𝕊,𝕊)
              deviceIdentifier = (,,) ⊳ (many digit ⋪ char ':')
                                      ⊵ (many digit ⋪ char ':')
                                      ⊵ many (alphaNum ∤ oneOf "/:_")
          in    (string "type:" ⋫ (InputType ⊳ parse))
              ∤ (InputId ⊳ deviceIdentifier)

----------------------------------------

data InputCommands = InputCommands InputSpecifier [InputSubCommand]
  deriving Show

instance Parse InputCommands where
  parse = InputCommands ⊳ þ "input" ⊵ braces parse

------------------------------------------------------------

data Font = Pango 𝕊 | NonPango 𝕊
  deriving Show

instance Parse Font where
  parse = token $ choice [ Pango ⊳ (string "pango:" ⋫ restOfLine1)
                         , NonPango ⊳ restOfLine1 ]

------------------------------------------------------------

data NormalOrInverse = Normal | Inverse
  deriving Show

instance Parse NormalOrInverse where
  parse = choice [ "normal" ⟹ Normal, "inverse" ⟹ Inverse ]

------------------------------------------------------------

data BindSymOrComment = BSOCBindSym BindSym | BSOCComment Comment
  deriving Show

instance Parse BindSymOrComment where
  parse = choice [ BSOCBindSym ⊳ parse, BSOCComment ⊳ parse ]

------------------------------------------------------------

newtype Comment = Comment' 𝕊
  deriving Show

instance Parse Comment where
  parse = Comment' ⊳ comment

------------------------------------------------------------

data SetVariable = SetV 𝕊 𝕊
  deriving Show

instance Parse SetVariable where
  parse = SetV ⊳ token (char '$' ⋫ nonSpace) ⊵ token restOfLine

------------------------------------------------------------

data Output = OutputBG 𝕊 𝕊 𝕊
  deriving Show

instance Parse Output where
  parse = choice [ "bg" ⟹ OutputBG ⊵ nonSpace' ⊵ nonSpace' ⊵ nonSpace'
                 ]

------------------------------------------------------------

data Mode = Mode' 𝕊 [ BindSymOrComment ]
  deriving Show

instance Parse Mode where
  parse = Mode' ⊳ (ŧ "mode" ⋫ nonSpace') ⊵ braces parse

------------------------------------------------------------

data Color = Color Word8 Word8 Word8
  deriving Show

instance Parse Color where
  parse = let readHex = read ∘ ("0x" ⊕) ⊳ count 2 hexDigit
          in  Color ⊳ (char '#' ⋫ readHex) ⊵ readHex ⊵ readHex

------------------------------------------------------------

data ColorAssignment = ColorAssignment Identifier Color
  deriving Show

instance Parse ColorAssignment where
  parse = ColorAssignment ⊳ tparse ⊵ tparse

------------------------------------------------------------

newtype BashWord = BashWord' 𝕊
  deriving Show

instance Parse BashWord where
  {- | Parse the rest of the line as a list of of words, much as bash would -}
  -- a single bash word, which may consist of (say),
  -- bare-stuff"followed by"$'quoted things'
  parse = BashWord' ⊳
    concat ⊳ some (choice [ unquoted_word, dquoted_word, quoted_word
                          , dollar_quoted_word, dollar_double_quoted_word
                          ])
    where metachars = "|&;()<> \t\n"
          unquoted_word = some (noneOf metachars)
          dq_chars = choice [ some (noneOf "\\\"\n")
                            , pure ⊳ (char '\\' ⋫ char '\\')
                            , (:) ⊳ char '\\' ⊵ (pure ⊳ notChar '\n')
                            ]
          dquoted_word = char '"'  ⋫ (ю ⊳ many dq_chars) ⋪ char '"'
          quoted_word  = char '\'' ⋫ many (notChar '\'') ⋪ char '\''

          dollar_quoted_word =
            let o_word_3      = (:) ⊳ oneOf "0123" ⊵ upto 2 octDigit
                o_8bit_string = try o_word_3 ∤ upto1 2 octDigit
                octal_8bit    = chr ∘ read ∘ ("0o" ⊕) ⊳ o_8bit_string
                read_hex      = chr ∘ read ∘ ("0x" ⊕)

                c_range a   z =
                  let offset_ord = ord a - 1
                  in  pure ∘ chr ∘ subtract offset_ord ∘ ord ⊳ satisfyRange a z

                chars =
                  let nhex n = pure ∘ read_hex ⊳ upto1 n hexDigit
                  in  choice [ some (noneOf "\'\\")
                             , char '\\' ⋫ choice [ 'a' ↝ "\BEL"
                                                  , 'b' ↝ "\BS"
                                                  , 'e' ↝ "\ESC"
                                                  , 'E' ↝ "\ESC"
                                                  , 'f' ↝ "\FF"
                                                  , 'n' ↝ "\LF"
                                                  , 'r' ↝ "\CR"
                                                  , 't' ↝ "\HT"
                                                  , 'v' ↝ "\VT"
                                                  , 'x' ↬ nhex 2
                                                  , 'u' ↬ nhex 4
                                                  , 'U' ↬ nhex 8
                                                  , 'c' ↬ (c_range 'a' 'z' ∤
                                                           c_range 'A' 'Z')
                                                  , pure ⊳ oneOf "'?\\\""
                                                  , pure ⊳ octal_8bit
                                                  ]
                               ]
            in  string "$'" ⋫ (ю ⊳ many chars) ⋪ char '\''

          dollar_double_quoted_word =
            string "$\"" ⋫ (unsafePerformIO ∘ getText ⊳ dq_chars) ⋪ char '"'

------------------------------------------------------------

data BashWordOrComment = BashComment Comment | BashWord BashWord
  deriving Show

instance Parse BashWordOrComment where
  parse = BashComment ⊳ parse ∤ BashWord ⊳ parse

------------------------------------------------------------

-- (shell parsing; note that sway just passes the whole line, including apparent
--  comments, to `sh`; thence, (ba)sh does any comment interpretation)

data BashLine = BashLine [BashWord] (𝕄 Comment)
  deriving Show

instance Parse BashLine where
  parse =
    let words_m_comment ∷ [BashWordOrComment] → BashLine
        words_m_comment (BashWord w : xs)   =
          let BashLine ws c = words_m_comment xs
          in  BashLine (w:ws) c
        words_m_comment [BashComment c]     = BashLine [] (𝕵 c)
        words_m_comment []                  = BashLine [] 𝕹
        words_m_comment (BashComment c : x) =
          error $ "non-terminating comment '" ⊕ show c ⊕ "' (" ⊕ show x ⊕ ")"

        isNonNLSpace c = isSpace c ∧ c ≢ '\n'
        nonNLSpace = satisfy isNonNLSpace
        someNonNLSpace = some nonNLSpace
    in words_m_comment ⊳ sepEndBy parse someNonNLSpace

------------------------------------------------------------

data BindSym = BindSymRegular 𝕊 𝕊 | BindSymExec 𝕊 BashLine
  deriving Show

{- | Note that sway doesn't do inline comments; however, the exec cmdline is
     passed to 'sh', which does -}
instance Parse BindSym where
  parse = ŧ "bindsym" ⋫ choice [ try $ BindSymExec ⊳ nonSpace' ⊵ ƕ "exec"
                               , BindSymRegular ⊳ nonSpace' ⊵ restOfLine ]

------------------------------------------------------------

data SwayBarMode = SwayBarModeDock      | SwayBarModeHide
                 | SwayBarModeInvisible | SwayBarModeOverlay
  deriving Show

instance Parse SwayBarMode where
  parse = choice [ "dock"      ⟹ SwayBarModeDock
                 , "hide"      ⟹ SwayBarModeHide
                 , "invisible" ⟹ SwayBarModeInvisible
                 , "overlay"   ⟹ SwayBarModeOverlay
                 ]

------------------------------------------------------------

data TopOrBottom = Top | Bottom
  deriving Show

instance Parse TopOrBottom where
  parse = choice [ "top" ⟹ Top, "bottom" ⟹ Bottom ]

------------------------------------------------------------

data SwayBarCommand = SwayBarStatusCommand BashLine
                    | SwayBarPosition      TopOrBottom
                    | SwayBarFont          Font
                    | SwayBarComment       Comment
                    | SwayBarMode          SwayBarMode
                    | SwayBarColors        [ 𝔼 ColorAssignment Comment ]
  deriving Show

instance Parse SwayBarCommand where
  parse = token $ choice [ SwayBarStatusCommand ⊳ þ "status_command"
                         , SwayBarPosition      ⊳ þ "position"
                         , SwayBarFont          ⊳ þ "font"
                         , SwayBarMode          ⊳ þ "mode"
                         , SwayBarColors        ⊳ (ŧ "colors" ⋫ braces parse)
                         , SwayBarComment       ⊳ parse
                         ]

------------------------------------------------------------

newtype SwayBar = SwayBar' [ SwayBarCommand ]
  deriving Show

instance Parse SwayBar where
  parse = SwayBar' ⊳ (ŧ "bar" ⋫ braces parse)

------------------------------------------------------------

data Clause = Comment           Comment
            | InputCommand      InputCommands
            | Font              Font
            | SetVariable       SetVariable
            | ExecAlways        BashLine
            | Output            𝕊 Output
            | BindSym           BindSym
            | FloatingModifier  𝕊 NormalOrInverse
            | Mode              Mode
            | SwayBar           SwayBar
  deriving Show

--------------------------------------------------------------------------------

instance Parse Clause where
  parse = choice [ Comment          ⊳ parse
                 , InputCommand     ⊳ parse
                 , Font             ⊳ þ "font"
                 , SetVariable      ⊳ þ "set"
                 , ExecAlways       ⊳ þ "exec_always"
                 , Output           ⊳ (ŧ "output" ⋫ nonSpace') ⊵ parse
                 , BindSym          ⊳ parse
                 , FloatingModifier ⊳ (ŧ "floating_modifier" ⋫ nonSpace') ⊵parse
                 , Mode             ⊳ parse
                 , SwayBar          ⊳ parse
                 ]

------------------------------------------------------------

swaymsgPath ∷ 𝕊
swaymsgPath = "/run/current-system/sw/bin/swaymsg"

----------------------------------------

{- | examine the current clause, along with the prior clause; if the current
     clause is a bindsym, print it.  The prior clause is used as a description
     of the action, if it is a suitably-formatted comment.
-}
maybePrintClause ∷ 𝕄 Clause → Clause → IO (𝕄 Clause)
maybePrintClause _prior c = do
  (case c of
      (BindSym b) → putStrLn ∘ pack $ show b
      (Mode m)    → putStrLn ∘ pack $ show m
      _           → return ())
  return (𝕵 c)

----------------------------------------

main ∷ IO ()
main = do
  cfg ← readProcess swaymsgPath [ "-t", "get_config", "--pretty" ] ""

  let prsr = spaces ⋫ many (token (parse @Clause))
  let r = parseString prsr mempty cfg
  case r of
    Failure e → putStrLn ∘ pack $ show e
    Success s → do
      foldM_ maybePrintClause 𝕹 s

-- that's all, folks! ----------------------------------------------------------
