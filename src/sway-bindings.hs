{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

import Debug.Trace  ( trace, traceShow )
import Base1
import Prelude  ( error )

-- base --------------------------------

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

----------------------------------------

ç ∷ TokenParsing η ⇒ ℂ → η ℂ
ç = token ∘ char

----------------------------------------

ŧ ∷ TokenParsing η ⇒ 𝕊 → η 𝕊
ŧ = token ∘ string

----------------------------------------

(Ⓣ) ∷ CharParsing η ⇒ α → 𝕊 → η α
(Ⓣ) a s = a <$ string s

----------------------------------------

namedParse ∷ TokenParsing η ⇒ (α → β) → 𝕊 → η α → η β
namedParse f s p = f ⊳ (ŧ s ⋫ p)

--------------------

(⇨) ∷ Parse α ⇒ 𝕊 → (α → β) → Parser β
s ⇨ f = namedParse f s parse

----------------------------------------

nonSpace ∷ CharParsing η ⇒ η 𝕊
nonSpace = many ∘ satisfy $ not ∘ isSpace

--------------------

nonSpace' ∷ TokenParsing η ⇒ η 𝕊
nonSpace' = token nonSpace

------------------------------------------------------------
--                         types                          --
------------------------------------------------------------

data InputType = Keyboard | TouchPad
  deriving Show

instance Parse InputType where
  parse ∷ Parser InputType
  parse = choice [ Keyboard Ⓣ "keyboard",  TouchPad Ⓣ "touchpad" ]

------------------------------------------------------------

data AccelProfile = Adaptive | Flat

instance Parse AccelProfile where
  parse = choice [ Adaptive Ⓣ "adaptive", Flat Ⓣ "flat" ]

------------------------------------------------------------

data ClickMethod = ClickNone | ButtonAreas | ClickFinger

instance Parse ClickMethod where
  parse = choice [ ClickNone   Ⓣ "none"
                 , ButtonAreas Ⓣ "button_areas"
                 , ClickFinger Ⓣ "clickfinger"
                 ]

------------------------------------------------------------

data InputSubCommands = XKBFile      𝕊
                      | IComment     Comment
                      | AccelProfile AccelProfile
                      | DWT          Abled
                      | ClickMethod  ClickMethod

instance Parse InputSubCommands where
  parse =
    choice [ namedParse XKBFile "xkb_file" nonSpace
           , "accel_profile" ⇨ AccelProfile
           , "dwt"           ⇨ DWT
           , "click_method"  ⇨ ClickMethod
           , IComment        ⊳ parse
           ]

----------------------------------------

data InputSpecifier = InputId_ (𝕊,𝕊,𝕊) | InputType_ InputType

instance Parse InputSpecifier where
  parse = let deviceIdentifier ∷ Parser (𝕊,𝕊,𝕊)
              deviceIdentifier = (,,) ⊳ (many digit ⋪ char ':')
                                      ⊵ (many digit ⋪ char ':')
                                      ⊵ many (alphaNum ∤ oneOf "/:_")
          in  (string "type:" ⋫ (InputType_ ⊳ parse)) ∤ (InputId_ ⊳ deviceIdentifier)

----------------------------------------

data InputCommands = InputId (𝕊,𝕊,𝕊) | InputType InputType
  deriving Show

instance Parse InputCommands where
  parse = let deviceIdentifier ∷ Parser (𝕊,𝕊,𝕊)
              deviceIdentifier = (,,) ⊳ (many digit ⋪ char ':')
                                      ⊵ (many digit ⋪ char ':')
                                      ⊵ many (alphaNum ∤ oneOf "/:_")

              input_id = (string "type:" ⋫ (InputType ⊳ parse))
                       ∤ (InputId ⊳ deviceIdentifier)
          in  ŧ "input" ⋫ (token input_id ⋪ braces (many $ token (parse @InputSubCommands)))

------------------------------------------------------------

data Font = Pango 𝕊 | NonPango 𝕊
  deriving Show

instance Parse Font where
  parse = token $ choice [ Pango ⊳ (string "pango:" ⋫ some (noneOf "\n"))
                         , NonPango ⊳ (some $ noneOf "\n") ]

------------------------------------------------------------

data NormalOrInverse = Normal | Inverse
  deriving Show

instance Parse NormalOrInverse where
  parse = choice [ Normal Ⓣ "normal", Inverse Ⓣ "inverse" ]

------------------------------------------------------------

data BindSymOrComment = BSOCBindSym BindSym | BSOCComment Comment
  deriving Show

instance Parse BindSymOrComment where
  parse = choice [ BSOCBindSym ⊳ parse, BSOCComment ⊳ parse ]

------------------------------------------------------------

newtype Comment = Comment' 𝕊
  deriving Show

instance Parse Comment where
  parse = Comment' ⊳ (ç '#' ⋫ many (noneOf "\n"))

------------------------------------------------------------

data Mode = Mode' 𝕊 [ BindSymOrComment ]
  deriving Show

instance Parse Mode where
  parse = Mode' ⊳ (ŧ "mode" ⋫ nonSpace') ⊵ braces (many $ token parse)

------------------------------------------------------------

data SwayBarMode = SwayBarModeDock      | SwayBarModeHide
                 | SwayBarModeInvisible | SwayBarModeOverlay
  deriving Show

instance Parse SwayBarMode where
  parse = choice [ SwayBarModeDock      Ⓣ "dock"
                 , SwayBarModeHide      Ⓣ "hide"
                 , SwayBarModeInvisible Ⓣ "invisible"
                 , SwayBarModeOverlay   Ⓣ "overlay"
                 ]

------------------------------------------------------------

swaymsgPath ∷ 𝕊
swaymsgPath = "/run/current-system/sw/bin/swaymsg"

upto ∷ ℕ → Parser a → Parser [a]
upto n p | n > 0 = (:) ⊳ try p ⊵ (upto (n-1) p ∤ return [])
          | otherwise = return []

upto1 ∷ ℕ → Parser a → Parser [a]
upto1 n p | n > 0 = (:) ⊳ p ⊵ upto (n-1) p
          | otherwise = return []

data Color = Color Word8 Word8 Word8
  deriving Show

instance Parse Color where
  parse = let readHex = read ∘ ("0x" ⊕) ⊳ count 2 hexDigit
          in  Color ⊳ (char '#' ⋫ readHex) ⊵ readHex ⊵ readHex

newtype Identifier = Identifier 𝕊
  deriving Show

instance Parse Identifier where
  parse = Identifier ⊳ some (satisfy (\ c → isAlpha c ∨ c ≡ '_'))

data ColorAssignment = ColorAssignment Identifier Color
  deriving Show

instance Parse ColorAssignment where
  parse = ColorAssignment ⊳ token parse ⊵ token parse

instance Parse α ⇒ Parse [α] where
  parse = many (token parse)

instance (Parse α, Parse β) ⇒ Parse (𝔼 α β) where
  parse = token $ choice [ 𝕷 ⊳ try parse, 𝕽 ⊳ parse ]

data SwayBarCommand = SwayBarStatusCommand ShCommand
                    | SwayBarPosition      TopOrBottom
                    | SwayBarFont          Font
                    | SwayBarComment       Comment
                    | SwayBarMode          SwayBarMode
                    | SwayBarColors        [ 𝔼 ColorAssignment Comment ]
  deriving Show

instance Parse SwayBarCommand where
  parse = token $ choice [ SwayBarStatusCommand ⊳ (ŧ "status_command" ⋫ parse)
                         , SwayBarPosition      ⊳ (ŧ "position" ⋫ parse)
                         , SwayBarFont          ⊳ (ŧ "font" ⋫ parse)
                         , SwayBarComment       ⊳ parse
                         , SwayBarMode          ⊳ (ŧ "mode" ⋫ parse)
                         , SwayBarColors        ⊳ (ŧ "colors" ⋫ braces parse)
                         ]

data SwayBar = SwayBar' [ SwayBarCommand ]
  deriving Show

instance Parse SwayBar where
  parse = SwayBar' ⊳ (ŧ "bar" ⋫ braces (many $ token parse))

data Clause = Comment           Comment
            | InputCommand      InputCommands
            | Font              Font
            | SetVariable       SetVariable
            | ExecAlways        ShCommand
            | Output            𝕊 Output
            | BindSym           BindSym
            | FloatingModifier  𝕊 NormalOrInverse
--            | ModeStart         𝕊
            | Mode              Mode
            | SwayBar           SwayBar
--            | SubSectionStart   𝕊
--            | SubSectionEnd
--            | StatusBarPosition TopOrBottom
  deriving Show

data BindSym = BindSymRegular 𝕊 𝕊 | BindSymExec 𝕊 ([𝕊], 𝕄 𝕊)
  deriving Show

floatingModifier ∷ Parser Clause
floatingModifier =
  FloatingModifier ⊳ (ŧ "floating_modifier" ⋫ nonSpace') ⊵ parse

-- (shell parsing; note that sway just passes the whole line, including apparent
--  comments, to `sh`; thence, (ba)sh does any comment interpretation)

-- a '#', either starting a word or by itself, makes the rest of the line a
-- comment

-- command_comment ∷ Parser 𝕊 (𝕄 𝕊)
-- command_comment = many (noneOf "\n#") -- # in a command is okay, probably

data CommentOrWord = BashComment 𝕊 | BashWord 𝕊
  deriving Show

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

      metachars ∷ [ℂ]
      metachars = "|&;()<> \t\n"

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

      bash_comment ∷ Parser 𝕊
      bash_comment = char '#' ⋫ many (noneOf "#\n")

      words_m_comment ∷ [CommentOrWord] → ([𝕊], 𝕄 𝕊)
      words_m_comment (BashWord w : xs)   = first (w:) (words_m_comment xs)
      words_m_comment [BashComment c]     = ([], 𝕵 c)
      words_m_comment []                  = ([], 𝕹)
      words_m_comment (BashComment c : x) =
        error $ "non-terminating comment '" ⊕ c ⊕ "' (" ⊕ show x ⊕ ")"

      isNonNLSpace c = isSpace c ∧ c ≢ '\n'
      nonNLSpace = satisfy isNonNLSpace
      someNonNLSpace = some nonNLSpace

  in words_m_comment ⊳ sepEndBy (BashComment ⊳ bash_comment ∤ BashWord ⊳ word) someNonNLSpace

{- | Note that sway doesn't do inline comments; however, the exec cmdline is
     passed to 'sh', which does -}
instance Parse BindSym where
  parse = ŧ "bindsym" ⋫ choice [ try $ BindSymExec ⊳ nonSpace' ⊵ token (string "exec") ⋫ restOfLineBash -- many (noneOf "\n")
                 , BindSymRegular ⊳ nonSpace' ⊵ many (noneOf "\n") ]

data SetVariable = SetV 𝕊 𝕊
  deriving Show

instance Parse SetVariable where
  parse = SetV ⊳ token (char '$' ⋫ nonSpace) ⊵ token (many (noneOf "\n"))

data Output = OutputBG 𝕊 𝕊 𝕊
  deriving Show

instance Parse Output where
  parse = choice [ OutputBG Ⓣ "bg" ⊵ nonSpace' ⊵ nonSpace' ⊵ nonSpace'
                 ]

newtype ShCommand = ShCommand ([𝕊], 𝕄 𝕊)
  deriving Show

instance Parse ShCommand where
  parse = ShCommand ⊳ restOfLineBash

data TopOrBottom = Top | Bottom
  deriving Show

instance Parse TopOrBottom where
  parse = choice [ Top Ⓣ "top", Bottom Ⓣ "bottom" ]

data Abled = Enabled | Disabled

instance Parse Abled where
  parse = choice [ Enabled Ⓣ "enabled", Disabled Ⓣ "disabled" ]

clause ∷ Parser Clause
clause =  choice [ Comment          ⊳ parse
                 , InputCommand     ⊳ parse
                 , Font             ⊳ (ŧ "font" ⋫ parse)
                 , SetVariable      ⊳ (ŧ "set" ⋫ parse)
                 , ExecAlways       ⊳ (ŧ "exec_always" ⋫ parse)
                 , Output           ⊳ (ŧ "output" ⋫ token nonSpace) ⊵ parse
                 , BindSym          ⊳ parse
                 , floatingModifier
                 , Mode             ⊳ parse
                 , SwayBar          ⊳ parse
--                 , ModeStart          ⊳ (ŧ "mode" ⋫ nonSpace' ⋪ ç '{')
--                 , SubSectionStart    ⊳ (ŧ "bar" ⋪ ç '{')
--                 , SubSectionEnd © '}'

--                 , StatusBarPosition  ⊳ (ŧ "position" ⋫ parse)
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
