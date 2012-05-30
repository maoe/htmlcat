{-# OPTIONS -Wall -O2 #-}
{-# OPTIONS_GHC -fspec-constr-count=20 #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, DeriveDataTypeable #-}
module HtmlCat.Color
    ( parseConsoleString, toHtml, runHtml, convHtml
    , ConsoleState(colorScheme), defaultConsoleState, ColorScheme(..)
    ) where

import Data.Attoparsec.Text (Parser, parseOnly, char, anyChar, string, digit, sepBy, try)
import Control.Applicative ((<|>), (<$), pure, many)
import Control.Monad.State (State, get, put, runState, foldM)
import Data.Data (Data, Typeable)
import Data.Text (Text)
import Data.Tuple (swap)
import qualified Data.Map as M
import qualified Data.Text as T

data DispColor =
      BlackV
    | RedV
    | GreenV
    | YellowV
    | BlueV
    | MagentaV
    | CyanV
    | WhiteV
    | PaletteV Int
    deriving (Show)

data StateChange =
      ResetV
    | BrightV
    | DimV
    | UnderscoreV
    | BlinkV
    | ReverseV
    | HiddenV
    | FgColorV DispColor
    | BgColorV DispColor
    deriving (Show)

data ConsoleString = ConsoleStringV Text | StateChangeV StateChange deriving (Show)

parseConsoleString :: Text -> [ConsoleString]
parseConsoleString = either (error . show) id . parseOnly ansiConsoleToken

ansiConsoleToken :: Parser [ConsoleString]
ansiConsoleToken = do
    cc <- try colorSequence
    nx <- ansiConsoleToken
    return $ cc ++ nx
  <|> do
    c  <- anyChar
    nx <- ansiConsoleToken
    return $ case nx of
        (ConsoleStringV ss):xs -> ConsoleStringV (T.cons c ss):xs
        a -> ConsoleStringV (T.singleton c):a
  <|> return []

colorSequence :: Parser [ConsoleString]
colorSequence = do
    _     <- string "\ESC["
    attrs <- many digit `sepBy` char ';'
    _     <- char 'm'
    return
        $ map StateChangeV
        $ colorCodeConv
        $ map (read . \x -> case x of "" -> "0"; a -> a)
        $ attrs

colorCodeConv :: [Int] -> [StateChange]
colorCodeConv (0:xs)      = ResetV      : colorCodeConv xs
colorCodeConv (1:xs)      = BrightV     : colorCodeConv xs
colorCodeConv (2:xs)      = DimV        : colorCodeConv xs
colorCodeConv (4:xs)      = UnderscoreV : colorCodeConv xs
colorCodeConv (5:xs)      = BlinkV      : colorCodeConv xs
colorCodeConv (7:xs)      = ReverseV    : colorCodeConv xs
colorCodeConv (8:xs)      = HiddenV     : colorCodeConv xs
colorCodeConv (n:xs)
    | 30 <= n && n <= 37  = FgColorV (basicColor $ n - 30) : colorCodeConv xs
    | 40 <= n && n <= 47  = BgColorV (basicColor $ n - 40) : colorCodeConv xs
colorCodeConv (38:5:n:xs) = FgColorV (PaletteV n)          : colorCodeConv xs
colorCodeConv (48:5:n:xs) = BgColorV (PaletteV n)          : colorCodeConv xs
colorCodeConv _           = []

basicColor :: Int -> DispColor
basicColor n = [BlackV, RedV, GreenV, YellowV, BlueV, MagentaV, CyanV, WhiteV] !! (n `mod` 8)

----------------------------------------------------------------

data ConsoleState = ConsoleStateV {
    isBright :: Bool,
    isBlink :: Bool,
    isUnderscore :: Bool,
    fgColor :: DispColor,
    bgColor :: DispColor,
    colorScheme :: ColorScheme
} deriving (Show)

defaultConsoleState :: ConsoleState
defaultConsoleState = ConsoleStateV {
    isBright = False,
    isBlink = False,
    isUnderscore = False,
    fgColor = WhiteV,
    bgColor = BlackV,
    colorScheme = WhiteBackground
}

toHtml :: [ConsoleString] -> Text
toHtml = fst . runHtml defaultConsoleState

convHtml :: ConsoleState -> [ConsoleString] -> (ConsoleState, Text)
convHtml = (swap .) . runHtml

runHtml :: ConsoleState -> [ConsoleString] -> (Text, ConsoleState)
runHtml s cs = foldM stepHtml T.empty cs `runState` s

stepHtml :: Text -> ConsoleString -> State ConsoleState Text
stepHtml r (StateChangeV ResetV)       = (r <$) $ get >>= \s -> put defaultConsoleState { colorScheme = colorScheme s }
stepHtml r (StateChangeV BrightV)      = (r <$) $ get >>= \s -> put s { isBright = True }
stepHtml r (StateChangeV DimV)         = pure r -- Ignore
stepHtml r (StateChangeV UnderscoreV)  = (r <$) $ get >>= \s -> put s { isUnderscore = True }
stepHtml r (StateChangeV BlinkV)       = (r <$) $ get >>= \s -> put s { isBlink = True }
stepHtml r (StateChangeV ReverseV)     = (r <$) $ get >>= \s -> put s { fgColor = bgColor s, bgColor = fgColor s }
stepHtml r (StateChangeV HiddenV)      = pure r -- Ignore
stepHtml r (StateChangeV (FgColorV c)) = (r <$) $ get >>= \s -> put s { fgColor = c }
stepHtml r (StateChangeV (BgColorV c)) = (r <$) $ get >>= \s -> put s { bgColor = c }
stepHtml r (ConsoleStringV ss)         = do
    st <- get
    return $ r +++ "<span style='" +++ style st +++ "'>" +++ esc ss +++ "</span>"

esc :: Text -> Text
esc = T.replace ">" "&gt;"
    . T.replace "<" "&lt;"
    . T.replace "&" "&amp;"

style :: ConsoleState -> Text
style st = T.intercalate "; " $ map (\(k, v) -> k +++ ": " +++ T.intercalate " " v) $ M.assocs
    $ (if isBright     st then M.insertWith (++) ("font-weight")     ["bold"]      else id)
    $ (if isBlink      st then M.insertWith (++) ("text-decoration") ["blink"]     else id)
    $ (if isUnderscore st then M.insertWith (++) ("text-decoration") ["underline"] else id)
    $  M.insert ("color")            [getRGB (colorScheme st) $ fgColor st]
    $  M.insert ("background-color") [getRGB (colorScheme st) $ bgColor st]
    $  M.empty

----------------------------------------------------------------

data ColorScheme = WhiteBackground | BlackBackground deriving (Show, Enum, Eq, Data, Typeable)

getRGB :: ColorScheme -> DispColor -> Text
getRGB WhiteBackground c = case c of
    -- Actually, BlackV is default background color.
    -- We are using white background now, so interpret it as white.
    BlackV      -> "#FFF"
    RedV        -> "#900"
    GreenV      -> "#090"
    YellowV     -> "#990"
    BlueV       -> "#009"
    MagentaV    -> "#909"
    CyanV       -> "#099"
    WhiteV      -> "#000" -- Default foreground color.
    PaletteV  0 -> "#000"
    PaletteV  1 -> "#900"
    PaletteV  2 -> "#090"
    PaletteV  3 -> "#990"
    PaletteV  4 -> "#009"
    PaletteV  5 -> "#909"
    PaletteV  6 -> "#099"
    PaletteV  7 -> "#CCC"
    PaletteV  8 -> "#999"
    PaletteV  9 -> "#C00"
    PaletteV 10 -> "#0C0"
    PaletteV 11 -> "#CC0"
    PaletteV 12 -> "#00C"
    PaletteV 13 -> "#C0C"
    PaletteV 14 -> "#0CC"
    PaletteV 15 -> "#FFF"
    PaletteV n
        | 16       <= n && n < 16 + 216      -> paletteNumToRgb   (n - 16)
        | 16 + 216 <= n && n < 16 + 216 + 24 -> grayscaleNumToRgb (n - 16 - 216)
        | otherwise -> "#FFF"

getRGB BlackBackground c = case c of
    BlackV      -> "#000"
    RedV        -> "#C66"
    GreenV      -> "#6C6"
    YellowV     -> "#CC6"
    BlueV       -> "#66C"
    MagentaV    -> "#C6C"
    CyanV       -> "#6CC"
    WhiteV      -> "#CCC"
    PaletteV  0 -> "#000"
    PaletteV  1 -> "#C66"
    PaletteV  2 -> "#6C6"
    PaletteV  3 -> "#CC6"
    PaletteV  4 -> "#66C"
    PaletteV  5 -> "#C6C"
    PaletteV  6 -> "#6CC"
    PaletteV  7 -> "#CCC"
    PaletteV  8 -> "#666"
    PaletteV  9 -> "#F66"
    PaletteV 10 -> "#6F6"
    PaletteV 11 -> "#FF6"
    PaletteV 12 -> "#66F"
    PaletteV 13 -> "#F6F"
    PaletteV 14 -> "#6FF"
    PaletteV 15 -> "#FFF"
    PaletteV n
        | 16       <= n && n < 16 + 216      -> paletteNumToRgb   (n - 16)
        | 16 + 216 <= n && n < 16 + 216 + 24 -> grayscaleNumToRgb (n - 16 - 216)
        | otherwise -> "#000"

-- See also: http://d.hatena.ne.jp/kakurasan/20080703/p1
paletteNumToRgb :: Int -> Text
paletteNumToRgb n = T.concat ["#", p !! r, p !! g, p !! b] where
    r = n `div` 36 `rem` 6
    g = n `div`  6 `rem` 6
    b = n          `rem` 6
    p = ["00", "5f", "87", "af", "d7", "ff"]

grayscaleNumToRgb :: Int -> Text
grayscaleNumToRgb n = "#" +++ T.replicate 3 (lst !! n) where
    lst =
        [ "08", "12", "1C", "26", "30", "3A"
        , "44", "4E", "58", "62", "6C", "76"
        , "80", "8A", "94", "9E", "A8", "B2"
        , "BC", "C6", "D0", "DA", "E4", "EE"
        ]

----------------------------------------------------------------

(+++) :: Text -> Text -> Text
(+++) = T.append
infixr 5 +++
