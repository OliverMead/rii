{-
        <<<<<<<<<<<<<<                                                                                                                         <<<<<<<<<<<<<<
       <:::::<<:::::< ________            ____                                 ____            __            ____            __               <:::::<<:::::<
      <:::::<<:::::< /_  __/ /_  ___     / __ \___ _   _____  ______________  /  _/___  ____ _/ /_____  ____/  _/___  ____ _/ /_____  _____  <:::::<<:::::<
     <:::::<<:::::<   / / / __ \/ _ \   / /_/ / _ \ | / / _ \/ ___/ ___ / _ \ / // __ \/ __ `/ __/ __ \/ ___/ // __ \/ __ `/ __/ __ \/ ___/ <:::::<<:::::<
    <:::::<<:::::<   / / / / / /  __/  / _, _/  __/ |/ /  __/ /  (__  )/  __// // / / / /_/ / /_/ /_/ / / _/ // / / / /_/ / /_/ /_/ / /    <:::::<<:::::<
   <:::::<<:::::<   /_/ /_/ /_/\___/  /_/ |_|\___/|___/\___/_/  /____/ \___/___/_/ /_/\__,_/\__/\____/_/ /___/_/ /_/\__,_/\__/\____/_/    <:::::<<:::::<
  <:::::<<:::::<    ---------------  --------------- --------------- ---------------------- ---------------  -----------------------/    <:::::<<:::::<
   <:::::<<:::::<   -:::::::::::::-  -:::::::::::::- -:::::::::::::--:::::::::::::::::::::- -:::::::::::::-  -:::::::::::::- ------/      <:::::<<:::::<
    <:::::<<:::::<  ---------------  --------------- --------------- ---------------------- ---------------  ---------------------/        <:::::<<:::::<
     <:::::<<:::::<                                                                                                                         <:::::<<:::::<
      <:::::<<:::::<                                                                                                                         <:::::<<:::::<
       <:::::<<:::::<                                                                                                                         <:::::<<:::::<
        <<<<<<<<<<<<<<                                                                                                                         <<<<<<<<<<<<<<
-}
{-| Description    : The ReverseInatorInator, for various ways of reversing text
    Copyright      : (C) Oliver Mead, 2020
    License        : GPL-2 or later
    Maintainer     : oliver.j.mead@protonmail.com

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
-}

module Main where

import           System.Console.GetOpt
import           System.Environment
import           System.IO
import           System.Exit
import           Text.Printf
import           Control.Monad

version :: (Int, Int)
version = (0, 1)

fullname :: String
fullname = "The ReverseInatorInator"

-- | Defines the possible operations in the program,
--   for composition using `genReversal` and a boolean mask
operations :: [String -> String]
operations = [reverseChars, reverseWords, reverseLines]

-- | Entry point of the program, hands arguments over to useArgs
main :: IO ()
main = getArgs >>= useArgs

-- | Generate a 'Usage' header with the invocation name,
-- | full name and a short list of possible options
header :: (PrintfType r) => String -> r
header name = printf "Usage: %s - [-%s] [file ...]" name opSummary

-- *** OPERATION DEFINITIONS *** --

-- | Reverse the order of words in the input
reverseWords :: String -> String
reverseWords = unlines . (unwords . reverse . words <$>) . lines

-- | Reverse the order of lines
reverseLines :: String -> String
reverseLines = unlines . reverse . lines

-- | Reverse words' characters
reverseChars :: String -> String
reverseChars = unlines . (unwords . (reverse <$>) . words <$>) . lines

-- *** CLI OPTION HANDLING *** --

data Options = Options { optR :: Bool -- behave like the rev posix utility
                       , optLines :: Bool
                       , optWords :: Bool
                       , optChars :: Bool
                       , optHelp :: Maybe (String -> String)
                       , optVersion :: Maybe (String -> String)
                       }

-- | Define default behaviour when no options are given
startOpts :: Options
startOpts = Options { optR       = True
                    , optLines   = False
                    , optWords   = False
                    , optChars   = False
                    , optHelp    = Nothing
                    , optVersion = Nothing
                    }

-- | All of the single letter options
opSummary :: String
opSummary = foldl1 (++) $ map (\(Option o _ _ _) -> o) options

-- | define the command line options of the program
options :: [OptDescr (Options -> Options)]
options =
  [ Option "l"
           ["lines"]
           (NoArg (\o -> o { optLines = True, optR = False }))
           "reverse the order of the lines in the text"
  , Option "w"
           ["words"]
           (NoArg (\o -> o { optWords = True, optR = False }))
           "reverse the order of words in each line"
  , Option "c"
           ["chars"]
           (NoArg (\o -> o { optChars = True, optR = False }))
           "reverse the order of characters in each word"
  , Option "h"
           ["help"]
           (NoArg (\o -> o { optHelp = Just $ flip usageInfo options }))
           "show this message and exit"
  , Option "v"
           ["version"]
           (NoArg (\o -> o { optVersion = Just versionMsg }))
           "display version information and exit"
  ]

-- | Create the version message from the program name
versionMsg :: (PrintfType r) => String -> r
versionMsg name = printf "%s - The ReverseInatorInator, version %d.%d\n"
                         name
                         (fst version)
                         (snd version)

-- | Operate on the given command-line arguments
useArgs :: [String] -> IO ()
useArgs cliargs = case getOpt Permute options cliargs of
  -- No unrecognised areguments
  (optactions, args, []) ->
    execute (foldl (\a b -> b a) startOpts optactions) args
  (_, _, errors) -> getProgName >>= printError >> exitWith (ExitFailure 1)
   where
    printError str =
      hPutStrLn stderr $ concat errors ++ usageInfo (header str) options

-- | Pattern match the options object to determine how to run
execute :: Options -> [String] -> IO ()
execute (Options { optHelp = Nothing, optVersion = Just f }) _ =
  getProgName >>= (putStr . f) >> exitSuccess
execute (Options { optHelp = Just g, optVersion = Just f }) _ =
  getProgName -- Version and help chosen
    >>= (\s ->
          (putStr $ f s)
            >> (printf "%s\n %s" (header s :: String) (g s))
            >> exitSuccess
        )
execute (Options { optHelp = Just f }) _ =
  getProgName
    >>= (\s -> printf "%s - %s\n %s\n %s" s fullname (header s :: String) (f s))
    >>  exitSuccess
execute (Options { optR = True }) [] =
  -- no args, no mapM_
  interact' (newline . tail . reverse) "" >> exitSuccess
execute (Options { optLines = l, optWords = w, optChars = c }) [] =
  -- no args, no mapM_
  interact' (genReversal [c, w, l]) "" >> exitSuccess
execute (Options { optR = True }) args =
  -- args -> mapM_ over them
  mapM_ (interact' (newline . tail . reverse)) args >> exitSuccess
execute (Options { optLines = l, optWords = w, optChars = c }) args =
  -- args -> mapM_ over them
  mapM_ (interact' (genReversal [c, w, l])) args >> exitSuccess

-- *** UTILITIES *** --

-- | Read from file if given, if not use stdin
--   Takes the function (String -> String) to be applied
interact' :: (String -> String) -> String -> IO ()
interact' f "" = interact f
interact' f s =
  -- (foldM (\s m -> (++ s) <$> m) "" $ map (\a -> readFile a) as)
  return s >>= readFile >>= (return . f) >>= putStr

-- | Generate a reversal function based on a boolean mask
genReversal :: [Bool] -> (String -> String)
genReversal = foldl1 (.) . boolfilter operations

-- | Filter a list of 'a' based on a corresponding list of 'Bool'
boolfilter :: [a] -> [Bool] -> [a]
boolfilter = ((map fst . filter snd) .) . zip

-- | Add a newline (\n) to the end of a string
newline :: String -> String
newline = (++ "\n")

-- -- | Inline if, similar to '() ? _ : _' in C and other languages
-- (?) :: Bool -> a -> a -> a
-- (?) True  = const
-- (?) False = flip const
