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

-- | Entry point of the program, hands arguments over to handle
main :: IO ()
main = getArgs >>= handle

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

-- | Operated on the given command-line arguments
handle :: [String] -> IO ()
handle cliargs = case getOpt Permute options cliargs of
  -- No unrecognised areguments
    (optactions, args, []) -> do
        let -- unpack the options structure
            Options { optR = r, optLines = l, optWords = w, optChars = c, optHelp = h, optVersion = v }
                = foldl (\a b -> b a) (startOpts) optactions
            chosenOp = genReversal [c, w, l]
        case v of
            Nothing   -> return ()
            -- the `-v` option was given, display version info and exit
            Just verf -> getProgName >>= putVer >> exitSuccess
                where putVer str = putStr $ verf str
        case h of
            Nothing    -> return ()
            Just helpf -> getProgName >>= putHelp >> exitSuccess
              where
                putHelp str = printf "%s - %s\n %s\n %s"
                                     str
                                     fullname
                                     (header str :: String)
                                     (helpf str)
        when r (interact' (newline . tail . reverse) args >> exitSuccess)
        interact' chosenOp args >> exitSuccess
    (_, _, errors) -> getProgName >>= printError >> exitWith (ExitFailure 1)
      where
        printError str =
            hPutStrLn stderr $ concat errors ++ usageInfo (header str) options

-- *** UTILITIES *** --

-- | Read from file(s) if given, if not use stdin
--   Takes the function (String -> String) to be applied
--   and a reversed list of arguments
interact' :: (String -> String) -> [String] -> IO ()
interact' f [] = interact f
interact' f as =
    (foldM (\s m -> (++ s) <$> m) "" $ map (\a -> readFile a) as)
        >>= (return . f)
        >>= putStr

-- | Generate a reveral function based on a boolean mask
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
