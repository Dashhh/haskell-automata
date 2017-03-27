import Auto
import Text.Read
import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import System.IO
import System.Exit

type InputText = String
type Size = Int
type State = Int
type Transition = (State,Char,States)
type States = [Int]

main = getFileName >>= getFile >>= clearWhites >>= generateAnswer >>= putStrLn

generateAnswer :: [InputText] -> IO InputText
generateAuto :: (Maybe [States], Maybe [Transition], Maybe InputText) -> IO String
checkAll :: (Maybe [States], Maybe [Transition], Maybe InputText) -> Bool
checkSize :: Maybe InputText -> Maybe Size
checkInit :: Maybe InputText -> Maybe Size -> Maybe States
checkAccepting :: Maybe InputText -> Maybe Size -> Maybe States
checkTransitions :: [InputText] -> Maybe Size -> Maybe [Transition]
checkInputWord :: InputText -> Maybe InputText

transitionLines :: [InputText] -> [InputText]
isState :: InputText -> State -> Maybe State
areLetters :: Either InputText InputText -> Maybe InputText
areStates :: [InputText] -> Size -> Maybe States
transformTransition :: (Maybe State ,Maybe [Char], Maybe States) -> Maybe [Transition]
parseTransition :: InputText -> Size -> Maybe [Transition]
parseTransitions :: [InputText] -> Size -> Maybe [Transition]

parseList :: Maybe InputText -> Maybe Size -> Maybe States
checkList :: InputText -> Maybe [InputText]
numericCheck :: Maybe [InputText] -> Maybe Size -> Maybe States
removeEdges :: InputText -> Maybe InputText
checkCommas :: Maybe InputText -> Maybe InputText
replaceCommas :: Maybe InputText -> Maybe [InputText]
convertToInt :: Maybe [InputText] -> Maybe States

clearWhites :: InputText -> IO [InputText]
getFile :: String -> IO InputText
getFileName :: IO String
parseFileName :: [String] -> IO String

unique :: (Eq a) => [a] -> Bool
maybeNothing :: (a -> Maybe b) -> Maybe a -> Maybe b
takeLine :: [InputText] -> Int -> Maybe InputText
readMaybeInt :: String -> Maybe Int


generateAnswer input = generateAuto (states,transitions,word) where
    size = checkSize $ takeLine input 0
    st = maybeNothing generate size
    generate size = Just [1..size]
    ist = checkInit (takeLine input 1) size
    ast = checkAccepting (takeLine input 2) size
    states = sequence [st,ist,ast]
    transitions = checkTransitions (transitionLines input) size
    word = checkInputWord (last input)

generateAuto xs
    |checkAll xs = makeAuto $ unwrap xs
    |otherwise = return "BAD INPUT" where
        unwrap (st,trans,word) = (fromJust st, fromJust trans, fromJust word)    
        makeAuto (sts,qtrans,word) = 
            return $ show $ accepts (fromLists (sts !! 0) (sts !! 1) (sts !! 2) qtrans) word

checkAll (st,trans,word)
    |isJust st && isJust trans && isJust word = True
    |otherwise = False

checkSize xs = maybeNothing Just firstLine where
    firstLine = maybeNothing readMaybeInt xs

checkInit xs size = maybeNothing Just parsedLine where
    parsedLine = parseList xs size

checkAccepting xs size = maybeNothing Just parsedLine where
    parsedLine = parseList xs size

checkTransitions xs size
    |isJust size = parseTransitions xs $ fromJust size
    |otherwise = Nothing

checkInputWord xs = maybeNothing validate $ inputWord xs where
    validate xs = areLetters $ Right xs
    inputWord xs
        |length worded == 0 = Just ""
        |length worded == 1 = Just $ worded !! 0
        |otherwise = Nothing where
            worded = words xs

-- Transition parsing functions

transitionLines xs = list where
    list = omitLast $ drop 3 xs 
    omitLast [] = []
    omitLast (y:[]) = []
    omitLast (y:ys) = y:omitLast ys

isState x states = maybeNothing check $ readMaybe x where
    check st
        |st > 0 && st <= states = Just st
        |otherwise = Nothing

areLetters xs = maybeNothing check (either unique Just xs) where
    unique xs
        |length xs == length (nub xs) = Just xs
        |otherwise = Nothing
    check xs
        |all validate xs = Just xs
        |otherwise = Nothing
    validate x = isUpper x && isLetter x

areStates xs states = mapM check xs where
    check = flip isState states

transformTransition (st,letters,newStates) = maybeNothing checkLetters st where
    checkLetters x = maybeNothing (checkNewStates x) letters
    checkNewStates x y = maybeNothing (accept x y) newStates
    accept x y z = Just $ makeTransition x y z
    makeTransition x [] z = []
    makeTransition x (y:ys) z = [(x,y,z)] ++ makeTransition x ys z

parseTransition xs states  = transformTransition trans where
    trans = makeTrans $ words xs
    makeTrans (x:y:[]) = (Nothing,Nothing,Nothing)
    makeTrans (x:y:ys) = (isState x states, areLetters $ Left y, areStates ys states)
    makeTrans xs = (Nothing,Nothing,Nothing)

parseTransitions xs states = maybeNothing unwrap $ mapM transition xs where
    transition = flip parseTransition states
    unwrap x = Just $ concat x


-- List parsing functions

parseList xs size = numericCheck list size where
    list = maybeNothing checkList xs

checkList = replaceCommas . checkCommas . removeEdges

numericCheck xs sizeM = maybeNothing validate ist where
    ist = convertToInt xs
    size = maybe (-1) id sizeM
    validate states
        |(all (>0) states) && (all (<=size) states) && unique states = Just states
        |otherwise = Nothing

removeEdges xs
    |validate && edges = Just $ cut ys
    |otherwise = Nothing where
        ys = concat $ words xs
        validate = length (words xs) == 1
        edges = head ys == '[' && last ys == ']'
        cut [] = []
        cut [x] = []
        cut xs = tail (init xs)

checkCommas = maybeNothing check where
    check xs = maybeNothing Just $ validate xs
    validate xs
        |length xs == 0 = Just xs
        |edges xs && inner xs = Just xs
        |otherwise = Nothing
    edges xs = all isDigit [head xs,last xs]
    inner (x:[]) = True
    inner (x:y:ys) = checkComma x y && inner (y:ys)
    checkComma x y
        |isDigit x && isDigit y = True
        |isDigit x && y == ',' = True
        |isDigit y && x == ',' = True
        |otherwise = False

replaceCommas = maybe Nothing replace where
    replace xs = Just $ commaSpace xs
    commaSpace xs = words $ fmap commaToSpace xs
    commaToSpace ',' = ' '
    commaToSpace x = x

convertToInt = maybeNothing toInt where
    toInt = mapM readMaybeInt

-- File parsing functions

clearWhites plain = return filtered where
    filtered = filter (/="") $ lines plain

getFile x = openFile x ReadMode >>= hGetContents

getFileName = getArgs >>= parseFileName

parseFileName x
    |x == [] = die "NO FILENAME"
    |length (head x) == 0 = die "BAD FILENAME"
    |otherwise = return (head x)

-- Utils

unique xs = length xs == (length  $ nub xs)

maybeNothing = maybe Nothing

takeLine xs n
    |length xs > n = Just $ xs !! n
    |otherwise = Nothing where

readMaybeInt = readMaybe
