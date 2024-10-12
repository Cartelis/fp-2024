{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where


import qualified Data.Char as C
import qualified Data.List as L
import Text.ParserCombinators.ReadP (char)


-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query

-- | The instances are needed basically for tests
instance Eq Query where
  (==) _ _= False

instance Show Query where
  show _ = ""


type Parser a = String -> Either String (a, String)


-- >>> parseChar 'a' "aaa"
-- Right ('a',"aa")
-- >>> parseChar '*' "fdf"
-- Left "* is not found in fdf"
-- >>> parseChar 'a' "asd"
-- Right ('a',"sd")
parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar c s@(h:t) = if c == h then Right (c, t) else Left (c : " is not found in " ++ s)


-- >>> parseLetter "fsdf"
-- Right ('f',"sdf")
parseLetter :: Parser Char
parseLetter [] = Left "Cannot find any letter in an empty input"
parseLetter s@(h:t) = if C.isLetter h then Right (h, t) else Left (s ++ " does not start with a letter")

-- >>> parseWhitespace " asd"
-- Right (' ',"asd")
-- >>> parseWhitespace "kda"
-- Left "kda does not start with a whitespace"
parseWhitespace :: Parser Char
parseWhitespace [] = Left "Cannot find any whitespace in an empty input"
parseWhitespace s@(h:t) = if ' ' == h then Right (' ', t) else Left (s ++ " does not start with a whitespace")

-- >>> parseNumber "65723d"
-- Right (65723,"d")
-- >>> parseNumber "65723 d "
-- Right (65723," d ")
parseNumber :: Parser Integer
parseNumber [] = Left "Cannot parse a number in an empty input"
parseNumber str =
    let
        digits = L.takeWhile C.isDigit str
        rest = drop (length digits) str
    in
        case digits of
            [] -> Left "Not a number"
            _ -> Right (read digits, rest)

-- >>> parseText ""
-- Left "Cannot find any text made out of letters in an empty input"
-- >>> parseText "1automotive142"
-- Left "Not a letter"
-- >>> parseText "Automotive 142"
-- Right ("Automotive"," 142")
parseText :: Parser String
parseText [] = Left "Cannot find any text made out of letters in an empty input"
parseText str =
    let
        letters = L.takeWhile C.isLetter str
        rest = drop (length letters) str
    in
        case letters of
            [] -> Left "Not a letter"
            _ -> Right (letters, rest)


or2 :: Parser a -> Parser a -> Parser a
or2 a b = \input ->
    case a input of
        Right r1 -> Right r1
        Left e1 ->
            case b input of
                Right r2 -> Right r2
                Left e2 -> Left (e1 ++ ", " ++ e2)

-- >>> parseTextNum "12345abcd"
-- Right (Right 12345,"abcd")
-- >>> parseTextNum "abcDEF56789"
-- Right (Left "abcDEF","56789")
-- >>> parseTextNum "M850i"
-- Right (Left "M","850i")
-- >>> parseTextNum "190kW"
-- Right (Right 190,"kW")
parseTextNum :: Parser (Either String Integer)
parseTextNum input =
    case parseText input of
        Right (txt, rest) -> Right (Left txt, rest) -- Text parsing succeeded
        Left e1 ->
            case parseNumber input of
                Right (num, rest) -> Right (Right num, rest) -- Number parsing succeeded
                Left e2 -> Left (e1 ++ "; " ++ e2) -- Both parsers failed

-- >>> parseTextNumWhitespace " 234"
-- Right (Left " ","234")
-- >>> parseTextNumWhitespace "abcd "
-- Right (Left "abcd"," ")
-- >>> parseTextNumWhitespace "avcd 234"
-- Right (Left "avcd"," 234")
-- >>> parseTextNumWhitespace "-"
-- Left "Not a letter; Not a number; - does not start with a whitespace"
-- >>> parseTextNumWhitespace "390kW "
-- Right (Right 390,"kW ")
-- >>> parseTextNumWhitespace "kW "
-- Right (Left "kW"," ")
parseTextNumWhitespace :: Parser (Either String Integer)
parseTextNumWhitespace input =
    case parseTextNum input of
        Right (Left txt, rest) -> Right (Left txt, rest)
        Right (Right num, rest) -> Right (Right num, rest)
        Left e1 ->
            case parseWhitespace input of
                Right (whitespace, rest) -> Right(Left [whitespace], rest)
                Left e2 -> Left (e1 ++ "; " ++ e2)



-- >>> parseAlphaNum "ju25ss"
-- Right ("ju25ss","")
-- >>> parseAlphaNum " hus2"
-- Left "Not a letter or a digit"
-- >>> parseAlphaNum "ju25s do"
-- Right ("ju25s"," do")
-- >>> parseAlphaNum "8V"
-- Right ("8V","")
-- >>> parseAlphaNum "W12"
-- Right ("W12","")
parseAlphaNum :: Parser String
parseAlphaNum [] = Left "Cannot find any text made out of letters or digits in an empty input"
parseAlphaNum str =
    let
        lettersDigits = L.takeWhile (\c -> C.isLetter c || C.isDigit c) str
        rest = drop (length lettersDigits) str
    in
        case lettersDigits of
            [] -> Left "Not a letter or a digit"
            _ -> Right (lettersDigits, rest)

-- >>> isDot ''
-- Parser error on `''`
-- Character literals may not be empty
isDot :: Char -> Bool
isDot c = c == '.'


-- >>> parseDouble "4.23"
-- Right (4.23,"")
-- >>> parseDouble "0.69"
-- Right (0.69,"")
-- >>> parseDouble "a.23"
-- Left "Not a digit or dot"
-- >>> parseDouble ""
-- Left "Cannot parse a double number in an empty input"
-- >>> parseDouble ".528"
-- Prelude.read: no parse
parseDouble :: Parser Double
parseDouble [] = Left "Cannot parse a double number in an empty input"
parseDouble str =
    let
        doubleNum = L.takeWhile (\c -> C.isDigit c || isDot c) str
        rest = drop (length doubleNum) str
    in
        case doubleNum of
            [] -> Left "Not a digit or dot"
            _ -> Right(read doubleNum, rest)

-- >>> parseNumTextWhitespaceOrdered "190kW 750Nm"
-- Right ((190,"kW",' '),"750Nm")
parseNumTextWhitespaceOrdered :: Parser (Integer, String, Char)
parseNumTextWhitespaceOrdered [] = Left "Empty input"
parseNumTextWhitespaceOrdered input = do
    (power, rest1) <- parseNumber input
    (kWText, rest2) <- parseText rest1
    (ws, rest3) <- parseWhitespace rest2
    return ((power, kWText, ws), rest3)

-- >>> parseTextWhitespaceOrdered "BMW M850i "
-- Right (("BMW",' '),"M850i ")
-- >>> parseTextWhitespaceOrdered "Black Coupe "
-- Right (("Black",' '),"Coupe ")
-- >>> parseTextWhitespaceOrdered "85 Coupe "
-- Left "Not a letter"
-- >>> parseTextWhitespaceOrdered "Car BMW"
-- Right (("Car",' '),"BMW")
parseTextWhitespaceOrdered :: Parser (String, Char)
parseTextWhitespaceOrdered [] = Left "Empty input"
parseTextWhitespaceOrdered input = do
    (text, rest1) <- parseText input
    (ws, rest2) <- parseWhitespace rest1
    return ((text, ws), rest2)


-- >>> parseDisplacement "4.4 L V8 "
-- Right ((4.4," L "),"V8 ")
-- >>> parseDisplacement "8.9 L V12 500kW"
-- Right ((8.9," L "),"V12 500kW")
-- >>> parseDisplacement "a.4 L V8 "
-- Left "Not a digit or dot"
-- >>> parseDisplacement ""
-- Left "Empty displacement input"
parseDisplacement :: Parser (Double, String)
parseDisplacement [] = Left "Empty displacement input"
parseDisplacement input = do
    (displacement, rest1) <- parseDouble input
    (ws1, rest2) <- parseWhitespace rest1
    (charL, rest3) <- parseChar 'L' rest2
    (ws2, rest4) <- parseWhitespace rest3
    return ((displacement, concat [[ws1], [charL], [ws2]]), rest4)

-- >>> parseFuelMeasurement "l/100km "
-- Right ("l/100km"," ")
parseFuelMeasurement :: Parser String
parseFuelMeasurement [] = Left "Empty fuel measurement input"
parseFuelMeasurement input =
    let
        fuelMeasurement = L.takeWhile (\c -> C.isLetter c || C.isDigit c || c == '/') input
        rest = drop (length fuelMeasurement) input
    in
        case fuelMeasurement of
            [] -> Left "Not a letter, a digit or /"
            _ -> Right (fuelMeasurement, rest)


-- >>> parseFuelConsumption "71.2kWh/100km "
-- Right ((71.2,"kWh/100km"),"")
parseFuelConsumption :: Parser (Double, String)
parseFuelConsumption [] = Left "Empty fuel consumption input"
parseFuelConsumption input = do
    (amount, rest1) <- parseDouble input
    (measurement, rest2) <- parseFuelMeasurement rest1
    (_, rest3) <- parseWhitespace rest2
    return ((amount, measurement), rest3) <|> ((0, ""), input)



-- type Parser a = String -> Either String (a, String)
-- >>> parseCar1 "Car BMW M850i Black Coupe 4.4 L V8 390kW 750Nm Turbocharged Petrol AWD Automatic 18153km "
-- Right [Left "Car",Left "BMW",Left "M850i",Left "Black",Left "Coupe",Right (Right 4.4),Left "V8",Right (Left 390),Right (Left 750),Left "Turbocharged",Left "Petrol",Left "AWD",Left "Automatic",Right (Left 18153)]
-- >>> parseCar1 "Car E 3zx Orange Crossover 1kW 4Nm Electric 4WD Manual 71.2kWh/100km 8km "
-- Left "kW 4Nm Electric 4WD Manual 71.2kWh/100km 8km  does not start with a whitespace"
parseCar1 :: String -> Either String [Either String (Either Integer Double)]
parseCar1 [] = Left "Empty car input"
parseCar1 input = do
    -- (carString, rest1) <- parseText input
    -- (ws1, rest2) <- parseWhitespace rest1
    ((carString, ws1), rest1) <- parseTextWhitespaceOrdered input
    -- (carName, rest3) <- parseText rest2
    -- (ws2, rest4) <- parseWhitespace rest3
    ((carName, ws2), rest2) <- parseTextWhitespaceOrdered rest1
    (modelName, rest3) <- parseAlphaNum rest2
    (ws3, rest4) <- parseWhitespace rest3
    -- (carColor, rest7) <- parseText rest6
    -- (ws4, rest8) <- parseWhitespace rest7
    ((carColor, ws4), rest5) <- parseTextWhitespaceOrdered rest4
    -- (carBodyType, rest9) <- parseText rest8
    -- (ws5, rest10) <- parseWhitespace rest9
    ((carBodyType, ws5), rest6) <- parseTextWhitespaceOrdered rest5
    -- (carDisplacement, rest7) <- parseDouble rest6
    -- (ws6, rest8) <- parseWhitespace rest7
    -- (charL, rest9) <- parseChar 'L' rest8
    -- (ws7, rest14) <- parseWhitespace rest13
    ((carDisplacement, wsLCharws), rest7) <- parseDisplacement rest6
    (engineLayout, rest8) <- parseAlphaNum rest7
    (ws6, rest9) <- parseWhitespace rest8
    -- (power, rest17) <- parseTextNumWhitespace rest16
    -- (kWReading, rest18) <- parseTextNumWhitespace rest17
    -- (ws9, rest19) <- parseWhitespace rest18
    ((power, kWText, ws9), rest10) <- parseNumTextWhitespaceOrdered rest9
    ((torque, nMText, ws10), rest11) <- parseNumTextWhitespaceOrdered rest10
    ((inductionType, ws11), rest12) <- parseTextWhitespaceOrdered rest11
    ((fuelType, ws12), rest13) <- parseTextWhitespaceOrdered rest12
    (driveType, rest14) <- parseAlphaNum rest13
    (ws13, rest15) <- parseWhitespace rest14
    ((gearbox, ws14), rest16) <- parseTextWhitespaceOrdered rest15
    ((milleage, kmText, ws15), rest17) <- parseNumTextWhitespaceOrdered rest16
    return [Left carString, Left carName, Left modelName, Left carColor, Left carBodyType,
            Right (Right carDisplacement), Left engineLayout, Right (Left power), Right (Left torque),
            Left inductionType, Left fuelType, Left driveType, Left gearbox, Right (Left milleage)]


-- >>> parseCar2 "Car E 3zx Orange Crossover 1kW 4Nm Electric 4WD Manual 71.2kWh/100km 8km "
-- Left "Not a letter"
parseCar2 :: String -> Either String [Either String (Either Integer Double)]
parseCar2 [] = Left "Empty car input"
parseCar2 input = do
    ((carString, ws1), rest1) <- parseTextWhitespaceOrdered input
    ((carName, ws2), rest2) <- parseTextWhitespaceOrdered rest1
    (modelName, rest3) <- parseAlphaNum rest2
    (ws3, rest4) <- parseWhitespace rest3
    ((carColor, ws4), rest5) <- parseTextWhitespaceOrdered rest4
    ((carBodyType, ws5), rest6) <- parseTextWhitespaceOrdered rest5
    ((power, kWText, ws9), rest7) <- parseNumTextWhitespaceOrdered rest6
    ((torque, nMText, ws10), rest8) <- parseNumTextWhitespaceOrdered rest7
    ((fuelType, ws12), rest9) <- parseTextWhitespaceOrdered rest8
    (driveType, rest10) <- parseAlphaNum rest9
    (ws13, rest11) <- parseWhitespace rest10
    ((gearbox, ws14), rest12) <- parseTextWhitespaceOrdered rest11
    ((milleage, kmText, ws15), rest13) <- parseNumTextWhitespaceOrdered rest12
    return [Left carString, Left carName, Left modelName, Left carColor, Left carBodyType,
            Right (Left power), Right (Left torque), Left fuelType, Left driveType,
            Left gearbox, Right (Left milleage)]

-- or2 :: Parser a -> Parser a -> Parser a
-- or2 a b = \input ->
--     case a input of
--         Right r1 -> Right r1
--         Left e1 ->
--             case b input of
--                 Right r2 -> Right r2
--                 Left e2 -> Left (e1 ++ ", " ++ e2)


-- parseCar :: String -> Either String [Either String (Either Integer Double)]
-- parseCar = or2 parseCar1 parseCar2

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery _ = Left "Not implemented 2"

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = error "Not implemented 1"

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition _ _ = Left "Not implemented 3"
