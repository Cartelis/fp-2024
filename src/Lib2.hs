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

-- >>> FuelEngine 4.4 "V8" 390 800 "Turbocharged" "Petrol"
-- FuelEngine {displacement = 4.4, engineLayout = "V8", power = 390, torque = 800, induction = "Turbocharged", fuel = "Petrol"}
-- >>> ElectricEngine 260 400
-- ElectricEngine {power = 260, torque = 400}
data Engine
    = FuelEngine {
        displacement :: Double,
        engineLayout :: String,
        power :: Integer,
        torque :: Integer,
        induction :: String,
        fuel :: String
    }
    | ElectricEngine {
        power :: Integer,
        torque :: Integer
    } deriving Show

data Powertrain = Powertrain {
    engine :: Engine,
    drive_type :: String,
    transmission :: String
} deriving Show

data Car = Car {
    make :: String,
    model :: String,
    color :: String,
    body_type :: String,
    powertrain :: Powertrain,
    consumption :: Double,
    mileage:: Integer
} deriving Show


-- >>> parseChar 'a' "aaa"
-- Right ('a',"aa")
-- >>> parseChar '*' "fdf"
-- Left "* is not found in fdf"
-- >>> parseChar 'a' "asd"
-- Right ('a',"sd")
-- >>> parseChar '/' "/100km"
-- Right ('/',"100km")
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


many :: Parser a -> Parser [a]
many p = many' p []
    where
        many' p' acc = \input ->
            case p' input of
                Left _ -> Right (acc, input)
                Right (v, r) -> many' p' (acc ++ [v]) r


and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' c a b = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) -> Right (c v1 v2, r2)
                Left e2 -> Left e2
        Left e1 -> Left e1


and3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3 d a b c = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) -> Right (d v1 v2 v3, r3)
                        Left e3 -> Left e3
                Left e2 -> Left e2
        Left e1 -> Left e1


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


-- >>> parseTextWhitespaceOrdered "BMW M850i "
-- Right ("BMW","M850i ")
-- >>> parseTextWhitespaceOrdered "Black Coupe "
-- Right ("Black","Coupe ")
-- >>> parseTextWhitespaceOrdered "85 Coupe "
-- Right ("85","Coupe ")
-- >>> parseTextWhitespaceOrdered "Car BMW"
-- Right ("Car","BMW")
-- >>> parseTextWhitespaceOrdered "M850i Car"
-- Right ("M850i","Car")
parseAlphaNumWhitespaceOrdered :: Parser String
parseAlphaNumWhitespaceOrdered = and2' (\a _ -> a) parseAlphaNum parseWhitespace


-- >>> parseDisplacement "4.4 L V8 "
-- Right (4.4,"V8 ")
-- >>> parseDisplacement "8.9 L V12 500kW"
-- Right (8.9,"V12 500kW")
-- >>> parseDisplacement "a.4 L V8 "
-- Left "Not a digit or dot"
-- >>> parseDisplacement ""
-- Left "Cannot parse a double number in an empty input"
parseDisplacement :: Parser Double
parseDisplacement = and2' (\a _ -> a) parseDouble (and3 (\_ b _ -> b) parseWhitespace (parseChar 'L') parseWhitespace)

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
-- Right (71.2,"")
-- >>> parseFuelConsumption "71.2kWh\100km "
-- Right (0.0,"71.2kWhdkm ")
-- >>> parseFuelConsumption "12.7l/100km 125km "
-- Right (12.7,"125km ")
-- >>> parseFuelConsumption "125km "
-- Right (0.0,"125km ")
-- >>> parseFuelConsumption "256.6kWh/100km 2km "
-- Right (256.6,"2km ")
parseFuelConsumption :: Parser Double
parseFuelConsumption input = case parseFuelConsumptionInner input of
                                Right (v, r) -> Right (v, r)
                                Left e1 ->
                                    if e1 == "/ is not found in  "
                                        then Right (0, input)
                                        else Left e1
-- >>> parseFuelConsumptionInner "1km "
-- Left "/ is not found in  "
parseFuelConsumptionInner :: Parser Double
parseFuelConsumptionInner = and3 (\a _ _ -> a) parseDouble 
                        (and3 (\_ _ _ -> ()) parseAlphaNum (parseChar '/') parseAlphaNum)
                        parseWhitespace

-- >>> parseNumberTextWhitespace "390kW 780Nm"
-- Right (390,"780Nm")
-- >>> parseNumberTextWhitespace "2km "
-- Right (2,"")
parseNumberTextWhitespace :: Parser Integer
parseNumberTextWhitespace = and3 (\a _ _ -> a) parseNumber parseText parseWhitespace


-- and6 :: (a -> b -> c -> d -> e -> f -> g) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g 
-- and6 g a b c d e f = \input ->
--     case a input of
--         Right (v1, r1) ->
--             case b r1 of
--                 Right (v2, r2) ->
--                     case c r2 of
--                         Right (v3, r3) ->
--                             case d r3 of
--                                 Right (v4, r4) ->
--                                     case e r4 of
--                                         Right (v5, r5) ->
--                                             case f r5 of
--                                                 Right (v6, r6) -> Right (g v1 v2 v3 v4 v5 v6, r6)
--                                                 Left e6 -> Left e6
--                                         Left e5 -> Left e5
--                                 Left e4 -> Left e4   
--                         Left e3 -> Left e3
--                 Left e2 -> Left e2
--         Left e1 -> Left e1



-- >>> parseFuelEngine "4.4 L V8 390kW 800Nm Turbocharged Petrol "
-- Right (FuelEngine {displacement = 4.4, engineLayout = "V8", power = 390, torque = 800, induction = "Turbocharged", fuel = "Petrol"},"")
parseFuelEngine :: Parser Engine
parseFuelEngine input = case parseFuelEngineInfo input of
                    Right (((disp, layout, pow), (torq, ind, fuelType)), r) ->
                        Right (FuelEngine disp layout pow torq ind fuelType, r)
                    Left e1 -> Left e1

-- >>> parseFuelEngineInfo "4.4 L V8 390kW 800Nm Turbocharged Petrol "
-- Right (((4.4,"V8",390),(800,"Turbocharged","Petrol")),"")
parseFuelEngineInfo :: Parser ((Double, String, Integer), (Integer, String, String))
parseFuelEngineInfo = and2' (,)
                    (and3 (,,) parseDisplacement parseAlphaNumWhitespaceOrdered parseNumberTextWhitespace)
                    (and3 (,,) parseNumberTextWhitespace parseAlphaNumWhitespaceOrdered parseAlphaNumWhitespaceOrdered)
-- parseFuelEngineInfo = and6 (\a b c d e f -> (a, b, c, d, e, f)) 
                    -- parseDisplacement parseAlphaNumWhitespaceOrdered parseNumberTextWhitespace
                    -- parseNumberTextWhitespace parseAlphaNumWhitespaceOrdered parseAlphaNumWhitespaceOrdered

-- >>> parseElectricEngine "202kW 25Nm Electric 4WD"
-- Right (ElectricEngine {power = 202, torque = 25},"4WD")
parseElectricEngine :: Parser Engine
parseElectricEngine input = case (and3 (\a b _ -> (a, b))
                                    parseNumberTextWhitespace parseNumberTextWhitespace parseAlphaNumWhitespaceOrdered) input of 
                                Right ((pow, torq), r) -> Right(ElectricEngine pow torq, r)
                                Left e1 -> Left e1

parseEngine :: Parser Engine
parseEngine = or2 parseFuelEngine parseElectricEngine

-- >>> parsePowertrain "202kW 25Nm Electric 4WD Automatic 2km "
-- Right (Powertrain {engine = ElectricEngine {power = 202, torque = 25}, drive_type = "4WD", transmission = "Automatic"},"2km ")
-- >>> parsePowertrain "0.7 L D 8kW 5Nm Supercharged Turbocharged Petrol FWD Manual "
-- Right (Powertrain {engine = FuelEngine {displacement = 0.7, engineLayout = "D", power = 8, torque = 5, induction = "Supercharged", fuel = "Turbocharged"}, drive_type = "Petrol", transmission = "FWD"},"Manual ")
parsePowertrain :: Parser Powertrain
parsePowertrain input = case parsePowertrainInfo input of
                            Right((engine1, drive_type1, trans), r) ->
                                Right (Powertrain engine1 drive_type1 trans, r)
                            Left e1 -> Left e1

-- >>> parsePowertrainInfo "202kW 25Nm Electric 4WD Automatic 2km "
-- Right ((ElectricEngine {power = 202, torque = 25},"4WD","Automatic"),"2km ")
parsePowertrainInfo :: Parser (Engine, String, String)
parsePowertrainInfo = and3 (,,) parseEngine parseAlphaNumWhitespaceOrdered parseAlphaNumWhitespaceOrdered

-- >>> parseCar "sKXg E Red Wagon 06kW 8475Nm Electric RWD Automatic 924.1kWh/100km 07km "
-- Right (Car {make = "sKXg", model = "E", color = "Red", body_type = "Wagon", powertrain = Powertrain {engine = ElectricEngine {power = 6, torque = 8475}, drive_type = "RWD", transmission = "Automatic"}, consumption = 924.1, mileage = 7},"")
parseCar :: Parser Car
parseCar input = case parseCarInfo input of
                    Right (((make1, model1, color1), (body_type1, powertrain1, consumption1), miles), r) ->
                        Right (Car make1 model1 color1 body_type1 powertrain1 consumption1 miles, r)
                    Left e1 -> Left e1

-- >>> parseCarInfo "BMW M850i Black Coupe 4.4 L V8 390kW 750Nm Turbocharged Petrol AWD Automatic 18153km "
-- Right ((("BMW","M850i","Black"),("Coupe",Powertrain {engine = FuelEngine {displacement = 4.4, engineLayout = "V8", power = 390, torque = 750, induction = "Turbocharged", fuel = "Petrol"}, drive_type = "AWD", transmission = "Automatic"},0.0),18153),"")
-- >>> parseCarInfo "Z q Red1 Compact 0.7 L D 8kW 5Nm Supercharged Petrol FWD Manual 256.6kWh/100km 2km "
-- Right ((("Z","q","Red1"),("Compact",Powertrain {engine = FuelEngine {displacement = 0.7, engineLayout = "D", power = 8, torque = 5, induction = "Supercharged", fuel = "Petrol"}, drive_type = "FWD", transmission = "Manual"},256.6),2),"")
-- >>> parseCarInfo "bhb I Gray Muscle 0kW 673Nm Electric FWD Automatic 2.9l/100km 2km "
-- Right ((("bhb","I","Gray"),("Muscle",Powertrain {engine = ElectricEngine {power = 0, torque = 673}, drive_type = "FWD", transmission = "Automatic"},2.9),2),"")
parseCarInfo :: Parser ((String, String, String), (String, Powertrain, Double), Integer)
parseCarInfo = and3 (,,) (and3 (,,) parseAlphaNumWhitespaceOrdered parseAlphaNumWhitespaceOrdered parseAlphaNumWhitespaceOrdered)
                        (and3 (,,) parseAlphaNumWhitespaceOrdered parsePowertrain parseFuelConsumption) parseNumberTextWhitespace




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
