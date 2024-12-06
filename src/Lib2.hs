{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition,
    Garage(..),
    Car(..),
    Powertrain(..),
    Engine(..),
    parseAlphaNumWhitespaceOrdered,
    parseText,
    and2',
    many,
    parseCarWithPrefix,
    parseListOfCars,
    parseCarGarage,
    ) where


import qualified Data.Char as C
import qualified Data.List as L


-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query = CarGarage Garage
    | AddCar Car
    | RemoveCar Car
    | EditCar Car Car
    | ListCars
    | CalculatePollutionTax Car
    | View

-- | The instances are needed basically for tests
instance Eq Query where
    (==) (CarGarage g1) (CarGarage g2) = g1 == g2
    (==) (AddCar c1) (AddCar c2) = c1 == c2
    (==) (RemoveCar c1) (RemoveCar c2) = c1 == c2
    (==) (EditCar oldCar1 newCar1) (EditCar oldCar2 newCar2) =
        oldCar1 == oldCar2 && newCar1 == newCar2
    (==) ListCars ListCars = True
    (==) (CalculatePollutionTax car1) (CalculatePollutionTax car2) = car1 == car2
    (==) View View = True
    (==) _ _= False

instance Show Query where
    -- show _ = ""
    show (CarGarage garage) =
        "CarGarage: " ++ show garage
    show (AddCar car) =
        "Adding car: " ++ show car
    show (RemoveCar car) =
        "Removing car: " ++ show car
    show (EditCar oldCar newCar) =
        "Editing car from: " ++ show oldCar ++ "\nto: " ++ show newCar
    show View =
        "Viewing currnet sate"
    show ListCars =
        "A list of cars in main garage"
    show (CalculatePollutionTax _) = ""

instance Show State where
    show (State garageInState) = show garageInState

instance Show Garage where
    show (Garage g_name carList inner_g) =
        "Garage name: " ++ g_name ++ "\n" ++
        "Cars: \n" ++ concatMap (\car -> "   " ++ show car ++ "\n") carList ++
        "Inner garages: " ++ show inner_g++ "\n"

instance Show Car where
    show (Car carMake carModel carColor carBody_type carPowertrain carConsumption carMileage) =
        carMake ++ " " ++ carModel ++ " " ++ carColor ++ " " ++ carBody_type ++ " " ++
        show carPowertrain ++ " " ++ (if carConsumption > 0 then show carConsumption ++ "l/100km (kWh/100km) " else "") ++ show carMileage ++ "km"

instance Show Powertrain where
    show (Powertrain carEngine carDrive_type carTransmission) =
        show carEngine ++ " " ++ carDrive_type ++ " " ++ carTransmission

instance Show Engine where
    show (FuelEngine carDisplacement carEngineLayout carPower carTorque carInduction carFuel) =
        show carDisplacement ++ " L " ++ carEngineLayout ++ " " ++ show carPower ++ "kW " ++
            show carTorque ++ "Nm " ++ carInduction ++ " " ++ carFuel 
    show (ElectricEngine carPower carTorque) =
        show carPower ++ "kW " ++ show carTorque ++ "Nm"

type Parser a = String -> Either String (a, String)

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
    }
    deriving Eq

data Powertrain = Powertrain {
    engine :: Engine,
    drive_type :: String,
    transmission :: String
} deriving Eq

data Car = Car {
    make :: String,
    model :: String,
    color :: String,
    body_type :: String,
    powertrain :: Powertrain,
    consumption :: Double,
    mileage:: Integer
} deriving Eq

data Garage = Garage {
    garage_name :: String,
    cars :: [Car],
    inner_garage :: [Garage]
} deriving Eq

colorList :: [String]
colorList = ["Red", "Black", "Gray", "White", "Blue", "Silver", "Green", "Brown", "Orange", "Yellow", "Gold", "Purple"]

bodyTypeList :: [String]
bodyTypeList = ["Sedan", "Coupe", "Hatchback", "Pickup", "Off-road", "Sport", "Van", "Convertible", "Crossover", "SUV", "Wagon", "Muscle", "Compact"]

driveTypeList :: [String]
driveTypeList = ["AWD", "RWD", "FWD", "4WD"]

gearboxList :: [String]
gearboxList = ["Manual", "Automatic"]

fuelList :: [String]
fuelList = ["Diesel", "Petrol", "Hydrogen"]



or2 :: Parser a -> Parser a -> Parser a
or2 a b = \input ->
    case a input of
        Right r1 -> Right r1
        Left e1 ->
            case b input of
                Right r2 -> Right r2
                -- Left e2 -> Left (e1 ++ ", " ++ e2)
                Left _ -> Left e1

or3 :: Parser a -> Parser a -> Parser a -> Parser a
or3 a b c = \input ->
    case a input of
        Right r1 -> Right r1
        Left e1 ->
            case b input of
                Right r2 -> Right r2
                Left e2 ->
                    case c input of
                        Right r3 -> Right r3
                        Left e3 -> Left (e1 ++ ", " ++ e2 ++ ", " ++ e3)

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


parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar c s@(h:t) = if c == h then Right (c, t) else Left (c : " is not found in " ++ s)


parseWhitespace :: Parser Char
parseWhitespace [] = Left "Cannot find any whitespace in an empty input"
parseWhitespace s@(h:t) = if ' ' == h then Right (' ', t) else Left (s ++ " does not start with a whitespace")


-- <digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
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


-- <letter> ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
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


-- <string> ::= <letter> | <letter> <string> | <digit> | <digit> <string>
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


isDot :: Char -> Bool
isDot c = c == '.'


-- <fuel_consumption> ::= <digit> "." <digit> | <digit> <fuel_consumption>
parseDouble :: Parser Double
parseDouble [] = Left "Cannot parse a double number in an empty input"
parseDouble str =
    let
        doubleNum = L.takeWhile (\c -> C.isDigit c || isDot c) str
        rest = drop (length doubleNum) str
    in
        case doubleNum of
            [] -> Left "Not a digit or dot"
            _ -> Right (read doubleNum, rest)


-- <engine_layout> ::= <string>
-- <garage_name> ::= <string>
parseAlphaNumWhitespaceOrdered :: Parser String
parseAlphaNumWhitespaceOrdered = and2' (\a _ -> a) parseAlphaNum parseWhitespace


-- <displacement> ::= <digit> "." <digit> " L" | <digit> <displacement>
parseDisplacement :: Parser Double
parseDisplacement = and2' (\a _ -> a) parseDouble (and3 (\_ b _ -> b) parseWhitespace (parseChar 'L') parseWhitespace)


-- <opt_consumption> ::= " " <fuel_consumption> "l/100km " | " " <fuel_consumption> "kWh/100km " | " "
parseFuelConsumption :: Parser Double
parseFuelConsumption input = case parseFuelConsumptionInner input of
                                Right ((v, measurement), r) ->
                                    if measurement == "l/100km" || measurement == "kWh/100km"
                                        then Right (v, r)
                                        else Left ("Not supported fuel measurement: " ++ measurement)
                                Left e1 ->
                                    if e1 == "Cannot find forward slash"
                                        then Right (0, input)
                                        else Left e1


parseFuelConsumptionInner :: Parser (Double, String)
parseFuelConsumptionInner = and3 (\a b _ -> (a, b)) parseDouble
                        (and3 (\a b c -> a ++ [b] ++ c) parseAlphaNum parseForwardSlash parseAlphaNum)
                        parseWhitespace


parseForwardSlash :: Parser Char
parseForwardSlash [] = Left "Cannot find forward slash in an empty input"
parseForwardSlash (h : t)
  | h == '/' = Right (h, t)
  | h == '\\' = Left "Wrong fuel consumption format"
  | otherwise = Left "Cannot find forward slash"


-- <power> ::= <digit> "kW" | <digit> <power>
-- <torque> ::= <digit> "Nm" | <digit> <torque>
-- <mileage> ::= <digit> "km " | <digit> <mileage>
parseNumberTextWhitespace :: Parser Integer
parseNumberTextWhitespace = and3 (\a _ _ -> a) parseNumber parseText parseWhitespace


-- <induction> ::= "Turbocharged" | "Supercharged" | "Naturally aspirated"
parseInduction :: Parser String
parseInduction input =
    case parseAlphaNumWhitespaceOrdered input of
        Right (v1, r1) ->
            if v1 == "Naturally"
                then
                    case parseAlphaNumWhitespaceOrdered r1 of
                        Right (v2, r2) ->
                            if v2 == "aspirated"
                                then Right (v1 ++ " " ++ v2, r2)
                                else Left ("Not supported induction type: " ++ v1 ++ " " ++ v2)
                        Left e2 -> Left e2
                else if v1 == "Turbocharged" || v1 == "Supercharged"
                    then Right (v1, r1)
                    else Left ("Not supported induction type: " ++ v1)
        Left e1 -> Left e1


-- <fuel> ::= "Diesel" | "Petrol" | "Hydrogen"
parseFuel :: Parser String
parseFuel input =
    case parseAlphaNumWhitespaceOrdered input of
        Right (v, r) ->
            if elem v fuelList
                then Right (v, r)
                else Left ("Not supported fuel: " ++ v)
        Left e1 -> Left e1


parseFuelEngine :: Parser Engine
parseFuelEngine input = case parseFuelEngineInfo input of
                    Right (((disp, layout, pow), (torq, ind, fuelType)), r) ->
                        Right (FuelEngine disp layout pow torq ind fuelType, r)
                    Left e1 -> Left e1


parseFuelEngineInfo :: Parser ((Double, String, Integer), (Integer, String, String))
parseFuelEngineInfo = and2' (,)
                    (and3 (,,) parseDisplacement parseAlphaNumWhitespaceOrdered parseNumberTextWhitespace)
                    (and3 (,,) parseNumberTextWhitespace parseInduction parseFuel)


parseElectricEngine :: Parser Engine
parseElectricEngine input = case (and3 (\a b _ -> (a, b))
                                    parseNumberTextWhitespace parseNumberTextWhitespace parseAlphaNumWhitespaceOrdered) input of
                                Right ((pow, torq), r) -> Right (ElectricEngine pow torq, r)
                                Left e1 -> Left e1


-- <engine> ::= <displacement> " " <engine_layout> " " <power> " " <torque> " " <induction> " " <fuel> | <power> " " <torque> " Electric"
parseEngine :: Parser Engine
parseEngine = or2 parseFuelEngine parseElectricEngine


-- <drive_type> ::= "AWD" | "RWD" | "FWD" | "4WD"
parseDriveType :: Parser String
parseDriveType input =
    case parseAlphaNumWhitespaceOrdered input of
        Right (v, r) ->
            if elem v driveTypeList
                then Right (v, r)
                else Left ("Not supported drive type: " ++ v)
        Left e1 -> Left e1


-- <transmission> ::= "Manual" | "Automatic"
parseGearbox :: Parser String
parseGearbox input =
    case parseAlphaNumWhitespaceOrdered input of
        Right (v, r) ->
            if elem v gearboxList
                then Right (v, r)
                else Left ("Not supported gearbox: " ++ v)
        Left e1 -> Left e1


-- <powertrain> ::= <engine> " " <drive_type> " " <transmission>
parsePowertrain :: Parser Powertrain
parsePowertrain input = case parsePowertrainInfo input of
                            Right((engine1, drive_type1, trans), r) ->
                                Right (Powertrain engine1 drive_type1 trans, r)
                            Left e1 -> Left e1


parsePowertrainInfo :: Parser (Engine, String, String)
parsePowertrainInfo = and3 (,,) parseEngine parseDriveType parseGearbox


parseModelNameOne :: Parser String
parseModelNameOne [] = Left "Cannot find any model name made out od letters or digits in an empty input"
parseModelNameOne input =
        case parseAlphaNumWhitespaceOrdered input of
            Right (v, r) ->
                if elem v colorList
                    then Left "Color read"
                    else Right (v, r)
            Left e1 -> Left e1


trimTrailing :: String -> String
trimTrailing = reverse . dropWhile C.isSpace . reverse


-- <model_name> ::= <string> " " | <string> " " <model_name>
parseModelName :: Parser String
parseModelName input =
    case parseModelName' input of
        Right (stringList, r) -> Right (trimTrailing (concatStringListToString stringList), r)
        Left e -> Left e


parseModelName' :: Parser [String]
parseModelName' = and2' (:) parseModelNameOne (many parseModelNameOne)


concatStringListToString :: [String] -> String
concatStringListToString [] = ""
concatStringListToString (h:t) = h ++ " " ++ concatStringListToString t


-- <color> ::= "Red" | "Black" | "Gray" | "White" | "Blue" | "Silver" | "Green" | "Brown" | "Orange" | "Yellow" | "Gold" | "Purple"
parseColor :: Parser String
parseColor input =
    case parseAlphaNumWhitespaceOrdered input of
        Right (v, r) ->
            if elem v colorList
                then Right (v, r)
                else Left ("Not supported color: " ++ v)
        Left e1 -> Left e1


-- <make> ::= <stringMake>
parseMake :: Parser String
parseMake = and2' (\a _ -> a) parseAlphaWithMinus parseWhitespace


-- <stringMake> ::= <letterOrMinus> | <letterOrMinus> <stringMake>
-- <letterOrMinus> ::= <letter> | "-"
parseAlphaWithMinus :: Parser String
parseAlphaWithMinus [] = Left "Cannot find any text made out of letters or - in an empty input"
parseAlphaWithMinus str =
    let
        lettersMinus = L.takeWhile (\c -> C.isLetter c || c == '-') str
        rest = drop (length lettersMinus) str
    in
        case lettersMinus of
            [] -> Left "Not a letter or -"
            _ -> Right (lettersMinus, rest)


parseAlphaNumWithMinus :: Parser String
parseAlphaNumWithMinus [] = Left "Cannot find any text made out of letters or digits in an empty input"
parseAlphaNumWithMinus str =
    let
        lettersDigits = L.takeWhile (\c -> C.isLetter c || C.isDigit c || c == '-') str
        rest = drop (length lettersDigits) str
    in
        case lettersDigits of
            [] -> Left "Not a letter, a digit or -"
            _ -> Right (lettersDigits, rest)


parseAlphaNumWithMinusWhitespace :: Parser String
parseAlphaNumWithMinusWhitespace = and2' (\a _ -> a) parseAlphaNumWithMinus parseWhitespace


-- <body_type> ::= "Sedan" | "Coupe" | "Hatchback" | "Pickup" | "Off-road" | "Sport" | "Van" | "Convertible" | "Crossover" | "SUV" | "Wagon" | "Muscle" | "Compact"
parseBodyType :: Parser String
parseBodyType input =
    case parseAlphaNumWithMinusWhitespace input of
        Right (v, r) ->
            if elem v bodyTypeList
                then Right (v, r)
                else Left ("Not supported body type: " ++ v)
        Left e1 -> Left e1


-- <model> ::= <model_name> <color> " " <body_type> " " <powertrain> <opt_consumption> <mileage>
parseModel :: Parser ((String, String, String), (Powertrain, Double, Integer))
parseModel = and2' (,) (and3 (,,) parseModelName parseColor parseBodyType)
                        (and3 (,,) parsePowertrain parseFuelConsumption parseNumberTextWhitespace)


parseCar :: Parser Car
parseCar input = case parseCarInfo input of
    Right ((make', ((model', color', body_type'), (powertrain', consumption', mileage'))), r) ->
        Right (Car make' model' color' body_type' powertrain' consumption' mileage', r)
    Left e -> Left e


parseCarInfo :: Parser (String, ((String, String, String), (Powertrain, Double, Integer)))
parseCarInfo = and2' (,) parseMake parseModel


parseCarPrefix :: Parser Bool
parseCarPrefix input = case parseAlphaNumWhitespaceOrdered input of
                            Right (v, r) -> Right (v == "Car", r)
                            Left err -> Left err


-- <car> ::= "Car " <make> " " <model>
parseCarWithPrefix :: Parser Car
parseCarWithPrefix input = case parseCarPrefix input of
                            Right (carPrefixFound, r) ->
                                if carPrefixFound then parseCar r else Left "Prefix 'Car' not found"
                            Left e1 -> Left e1


-- <car_list> ::= <car> | <car> <car_list>
parseListOfCars :: Parser [Car]
parseListOfCars = and2' (:) parseCarWithPrefix
                        (many parseCarWithPrefix)


-- <CarGarage> ::= "CarGarage" <garage>
parseCarGarage :: Parser Query
parseCarGarage input =
    case parseAlphaNumWhitespaceOrdered input of
        Right (prefix, r1) ->
            if prefix == "CarGarage"
                then
                    case parseGarage r1 of
                        Right (garage, r2) -> Right (CarGarage garage, r2)
                        Left e2 -> Left e2
                else Left "Not supported prefix/command"
        Left e1 -> Left e1


-- <garage> ::= " Garage " <garage_name> " " <car_list> "(" <inner_garage> ") "
parseGarage :: Parser Garage
parseGarage input =
    case parseGarageInfo input of
        Right ((name, carList), r1) ->
            case parseGarage' r1 of
                Right (garages, r2) -> Right (Garage name carList garages, r2)
                Left _ -> Right (Garage name carList [], r1)
        Left e1 -> Left e1


parseGarageInfo :: Parser (String, [Car])
parseGarageInfo input =
    case parseAlphaNumWhitespaceOrdered input of
        Right (w, r) ->
            if w == "Garage"
                then and2' (,) parseAlphaNumWhitespaceOrdered parseListOfCars r
                else Left ("Prefix Garage not found: " ++ w)
        Left e -> Left e


-- <inner_garage> ::= <garage> | <garage> <inner_garage> | " "
parseGarage' :: Parser [Garage]
parseGarage' [] = Left ""
parseGarage' input =
    case parseParenthesesStartWhitespace input of
        Right (_, r1) ->
            case parseGarage r1 of
                Right (garage, r2) ->
                    case parseGarage' r2 of
                        Right (garages, r3) ->
                            Right (garage : garages, r3)
                        Left _ ->
                            case parseParenthesesEndWhitespace r2 of
                                Right (_, r3) -> Right ([garage], r3)
                                Left e3 -> Left e3
                Left _ ->
                    case parseParenthesesEndWhitespace r1 of
                        Right (_, r2) -> Right ([], r2)
                        Left e2 -> Left e2
        Left _ ->
            case parseWhitespace input of
                Right (_, r1) ->
                    case parseGarage r1 of
                        Right (garage, r2) ->
                            case parseGarage' r2 of
                                Right (garages, r3) ->
                                    Right (garage : garages, r3)
                                Left _ ->
                                    case parseParenthesesEndWhitespace r2 of
                                        Right (_, r3) -> Right ([garage], r3)
                                        Left e3 -> Left e3
                        Left _ ->
                            case parseParenthesesEndWhitespace r1 of
                                Right (_, r2) -> Right ([], r2)
                                Left e2 -> Left e2
                Left e1 -> Left e1


parseParenthesesStartWhitespace :: Parser Bool
parseParenthesesStartWhitespace [] = Left "Parentheses start and whitespace not found"
parseParenthesesStartWhitespace input =
    case parseChar '(' input of
        Right (_, r1) ->
            case parseWhitespace r1 of
                Right (_, r2) -> Right (True, r2)
                Left e2 -> Left e2
        Left e1 -> Left e1


parseParenthesesEndWhitespace :: Parser Bool
parseParenthesesEndWhitespace [] = Left "Parentheses end and whitespace not found"
parseParenthesesEndWhitespace input =
    case parseChar ')' input of
        Right (_, r1) ->
            case parseWhitespace r1 of
                Right (_, r2) -> Right (True, r2)
                Left e2 -> Left e2
        Left e1 -> Left e1


-- <command> ::= <command_type> " " <car> | "EditCar " <car> "to " <car> | "ListCars" | "View"
parseCommand :: Parser Query
parseCommand = parseAddOrRemoveOrCalculatePollutionCar `or2` parseEditCar `or2` parseListCars `or2` parseView


-- <command_type> ::= "AddCar" | "RemoveCar" | "CalculatePollutionTax"
parseAddOrRemoveOrCalculatePollutionCar :: Parser Query
parseAddOrRemoveOrCalculatePollutionCar [] = Left "Cannot parse empty input"
parseAddOrRemoveOrCalculatePollutionCar input =
    case parseAlphaNumWhitespaceOrdered input of
        Right (word, r1) ->
            if word == "AddCar"
                then case parseCarWithPrefix r1 of
                    Right (car, r2) -> Right (AddCar car, r2)
                    Left e2 -> Left e2
                else if word == "RemoveCar"
                    then case parseCarWithPrefix r1 of
                        Right (car, r2) -> Right (RemoveCar car, r2)
                        Left e2 -> Left e2
                else if word == "CalculatePollutionTax"
                    then case parseCarWithPrefix r1 of
                        Right (car, r2) -> Right (CalculatePollutionTax car, r2)
                        Left e2 -> Left e2
                else Left "Given command not found"
        Left e1 -> Left e1


parseEditCar :: Parser Query
parseEditCar [] = Left "Cannot parse empty input"
parseEditCar input =
    case parseAlphaNumWhitespaceOrdered input of
        Right (word1, r1) ->
            if word1 == "EditCar"
                then case parseCarWithPrefix r1 of
                    Right (oldCar, r2) ->
                        case parseAlphaNumWhitespaceOrdered r2 of
                            Right (word2, r3) ->
                                if word2 == "to"
                                    then case parseCarWithPrefix r3 of
                                        Right (newCar, r4) -> Right (EditCar oldCar newCar, r4)
                                        Left e4 -> Left e4
                                    else Left "Given command not found"
                            Left e3 -> Left e3
                    Left e2 -> Left e2
                else Left "Given command not found"
        Left e1 -> Left e1


parseView :: Parser Query
parseView [] = Left "Cannot parse empty input"
parseView input =
    -- case parseText input of
    case parseAlphaNumWhitespaceOrdered input of
        Right (word, r1) ->
            if word == "View"
                then Right (View, r1)
                else Left "Given command not found"
        Left e1 -> Left e1


parseListCars :: Parser Query
parseListCars [] = Left "Cannot parse empty input"
parseListCars input =
    -- case parseText input of
    case parseAlphaNumWhitespaceOrdered input of
        Right (word, r1) ->
            if word == "ListCars"
                then Right (ListCars, r1)
                else Left "Given command not found"
        Left e1 -> Left e1


-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String (Query, String)
parseQuery [] = Left "Cannot parse empty input"
parseQuery input =
    case or2 parseCarGarage parseCommand input of
        Right (query, r) -> Right (query, r)
        -- _ -> Left "Failed to parse query: Unknown command"
        Left e -> Left ("Failed to parse query: " ++ e)


-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State = State {
    stateGarage :: Garage
} deriving Eq


-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = State {
    stateGarage = Garage {
        garage_name = "",
        cars = [],
        inner_garage = []
    }
}


removeFirstCarOccurence :: Car -> [Car] -> [Car]
removeFirstCarOccurence _ [] = []
removeFirstCarOccurence carToRemove (h:t)
    | carToRemove == h
        = t
    | otherwise
        = h : removeFirstCarOccurence carToRemove t


editCarInList :: Car -> Car -> [Car] -> [Car]
editCarInList _ _ [] = []
editCarInList oldCar newCar (h:t)
    | oldCar == h
        = newCar : t
    | otherwise
        = h : editCarInList oldCar newCar t


-- Function to get all cars from a garage and its inner garages
getAllCars :: Garage -> [Car]
getAllCars garage = cars garage ++ innerCars (inner_garage garage)


-- Helper function to gather cars from inner garages
innerCars :: [Garage] -> [Car]
innerCars [] = []
innerCars (g:gs) = getAllCars g ++ innerCars gs

calculatePollutionTaxOnEngine :: Engine -> Int
calculatePollutionTaxOnEngine givenEngine
    | power givenEngine >= 400 = 360
    | power givenEngine >= 250 = 280
    | power givenEngine >= 150 = 160
    | otherwise = 0


-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
-- stateTransition _ _ = Left "Not implemented 3"
stateTransition state (CarGarage garage) =
    let
        newState = state { stateGarage = garage }
    in
        Right (Just "CarGarage state updated!", newState)

stateTransition state (AddCar car) =
    let
        currentGarage = stateGarage state
        updatedCarList = car : cars currentGarage
        updatedGarage = currentGarage { cars = updatedCarList }
        newState = state { stateGarage = updatedGarage }
    in
        Right (Just "Car added!", newState)

stateTransition state (RemoveCar car) =
    let
        currentGarage = stateGarage state
        currentCarList = cars currentGarage
        updatedCarList = removeFirstCarOccurence car currentCarList
        updatedGarage = currentGarage { cars = updatedCarList }
        newState = state { stateGarage = updatedGarage }
    in
        Right (Just "Car removed!", newState)

stateTransition state (EditCar oldCar newCar) =
    let
        currentGarage = stateGarage state
        currentCarList = cars currentGarage
        updatedCarList = editCarInList oldCar newCar currentCarList
        updatedGarage = currentGarage { cars = updatedCarList }
        newState = state { stateGarage = updatedGarage }
    in
        Right (Just "Car edited!", newState)

stateTransition state ListCars =
    let
        allCarList = getAllCars (stateGarage state)
    in
        Right (Just ("Listing cars: " ++ show allCarList), state)

stateTransition state (CalculatePollutionTax givenCar) =
    let
        engineInCar = engine (powertrain givenCar)
        calculatedTax = calculatePollutionTaxOnEngine engineInCar
    in
        Right (Just ("Pollution tax on given car: " ++ show calculatedTax ++ " Eur"), state)


stateTransition state View =
    Right (Just ("Current state: \n" ++ show state), state)
