{-# LANGUAGE ImportQualifiedPost #-}
module Parsers
    ( parseQuery,
    AppState(..),
    emptyState,
    parseAlphaNumWhitespaceOrdered,
    parseText,
    and2',
    many,
    parseCarWithPrefix,
    parseListOfCars,
    parseCarGarage,
    parseCommand,
    parse,
    ) where


import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT, catchE)
import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Control.Monad.Trans.Class(lift)

import qualified Data.Char as C
import qualified Data.List as L
import Lib2 qualified
import Lib3 qualified

type Parser a = ExceptT String (State String) a
-- Old Parser:
-- type Parser a = String -> Either String (a, String)

parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runExceptT parser)


-- Lib3 parsers



parseCommand :: Parser Lib3.Command
parseCommand = do
    input <- lift get
    case parse parseAlphaNumWhitespaceOrdered (removeNewLines input) of
        (Right word, r1) ->
            if word == "Load" then
                lift $ put r1 >> return Lib3.LoadCommand
            else if word == "Save" then
                lift $ put r1 >> return Lib3.SaveCommand
            else
                case parse parseStatements (removeNewLines input) of
                    (Right statements, r2) -> lift $ put r2 >> return (Lib3.StatementCommand statements)
                    (Left e2, _) -> throwE e2
        (Left e1, _) -> throwE e1


-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: Parser Lib3.Statements
parseStatements = do
    input <- lift get
    case parse parseAlphaNumWhitespaceOrdered input of
        (Right word, r1) ->
            if word == "BEGIN" then
                case parse parseListOfStatements r1 of
                    (Right qs, r2) ->
                        case parse parseAlphaNumWhitespaceOrdered r2 of
                            (Right wordEnd, r4) ->
                                if wordEnd == "END" then
                                    lift $ put r4 >> return (Lib3.Batch qs)
                                else
                                    throwE "END word not found or query written wrong"
                            (Left e4, _) -> throwE e4
                    (Left e2, _) -> throwE e2
            else
                case parse parseQuery input of
                    (Right q, r3) -> lift $ put r3 >> return (Lib3.Single q)
                    (Left e3, _) -> throwE e3
        (Left e1, _) -> throwE e1


parseListOfStatements :: Parser [Lib2.Query]
parseListOfStatements = and2' (:) parseQuery (many parseQuery)


removeNewLines :: String -> String
removeNewLines = filter (/= '\n')



-- Lib2 parsers



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
or2 a b = do
    resultA <- a `catchE` \e1 -> do
        resultB <- b
        return resultB `catchE` \_ -> throwE e1
    return resultA

-- or2 :: Parser a -> Parser a -> Parser a
-- or2 a b = do
--     resultA <- a `catchE` \_ -> do
--         input <- lift get
--         lift (put input)
--         b
--     return resultA


-- or2 :: Parser a -> Parser a -> Parser a
-- or2 a b = \input ->
--     case a input of
--         Right r1 -> Right r1
--         Left e1 ->
--             case b input of
--                 Right r2 -> Right r2
--                 -- Left e2 -> Left (e1 ++ ", " ++ e2)
--                 Left _ -> Left e1


or3 :: Parser a -> Parser a -> Parser a -> Parser a
or3 a b c = do
    resultA <- a
    return resultA `catchE` \e1 -> do
        resultB <- b
        return resultB `catchE` \e2 -> do
            resultC <- c
            return resultC `catchE` \e3 -> throwE (e1 ++ ", " ++ e2 ++ ", " ++ e3)


-- or3 :: Parser a -> Parser a -> Parser a -> Parser a
-- or3 a b c = \input ->
--     case a input of
--         Right r1 -> Right r1
--         Left e1 ->
--             case b input of
--                 Right r2 -> Right r2
--                 Left e2 ->
--                     case c input of
--                         Right r3 -> Right r3
--                         Left e3 -> Left (e1 ++ ", " ++ e2 ++ ", " ++ e3)


many :: Parser a -> Parser [a]
many p = (do
    x <- p
    xs <- many p
    return (x : xs)
  ) `catchE` \_ -> return []


-- many :: Parser a -> Parser [a]
-- many p = many' p []
--     where
--         many' p' acc = \input ->
--             case p' input of
--                 Left _ -> Right (acc, input)
--                 Right (v, r) -> many' p' (acc ++ [v]) r


and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' c a b = do
    resultA <- a
    resultB <- b
    return (c resultA resultB)


-- and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
-- and2' c a b = \input ->
--     case a input of
--         Right (v1, r1) ->
--             case b r1 of
--                 Right (v2, r2) -> Right (c v1 v2, r2)
--                 Left e2 -> Left e2
--         Left e1 -> Left e1


and3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3 d a b c = do
    resultA <- a
    resultB <- b
    resultC <- c
    return (d resultA resultB resultC)


-- and3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
-- and3 d a b c = \input ->
--     case a input of
--         Right (v1, r1) ->
--             case b r1 of
--                 Right (v2, r2) ->
--                     case c r2 of
--                         Right (v3, r3) -> Right (d v1 v2 v3, r3)
--                         Left e3 -> Left e3
--                 Left e2 -> Left e2
--         Left e1 -> Left e1


parseChar :: Char -> Parser Char
parseChar c = do
    input <- lift get
    case input of
        [] -> throwE ("Cannot find " ++ [c] ++ " in an empty input")
        s@(h:t) -> if c == h then lift $ put t >> return h else throwE (c : " is not found in " ++ s)


parseWhitespace :: Parser Char
parseWhitespace = do
    input <- lift get
    case input of
        [] -> throwE "Cannot find any whitespace in an empty input"
        s@(h:t) -> if ' ' == h then lift $ put t >> return ' ' else throwE (s ++ " does not start with a whitespace")


-- <digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
parseNumber :: Parser Integer
parseNumber = do
    input <- lift get
    case input of
        [] -> throwE "Cannot parse a number in an empty input"
        str ->
            let
                digits = L.takeWhile C.isDigit str
                rest = drop (length digits) str
            in
                case digits of
                    [] -> throwE "Not a number"
                    _ -> lift $ put rest >> return (read digits)


-- <letter> ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
parseText :: Parser String
parseText = do
    input <- lift get
    case input of
        [] -> throwE "Cannot find any text made out of letters in an empty input"
        str ->
            let
                letters = L.takeWhile C.isLetter str
                rest = drop (length letters) str
            in
                case letters of
                    [] -> throwE "Not a letter"
                    _ -> lift $ put rest >> return letters


-- <string> ::= <letter> | <letter> <string> | <digit> | <digit> <string>
parseAlphaNum :: Parser String
parseAlphaNum = do
    input <- lift get
    case input of
        [] -> throwE "Cannot find any text made out of letters or digits in an empty input"
        str ->
            let
                lettersDigits = L.takeWhile (\c -> C.isLetter c || C.isDigit c) str
                rest = drop (length lettersDigits) str
            in
                case lettersDigits of
                    [] -> throwE "Not a letter or a digit"
                    _ -> lift $ put rest >> return lettersDigits


isDot :: Char -> Bool
isDot c = c == '.'


-- <fuel_consumption> ::= <digit> "." <digit> | <digit> <fuel_consumption>
parseDouble :: Parser Double
parseDouble = do
    input <- lift get
    case input of
        [] -> throwE "Cannot parse a double number in an empty input"
        str ->
            let
                doubleNum = L.takeWhile (\c -> C.isDigit c || isDot c) str
                rest = drop (length doubleNum) str
            in
                case doubleNum of
                    [] -> throwE "Not a digit or dot"
                    _ -> lift $ put rest >> return (read doubleNum)


-- <engine_layout> ::= <string>
-- <garage_name> ::= <string>
parseAlphaNumWhitespaceOrdered :: Parser String
parseAlphaNumWhitespaceOrdered = and2' (\a _ -> a) parseAlphaNum parseWhitespace


-- <displacement> ::= <digit> "." <digit> " L" | <digit> <displacement>
parseDisplacement :: Parser Double
parseDisplacement = and2' (\a _ -> a) parseDouble (and3 (\_ b _ -> b) parseWhitespace (parseChar 'L') parseWhitespace)


-- <opt_consumption> ::= " " <fuel_consumption> "l/100km " | " " <fuel_consumption> "kWh/100km " | " "
parseFuelConsumption :: Parser Double
parseFuelConsumption = do
    input <- lift get
    case parse parseFuelConsumptionInner input of    
        (Right (v, measurement), r) ->
            if measurement == "l/100km" || measurement == "kWh/100km"
                then lift $ put r >> return v
                else throwE ("Not supported fuel measurement: " ++ measurement)
        (Left e1, _) ->
            if e1 == "Cannot find forward slash"
                then lift $ put input >> return 0
                else throwE e1


parseFuelConsumptionInner :: Parser (Double, String)
parseFuelConsumptionInner = and3 (\a b _ -> (a, b)) parseDouble
                        (and3 (\a b c -> a ++ [b] ++ c) parseAlphaNum parseForwardSlash parseAlphaNum)
                        parseWhitespace


parseForwardSlash :: Parser Char
parseForwardSlash = do
    input <- lift get
    case input of
        [] -> throwE "Cannot find forward slash in an empty input"
        (h : t)
          | h == '/' -> lift $ put t >> return h
          | h == '\\' -> throwE "Wrong fuel consumption format"
          | otherwise -> throwE "Cannot find forward slash"


-- <power> ::= <digit> "kW" | <digit> <power>
-- <torque> ::= <digit> "Nm" | <digit> <torque>
-- <mileage> ::= <digit> "km " | <digit> <mileage>
parseNumberTextWhitespace :: Parser Integer
parseNumberTextWhitespace = and3 (\a _ _ -> a) parseNumber parseText parseWhitespace


-- <induction> ::= "Turbocharged" | "Supercharged" | "Naturally aspirated"
parseInduction :: Parser String
parseInduction = do
    input <- lift get
    case parse parseAlphaNumWhitespaceOrdered input of
        (Right v1, r1) ->
            if v1 == "Naturally"
                then
                    case parse parseAlphaNumWhitespaceOrdered r1 of
                        (Right v2, r2) ->
                            if v2 == "aspirated"
                                then lift $ put r2 >> return (v1 ++ " " ++ v2)
                                else throwE ("Not supported induction type: " ++ v1 ++ " " ++ v2)
                        (Left e2, _) -> throwE e2
                else if v1 == "Turbocharged" || v1 == "Supercharged"
                    then lift $ put r1 >> return v1
                    else throwE ("Not supported induction type: " ++ v1)
        (Left e1, _) -> throwE e1


-- <fuel> ::= "Diesel" | "Petrol" | "Hydrogen"
parseFuel :: Parser String
parseFuel = do
    input <- lift get
    case parse parseAlphaNumWhitespaceOrdered input of
        (Right v, r) ->
            if elem v fuelList
                then lift $ put r >> return v
                else throwE ("Not supported fuel: " ++ v)
        (Left e1, _) -> throwE e1


parseFuelEngine :: Parser Lib2.Engine
parseFuelEngine = do
    input <- lift get
    case parse parseFuelEngineInfo input of
        (Right ((disp, layout, pow), (torq, ind, fuelType)), r) ->
            lift $ put r >> return (Lib2.FuelEngine disp layout pow torq ind fuelType)
        (Left e1, _) -> throwE e1


parseFuelEngineInfo :: Parser ((Double, String, Integer), (Integer, String, String))
parseFuelEngineInfo = and2' (,)
                    (and3 (,,) parseDisplacement parseAlphaNumWhitespaceOrdered parseNumberTextWhitespace)
                    (and3 (,,) parseNumberTextWhitespace parseInduction parseFuel)


parseElectricEngine :: Parser Lib2.Engine
parseElectricEngine = do
    input <- lift get
    case parse (and3 (\a b _ -> (a, b)) parseNumberTextWhitespace parseNumberTextWhitespace parseAlphaNumWhitespaceOrdered) input of
                                (Right (pow, torq), r) -> lift $ put r >> return (Lib2.ElectricEngine pow torq)
                                (Left e1, _) -> throwE e1


-- <engine> ::= <displacement> " " <engine_layout> " " <power> " " <torque> " " <induction> " " <fuel> | <power> " " <torque> " Electric"
parseEngine :: Parser Lib2.Engine
parseEngine = or2 parseFuelEngine parseElectricEngine


-- <drive_type> ::= "AWD" | "RWD" | "FWD" | "4WD"
parseDriveType :: Parser String
parseDriveType = do
    input <- lift get
    case parse parseAlphaNumWhitespaceOrdered input of
        (Right v, r) ->
            if elem v driveTypeList
                then lift $ put r >> return v
                else throwE ("Not supported drive type: " ++ v)
        (Left e1, _) -> throwE e1


-- <transmission> ::= "Manual" | "Automatic"
parseGearbox :: Parser String
parseGearbox = do
    input <- lift get
    case parse parseAlphaNumWhitespaceOrdered input of
        (Right v, r) ->
            if elem v gearboxList
                then lift $ put r >> return v
                else throwE ("Not supported gearbox: " ++ v)
        (Left e1, _) -> throwE e1


-- <powertrain> ::= <engine> " " <drive_type> " " <transmission>
parsePowertrain :: Parser Lib2.Powertrain
parsePowertrain = do
    input <- lift get
    case parse parsePowertrainInfo input of
        (Right (engine1, drive_type1, trans), r) ->
            lift $ put r >> return (Lib2.Powertrain engine1 drive_type1 trans)
        (Left e1, _) -> throwE e1


parsePowertrainInfo :: Parser (Lib2.Engine, String, String)
parsePowertrainInfo = and3 (,,) parseEngine parseDriveType parseGearbox


parseModelNameOne :: Parser String
parseModelNameOne = do
    input <- lift get
    case input of
        [] -> throwE "Cannot find any model name made out od letters or digits in an empty input"
        str ->
            case parse parseAlphaNumWhitespaceOrdered str of
                (Right v, r) ->
                    if elem v colorList
                        then throwE "Color read"
                        else lift $ put r >> return v
                (Left e1, _) -> throwE e1


trimTrailing :: String -> String
trimTrailing = reverse . dropWhile C.isSpace . reverse


-- <model_name> ::= <string> " " | <string> " " <model_name>
parseModelName :: Parser String
parseModelName = do
    input <- lift get 
    case parse parseModelName' input of
        (Right stringList, r) -> lift $ put r >> return (trimTrailing (concatStringListToString stringList))
        (Left e, _) -> throwE e


parseModelName' :: Parser [String]
parseModelName' = and2' (:) parseModelNameOne (many parseModelNameOne)


concatStringListToString :: [String] -> String
concatStringListToString [] = ""
concatStringListToString (h:t) = h ++ " " ++ concatStringListToString t


-- <color> ::= "Red" | "Black" | "Gray" | "White" | "Blue" | "Silver" | "Green" | "Brown" | "Orange" | "Yellow" | "Gold" | "Purple"
parseColor :: Parser String
parseColor = do
    input <- lift get
    case parse parseAlphaNumWhitespaceOrdered input of
        (Right v, r) ->
            if elem v colorList
                then lift $ put r >> return v
                else throwE ("Not supported color: " ++ v)
        (Left e1, _) -> throwE e1


-- <make> ::= <stringMake>
parseMake :: Parser String
parseMake = and2' (\a _ -> a) parseAlphaWithMinus parseWhitespace


-- <stringMake> ::= <letterOrMinus> | <letterOrMinus> <stringMake>
-- <letterOrMinus> ::= <letter> | "-"
parseAlphaWithMinus :: Parser String
parseAlphaWithMinus = do
    input <- lift get
    case input of
        [] -> throwE "Cannot find any text made out of letters or - in an empty input"
        str ->
            let
                lettersMinus = L.takeWhile (\c -> C.isLetter c || c == '-') str
                rest = drop (length lettersMinus) str
            in
                case lettersMinus of
                    [] -> throwE "Not a letter or -"
                    _ -> lift $ put rest >> return lettersMinus


parseAlphaNumWithMinus :: Parser String
parseAlphaNumWithMinus = do
    input <- lift get
    case input of
        [] -> throwE "Cannot find any text made out of letters or digits in an empty input"
        str ->
            let
                lettersDigits = L.takeWhile (\c -> C.isLetter c || C.isDigit c || c == '-') str
                rest = drop (length lettersDigits) str
            in
                case lettersDigits of
                    [] -> throwE "Not a letter, a digit or -"
                    _ -> lift $ put rest >> return lettersDigits


parseAlphaNumWithMinusWhitespace :: Parser String
parseAlphaNumWithMinusWhitespace = and2' (\a _ -> a) parseAlphaNumWithMinus parseWhitespace


-- <body_type> ::= "Sedan" | "Coupe" | "Hatchback" | "Pickup" | "Off-road" | "Sport" | "Van" | "Convertible" | "Crossover" | "SUV" | "Wagon" | "Muscle" | "Compact"
parseBodyType :: Parser String
parseBodyType = do
    input <- lift get
    case parse parseAlphaNumWithMinusWhitespace input of
        (Right v, r) ->
            if elem v bodyTypeList
                then lift $ put r >> return v
                else throwE ("Not supported body type: " ++ v)
        (Left e1, _) -> throwE e1


-- <model> ::= <model_name> <color> " " <body_type> " " <powertrain> <opt_consumption> <mileage>
parseModel :: Parser ((String, String, String), (Lib2.Powertrain, Double, Integer))
parseModel = and2' (,) (and3 (,,) parseModelName parseColor parseBodyType)
                        (and3 (,,) parsePowertrain parseFuelConsumption parseNumberTextWhitespace)


parseCar :: Parser Lib2.Car
parseCar = do
    input <- lift get
    case parse parseCarInfo input of
        (Right (make', ((model', color', body_type'), (powertrain', consumption', mileage'))), r) ->
            lift $ put r >> return (Lib2.Car make' model' color' body_type' powertrain' consumption' mileage')
        (Left e, _) -> throwE e


parseCarInfo :: Parser (String, ((String, String, String), (Lib2.Powertrain, Double, Integer)))
parseCarInfo = and2' (,) parseMake parseModel


parseCarPrefix :: Parser Bool
parseCarPrefix = do
    input <- lift get
    case parse parseAlphaNumWhitespaceOrdered input of
                            (Right v, r) -> lift $ put r >> return (v == "Car")
                            (Left err, _) -> throwE err


-- <car> ::= "Car " <make> " " <model>
parseCarWithPrefix :: Parser Lib2.Car
parseCarWithPrefix = do
    input <- lift get
    case parse parseCarPrefix input of
        (Right carPrefixFound, r) ->
            if carPrefixFound
                -- then lift $ put r >> parseCar
                then do
                    lift $ put r
                    parseCar
                else throwE "Prefix 'Car' not found"
        (Left e1, _) -> throwE e1


-- <car_list> ::= <car> | <car> <car_list>
parseListOfCars :: Parser [Lib2.Car]
parseListOfCars = and2' (:) parseCarWithPrefix
                        (many parseCarWithPrefix)


-- <CarGarage> ::= "CarGarage" <garage>
parseCarGarage :: Parser Lib2.Query
parseCarGarage = do
    input <- lift get
    case parse parseAlphaNumWhitespaceOrdered input of
        (Right prefix, r1) ->
            if prefix == "CarGarage"
                then
                    case parse parseGarage r1 of
                        (Right garage, r2) -> lift $ put r2 >> return (Lib2.CarGarage garage)
                        (Left e2, _) -> throwE e2
                else throwE "Not supported prefix/command"
        (Left e1, _) -> throwE e1


-- <garage> ::= " Garage " <garage_name> " " <car_list> "(" <inner_garage> ") "
parseGarage :: Parser Lib2.Garage
parseGarage = do
    input <- lift get
    case parse parseGarageInfo input of
        (Right (name, carList), r1) ->
            case parse parseGarage' r1 of
                (Right garages, r2) -> lift $ put r2 >> return (Lib2.Garage name carList garages)
                (Left _, _) -> lift $ put r1 >> return (Lib2.Garage name carList [])
        (Left e1, _) -> throwE e1


parseGarageInfo :: Parser (String, [Lib2.Car])
parseGarageInfo = do
    input <- lift get
    case parse parseAlphaNumWhitespaceOrdered input of
        (Right w, r) ->
            if w == "Garage"
                then do
                    lift $ put r
                    and2' (,) parseAlphaNumWhitespaceOrdered parseListOfCars
                else throwE ("Prefix Garage not found: " ++ w)
        (Left e, _) -> throwE e


-- <inner_garage> ::= <garage> | <garage> <inner_garage> | " "
parseGarage' :: Parser [Lib2.Garage]
parseGarage' = do
    input <- lift get
    case input of
        [] -> throwE ""
        str ->
            case parse parseParenthesesStartWhitespace str of
                (Right _, r1) ->
                    case parse parseGarage r1 of
                        (Right garage, r2) ->
                            case parse parseGarage' r2 of
                                (Right garages, r3) ->
                                    lift $ put r3 >> return (garage : garages)
                                (Left _, _) ->
                                    case parse parseParenthesesEndWhitespace r2 of
                                        (Right _, r3) -> lift $ put r3 >> return [garage]
                                        (Left e3, _) -> throwE e3
                        (Left _, _) ->
                            case parse parseParenthesesEndWhitespace r1 of
                                (Right _, r2) -> lift $ put r2 >> return []
                                (Left e2, _) -> throwE e2
                (Left _, _) ->
                    case parse parseWhitespace input of
                        (Right _, r1) ->
                            case parse parseGarage r1 of
                                (Right garage, r2) ->
                                    case parse parseGarage' r2 of
                                        (Right garages, r3) ->
                                            lift $ put r3 >> return (garage : garages)
                                        (Left _, _) ->
                                            case parse parseParenthesesEndWhitespace r2 of
                                                (Right _, r3) -> lift $ put r3 >> return [garage]
                                                (Left e3, _) -> throwE e3
                                (Left _, _) ->
                                    case parse parseParenthesesEndWhitespace r1 of
                                        (Right _, r2) -> lift $ put r2 >> return []
                                        (Left e2, _) -> throwE e2
                        (Left e1, _) -> throwE e1


parseParenthesesStartWhitespace :: Parser Bool
parseParenthesesStartWhitespace = do
    input <- lift get
    case input of
        [] -> throwE "Parentheses start and whitespace not found"
        str ->
            case parse (parseChar '(') str of
                (Right _, r1) ->
                    case parse parseWhitespace r1 of
                        (Right _, r2) -> lift $ put r2 >> return True
                        (Left e2, _) -> throwE e2
                (Left e1, _) -> throwE e1


parseParenthesesEndWhitespace :: Parser Bool
parseParenthesesEndWhitespace = do
    input <- lift get
    case input of
        [] -> throwE "Parentheses end and whitespace not found"
        str ->
            case parse (parseChar ')') str of
                (Right _, r1) ->
                    case parse parseWhitespace r1 of
                        (Right _, r2) -> lift $ put r2 >> return True
                        (Left e2, _) -> throwE e2
                (Left e1, _) -> throwE e1


-- <command> ::= <command_type> " " <car> | "EditCar " <car> "to " <car> | "ListCars" | "View"
parseCommandBNF :: Parser Lib2.Query
parseCommandBNF = parseAddOrRemoveOrCalculatePollutionCar `or2` parseEditCar `or2` parseListCars `or2` parseView


-- <command_type> ::= "AddCar" | "RemoveCar" | "CalculatePollutionTax"
parseAddOrRemoveOrCalculatePollutionCar :: Parser Lib2.Query
parseAddOrRemoveOrCalculatePollutionCar = do
    input <- lift get
    case input of
        [] -> throwE "Cannot parse empty input"
        str ->
            case parse parseAlphaNumWhitespaceOrdered str of
                (Right word, r1) ->
                    if word == "AddCar"
                        then case parse parseCarWithPrefix r1 of
                            (Right car, r2) -> lift $ put r2 >> return (Lib2.AddCar car)
                            (Left e2, _) -> throwE e2
                        else if word == "RemoveCar"
                            then case parse parseCarWithPrefix r1 of
                                (Right car, r2) -> lift $ put r2 >> return (Lib2.RemoveCar car)
                                (Left e2, _) -> throwE e2
                        else if word == "CalculatePollutionTax"
                            then case parse parseCarWithPrefix r1 of
                                (Right car, r2) -> lift $ put r2 >> return (Lib2.CalculatePollutionTax car)
                                (Left e2, _) -> throwE e2
                        else throwE "Given command not found"
                (Left e1, _) -> throwE e1


parseEditCar :: Parser Lib2.Query
parseEditCar = do
    input <- lift get
    case input of
        [] -> throwE "Cannot parse empty input"
        str ->
            case parse parseAlphaNumWhitespaceOrdered str of
                (Right word1, r1) ->
                    if word1 == "EditCar"
                        then case parse parseCarWithPrefix r1 of
                            (Right oldCar, r2) -> 
                                case parse parseAlphaNumWhitespaceOrdered r2 of
                                    (Right word2, r3) ->
                                        if word2 == "to"
                                            then case parse parseCarWithPrefix r3 of
                                                (Right newCar, r4) -> lift $ put r4 >> return (Lib2.EditCar oldCar newCar)
                                                (Left e4, _) -> throwE e4
                                            else throwE "Given command not found"
                                    (Left e3, _) -> throwE e3
                            (Left e2, _) -> throwE e2
                        else throwE "Given command not found"
                (Left e1, _) -> throwE e1


parseView :: Parser Lib2.Query
parseView = do
    input <- lift get
    case input of
        [] -> throwE "Cannot parse empty input"
        str ->
            -- case parseText input of
            case parse parseAlphaNumWhitespaceOrdered str of
                (Right word, r1) ->
                    if word == "View"
                        then lift $ put r1 >> return Lib2.View
                        else throwE "Given command not found"
                (Left e1, _) -> throwE e1


parseListCars :: Parser Lib2.Query
parseListCars = do
    input <- lift get
    case input of
        [] -> throwE "Cannot parse empty input"
        str ->
            -- case parseText input of
            case parse parseAlphaNumWhitespaceOrdered str of
                (Right word, r1) ->
                    if word == "ListCars"
                        then lift $ put r1 >> return Lib2.ListCars
                        else throwE "Given command not found"
                (Left e1, _) -> throwE e1


-- | Parses user's input.
-- The function must have tests.
parseQuery :: Parser Lib2.Query
parseQuery = do
    input <- lift get
    case input of
        [] -> throwE "Cannot parse empty input"
        str ->
            case parse (or2 parseCarGarage parseCommandBNF) str of
                (Right query, r) -> lift $ put r >> return query
                -- _ -> Left "Failed to parse query: Unknown command"
                (Left e, _) -> throwE ("Failed to parse query: " ++ e)


-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data AppState = AppState {
    stateGarage :: Lib2.Garage
} deriving (Show, Eq)


-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: AppState
emptyState = AppState {
    stateGarage = Lib2.Garage {
        Lib2.garage_name = "",
        Lib2.cars = [],
        Lib2.inner_garage = []
    }
}


removeFirstCarOccurence :: Lib2.Car -> [Lib2.Car] -> [Lib2.Car]
removeFirstCarOccurence _ [] = []
removeFirstCarOccurence carToRemove (h:t)
    | carToRemove == h
        = t
    | otherwise
        = h : removeFirstCarOccurence carToRemove t


editCarInList :: Lib2.Car -> Lib2.Car -> [Lib2.Car] -> [Lib2.Car]
editCarInList _ _ [] = []
editCarInList oldCar newCar (h:t)
    | oldCar == h
        = newCar : t
    | otherwise
        = h : editCarInList oldCar newCar t


-- Function to get all cars from a garage and its inner garages
getAllCars :: Lib2.Garage -> [Lib2.Car]
getAllCars garage = Lib2.cars garage ++ innerCars (Lib2.inner_garage garage)


-- Helper function to gather cars from inner garages
innerCars :: [Lib2.Garage] -> [Lib2.Car]
innerCars [] = []
innerCars (g:gs) = getAllCars g ++ innerCars gs

calculatePollutionTaxOnEngine :: Lib2.Engine -> Int
calculatePollutionTaxOnEngine givenEngine
    | Lib2.power givenEngine >= 400 = 360
    | Lib2.power givenEngine >= 250 = 280
    | Lib2.power givenEngine >= 150 = 160
    | otherwise = 0
