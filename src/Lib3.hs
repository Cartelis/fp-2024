{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    Command(..),
    Statements(..)
    ) where

import Control.Concurrent ( Chan, readChan, writeChan, newChan )
import Control.Concurrent.STM(STM, TVar, writeTVar, atomically)
import qualified Lib2

import Control.Exception (try, IOException)
import Data.List (intercalate)
import Control.Concurrent.STM.TVar (readTVar, readTVarIO)
import Data.Either (isLeft)



data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = do
  op <- readChan chan
  case op of
    Save content ackChan -> do
      result <- try (writeFile "storage.txt" content) :: IO (Either IOException ())
      case result of
        Left ex -> do
          putStrLn $ "Error saving to file: " ++ show ex
        Right _ -> do
          putStrLn "Successfully saved content"
      writeChan ackChan ()
      storageOpLoop chan

    Load replyChan -> do
      result <- try (readFile "storage.txt") :: IO (Either IOException String)
      case result of
        Left ex -> do
          putStrLn $ "Error loading from file: " ++ show ex
          writeChan replyChan ""
        Right content -> do
          putStrLn "Successfully loaded content from file"
          writeChan replyChan content
      storageOpLoop chan


data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)


data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)


-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand input =
  case Lib2.parseAlphaNumWhitespaceOrdered (removeNewLines input) of
    Right(word, r1) ->
      if word == "Load" then
        Right (LoadCommand, r1)
      else if word == "Save" then
        Right (SaveCommand, r1)
      else
        case parseStatements (removeNewLines input) of
          Right(statements, r2) -> Right (StatementCommand statements, r2)
          Left e2 -> Left e2
    Left e1 -> Left e1


-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements input =
  case Lib2.parseAlphaNumWhitespaceOrdered input of
    Right(word, r1) ->
      if word == "BEGIN" then
        case parseListOfStatements r1 of
          Right(qs, r2) ->
            case Lib2.parseAlphaNumWhitespaceOrdered r2 of
              Right(wordEnd, r4) ->
                if wordEnd == "END" then
                  Right (Batch qs, r4)
                else
                  Left "END word not found or query written wrong"
              Left e4 -> Left e4
          Left e2 -> Left e2
      else
        case Lib2.parseQuery input of
          Right (q, r3) -> Right (Single q, r3)
          Left e3 -> Left e3
    Left e1 -> Left e1


parseListOfStatements :: String -> Either String ([Lib2.Query], String)
parseListOfStatements = Lib2.and2' (:) Lib2.parseQuery (Lib2.many Lib2.parseQuery)


removeNewLines :: String -> String
removeNewLines = filter (/= '\n')


-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState state = Single (Lib2.CarGarage (Lib2.stateGarage state))


-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements stmts =
  case stmts of
    Single query -> renderQuery query
    Batch queries ->
      "BEGIN " ++ concatMap renderQuery queries ++ "END " 


renderQuery :: Lib2.Query -> String
renderQuery query =
  case query of
    (Lib2.CarGarage garage) -> "CarGarage" ++ renderGarage garage
    (Lib2.AddCar car) -> "AddCar " ++ showCarAsQuery car
    (Lib2.RemoveCar car) -> "RemoveCar " ++ showCarAsQuery car
    (Lib2.EditCar car1 car2) -> "EditCar " ++ showCarAsQuery car1 ++ "to " ++ showCarAsQuery car2
    Lib2.ListCars -> "ListCars "
    (Lib2.CalculatePollutionTax car) -> "CalculatePollutionTax " ++ showCarAsQuery car
    Lib2.View -> "View "


renderGarage :: Lib2.Garage -> String
renderGarage garage = " Garage " ++ Lib2.garage_name garage ++ " " ++ showCarListAsQuery (Lib2.cars garage) ++ "(" ++ renderInnerGarage (Lib2.inner_garage garage) ++ ") "


renderInnerGarage :: [Lib2.Garage] -> String
renderInnerGarage [] = " "
renderInnerGarage (g:gs) = renderGarage g ++ renderInnerGarage gs


showCarListAsQuery :: [Lib2.Car] -> String
showCarListAsQuery cars = intercalate "" (map showCarAsQuery cars)


showCarAsQuery :: Lib2.Car -> String
showCarAsQuery givenCar =
  "Car " ++ Lib2.make givenCar ++ " " ++ Lib2.model givenCar ++ " " ++ Lib2.color givenCar ++ " " ++ Lib2.body_type givenCar ++
  " " ++ showPowertrainAsQuery (Lib2.powertrain givenCar) ++ showConsumptionAsQuery givenCar ++ show (Lib2.mileage givenCar) ++ "km "


showPowertrainAsQuery :: Lib2.Powertrain -> String
showPowertrainAsQuery givenPowertrain = showEngineAsQuery (Lib2.engine givenPowertrain) ++ Lib2.drive_type givenPowertrain ++ " " ++ Lib2.transmission givenPowertrain ++ " "


showEngineAsQuery :: Lib2.Engine -> String
showEngineAsQuery givenEngine =
  case givenEngine of
    Lib2.FuelEngine disp lay pow torq induct fuel ->  show disp ++ " L " ++ lay ++ " " ++ show pow ++ "kW " ++
      show torq ++ "Nm " ++ induct ++ " " ++ fuel ++ " "

    Lib2.ElectricEngine pow torq -> show pow ++ "kW " ++ show torq ++ "Nm " ++ "Electric "


showConsumptionAsQuery :: Lib2.Car -> String
showConsumptionAsQuery givenCar =
  if Lib2.consumption givenCar /= 0
    then
      case Lib2.engine (Lib2.powertrain givenCar) of
        Lib2.FuelEngine {} ->
          show (Lib2.consumption givenCar) ++ "l/100km "
        Lib2.ElectricEngine _ _ ->
          show (Lib2.consumption givenCar) ++ "kWh/100km "
    else
      ""


-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp ->
                   IO (Either String (Maybe String))
stateTransition stateVar command ioChan = do
  case command of
    StatementCommand (Batch queries) -> do
      results <- atomically $ mapM (processSingleQuery stateVar) queries

      mapM_ printQueryResponse results

      if any isLeft results
        then return $ Left "One or more queries in the batch failed."
        else return $ Right $ Just "Batch of queries proccessed succesfully"


    StatementCommand (Single query) -> do
      result <- atomically $ processSingleQuery stateVar query
      printQueryResponse result

      if isLeft result
        then return $ Left "Query failed."
        -- else return $ Right Nothing
        else
          case result of 
            Right (Just msg) -> return $ Right (Just msg)
            Right Nothing -> return $ Right Nothing
            Left errMsg -> return $ Left errMsg


    LoadCommand -> do
      responceChan <- newChan
      writeChan ioChan (Load responceChan)
      loadedStateAsString <- readChan responceChan
      case parseStatements loadedStateAsString of
        Right (parsedState, _) ->
          case parsedState of
            Single (Lib2.CarGarage stateGarage) -> do
              atomically $ writeTVar stateVar $ Lib2.State stateGarage
              return $ Right $ Just "State loaded\n"
            _ -> return $ Left "Failed to load state from the file"
        Left _ ->
          return $ Left "Failed to parse the loaded state"


    SaveCommand -> do
      -- Getting state
      currentState <- readTVarIO stateVar
      let statementsAsString = renderStatements $ marshallState currentState
      -- Creating Chan () for response
      responceChan <- newChan
      -- Sending save operation to channel
      writeChan ioChan (Save statementsAsString responceChan)
      -- Waiting for response
      _ <- readChan responceChan
      return $ Right $ Just "State saved\n"


processSingleQuery :: TVar Lib2.State -> Lib2.Query -> STM (Either String (Maybe String))
processSingleQuery givenState query = do
      currentState <- readTVar givenState
      case Lib2.stateTransition currentState query of
        Right (msg, updatedState) -> do
          writeTVar givenState updatedState
          case msg of
            Just str -> return $ Right $ Just str
            Nothing -> return $ Right Nothing
        Left err -> return $ Left err


printQueryResponse :: Either String (Maybe String) -> IO ()
printQueryResponse (Left err) = putStrLn $ "Failed: " ++ err
printQueryResponse (Right Nothing) = putStrLn "Success: No message returned"
printQueryResponse (Right (Just msg)) = putStrLn $ "Success: " ++ msg