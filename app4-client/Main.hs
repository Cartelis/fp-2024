{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveFunctor #-}
module Main (main) where

import Data.ByteString
import Network.Wreq
import Data.String.Conversions
import Control.Lens

import Control.Monad.Free (Free (..), liftF)
import Data.IORef
import System.IO as SIO
import Lib2 qualified
import Lib3 qualified
import Parsers (parse, parseCommand)
import Control.Concurrent.MVar
import GHC.IO (unsafePerformIO)
import GHC.Conc (forkIO)
import GHC.Conc.IO (threadDelay)


data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)


data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)


data MyDomainAlgebra next = Load (() -> next)
                          | CarGarage String (() -> next)
                          | Add String (() -> next)
                          | Remove String (() -> next)
                          | Edit String String (() -> next)
                          | ListCars (String -> next)
                          | CalculatePollutionTax String (String -> next)
                          | View (String -> next)
                          | Save (() -> next)
                          deriving Functor

type MyDomain = Free MyDomainAlgebra

load :: MyDomain ()
load = liftF $ Load id

carGarage :: String -> MyDomain()
carGarage garage = liftF $ CarGarage garage id

add :: String -> MyDomain ()
add car = liftF $ Add car id

remove :: String -> MyDomain ()
remove car = liftF $ Remove car id

edit :: String -> String -> MyDomain ()
edit car1 car2 = liftF $ Edit car1 car2 id

listCars :: MyDomain String
listCars = liftF $ ListCars id

calculatePTax :: String -> MyDomain String
calculatePTax car = liftF $ CalculatePollutionTax car id

viewState :: MyDomain String
viewState = liftF $ View id

save :: MyDomain ()
save = liftF $ Save id





program :: MyDomain String
program = do
    -- _ <- viewState
    -- carGarage "TopSecret Car BMW M5 CS Green Sedan 4.4 L V8 465kW 860Nm Turbocharged Petrol AWD Automatic 20.8l/100km 152482km ( Garage GRToyota Car Toyota Supra GR Electric White Sport 350kW 700Nm Electric 4WD Automatic 74342km ( ) )"
    
    load
    
    -- add "BMW M850i Black Coupe 4.4 L V8 390kW 750Nm Turbocharged Petrol AWD Automatic 18153km"
    -- _ <- viewState
    
    edit "BMW M850i Black Coupe 4.4 L V8 390kW 750Nm Turbocharged Petrol AWD Automatic 18153km" "Mercedes-Benz AMG E 63 S 4MATIC Black Sedan 4.0 L V8 450kW 850Nm Turbocharged Petrol AWD Automatic 23.6l/100km 27602km"
    
    -- remove "Mercedes-Benz AMG E 63 S 4MATIC Black Sedan 4.0 L V8 450kW 850Nm Turbocharged Petrol AWD Automatic 23.6l/100km 27602km"
    -- returnedCars <- listCars
    -- add "Mercedes-Benz AMG E 63 S 4MATIC Black Sedan 4.0 L V8 450kW 850Nm Turbocharged Petrol AWD Automatic 23.6l/100km 27602km"
    
    add "Porsche 911 991 GT3 RS Yellow Sport 4.0 L Boxer6 370kW 469Nm Turbocharged Petrol RWD Automatic 17.9l/100km 42578km"
    
    -- cars <- listCars
    finalState <- viewState
    -- save
    -- tax <- calculatePTax "BMW M850i Black Coupe 4.4 L V8 390kW 750Nm Turbocharged Petrol AWD Automatic 18153km"
    return finalState





httpLock :: MVar ()
httpLock = unsafePerformIO $ newMVar ()




runHttpOne :: MyDomain a -> IO a
runHttpOne (Pure a) = return a
runHttpOne (Free step) = do
    next <- runStep step
    runHttpOne next
    where
        runStep :: MyDomainAlgebra a -> IO a
        runStep (Load next) = do
            let rawRequest = cs "Load " :: ByteString
            _ <- post "http://localhost:3000" rawRequest
            -- putStrLn $ cs $ resp ^. responseBody
            return $ next ()
        runStep (CarGarage garageString next) = do
            let rawRequest = cs ("CarGarage Garage " ++ garageString ++ " ") :: ByteString
            _ <- post "http://localhost:3000" rawRequest
            -- putStrLn $ cs $ resp ^. responseBody
            return $ next ()
        runStep (Add carString next) = do
            let rawRequest = cs ("AddCar Car " ++ carString ++ " ") :: ByteString
            _ <- post "http://localhost:3000" rawRequest
            -- putStrLn $ cs $ resp ^. responseBody
            return $ next ()
        runStep (Remove carString next) = do
            let rawRequest = cs ("RemoveCar Car " ++ carString ++ " ") :: ByteString
            _ <- post "http://localhost:3000" rawRequest
            -- putStrLn $ cs $ resp ^. responseBody
            return $ next ()
        runStep (Edit carString1 carString2 next) = do
            let rawRequest = cs ("EditCar Car " ++ carString1 ++ " to Car " ++ carString2 ++ " ") :: ByteString
            _ <- post "http://localhost:3000" rawRequest
            -- putStrLn $ cs $ resp ^. responseBody
            return $ next ()
        runStep (ListCars next) = do
            let rawRequest = cs "ListCars " :: ByteString
            resp <- post "http://localhost:3000" rawRequest
            -- putStrLn $ cs $ resp ^. responseBody
            return $ next (cs $ resp ^. responseBody)
        runStep (CalculatePollutionTax carString next) = do
            let rawRequest = cs ("CalculatePollutionTax Car " ++ carString ++ " ") :: ByteString
            resp <- post "http://localhost:3000" rawRequest
            -- putStrLn $ cs $ resp ^. responseBody
            return $ next (cs $ resp ^. responseBody)
        runStep (View next) = do
            let rawRequest = cs "View " :: ByteString
            resp <- post "http://localhost:3000" rawRequest
            -- putStrLn $ cs $ resp ^. responseBody
            return $ next (cs $ resp ^. responseBody)
        runStep (Save next) = do
            let rawRequest = cs "Save " :: ByteString
            _ <- post "http://localhost:3000" rawRequest
            -- putStrLn $ cs $ resp ^. responseBody
            return $ next ()




runCommandInMemory :: String -> IORef Lib2.State -> IO String
runCommandInMemory commandString state = do
    case parse parseCommand commandString of
        (Left e, _) -> do
            return ("Error parsing input: " ++ show e)

        (Right c, "") -> do
            case c of
                Lib3.StatementCommand (Lib3.Single query) -> do
                    extractedState <- readIORef state
                    case Lib2.stateTransition extractedState query of
                        Right (msg, updatedState) -> do
                            writeIORef state updatedState
                            case msg of
                                Just str -> return str
                                Nothing -> return "Success"
                        Left err -> do
                            return ("Error in state transition: " ++ show err)
                _ -> return "Invalid command"

        (Right _, r) -> do
            return ("Remaining input: " ++ show r)




runTest :: MyDomain a -> IO a
runTest p = do
    v <- newIORef Lib2.emptyState

    -- initialState <- readIORef v
    -- putStrLn $ "Initial State: " ++ show initialState

    runTest' v p

    -- finalState <- readIORef v
    -- putStrLn $ "Final State: " ++ show finalState

    -- If uncomment edit "runTest' v p" to "result <- runTest' v p"
    -- return result
    where
        runTest' :: IORef Lib2.State ->  MyDomain a -> IO a
        runTest' _ (Pure a) = return a
        runTest' v (Free step) = do
            next <- runStep v step
            runTest' v next
        runStep :: IORef Lib2.State -> MyDomainAlgebra a -> IO a
        runStep v (Load next) = do
            a <- SIO.readFile "./data"
            _ <- runCommandInMemory a v
            return $ next ()
        runStep v (CarGarage garageString next) = do
            _ <- runCommandInMemory ("CarGarage Garage " ++ garageString ++ " ") v
            return $ next ()
        runStep v (Add carString next) = do
            _ <- runCommandInMemory ("AddCar Car " ++ carString ++ " ") v
            return $ next ()
        runStep v (Remove carString next) = do
            _ <- runCommandInMemory ("RemoveCar Car " ++ carString ++ " ") v
            return $ next ()
        runStep v (Edit carString1 carString2 next) = do
            _ <- runCommandInMemory ("EditCar Car " ++ carString1 ++ " to Car " ++ carString2 ++ " ") v
            return $ next ()
        runStep v (ListCars next) = do
            result <- runCommandInMemory "ListCars " v
            return $ next result
        runStep v (CalculatePollutionTax carString next) = do
            result <- runCommandInMemory ("CalculatePollutionTax Car " ++ carString ++ " ") v
            return $ next result
        runStep v (View next) = do
            a <- readIORef v
            return $ next $ show a
        runStep v (Save next) = do
            a <- readIORef v
            SIO.writeFile "./data" (Lib3.renderStatements (Lib3.marshallState a))
            return $ next ()




runHttpSmart :: MyDomain a -> IO a
runHttpSmart p = do
    v <- newIORef Lib2.emptyState

    -- To load the saved state to active state in server
    let rawRequestLoad = cs "Load " :: ByteString
    _ <- post "http://localhost:3000" rawRequestLoad

    let rawRequestGetState = cs "GetState" :: ByteString
    serverState <- post "http://localhost:3000" rawRequestGetState
    -- putStrLn $ "\n\nServer state before: " ++ cs (serverState ^. responseBody)

    let serverStateString = cs (serverState ^. responseBody)

    result <- runHttpSmart' v serverStateString p

    finalState <- readIORef v
    -- putStrLn $ "\n\nTest state after: " ++ show finalState

    let finalStateString = Lib3.renderStatements (Lib3.marshallState finalState)

    if serverStateString == finalStateString
        then return ()
        else do
            let rawRequestCarGarage = cs finalStateString :: ByteString
            _ <- post "http://localhost:3000" rawRequestCarGarage
            threadDelay 3000000
            let rawRequestSave = cs "Save " :: ByteString
            _ <- post "http://localhost:3000" rawRequestSave
            return ()

    -- let rawRequestGetStateFinal = cs "GetState" :: ByteString
    -- serverStateFinal <- post "http://localhost:3000" rawRequestGetStateFinal
    -- putStrLn $ "\n\nServer state after: " ++ cs (serverStateFinal ^. responseBody)


    return result
    where
        runHttpSmart' :: IORef Lib2.State ->  String -> MyDomain a -> IO a
        runHttpSmart' _ _ (Pure a) = return a
        runHttpSmart' v serverStateString (Free step) = do
            next <- runStep v serverStateString step
            runHttpSmart' v serverStateString next
        runStep :: IORef Lib2.State -> String -> MyDomainAlgebra a -> IO a
        runStep v serverStateString (Load next) = do
            -- a <- SIO.readFile "./storage.txt"
            _ <- runCommandInMemory serverStateString v
            return $ next ()
        runStep v _ (CarGarage garageString next) = do
            _ <- runCommandInMemory ("CarGarage Garage " ++ garageString ++ " ") v
            return $ next ()
        runStep v _ (Add carString next) = do
            _ <- runCommandInMemory ("AddCar Car " ++ carString ++ " ") v
            return $ next ()
        runStep v _ (Remove carString next) = do
            _ <- runCommandInMemory ("RemoveCar Car " ++ carString ++ " ") v
            return $ next ()
        runStep v _ (Edit carString1 carString2 next) = do
            _ <- runCommandInMemory ("EditCar Car " ++ carString1 ++ " to Car " ++ carString2 ++ " ") v
            return $ next ()
        runStep v _ (ListCars next) = do
            result <- runCommandInMemory "ListCars " v
            return $ next result
        runStep v _ (CalculatePollutionTax carString next) = do
            result <- runCommandInMemory ("CalculatePollutionTax Car " ++ carString ++ " ") v
            return $ next result
        runStep v _ (View next) = do
            a <- readIORef v
            return $ next $ show a
        runStep _ _ (Save next) = do
            -- a <- readIORef v
            -- SIO.writeFile "./data" (Lib3.renderStatements (Lib3.marshallState a))
            return $ next ()



makeRequestWithLock :: ByteString -> IO ()
makeRequestWithLock requestData = do
    _ <- takeMVar httpLock
    putStrLn $ "Making request: " ++ show requestData

    -- Wait 2 seconds
    threadDelay 2000000
    _ <- post "http://localhost:3000" requestData
    putStrLn $ "Received response for: " ++ show requestData

    putMVar httpLock ()


simulateRequests :: IO ()
simulateRequests = do
    -- Multiple threads for simulating concurrent requests
    _ <- forkIO $ makeRequestWithLock (cs "Request 1" :: ByteString)
    _ <- forkIO $ makeRequestWithLock (cs "Request 2" :: ByteString)
    _ <- forkIO $ makeRequestWithLock (cs "Request 3" :: ByteString)
    -- Wait for threads to finish
    threadDelay 10000000


-- For writing http requests with locks
runHttpRequest :: (MyDomain a -> IO a) -> MyDomain a -> IO a
runHttpRequest httpMethod givenProgram = do
    _ <- takeMVar httpLock

    result <- httpMethod givenProgram

    putMVar httpLock ()
    return result


main :: IO ()
main = do
    -- Without httpLock
    -- result <- runHttpOne program
    -- putStrLn result
    -- return ()

    -- With httpLock
    result <- runHttpRequest runHttpOne program
    putStrLn result
    return ()

    -- result <- runTest program
    -- putStrLn result
    -- return ()

    -- result <- runHttpRequest runHttpSmart program
    -- putStrLn result
    -- return ()

    -- For testing/showing httpLock functionality 
    -- putStrLn "Test start"
    -- simulateRequests
    -- putStrLn "Test end"
    -- return ()