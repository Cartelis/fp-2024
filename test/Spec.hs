{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [ 
    testCase "List of completions is not empty" $
      null Lib1.completions @?= False,

    testCase "Parsing case 1 - trying to parse query with empty input" $
      Lib2.parseQuery "" @?= Left "Cannot parse empty input",
    
    testCase "Parsing case 2 - trying to parse invalid command" $
      Lib2.parseQuery "addcar Toyota" @?= Left "Failed to parse query: Not supported prefix/command",

    testCase "Parsing case 3 - trying AddCar command" $
      Lib2.parseQuery "AddCar Car - RW 4 Orange Sport 17kW 32Nm Electric 4WD Manual 7km "
        @?= Right (Lib2.AddCar (Lib2.Car "-" "RW 4" "Orange" "Sport" (Lib2.Powertrain (Lib2.ElectricEngine 17 32) "4WD" "Manual") 0.0 7)),
  
    testCase "Parsing case 4 - trying ListCars command" $
      Lib2.parseQuery "ListCars" @?= Right Lib2.ListCars,

    testCase "Parsing case 5 - trying RemoveCar command" $
      Lib2.parseQuery "RemoveCar Car Mercedes-Benz AMG E 63 S 4MATIC Black Sedan 4.0 L V8 450kW 850Nm Turbocharged Petrol AWD Automatic 23.6l/100km 27602km "
        @?= Right (Lib2.RemoveCar (Lib2.Car "Mercedes-Benz" "AMG E 63 S 4MATIC" "Black" "Sedan" (Lib2.Powertrain (Lib2.FuelEngine 4.0 "V8" 450 850 "Turbocharged" "Petrol") "AWD" "Automatic") 23.6 27602)),
    
    testCase "Parsing case 6 - trying View command" $
      Lib2.parseQuery "View" @?= Right Lib2.View,

    testCase "Parsing case 7 - trying EditCar command" $
      Lib2.parseQuery "EditCar Car N PU 8 Green Sedan 63283.6 L Nu 03kW 1Nm Turbocharged Hydrogen FWD Automatic 2265.6kWh/100km 922km to Car -EMw 8 White Van 79.0 L 9 9kW 2Nm Turbocharged Petrol FWD Automatic 1km "
        @?= Right (Lib2.EditCar (Lib2.Car "N" "PU 8" "Green" "Sedan" (Lib2.Powertrain (Lib2.FuelEngine 63283.6 "Nu" 3 1 "Turbocharged" "Hydrogen") "FWD" "Automatic") 2265.6 922)
          (Lib2.Car "-EMw" "8" "White" "Van" (Lib2.Powertrain (Lib2.FuelEngine 79.0 "9" 9 2 "Turbocharged" "Petrol") "FWD" "Automatic") 0.0 1)),
  
    testCase "Parsing case 8 - trying CarGarage command" $
      Lib2.parseQuery "CarGarage Garage 6 Car --- w8 o Silver Sport 88kW 6Nm Electric AWD Manual 18.3l/100km 86km ( Garage UL Car Z Y Yellow Van 7.6 L Z 88kW 20Nm Turbocharged Diesel RWD Manual 4.4kWh/100km 16km ( )  ) "
        @?= Right (Lib2.CarGarage (Lib2.Garage "6" [Lib2.Car "---" "w8 o" "Silver" "Sport" (Lib2.Powertrain (Lib2.ElectricEngine 88 6) "AWD" "Manual") 18.3 86]
          [Lib2.Garage "UL" [Lib2.Car "Z" "Y" "Yellow" "Van" (Lib2.Powertrain (Lib2.FuelEngine 7.6 "Z" 88 20 "Turbocharged" "Diesel") "RWD" "Manual") 4.4 16] []]))
  ]

propertyTests :: TestTree
propertyTests = testGroup "some meaningful name"
  [
    QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  ]