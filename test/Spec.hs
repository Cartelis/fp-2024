{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Data.List (sort)
import Data.Ord

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified

import Debug.Trace (trace)


instance Arbitrary Lib2.Engine where
  arbitrary = oneof
    [Lib2.FuelEngine <$> genPositiveDouble <*> genAlphanumericString <*> genPositiveInt <*> genPositiveInt <*> genInduction <*> genFuel,
    Lib2.ElectricEngine <$> genPositiveInt <*> genPositiveInt]

instance Arbitrary Lib2.Powertrain where
  arbitrary = Lib2.Powertrain <$> arbitrary <*> genDriveType <*> genTransmission


instance Arbitrary Lib2.Car where
  arbitrary = Lib2.Car
    <$> genMake <*> genModel <*> genColor <*> genBodyType <*> arbitrary <*> genPositiveDouble <*> genMileage

instance Arbitrary Lib2.Garage where
  arbitrary = Lib2.Garage <$> genAlphanumericString <*> genCarList <*> (genInnerGarages 2)

instance Arbitrary Lib2.Query where
  arbitrary = oneof
    [
      Lib2.CarGarage <$> arbitrary,
      Lib2.AddCar <$> arbitrary,
      Lib2.RemoveCar <$> arbitrary,
      Lib2.EditCar <$> arbitrary <*> arbitrary,
      pure Lib2.ListCars,
      Lib2.CalculatePollutionTax <$> arbitrary,
      pure Lib2.View
    ]

instance Arbitrary Lib3.Statements where
  arbitrary = oneof
    [
      Lib3.Batch <$> genNonEmptyQueryBatch,
      Lib3.Single <$> arbitrary
    ]

-- Helper function to generate inner garages with limited depth
genInnerGarages :: Int -> Gen [Lib2.Garage]
genInnerGarages 0 = return []  -- No inner garages at depth 0
genInnerGarages depth = do
  size <- choose (0, 2)  -- Limit the number of inner garages per level
  innerGarages <- vectorOf size (genInnerGarage (depth - 1))
  return innerGarages

-- Generator for a single inner garage with recursive depth control
genInnerGarage :: Int -> Gen Lib2.Garage
genInnerGarage depth = do
  name <- genAlphanumericString
  cars <- genCarList
  innerGarages <- genInnerGarages depth  -- Recursively generate inner garages with reduced depth
  return $ Lib2.Garage name cars innerGarages


genMileage :: Gen Integer
genMileage = choose (0, 1000000)

genCarList :: Gen [Lib2.Car]
genCarList = do
  size <- choose (1, 2)
  vectorOf size arbitrary 

genNonEmptyQueryBatch :: Gen [Lib2.Query]
genNonEmptyQueryBatch = do
  size <- choose (1, 2)
  vectorOf size arbitrary
  

genAlphanumericString :: Gen String
genAlphanumericString = do
  len <- choose (1, 15)
  -- frequency - you provide the possibility and what to do. So every generator here has 1/3 possibility
  vectorOf len (frequency [(1, choose ('a', 'z')), (1, choose ('A', 'Z')), (1, choose ('0', '9'))])


genPositiveInt :: Gen Integer
genPositiveInt = choose (1, 1000)

genPositiveDouble :: Gen Double
genPositiveDouble = do
  intPart <- choose (0, 9) :: Gen Int
  fracPart <- choose (0, 9) :: Gen Int
  return $ fromIntegral intPart + (fromIntegral fracPart / 10)

genDriveType :: Gen String
-- elements generate random value from list
genDriveType = elements ["AWD", "RWD", "FWD", "4WD"]

genTransmission :: Gen String
genTransmission = elements ["Manual", "Automatic"]

genInduction :: Gen String
genInduction = elements ["Turbocharged", "Supercharged", "Naturally aspirated"]

genFuel :: Gen String
genFuel = elements ["Diesel", "Petrol", "Hydrogen"]

genMake :: Gen String
genMake = do
  len <- choose (1, 15)
  vectorOf len $ frequency [(1, choose ('a', 'z')), (1, choose ('A', 'Z')), (1, pure '-')]

genModel :: Gen String
genModel = do
  wordCount <- choose (1, 3)
  modelWords <- vectorOf wordCount genAlphanumericString
  return $ unwords modelWords

genColor :: Gen String
genColor = elements ["Red", "Black", "Gray", "White", "Blue", "Silver", "Green", "Brown", "Orange", "Yellow", "Gold", "Purple"]

genBodyType :: Gen String
genBodyType = elements ["Sedan", "Coupe", "Hatchback", "Pickup", "Off-road", "Sport", "Van", "Convertible", "Crossover", "SUV", "Wagon", "Muscle", "Compact"]




main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, lib3UnitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 unit tests"
  [
    testCase "List of completions is not empty" $
      null Lib1.completions @?= False,

    testCase "Parsing case 1 - trying to parse query with empty input" $
      Lib2.parseQuery "" @?= Left "Cannot parse empty input",

    testCase "Parsing case 2 - trying to parse invalid command" $
      Lib2.parseQuery "addcar Toyota" @?= Left "Failed to parse query: Not supported prefix/command",

    testCase "Parsing case 3 - trying AddCar command" $
      Lib2.parseQuery "AddCar Car - RW 4 Orange Sport 17kW 32Nm Electric 4WD Manual 7km "
        @?= Right (Lib2.AddCar (Lib2.Car "-" "RW 4" "Orange" "Sport" (Lib2.Powertrain (Lib2.ElectricEngine 17 32) "4WD" "Manual") 0.0 7), ""),

    testCase "Parsing case 4 - trying ListCars command" $
      Lib2.parseQuery "ListCars " @?= Right (Lib2.ListCars, ""),

    testCase "Parsing case 5 - trying RemoveCar command" $
      Lib2.parseQuery "RemoveCar Car Mercedes-Benz AMG E 63 S 4MATIC Black Sedan 4.0 L V8 450kW 850Nm Turbocharged Petrol AWD Automatic 23.6l/100km 27602km "
        @?= Right (Lib2.RemoveCar (Lib2.Car "Mercedes-Benz" "AMG E 63 S 4MATIC" "Black" "Sedan" (Lib2.Powertrain (Lib2.FuelEngine 4.0 "V8" 450 850 "Turbocharged" "Petrol") "AWD" "Automatic") 23.6 27602), ""),

    testCase "Parsing case 6 - trying View command" $
      Lib2.parseQuery "View " @?= Right (Lib2.View, ""),

    testCase "Parsing case 7 - trying EditCar command" $
      Lib2.parseQuery "EditCar Car N PU 8 Green Sedan 63283.6 L Nu 03kW 1Nm Turbocharged Hydrogen FWD Automatic 2265.6kWh/100km 922km to Car -EMw 8 White Van 79.0 L 9 9kW 2Nm Turbocharged Petrol FWD Automatic 1km "
        @?= Right (Lib2.EditCar (Lib2.Car "N" "PU 8" "Green" "Sedan" (Lib2.Powertrain (Lib2.FuelEngine 63283.6 "Nu" 3 1 "Turbocharged" "Hydrogen") "FWD" "Automatic") 2265.6 922)
          (Lib2.Car "-EMw" "8" "White" "Van" (Lib2.Powertrain (Lib2.FuelEngine 79.0 "9" 9 2 "Turbocharged" "Petrol") "FWD" "Automatic") 0.0 1), ""),

    testCase "Parsing case 8 - trying CarGarage command" $
      Lib2.parseQuery "CarGarage Garage 6 Car --- w8 o Silver Sport 88kW 6Nm Electric AWD Manual 18.3l/100km 86km ( Garage UL Car Z Y Yellow Van 7.6 L Z 88kW 20Nm Turbocharged Diesel RWD Manual 4.4kWh/100km 16km ( )  ) "
        @?= Right (Lib2.CarGarage (Lib2.Garage "6" [Lib2.Car "---" "w8 o" "Silver" "Sport" (Lib2.Powertrain (Lib2.ElectricEngine 88 6) "AWD" "Manual") 18.3 86]
          [Lib2.Garage "UL" [Lib2.Car "Z" "Y" "Yellow" "Van" (Lib2.Powertrain (Lib2.FuelEngine 7.6 "Z" 88 20 "Turbocharged" "Diesel") "RWD" "Manual") 4.4 16] []]), "")
  ]

lib3UnitTests :: TestTree
lib3UnitTests = testGroup "Lib3 unit tests"
  [
    testCase "Parsing case 1 - trying to parse a batch" $
      Lib3.parseCommand "BEGIN View ListCars END "
        @?= Right (Lib3.StatementCommand (Lib3.Batch [Lib2.View, Lib2.ListCars]), ""),

    testCase "Parsing case 2 - trying to parse a batch" $
      Lib3.parseCommand "BEGIN CalculatePollutionTax Car I t White Off-road 5.3 L 2 9kW 2530Nm Turbocharged Hydrogen 4WD Manual 60.4l/100km 4km AddCar Car - d A 53s Red Van 78kW 31Nm Electric RWD Manual 8.3l/100km 4km RemoveCar Car - d A 53s Red Van 78kW 31Nm Electric RWD Manual 8.3l/100km 4km END "
        @?= Right (Lib3.StatementCommand (Lib3.Batch [Lib2.CalculatePollutionTax (Lib2.Car "I" "t" "White" "Off-road" (Lib2.Powertrain (Lib2.FuelEngine 5.3 "2" 9 2530 "Turbocharged" "Hydrogen") "4WD" "Manual") 60.4 4), Lib2.AddCar (Lib2.Car "-" "d A 53s" "Red" "Van" (Lib2.Powertrain (Lib2.ElectricEngine 78 31) "RWD" "Manual") 8.3 4), Lib2.RemoveCar (Lib2.Car "-" "d A 53s" "Red" "Van" (Lib2.Powertrain (Lib2.ElectricEngine 78 31) "RWD" "Manual") 8.3 4)]), ""),

    testCase "Parsing case 3 - trying to parse a batch (Failed to parse)" $
      Lib3.parseCommand "BEGIN View ListCars ViewListCars END "
        @?= Left "END word not found or query written wrong",

    testCase "Parsing case 4 - trying to parse a batch (Failed to parse)" $
      Lib3.parseCommand "BEGIN ViewListCars View END "
        @?= Left "Failed to parse query: Not supported prefix/command",

    testCase "Parsing case 5 - trying to parse a batch (Failed to parse)" $
      Lib3.parseCommand "BEGIN CalculatePollutionTax Car I t White Off-road 5.3 L 2 9kW 2530Nm Turbocharged Hydrogen 4WD Manual 60.4l/100km 4km AddCar Car - d A 53s Red Van 78kW 31Nm Electric RWD Manual 8.3l/100km END "
        @?= Left "END word not found or query written wrong"
  ]



-- Property test: A Batch should always contain a list of queries
prop_batchContainsQueries :: Lib3.Statements -> Bool
prop_batchContainsQueries (Lib3.Batch queries) = not (null queries)  -- Ensures non-empty Batch
prop_batchContainsQueries _ = True  -- For Single, this property does not apply.

-- Statements property test with trace
prop_parseRenderStatements :: Lib3.Statements -> Bool
prop_parseRenderStatements statement = 
  trace ("Initial value: " ++ show statement) $ 
    let
      -- Rendering the statement
      rendered = Lib3.renderStatements statement
      -- Parsing the rendered statement
      (parsed, rest) =
        case Lib3.parseStatements rendered of
          Right(p, r) -> (p, r)
          Left e -> (Lib3.Single Lib2.View, e)
    in
      -- Return the comparison of parsed result with expected value
      -- trace ("Rendered: " ++ show rendered) $
      trace ("Parsed: " ++ show parsed) $
      -- trace ("Rest: " ++ show rest) $
      trace ("parsed == statement: " ++ show (parsed == statement))
      trace ("rest == \"\": " ++ show (rest == ""))
      parsed == statement && rest == ""


propertyTests :: TestTree
propertyTests = testGroup "Lib3 property tests"
  [
    QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list),

    QC.testProperty "parseStatements (renderStatements s) == Right(s, \"\")" $ -- prop_parseRenderStatements,
      \statement -> Lib3.parseStatements (Lib3.renderStatements statement) == Right (statement, ""),

    QC.testProperty "Batch contains queries" prop_batchContainsQueries
  ]