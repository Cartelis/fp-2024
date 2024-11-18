# fp-2024

Domain - *car garage*.

This application is designed to manage a car garage's inventory.

### The main entities
1. CarGarage: data structure for car garage. It allows to store cars in simple garage or nested garages.
1. Command: operation choice (add, remove, edit, view list).
1. Car: has information about the whole car.
1. Make: shows which company produces(or produced) this car (e.g., Volkswagen, Audi, BMW).
1. Model: shows exact model name (e.g., Golf, A6, M850i), provides information about the car's engine, which wheels drive the car and gearbox.

The operations include adding/removing a car to/from the garage, editing information about a car, listing all cars in the garage, calculating pollution tax on given car. 

### BNF

```
<CarGarage> ::= "CarGarage" <garage>
<garage> ::= " Garage " <garage_name> " " <car_list> "(" <inner_garage> ") "
<car_list> ::= <car> | <car> <car_list>
<inner_garage> ::= <garage> | <garage> <inner_garage> | " "
<garage_name> ::= <string>

<command> ::= <command_type> " " <car> | "EditCar " <car> "to " <car> | "ListCars " | "View " | "Load " | "Save "
<command_type> ::= "AddCar" | "RemoveCar" | "CalculatePollutionTax"

<queryBatch> ::= "BEGIN " <queries> "END "
<queries> ::= <query> | <query> <queries>
<query> ::= <command> | <CarGarage>

<car> ::= "Car " <make> " " <model>

<make> ::= <stringMake>

<model> ::= <model_name> <color> " " <body_type> " " <powertrain> <opt_consumption> <mileage>
<model_name> ::= <string> " " | <string> " " <model_name>
<color> ::= "Red" | "Black" | "Gray" | "White" | "Blue" | "Silver" | "Green" | "Brown" | "Orange" | "Yellow" | "Gold" | "Purple"
<body_type> ::= "Sedan" | "Coupe" | "Hatchback" | "Pickup" | "Off-road" | "Sport" | "Van" | "Convertible" | "Crossover" | "SUV" | "Wagon" | "Muscle" | "Compact" 

<powertrain> ::= <engine> " " <drive_type> " " <transmission>
<engine> ::= <displacement> " " <engine_layout> " " <power> " " <torque> " " <induction> " " <fuel> | <power> " " <torque> " Electric"
<displacement> ::= <digit> "." <digit> " L" | <digit> <displacement>
<engine_layout> ::= <string>
<power> ::= <digit> "kW" | <digit> <power>
<torque> ::= <digit> "Nm" | <digit> <torque>
<induction> ::= "Turbocharged" | "Supercharged" | "Naturally aspirated"
<fuel> ::= "Diesel" | "Petrol" | "Hydrogen"
<drive_type> ::= "AWD" | "RWD" | "FWD" | "4WD"
<transmission> ::= "Manual" | "Automatic"
<opt_consumption> ::= " " <fuel_consumption> "l/100km " | " " <fuel_consumption> "kWh/100km " | " "
<fuel_consumption> ::= <digit> "." <digit> | <digit> <fuel_consumption>
<mileage> ::= <digit> "km " | <digit> <mileage>

<stringMake> ::= <letterOrMinus> | <letterOrMinus> <stringMake>
<letterOrMinus> ::= <letter> | "-"
<string> ::= <letter> | <letter> <string> | <digit> | <digit> <string>
<letter> ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"


```
Command example:
AddCar Car BMW M850i Black Coupe 4.4 L V8 390kW 750Nm Turbocharged Petrol AWD Automatic 18153km 


### Changes to BNF in Lab. 2


BNF updates:

**Garage**: Garage now can have one or more cars, before it was set to two cars. Also, updated constant strings for clarity.

From:
```
<garage> ::= " " <garage_name> " " <car> <car> "(" <inner_garage> ") "
```
To:
```
<garage> ::= " Garage " <garage_name> " " <car_list> "(" <inner_garage> ") "
<car_list> ::= <car> | <car> <car_list>
```

**Command**: Updated command names, added ``View`` command. Added ``CalculatePollutionTax`` command to calculate the pollution tax on given car.


**Make**: Car make can now how one special string (consists of alphabet letters and '-' symbol) instead of a list of strings. Most
popular manufacturers' names are made of only one special string.

From:
```
<make> ::= <manufacturer> | <manufacturer> <make>
<manufacturer> ::= <string>
```
To:
```
<make> ::= <stringMake>
```

**Model and model_name**: Updated for clarity.

From:
```
<model> ::= <model_name> " " <color> " " <body_type> " " <powertrain> <opt_consumption> <mileage> | <model_name> " " <model>
<model_name> ::= <string>
```
To:
```
<model> ::= <model_name> <color> " " <body_type> " " <powertrain> <opt_consumption> <mileage>
<model_name> ::= <string> " " | <string> " " <model_name>
```

**Induction**: Made more logical, that the car can have one induction, not a list, because car cannot be naturally aspirated and turbocharged at the same time.

From:
```
<induction> ::= <induction_type> | <induction_type> " " <induction>
<induction_type> ::= "Turbocharged" | "Supercharged" | "Naturally aspirated"
```
To:
```
<induction> ::= "Turbocharged" | "Supercharged" | "Naturally aspirated"
```

**stringMake**: Added a new string type just for make of the car.
```
<stringMake> ::= <letterOrMinus> | <letterOrMinus> <stringMake>
<letterOrMinus> ::= <letter> | "-"
```

### Changes to Lab. 2 code for Lab. 3

**parseQuery**: Added a return string (what is left after parsing) for parseStatements to work correctly.

**module Lib2**: Updated the list of exported parsers and commands to reuse them in Lib3.

**parseListCars** and **parseView**: Added a space at the end (needed for Batch of statements). In order to differentiate where one query ends and other begins.

### Changes to BNF for Lab. 3
**command**: 
1. Added a space (' ') to "ListCars" and "View" (i.e., "ListCars" -> "ListCars " and "View" -> "View ").  
Explained why in the Section: *Changes to Lab. 2 code for Lab. 3*, Line: *parseListCars and parseView*. 
2. Implemented "Load " and "Save " (space at the end) commands.  
Usage: 
    * after using "Load " it returns to terminal, whether loading content from a file and state was successful:
        ```
        >>> Load 
        Successfully loaded content from file
        State loaded
        ```
    * after using "Save " it returns to terminal, whether saving content to a file and state was successful:
        ```
        >>> Save 
        Successfully saved content
        State saved
        ```

**queryBatch**: Added syntax for grouping queries (batch processing). My queries are seperated by space (i.e., "BEGIN query1 query2 END ").  
Examples:
1. BEGIN ListCars END 
2. BEGIN ListCars CarGarage Garage UniqueCars Car Toyotas Supra Mk5 Green Sport 3.0 L I6 310kW 730Nm Turbocharged Petrol RWD Manual 9.7l/100km 24695km ( ) END 
3. BEGIN AddCar Car BMW 535d Black Sedan 3.0 L I6 250kW 820Nm Turbocharged Diesel RWD Automatic 276175km AddCar Car Tesla Model 3 Gray Sedan 208kW 420Nm Electric AWD Automatic 14.7kWh/100km 63km CalculatePollutionTax Car BMW 535d Black Sedan 3.0 L I6 250kW 820Nm Turbocharged Diesel RWD Automatic 276175km ListCars END 