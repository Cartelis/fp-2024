# fp-2024

Domain - *car garage*.

This application is designed to manage a car garage's inventory.

### The main entities
1. CarGarage: data structure for car garage. It allows to store cars in simple garage or nested garages.
1. Command: operation choice (add, remove, edit, view list).
1. Car: has information about the whole car.
1. Make: shows which company produces(or produced) this car (e.g., Volkswagen, Audi, BMW).
1. Model: shows exact model name (e.g., Golf, A6, M850i), provides information about the car's engine, which wheels drive the car and gearbox.

The operations include adding/removing a car to/from the garage, editing information about a car, listing all cars in the garage. 

### BNF

```
<CarGarage> ::= "CarGarage" <garage>
<garage> ::= " garage " <garage_name> " " <car_list> "(" <inner_garage> ") "
<car_list> ::= <car> | <car> <car_list>
<inner_garage> ::= <garage> | <garage> <inner_garage> | " "
<garage_name> ::= <string>

<command> ::= <command_type> " " <car> | "edit " <car> "to " <car> | "list cars"
<command_type> ::= "add" | "remove"

<car> ::= "Car " <make> " " <model>

<make> ::= <manufacturer> | <manufacturer> <make>
<manufacturer> ::= <stringChar>

<model> ::= <model_name> " " <color> " " <body_type> " " <powertrain> <opt_consumption> <mileage> | <model_name> " " <model>
<model_name> ::= <string>
<color> ::= "Red" | "Black" | "Gray" | "White" | "Blue" | "Silver" | "Green" | "Brown" | "Orange" | "Yellow" | "Gold" | "Purple"
<body_type> ::= "Sedan" | "Coupe" | "Hatchback" | "Pickup" | "Off-road" | "Sport" | "Van" | "Convertible" | "Crossover" | "SUV" | "Wagon" | "Muscle" | "Compact" 

<powertrain> ::= <engine> " " <drive_type> " " <transmission>
<engine> ::= <displacement> " " <engine_layout> " " <power> " " <torque> " " <induction> " " <fuel> | <power> " " <torque> " Electric"
<displacement> ::= <digit> "." <digit> " L" | <digit> <displacement>
<engine_layout> ::= <string>
<power> ::= <digit> "kW" | <digit> <power>
<torque> ::= <digit> "Nm" | <digit> <torque>
<induction> ::= <induction_type> | <induction_type> " " <induction>
<induction_type> ::= "Turbocharged" | "Supercharged" | "Naturally aspirated"
<fuel> ::= "Diesel" | "Petrol" | "Hydrogen"
<drive_type> ::= "AWD" | "RWD" | "FWD" | "4WD"
<transmission> ::= "Manual" | "Automatic"
<opt_consumption> ::= " " <fuel_consumption> "l/100km " | " " <fuel_consumption> "kWh/100km " | " "
<fuel_consumption> ::= <digit> "." <digit> | <digit> <fuel_consumption>
<mileage> ::= <digit> "km " | <digit> <mileage>

<stringChar> ::= <letter> | <letter> <stringChar>
<string> ::= <letter> | <letter> <string> | <digit> | <digit> <string>
<letter> ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

```
Command example:
add Car BMW M850i Black Coupe 4.4 L V8 390kW 750Nm Turbocharged Petrol AWD Automatic 18153km 