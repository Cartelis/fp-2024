# fp-2024

Domain - car garage.

The main entities are make, mode and engine.

The operations include adding/removing a car to/from the garage and checking information about a car.

### BNF

```
<car> ::= <make> <model>
<make> ::= <manufacturer> <EOL> | <manufacturer> <make>
<model> ::= <model-name> <body-type> <powertrain> <opt-consumption> <EOL> | <model-name> <model>
<powertrain> ::= <engine> <drive-type> <transmission>
<engine> ::= <displacement> <engine-layout> <power> <torque> <induction> <fuel> | <power> <torque> <fuel>
<opt-consumption> ::= <fuel-consumption> "l/100km" | <fuel-consumption> "kWh/100km"
```
##### Induction - turbocharged, supercharged or naturally aspirated.