## monoRail

### Chris Hamm
### 2023-05-23

A really feable attempt to create a similator for the WDW monorail ala Bruce Laval. 

The first working versuion of this uses `R` because of the `R6Class` function. I create `R6Class`s for `Train` and `Station`, then `trains` and `station` objects, and then it is off to loop though the mumbo jumbo. The script `workingMonoRail.R` "works" but I'm not sure that the position is correct; it also lacks a lot of the functionality I want.

1. 2023-05-23: First version in `R` and I don't like it much.
1. 2024-12-04: Working in `python` and I got a much better simulation working. Hard coded times and parameters but will add distributions later.