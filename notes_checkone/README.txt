Currently both int and float are one datatype since the parser does not currently distinguish between the two.
Strings are being held as a list of strings to facilitate array operations
The store is a global mutable hash-set to prevent threading the store through all function calls
The environment has been implemented as a stack where the top stack frame consist of the current environment and a new frame is added for function application and Lets then popped after the body finishes.

We have passed 3 in types:
test_booleans.py
test_floats.py
types_truthy1.py
