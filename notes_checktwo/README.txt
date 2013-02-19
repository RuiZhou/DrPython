Currently both int and float are one datatype since the parser does not currently distinguish between the two.

Strings are being held as a list of strings to facilitate array operations

The store is a global mutable hash-set to prevent threading the store through all function calls

Everything is an object.

The environment is being implemented as a global stack of environments (hash tables).
	When a function is applied, its environment is pushed onto the stack, then is it popped after the application completes.
	Same is true of CLet.

Classes are interpreted into closures with static members treated as instance members of the closure.

Variables in class body are hoisted to the top of the class during desugaring.

Built-in classes are bound in python-lib.rkt along with the other built-in functions.

VAnswer is a control structure used to pass values and pop up errors/returns

Methods/Functions/Lambdas are all the same in the core language

Members are stored as an environment to facilitate lookup

Static members are only stored in the class object and instance objects contain pointers to their class in order to access static members

