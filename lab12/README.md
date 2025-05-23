# lab12 - Stack based interpreter for BPROG-Language

## Starting Thoughts

When i first Encountered this task, i had a rather bad approach. Implementing the stack, with input and output was pretty trivial, in addition to parsing the input into separate words. The problem arised when i added more and more functions. As i hand't implemented any generic types for the types i was working with, and it ended up being a lot of hard coding, where any new function had to very maunually work with everything else.

As i experienced that this approach vas gonna be very innefficient and time consuming i had to have another way of completing it.

## Usage
The interpreter is stack based, and input like `2 5 7 + *` will work like this:
```hs
2
2 5
2 5 7
2 12    -- +
24      -- *
```
In other words, the input is _always_ parsed as if one and one token was added, and does not care if multiple values are added at the same time.

## Types
I created several type constructors that would host the different builtin functions, value types my language allows, different error types, and the stack end environment of the interpreter.

### Variables

Once these types are implemented, the parser can infer what type each variable is, by checking if they can be converted to anything else than the default string.

### Containers

Multi word strings, lists and "fucntions" (quotes) were more difficult to parse since i had to make sure that there exists an appropriate ending to the opening token. This also has to be done recursively.

## Built in functions

### Functions that trigger at the top of the stack

With proper types being implemented, the default functions that only work with and alter the top element of the stack were easy to make, as they always trigger and execute right away. Examplewise the duplicate function:
```hs
primDup  e stk =  case stk of
  x:xs -> Right(e,x:x:xs)
  _  -> Left StackEmpty
```

### Functions that trigger in the stack

Functions like map and times, that require elements on top of them on the stack (given the task description) were implemented in a different way.

If there were not designated test cases that had to be passed, i would change these functions to also trigger on top of the stack, because how they work is probably not optimal.

Whenever there is an update to the stack, i always check if there is something in the stack that shold trigger. This means that i check if `if` is third on the stack, if `foldl` is second on the stack, and accordingly for other keywords.

To implement this differently, I could have the execution of other functions _hang_ for the appropriate amount of further inputs, but i found that my current implementation is easier to implement, and i kept it because it is suitable and functional for this case.

## User defined variables and functions
To save the user defined variables and functions with `:=` and `fun`, I save them in the `env` field in the interpreter-wide Map. This is always checked before adding the inputs as other functions or strings to the stack.

## Closing thoughts
This task really challenged my understanding and usage of custom types and datas in Haskell. It almost felt like i forced object oriented programming into haskell with "structs" like 
```hs
data Value
  = VInt    Integer
  | VFloat  Double
  | VBool   Bool
  | VString String
  | VList   [Value]
  | VQuote  [Token]
  deriving (Eq)
```
and
```hs
type Stack = [Value]
```
in some sense, they even have their own custom "methods":

```hs
instance Show Value where
  show (VInt i)      = show i
  show (VFloat d)    = show d
  show (VBool True)  = "True"
  show (VBool False) = "False"
  show (VString s)   = show s
  show (VList vs)    = "[" ++ intercalate "," (map show vs) ++ "]"
  show (VQuote ts)   = "{ " ++ unwords (map show ts) ++ " }"
```

Of course, these are not actually objects and methods, but the use of them reminded me of that.
