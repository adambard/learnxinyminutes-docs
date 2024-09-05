---
language: BQN
filename: learnbqn.bqn
contributors:
    - ["Raghu Ranganathan", "https://github.com/razetime"]
---

BQN is a modern array language (similar to APL) that aims to eliminate burdensome aspects of the APL tradition.

It is recommended to try these code examples out in a REPL. The [online REPL](https://mlochbaum.github.io/BQN/try.html) is
recommended for quick start, since it comes with keyboard and easy to access help. You can try building
[CBQN](https://github.com/dzaima/CBQN) for a local install, but it will need keyboard setup.

```bqn
# This is a comment.
# The characters ',' and `⋄` are statement separators.

##################
# Main datatypes #
##################

# Numbers
1,2,3,4
¯1,¯2,¯3  # Negative numbers are written with a high minus
π,∞,¯π,¯∞ # Pi and Infinity are defined constants
1_234_456 # You can add underscores in between numbers
          # This does not change their value
1.3E4     # Scientific notation is supported

# Characters
'a','⥊'
'
'         # Yes, you can put *any* character in a character literal
@         # Null character ('\0' in C)
# Arrays
1‿2‿3       # Stranding, good for simple lists
⟨1,2,3⟩     # General list notation
⟨1‿2,2‿3⟩   # Both can be mixed
[1‿2,2‿3]   # Array notation
            # An array is multidimensional, as opposed to containing sublists.
            # It must be rectangular in shape (a grid structure rather than a tree structure)
[1‿2‿3,4‿5] # This is hence invalid
            # May be familiar coming from Numpy, MATLAB and similar languages.
"asdf"      # Character array (String)
"newline
separated"  # Allows newlines
"quo""tes"  # Escape a double quote by typing it twice
# Functions
1{𝕨+𝕩}3       # All functions are infix
              # 𝕨 is left argument, 𝕩 is right argument
{-𝕩}5         # 𝕨 can be omitted
1+3           # Same as the above
{𝕊𝕩}          # 𝕊 is a recursive call
              # (this function will loop forever)
{𝕨 𝕊 𝕩: 𝕨+𝕩}  # Functions can have headers (too many cases to discuss here)
              # Headers can define arity
{𝕊 a‿b: a}1‿2 # and also do basic pattern matching
              # (returns 1)

# Modifiers (higher order functions)
{𝕗,𝔽,𝕘,𝔾}      # 𝔽 and 𝔾 are the operands as callable functions
               # 𝕗 and 𝕘 are the operands as values
{𝔽𝕩}           # 1-modifiers use 𝔽/𝕗 ONLY
˜,˘,¨,⁼,⌜      # primitive 1-modifiers are superscripts
{𝕨𝔽𝔾𝕩}         # 2-modifiers MUST use both 𝔽/𝕗 and 𝔾/𝕘 in body or header
⊸,∘,○,⟜        # primitive 2-modifiers all have circles
+{⟨𝕗⟩}         # returns ⟨ + ⟩
1-{𝔽 𝕨 𝔾 𝕩 }×2 # returns ¯2 (operators are *also* infix)
               # (same as 1 -○× 2)

# Trains (Special form of function composition)
(+´÷≠) # Average (but how?)
# The above train is an F G H train, where
# (F G H) 𝕩 → (F 𝕩) G (H 𝕩)
# F ← +´, G ← ÷, H ← ≠
# In explicit form, this is
{(+´𝕩)÷≠𝕩}
# The second pattern is (f g) 𝕩 → f g 𝕩.
# longer trains are complex arrangements of these patterns, involving constants and Nothing (·).
# Read more about trains at https://mlochbaum.github.io/BQN/doc/train.html

# Evaluation order:
#  BQN evaluates functions right to left with no precedence rules governing *functions*. Functions are what
#  one would call operators in a mainstream language.
1÷2+3       # 1÷(2+3)   = 0.2
(1÷2)+3     # ((1÷2)+3) = 1.5

# Modifiers:
#  Modifiers are higher order functions, and bind tighter than functions. Modifiers execute left to right.
#  Modifiers can take non-function arguments e.g. Constant (`˙`)
+
1+˜2+○-∘×3  # 1(+˜)(2((+○-)∘×)3)

# Variables
#  Since the case of a variable matters to determine what it means, BQN variables are *case insensitive*
#  The case that a variable is written in can change the way it is interpreted by BQN.
#  Eg. `F` refers to a value as a callable function, whereas `f` refers to the same variable as just a value.
#  Variable assignment is done with `←`. Variables have naming conventions based on their value:
subject ← 1‿2‿3        # Arrays, single values, namespaces come under this
                       # name must start with with a lowercase letter
Function      ← {𝕨+𝕩}  # Primitive and user defined functions come under this, both monadic and dyadic
                       # Starts with an uppercase letter
_1modifier    ← {𝕨𝔽𝕩}  # Starts with an underscore
_2modifier_   ← {𝔽𝕨𝔾𝕩} # Starts and ends with an underscore
# Variable modification is done with `↩`. An existing name cannot be reassigned with `←`.
Func ↩ {"Hello"∾𝕩}
array_or_atom +↩ 2    # You can use a dyadic function for modification
                      #≡ 3‿4‿5
array_or_atom -↩      # Or a monadic function.
                      #≡ ¯3‿¯4‿¯5
#  Due to all functions being infix, you can use your own functions for modification as well:
array_or_atom {2⋆𝕩}↩  #≡ ⟨ 0.125, 0.0625, 0.03125 ⟩

##################
# BQN Primitives #
##################
# All of BQN's base primitives are a single character long. Refer to https://mlochbaum.github.io/BQN/help/index.html for
# examples.
# Here we will look at a few primitives from each section. You will want to consult the docs for detailed explanations.

# Primitive Functions
#  All BQN functions are variadic, and can take one or two arguments. The base functions have both monadic and dyadic overloads.
#  Usually the two overloads for a function are related.

## Arithmetic Functions
+, -, ×, ÷ # Add, Subtract, Signum/Multiply, Reciprocal/Divide , '*' does NOT do multiplication
           # ⌊∘÷ does floor division
√, ⋆       # Square root/Nth root, e^x/Power
#   All Arithmetic functions vectorize:
1 + 2‿3‿4     #≡ 3‿4‿5
1‿2‿3 + 2‿3‿4 #≡ 3‿5‿7
#   Character arithmetic(+ and - only):
"abc"+3       #≡ "def"
'a'-'d'       #≡ ¯3

## Logic Functions
∧, ∨, ¬       # For Booleans, return 1 or 0
≤, <, >, ≥, = # Vectorizing comparisons
≡, ≢          # Nonvectorizing comparisons

## Array manipulation Functions
↕             # Make a range
∾, ≍, ⋈       # Joining arrays together
a←1‿2‿3,b←4‿5 # Let us take a and b.
a∾b           #≡ 1‿2‿3‿4‿5
a≍b           #  Same as previous, since a and b are not multidimensional
              #  Adds an extra dimension, similar to a ⋈ for multidimensional arrays.
a⋈b           #≡ ⟨1‿2‿3, 4‿5⟩
⊑, ⊏          # Indexing
1⊑1‿2‿3       #≡ 2 (BQN is 0-indexed)
1‿2⊏1‿2‿3     #≡ 2‿3 (for multiple indices)
↑, ↓          # Getting a prefix, suffix of an array.
              # together they can be used for slicing
⥊             # Reshape/repeat items to create a new array

# Primitive 1-Modifiers
## Looping combinators
¨, ˘, ⌜ # Mapping/Zipping
´, ˝    # Fold from right
`       # Scan from left

## General combinators
˜       # duplicate argument/swap args - Very useful!
˙       # Create constant function
1 -˜ 2  #≡ 2 - 1
+˜ 2    #≡ 2 + 2

# Primitive 2-modifiers
## Control Flow
◶       # Choose from a list of funcs
⍟       # Repeat n times

## General Combinators
⊸, ⟜    # hook, hookf
∘, ○    # simple function composition

##########
# Blocks #
##########
# Code delimited by {}
# Lexically scoped
# For more info: https://mlochbaum.github.io/BQN/doc/block.html
# Can have headers, which are ways to explicitly define what a block should be.
# A block without headers is automatically inferred from its special variables (𝕨, 𝕩, ...).

# Function blocks
# Implicit variables(Capitals are functions):
#  - 𝕨, 𝕎 left argument
#  - 𝕩, 𝕏 right argument
#  - 𝕤, 𝕊 represent the block itself
#   Optional: one or more headers that trigger based on
#   - pattern match (':') o
#   - condition ('?') (similar to if-then-else)

{ # A factorial using headers:
  𝕊 0: 1;
  𝕊 𝕩: 𝕩×𝕊 𝕩-1
}
{ # Factorial with predicates
  𝕩<2 ? 1; # Similar to an if-else pattern.
  𝕩×𝕊 𝕩-1
}

# Modifier blocks
# create 1-modifiers and 2-modifiers, which have separate types
# Implicit variables(Capitals are functions):
#  - has 𝕨 and 𝕩 if needed
#  - 𝕗, 𝔽 left operand
#  - 𝕘, 𝔾 right operand (only in 2-modifiers)
#  - 𝕣 represents the block itself* (requires underscores as per convention)
# Same header rules as functions.
{ 𝕨=0 ? 𝔽 𝕩; 𝔾 𝕩 } # execute 𝔽 or 𝔾 based on whether left argument is 0.

# Namespace blocks
# Create immutable namespaces with fields
# Require exports (`⇐`) for accessible fields.
# Use '.' for field access
n←{
  A←+
  b⇐4
}
n.b #≡ 4
n.a # ERROR

# Immediate Blocks
#  No arguments taken
#  Run the code inside and return the last statement
#  Often responsible for strange errors.
#  Can be mistaken for other blocks easily
#  Good for avoiding scoping issues
{
  1‿2‿3
}
{+} # Trick for returning a function as a value
####################
# Basic constructs #
####################
# Functional programming
# `¨` is used for mapping, as discussed before:
{𝕩∾2}¨1‿2‿3 #≡ ⟨1‿2,2‿2,3‿2⟩
# ⋈¨ is a plain zip, which produces pairs.
# `¨` acts as a zipWith when used with two arguments:
1‿2‿3 {⟨𝕩+2,2⥊𝕨⟩} 4‿5‿6 #≡ ⟨⟨6,1‿1⟩,⟨7,2‿2⟩,⟨8,3‿3⟩⟩
# `/` is replicate, which serves several purposes *including* filtering.
# elements in 𝕩 are repeated by the corresponding number in 𝕨.
1‿2‿3‿0/4‿5‿6‿7 #≡ 4‿5‿5‿6‿6‿6
# a simple filter idiom is F⊸/:
{2|𝕩}⊸/67‿42‿83 # keep the odd elements
                #≡ 67‿83

# Conditionals
# There are two main ways to define a conditional.
## Predicate headers
{
  𝕩 > 2:  "greater than 2";
  𝕩 < 2: "lesser than 2";
  "equal to 2"
}

## Choose (function-based)
#  - 2-modifier
#  - 𝔾: list of functions that serve as bodies
#  - 𝔽: condition function that specifies which function from 𝔾 to select
#  The same conditional as above would be:
{⊑/⟨𝕩>2, 𝕩<2, 𝕩=2⟩}◶⟨
  {𝕊: "greater than 2"}
  {𝕊: "lesser than 2"}
  {𝕊: "equal to 2"}
⟩

## Some helpers for conditionals
If      ← {𝕏⍟𝕎@}´                 # Used as If ⟨Condition, Block⟩
IfElse  ← {c‿T‿F: c◶F‿T@}         # Used as IfElse ⟨Condition, Block, ElseBlock⟩

# Looping
# The primary form of unbounded looping is recursion (performed with 𝕊).
# BQN does not eliminate tail calls, but the while idiom can be used to work around this:
While ← {𝕩{𝔽⍟𝔾∘𝔽_𝕣_𝔾∘𝔽⍟𝔾𝕩}𝕨@}´  # While 1‿{... to run forever
DoWhile ← {𝕏@ ⋄ While 𝕨‿𝕩}´
# A For loop can be done with ¨, functions need not be pure.
```

## Ready for more?

- [Quickstart guide](https://mlochbaum.github.io/BQN/doc/quick.html)
- [Full length, explained documentation](https://mlochbaum.github.io/BQN/doc/index.html)
- [Short docs](https://mlochbaum.github.io/BQN/help/index.html)
- [BQN community!](https://mlochbaum.github.io/BQN/community/index.html)
