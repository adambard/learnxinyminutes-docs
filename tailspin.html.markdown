---
language: tailspin
filename: learntailspin.tt
contributors:
    - ["Torbjörn Gannholm", "https://github.com/tobega/"]

---

**Tailspin** works with streams of values in pipelines. You may often feel
that your program is the machine and that the input data is the program.

While Tailspin is unlikely to become mainstream, or even production-ready,
it will change the way you think about programming in a good way.

```c
// Comment to end of line

// Process data in a pipeline with steps separated by ->
// String literals are delimited by single quotes
// A bang (!) indicates a sink, or end of the pipe
// OUT is the standard output object, ::write is the message to write output
'Hello, World!' -> !OUT::write

// Output a newline by just entering it in the string (multiline strings)
'
' -> !OUT::write
// Or output the decimal unicode value for newline (10) between $# and ;
'$#10;' -> !OUT::write

// Define an immutable named value. Value syntax is very literal.
def names: ['Adam', 'George', 'Jenny', 'Lucy'];

// Stream the list to process each name. Note the use of $ to get the value.
// The current value in the pipeline is always just $
// String interpolation starts with a $ and ends with ;
$names... -> 'Hello $;!
' -> !OUT::write

// You can also stream in the interpolation and nest interpolations
// Note the list indexing with parentheses and the slice extraction
// Note the use of ~ to signify an exclusive bound to the range
// Outputs 'Hello Adam, George, Jenny and Lucy!'
'Hello $names(first);$names(first~..~last)... -> ', $;'; and $names(last);!
' -> !OUT::write

// Conditionally say different things to different people
// Matchers (conditional expressions) are delimited by angle brackets
// A set of matchers, evaluated top down, must be in templates (a function)
// Here it is an inline templates delimited by \( to \)
// Note the doubled '' and $$ to get a literal ' and $
$names... -> \(
  when <='Adam'> do 'What''s up $;?' !
  when <='George'> do 'George, where are the $$10 you owe me?' !
  otherwise 'Hello $;!' !
\) -> '$;$#10;' -> !OUT::write

// You can also define templates (functions)
// A lone ! emits the value into the calling pipeline without returning control
// The # sends the value to be matched by the matchers 
// Note that templates always take one input value and emit 0 or more outputs
templates collatz-sequence
  when <..0> do 'The start seed must be a positive integer' !
  when <=1> do $!
// The ?( to ) allows matching a computed value. Can be concatenated as "and"
  when <?($ mod 2 <=1>)> do
    $ !
    3 * $ + 1 -> #
  otherwise
    $ !
    $ ~/ 2 -> #
end collatz-sequence

// Collatz sequence from random start on one line separated by spaces
1000 -> SYS::randomInt -> $ + 1 -> collatz-sequence -> '$; ' -> !OUT::write
'
' -> !OUT::write

// Collatz sequence formatted ten per line by an indexed list template
// Note the square brackets creates a list of the enclosed pipeline results
// The \[i]( to \) defines a templates to apply to each value of a list,
// the i (or whatever identifier you choose) holds the index
[1000 -> SYS::randomInt -> $ + 1 -> collatz-sequence]
-> \[i](
  when <=1|?($i mod 10 <=0>)> do '$;$#10;' !
  otherwise '$; ' !
\)... -> !OUT::write

// A range can have an optional stride
def odd-numbers: [1..100:2];

// Use mutable state locally. One variable per templates, always called @
templates product
  @: $(first);
  $(first~..last)... -> @: $@ * $;
  $@ !
end product

$odd-numbers(6..8) -> product -> !OUT::write
'
' -> !OUT::write

// Use processor objects to hold mutable state.
// Note that the outer @ must be referred to by name in inner contexts
// A sink templates gives no output and is called prefixed by !
// A source templates takes no input and is called prefixed by $
processor Product
  @: 1;
  sink accumulate
    @Product: $@Product * $;
  end accumulate
  source result
    $@Product !
  end result
end Product

// The processor is a constructor templates. This one called with $ (no input)
def multiplier: $Product;

// Call object templates by sending messages with ::
1..7 -> !multiplier::accumulate
-1 -> !multiplier::accumulate
$multiplier::result -> 'The product is $;
' -> !OUT::write

// Syntax sugar for a processor implementing the collector interface
1..7 -> ..=Product -> 'The collected product is $;$#10;' -> !OUT::write

// Symbol sets (essentially enums) can be defined for finite sets of values
data colour #{green, red, blue, yellow}

// Use processor typestates to model state cleanly.
// The last named mutable state value set determines the typestate
processor Lamp
  def colours: $;
  @Off: 0;
  state Off
    source switchOn
      @On: $@Off mod $colours::length + 1;
      'Shining a $colours($@On); light$#10;' !
    end switchOn
  end Off
  state On
    source turnOff
      @Off: $@On;
      'Lamp is off$#10;' !
    end turnOff
  end On
end Lamp

def myLamp: [colour#green, colour#blue] -> Lamp;

$myLamp::switchOn -> !OUT::write // Shining a green light
$myLamp::turnOff -> !OUT::write  // Lamp is off
$myLamp::switchOn -> !OUT::write // Shining a blue light
$myLamp::turnOff -> !OUT::write  // Lamp is off
$myLamp::switchOn -> !OUT::write // Shining a green light

// Use regular expressions to test strings
['banana', 'apple', 'pear', 'cherry']... -> \(
  when <'.*a.*'> do '$; contains an ''a''' !
  otherwise '$; has no ''a''' !
\) -> '$;
' -> !OUT::write

// Use composers with regular expressions and defined rules to parse strings
composer parse-stock-line
  {inventory-id: <INT> (<WS>), name: <'\w+'> (<WS>), currency: <'.{3}'>,
    unit-price: <INT> (<WS>?) <parts>?}
  rule parts: associated-parts: [<part>+]
  rule part: <'[A-Z]\d+'> (<=','>?)
end parse-stock-line

'705 gizmo EUR5 A67,G456,B32' -> parse-stock-line -> !OUT::write
// {associated-parts: [A67, G456, B32], currency: EUR,
//     inventory-id: 705, name: gizmo, unit-price: 5}
'
' -> !OUT::write

// Stream a string to split it into glyphs.
// A list can be indexed/sliced by an array of indexes
// Outputs ['h','e','l','l','o'], indexing arrays/lists starts at 1
['abcdefghijklmnopqrstuvwxyz'...] -> $([8,5,12,12,15]) -> !OUT::write
'
' -> !OUT::write

// We have used only raw strings above.
// Strings can have different types as determined by a tag.
// Comparing different types is an error, unless a wider type bound is set
// Type bound is given in ´´ and '' means any string value, tagged or raw
templates get-string-type
  when <´''´ '.*'> do '$; is a raw string' !
  when <´''´ id´'\d+'> do '$; is a numeric id string' !
  when <´''´ =id´'foo'> do 'id foo found' !
  when <´''´ id´'.*'> do '$; is an id' !
  when <´''´ name´'.+'> do '$; is a name' !
  otherwise '$; is not a name or id, nor a raw string' !
end get-string-type

[name´'Anna', 'foo', id´'789', city´'London', id´'xzgh', id´'foo']...
-> get-string-type -> '$;
' -> !OUT::write

// Numbers can be raw, tagged or have a unit of measure
// Type .. is any numeric value, tagged, measure or raw
templates get-number-type
  when <´..´ =inventory-id´86> do 'inventory-id 86 found' !
  when <´..´ inventory-id´100..> do '$; is an inventory-id >= 100' !
  when <´..´ inventory-id´0..|..inventory-id´0> do '$; is an inventory-id' !
  when <´..´ 0"m"..> do '$; is an m-measure >= 0"m"' !
  when <´..´ ..0|0..> do '$; is a raw number' !
  otherwise '$; is not a positive m-measure nor an inventory-id, nor raw' !
end get-number-type

[inventory-id´86, inventory-id´6, 78"m", 5"s", 99, inventory-id´654]...
-> get-number-type -> '$;
' -> !OUT::write

// Measures can be used in arithmetic, "1" is the scalar unit
// When mixing measures you have to cast to the result measure
4"m" + 6"m" * 3"1" -> ($ ~/ 2"s")"m/s" -> '$;
' -> !OUT::write

// Tagged identifiers must be made into raw numbers when used in arithmetic
// Then you can cast the result back to a tagged identifier if you like
inventory-id´300 -> inventory-id´($::raw + 1) -> get-number-type -> '$;
' -> !OUT::write

// Fields get auto-typed, tagging raw strings or numbers by default
// You cannot assign the wrong type to a field
def item: { inventory-id: 23, name: 'thingy', length: 12"m" };

'Field inventory-id $item.inventory-id -> get-number-type;
' -> !OUT::write
'Field name $item.name -> get-string-type;
' -> !OUT::write
'Field length $item.length -> get-number-type;
' -> !OUT::write

// You can define types and use as type-tests. This also defines a field.
// It would be an error to assign a non-standard plate to a standard-plate field
data standard-plate <'[A-Z]{3}[0-9]{3}'>

[['Audi', 'XYZ345'], ['BMW', 'I O U']]... -> \(
  when <?($(2) <standard-plate>)> do {make: $(1), standard-plate: $(2)}!
  otherwise {make: $(1), vanity-plate: $(2)}!
\) -> '$;
' -> !OUT::write

// You can define union types
data age <"years"|"months">

[ {name: 'Cesar', age: 20"years"},
  {name: 'Francesca', age: 19"years"},
  {name: 'Bobby', age: 11"months"}]...
-> \(
// Conditional tests on structures look a lot like literals, with field tests
  when <{age: <13"years"..19"years">}> do '$.name; is a teenager'!
  when <{age: <"months">}> do '$.name; is a baby'!
// You don't need to handle all cases, 'Cesar' will just be ignored
\) -> '$;
' -> !OUT::write

// Array/list indexes start at 1 by default, but you can choose
// Slices return whatever overlaps with the actual array
[1..5] -> $(-2..2) -> '$;
' -> !OUT::write // Outputs [1,2]
0:[1..5] -> $(-2..2) -> '$;
' -> !OUT::write // Outputs [1,2,3]
-2:[1..5] -> $(-2..2) -> '$;
' -> !OUT::write // Outputs [1,2,3,4,5]

// Arrays can have indexes of measures or tagged identifiers
def game-map: 0"y":[
  1..5 -> 0"x":[
    1..5 -> level´1:[
      1..3 -> {
        level: $,
        terrain-id: 6 -> SYS::randomInt,
        altitude: (10 -> SYS::randomInt)"m"
      }
    ]
  ]
];

// Projections (indexing) can span several dimensions
$game-map(3"y"; 1"x"..3"x"; level´1; altitude:) -> '$;
' -> !OUT::write // Gives a list of three altitude values

// Flatten and do a grouping projection to get stats
// Count and Max are built-in collector processors
[$game-map... ... ...] -> $(collect {
      occurences: Count,
      highest-on-level: Max&{by: :(altitude:), select: :(level:)}
    } by $({terrain-id:}))
-> !OUT::write
'
' -> !OUT::write

// Relations are sets of structures/records.
// Here we get all unique {level:, terrain-id:, altitude:} combinations
def location-types: {|$game-map... ... ...|};

// Projections can re-map structures. Note § is the relative accessor
$location-types({terrain-id:, foo: §.level::raw * §.altitude})
-> '$;
' -> !OUT::write

// Relational algebra operators can be used on relations
($location-types join {| {altitude: 3"m"} |})
-> !OUT::write
'
' -> !OUT::write

// Define your own operators for binary operations
operator (left dot right)
  $left -> \[i]($ * $right($i)!\)... -> ..=Sum&{of: :()} !
end dot

([1,2,3] dot [2,5,8]) -> 'dot product: $;
' -> !OUT::write

// Supply parameters to vary templates behaviour
templates die-rolls&{sides:}
  1..$ -> $sides::raw -> SYS::randomInt -> $ + 1 !
end die-rolls

[5 -> die-rolls&{sides:4}] -> '$;
' -> !OUT::write

// Pass templates as parameters, maybe with some parameters pre-filled
source damage-roll&{first:, second:, third:}
  (1 -> first) + (1 -> second) + (1 -> third) !
end damage-roll

$damage-roll&{first: die-rolls&{sides:4},
  second: die-rolls&{sides:6}, third: die-rolls&{sides:20}}
-> 'Damage done is $;
' -> !OUT::write

// Write tests inline. Run by --test flag on command line
// Note the ~ in the matcher means "not",
// and the array content matcher matches elements < 1 and > 4
test 'die-rolls'
  assert [100 -> die-rolls&{sides: 4}] <~[<..~1|4~..>]> 'all rolls 1..4'
end 'die-rolls'

// Provide modified modules to tests (aka test doubles or mocks)
// IN is the standard input object and ::lines gets all lines
source read-numbers
  $IN::lines -> #
  when <'\d+'> do $!
end read-numbers

test 'read numbers from input'
  use shadowed core-system/
    processor MockIn
      source lines
        [
          '12a',
          '65',
          'abc'
        ]... !
      end lines
    end MockIn
    def IN: $MockIn;
  end core-system/
  assert $read-numbers <=65> 'Only 65 is read'
end 'read numbers from input'

// You can work with byte arrays
composer hexToBytes
  <HEX>
end hexToBytes

'1a5c678d' -> hexToBytes -> ($ and [x 07 x]) -> $(last-1..last) -> '$;
' -> !OUT::write // Outputs 0005
```

## Further Reading

- [Main Tailspin site](https://github.com/tobega/tailspin-v0/)
- [Tailspin language reference](https://github.com/tobega/tailspin-v0/blob/master/TailspinReference.md)
