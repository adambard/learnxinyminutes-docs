---
language: Ruby
author: Joel Walden
author_url: http://joelwalden.net
filename: learnruby.rb
---

Ruby was created by Yukihiro "Matz" Matsumoto in the 1990s. It is a multi-paradigm language with many uses, from command line scripting to full blown web applications.

Ruby was written for humans first and machines second, making it easy to read and (dare I say it) fun to program with.

Feedback would be highly appreciated! You can reach me at [@joelwalden](http://twitter.com/joelwalden) or joel.walden [at] [google's email service]

This guide is for Ruby 2.0.0

```ruby
# Single line comments start with a hash.
=begin
	Though rarely used, multi-line comments can be written
	like this. Note that the indentation is not
	significant, nor is it significant anywhere in Ruby.
=end

####################################################
## 1. Primitive Datatypes and Operators
####################################################

# You have numbers
3 #=> 3

# Math is what you would expect
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7

# Division is a bit tricky. It is integer division and floors the results
# automatically.
5 / 2 #=> 2

# To fix division we need to learn about floats.
5.0    # This is a float
5.0 / 2.0 #=> 2.5 Ahhh...much better

# Also, note that only one of the args has to be a float to make the result a float.
5.0 / 2 #=> 2.5

# Enforce precedence with parentheses
(1 + 3) * 2 #=> 8

# Boolean values are primitives (note the lowercase)
true
false

# Negate with bang (!)
!true #=> false
!false #=> true

# Equality is ==
1 == 1 #=> true
2 == 1 #=> false

# Equality takes type into account
1 == "1" #=> false

# Inequality is !=
1 != 1 #=> false
2 != 1 #=> true

# More comparisons
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# Strings are created with " or '
"This is a string."
'This is also a string.'

# Strings can be concatenated with addition.
"Hello " + "world!" #=> "Hello world!"

# A string can be treated like an array of characters
"This is a string"[0] #=> 'T'

# % can be used to format strings, like this:
"%s is a %s" % ["This", "string"] #=> "This is a string"

# But the more "Rubyish" way is to use interpolation.
# Let's create a couple of variables
var_1 = "Strings" # = is the assignment operator. Convention is to use lowercase and underscores for variables.
var_2 = "interpolated"

"#{var_1} can be #{var_2}!" #=> "Strings can be interpolated!"

# You can't use single quotes when interpolating
'#{var_1} can be #{var_2}!' #=> "\#{var_1} can be \#{var_2}"

# nil is an object
nil #=> nil

# You can use the .nil? method to compare objects to nil.
var_1.nil? #=> false

# Everything is an object in Ruby, including true and false. They even have a class!
true.class #=> TrueClass

# nil, 0, and empty strings/lists all evaluate to false (for control flow purposes; they are not equivalent).
# All other values are true


####################################################
## 2. Variables and Collections
####################################################

# Printing is pretty easy ("puts" is short for "outputs")
puts "I'm Ruby. Nice to meet you!"

# Ruby also has a "print" method, which does not enforce a newline after each argument, like "puts" does.
# I'll be using puts from now on for consistency.

# No need to declare variables before assigning to them.
some_var = 5    # Convention is to use lower_case_with_underscores
some_var #=> 5

# Accessing a previously unassigned variable is an exception.
# See Control Flow to learn more about exception handling.
some_other_var  #=> NameError: undefined local variable or method 'some_other_var' for main:Object

# Arrays store sequences
arr = []
# They can also be created with a method
second_arr = Array.new
# You can start with a prefilled list
other_arr = [4, 5, 6]

# Add stuff to the end of a list with push
arr.push(1)    #arr is now [1]
arr.push(2)    #arr is now [1, 2]
arr.push(4)    #arr is now [1, 2, 4]
arr.push(3)    #arr is now [1, 2, 4, 3]
# Remove from the end with pop
arr.pop()        #=> 3 and arr is now [1, 2, 4]
# Remove from the beginning with shift
arr.shift      #=> 1 and arr is now [2, 4]. Note we can exclude the parentheses.
# Let's put them back
arr.push(3)    # arr is now [2, 4, 3]
arr.unshift 1   # Makes sense. arr is now [1, 2, 4, 3] again! Again, we excluded the parentheses. The interpreter will add them.

# This is a good time to point out that methods (functions) are called on an object. They don't just hang out there and take arguments most of the time.
# This is a language design choice that makes it easier to see what is functionally happening in a program.
# (i.e: what is being affected).

# Access an array like you would any array
arr[0] #=> 1
# Look at the last element
arr[-1] #=> 3

# Looking out of bounds returns nil
arr[4] #=> nil

# You can look at ranges too!
arr[1..3] #=> [2, 4, 3]
# Three dots close the range
arr[1...3] #=> [2, 4]

# Remove an element by value with the delete method
arr.delete(2) #=> 2 and arr is now [1, 4, 3]

# Remove an element by index with the delete_at method
arr.delete_at(2) #=> 3 and arr is now [1, 4]

other_arr = [1, 2, 4]

# You can add arrays
arr + other_arr #=> [1, 4, 1, 2, 4]

# Check for existence in an array with include?
arr.include?(1) #=> true

# Examine the length of an array with length
arr.length #=> 2

# Hashes store mappings
empty_hash = {}
# You can also use a method call
empty_hash = Hash.new
# Here is a prefilled hash
filled_hash = {"one" => 1, "two" => 2, "three" => 3}

# Look up values with []
filled_hash["one"] #=> 1

# Get all keys as a list
filled_hash.keys #=> ["one", "two", "three"]

# Get all values as a list
filled_hash.values #=> [1, 2, 3]

# Check for existence of keys in a hash with include?
filled_hash.include?("one") #=> true
filled_hash.include?(1) #=> false

 # Looking up a non-existing key returns nil
filled_hash["four"] #=> nil

####################################################
## 3. Control Flow
####################################################

# Let's just make a variable
some_var = 5

# Here is an if statement. Indentation is insignificant in Ruby, but it's nice to stick with a formatting scheme.
# puts "some_var is smaller than 10"
if some_var > 10
    puts "some_var is bigger than 10."
elsif some_var < 10
    puts "some_var is smaller than 10."
else
    puts "some_var is indeed 10."
end   # Use the end keyword to close blocks



# for loops iterate over lists
# The following puts:
# dog is an animal
# cat is an animal
# mouse is an animal

for animal in ["dog", "cat", "mouse"]
    puts "#{animal} is an animal"
end

# You can also do ranges!
# The following puts:
#   1
#   2
#   3
#   4

for i in (1..4)
	puts i
end

# while loops go until a condition is no longer met.
# The following puts:
#    0
#    1
#    2
#    3

i = 0
while i < 4 do
    puts x
    i += 1  # Shorthand for i = i + 1
end

# Handle exceptions with a begin/rescue block

begin
    # Use raise to raise an error
    raise NameError
rescue NameError
    puts "Exception Caught!"
end


####################################################
## 4. Functions
####################################################

# Use def to create new functions
def add(x, y)
    puts "x is #{x} and y is #{y}"
    return x + y    # Return values with a return statement
end

# Calling functions with parameters
add(5, 6) #=> 11 and puts "x is 5 and y is 6"

# You can define functions that take a variable number of
# positional arguments
def varargs(*args)
    return args
end

# Note args is an array, which you can put anything in (including other arrays and hashes!)
varargs(1, 2, 3) #=> [1, 2, 3]

# Ruby supports anonymous functions, called blocks. Both syntaxes below are equivalent

(1..10).each do |i|
	puts i
end

(1..10).each {|i| puts i }

####################################################
## 5. Classes
####################################################

# Simply instantiate a class with the class keyword
class Human

     # A class variable. It is shared by all instances of this class.
    @@species = "H. sapiens"

    # Basic initializer
    def initialize(name, age=0):
        # Assign the argument to the "name" instance variable for the instance
        @name = name
		# If no age given, we will fall back to the default in the arguments list.
		@age = age
	end
	
	# Basic setter method
	def name=(name)
		@name = name
	end
	
	# Basic getter method
	def name
		@name
	end

    # A class method; uses self to distinguish from instance methods. (Can only be called on class, not an instance).
	
    def self.say(msg)
       puts "#{msg}"
	end

    def species
        @@species
	end

end


# Instantiate a class
jim = Human.new("Jim Halpert")

dwight = Human.new("Dwight K. Schrute")

# Let's call a couple of methods
jim.species #=> "H. sapiens"
jim.name #=> "Jim Halpert"
dwight.species #=> "H. sapiens"
dwight.name #=> "Dwight K. Schrute"

# Call the class method
Human.say("Hi") #=> "Hi"


####################################################
## 6. Modules
####################################################

# You can require modules
require 'mathn'  # Dummy example, this actually changes the behavior of the already-required Math module in ways not covered here.
puts Math.cos(Math::PI) #=> -1.0

# You can also embed modules in a class using include
# However, if the module is in an external file, it must be required first

# Ruby modules are just ordinary ruby files. You
# can write your own, and import them. The name of the 
# module is the same as the name of the file.
# They can include methods as well as constants (as seen in the above example)


```

## Further Reading

Still up for more? Try:

* [Learn Ruby The Hard Way](http://ruby.learncodethehardway.org/)
* [Ruby on Codecademy](http://www.codecademy.com/tracks/ruby)
* [The Official Docs](http://www.ruby-doc.org/)
* [Why's Poignant Guide to Ruby](http://mislav.uniqpath.com/poignant-guide/book/chapter-1.html) For the lovers of the abstract and weird
* [Railscasts](http://railscasts.com) Guides to the popular Ruby web framework
