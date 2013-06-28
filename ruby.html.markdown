---
language: ruby
author: David Underwood
author_url: http://theflyingdeveloper.com
---

```ruby
# This is a comment

=begin
This is a multiline comment
No-one uses them
You shouldn't either
=end


3 #=> 3


# Some basic arithmetic
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7

# Special values
nil #=> Nothing to see here
true #=> truth
false #=> falsehood

# Equality
1 == 1 #=> true
2 == 1 #=> false

# Inequality
1 != 1 #=> false
2 != 1 #=> true
!true  #=> false
!false #=> true

# More comparisons
1 < 10 #=> True
1 > 10 #=> False
2 <= 2 #=> True
2 >= 2 #=> True

'I am a string'
"I am a string too"

placeholder = "use variables inline"
"I can #{placeholder} when using double quoted strings"
#=> "I can use variables inline when using double quoted strings"


# print to the output
puts "I'm printing!"

# Variables
x = 25 #=> 25

# Note that assignment returns the value assigned
# This means you can do multiple assignment:

x = y = 10 #=> 10
x #=> 10
y #=> 10

# By convention, use snake_case for variable names
snake_case = true

# Use descriptive variable names
path_to_project_root = '/good/name/'
path = '/bad/name/'

# Arrays

# This is an array
[1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# Arrays can contain different types of items

array = [1, "hello", false] #=> => [1, "hello", false]

# Arrays can be indexed
# From the front
array[0] #=> 1
array[12] #=> nil

# From the end
array[-1] #=> 5

# With a start and end index
array[2, 4] #=> [3, 4, 5]

# Or with a range
array[1..3] #=> [2, 3, 4]

# Add to the end of an array like this
array << 6 #=> [1, 2, 3, 4, 5, 6]

# Or like this
array.push 7 #=> [1, 2, 3, 4, 5, 6, 7]

# Or to the beginning like this
array.unshift 0 #=> [0, 1, 2, 3, 4, 5, 6, 7]

# Remove the first item in an array

array.shift #=> [1, 2, 3, 4, 5, 6, 7]

# Or the last

array.pop #=> [1, 2, 3, 4, 5, 6]

# Note that push and pop do the opposite of each other
# Shift and unshift are the same.

# Control structures

if true
  "if statement"
elsif false
 "else if, optional"
else
 "else, also optional"
end

for counter in 1..5
  puts "iteration #{counter}"
end
#=> iteration 1
#=> iteration 2
#=> iteration 3
#=> iteration 4
#=> iteration 5

# HOWEVER
# No-one uses for loops
# Use `each` instead, like this:

(1..5).each do |counter|
  puts "iteration #{counter}"
end
#=> iteration 1
#=> iteration 2
#=> iteration 3
#=> iteration 4
#=> iteration 5

counter = 1
while counter <= 5 do
  puts "iteration #{counter}"
end
#=> iteration 1
#=> iteration 2
#=> iteration 3
#=> iteration 4
#=> iteration 5

grade = 'B'
case grade
when 'A'
  puts "way to go kiddo"
when 'B'
  puts "Better luck next time"
when 'C'
  puts "You can do better"
when 'D'
  puts "Scraping through"
when 'F'
  puts "You failed!"


# Functions

def double(x)
  x * 2
end

# Functions (and all blocks) implcitly return the value of the last statement
double(2) #=> 4

# Parentheses are optional where the result is unambiguous
double 3 #=> 6

double double 3 #=> 12

def sum(x,y)
  x + y
end

# Method arguments are separated by a comma
sum 3, 4 #=> 7

sum sum(3,4), 5 #=> 12

# yield
# All methods have an implicit, optional block parameter
# it can be called with the 'yield' keyword


```