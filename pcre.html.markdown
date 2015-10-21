---
language: PCRE
filename: pcre.txt
contributors:
    - ["Sachin Divekar", "http://github.com/ssd532"]
    
---

A regular expression (regex or regexp for short) is a special text string for describing a search pattern. e.g. to extract domain name from a string we can say `/.[a-z0-9]+$/` and it will match .com from `http://github.com/`.  

PCRE (Perl Compatible Regular Expressions) is a C library implementing regex. It was written in 1997 when Perl was the de-facto choice for complex text processing tasks. 


There are two different sets of metacharacters:
* Those that are recognized anywhere in the pattern except within square brackets
```
  \      general escape character with several uses
  ^      assert start of string (or line, in multiline mode)
  $      assert end of string (or line, in multiline mode)
  .      match any character except newline (by default)
  [      start character class definition
  |      start of alternative branch
  (      start subpattern
  )      end subpattern
  ?      extends the meaning of (
         also 0 or 1 quantifier
         also quantifier minimizer
  *      0 or more quantifier
  +      1 or more quantifier
         also "possessive quantifier"
  {      start min/max quantifier
```

* Those that are recognized within square brackets. Outside square brackets. They are also called as character classes.
 
```
 
  \      general escape character
  ^      negate the class, but only if the first character
  -      indicates character range
  [      POSIX character class (only if followed by POSIX syntax)
  ]      terminates the character class
  
```  

## Examples

```PCRE

```
## Further Reading


