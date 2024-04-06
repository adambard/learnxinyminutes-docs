---
language: PCRE
filename: pcre.txt
contributors:
    - ["Sachin Divekar", "http://github.com/ssd532"]

---

A regular expression (regex or regexp for short) is a special text string for describing a search pattern. e.g. to extract domain name from a string we can say `/^[a-z]+:/` and it will match `http:` from `http://github.com/`.

PCRE (Perl Compatible Regular Expressions) is a C library implementing regex. It was written in 1997 when Perl was the de-facto choice for complex text processing tasks. The syntax for patterns used in PCRE closely resembles Perl. PCRE syntax is being used in many big projects including PHP, Apache, R to name a few.


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

PCRE provides some generic character types, also called as character classes.

```
  \d     any decimal digit
  \D     any character that is not a decimal digit
  \h     any horizontal white space character
  \H     any character that is not a horizontal white space character
  \s     any white space character
  \S     any character that is not a white space character
  \v     any vertical white space character
  \V     any character that is not a vertical white space character
  \w     any "word" character
  \W     any "non-word" character
```

## Examples

We will test our examples on the following string:

```
66.249.64.13 - - [18/Sep/2004:11:07:48 +1000] "GET /robots.txt HTTP/1.0" 200 468 "-" "Googlebot/2.1"
```

 It is a standard Apache access log.

| Regex | Result          | Comment |
| :---- | :-------------- | :------ |
| `GET`   | GET | GET matches the characters GET literally (case sensitive) |
| `\d+.\d+.\d+.\d+` | 66.249.64.13 | `\d+` match a digit [0-9] one or more times defined by `+` quantifier, `\.` matches `.` literally |
| `(\d+\.){3}\d+` | 66.249.64.13 | `(\d+\.){3}` is trying to match group (`\d+\.`) exactly three times. |
| `\[.+\]` | [18/Sep/2004:11:07:48 +1000] | `.+` matches any character (except newline), `.` is any character |
| `^\S+` | 66.249.64.13 | `^` means start of the line, `\S+` matches any number of non-space characters |
| `\+[0-9]+` | +1000 | `\+` matches the character `+` literally. `[0-9]` character class means single number. Same can be achieved using `\+\d+` |

## Further Reading
[Regex101](https://regex101.com/) - Regular Expression tester and debugger
