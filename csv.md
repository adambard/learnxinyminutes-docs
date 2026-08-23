---
name: CSV
contributors:
    - [Timon Erhart, 'https://github.com/turbotimon/']
---

CSV (Comma-Separated Values) is a file format used to store tabular
data in plain text.

```csv
Name,Age,DateOfBirth,Comment
Alice,30,1993-05-14,
Bob,25,1998-11-02,
Eve,,,data might be missing because it's just text
"Charlie Brown",35,1988-03-21,strings can be quoted
"Louis XIV, King of France",76,1638-09-05,strings containing commas must be quoted
"Walter ""The Danger"" White",52,1958-09-07,quotes are escaped by doubling them up
Joe Smith,33,1990-06-02,"multi line strings
span multiple lines
there are no escape characters"
```

The first row might be a header of field names or there might be no header and
the first line is already data.

## Delimiters

Rows are separated by line breaks (`\n` or `\r\n`), columns are separated by a comma.

Tabs (`\t`) are sometimes used instead of commas and those files are called "TSVs"
(Tab-Separated Values). They are easier to paste into Excel.

Occasionally other characters can be used, for example semicolons (`;`) may be used
in Europe because commas are [decimal separators](https://en.wikipedia.org/wiki/Decimal_separator)
instead of the decimal point.

```csv
Name;Age;Grade
Alice;30;50,50
Bob;25;45,75
Charlie;35;60,00
```

## Data Types

CSV files do not inherently define data types. Numbers and dates are stored as
text. Interpreting and parsing them is left up to software using them.
Typically, data is interpreted as follows:

```csv
Data,Comment
100,Interpreted as a number (integer)
100.00,Interpreted as a number (floating-point)
2024-12-03,Interpreted as a date or a string (depending on the parser)
Hello World,Interpreted as text (string)
"1234",Interpreted as text instead of a number
```

## Further reading

* [Wikipedia](https://en.wikipedia.org/wiki/Comma-separated_values)
* [RFC 4180](https://datatracker.ietf.org/doc/html/rfc4180)
