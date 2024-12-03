---
language: CSV
filename: learncsv.csv
contributors:
- [Timon Erhart, 'https://github.com/turbotimon/']
---

CSV (Comma-Separated Values) is a lightweight file format used to store tabular data in plain text, designed for easy data exchange between programs, particularly spreadsheets and databases. Its simplicity and human readability have made it a cornerstone of data interoperability. It is often used for moving data between programs with incompatible or proprietary formats.

While RFC 4180 provides a standard for the format, in practice, the term "CSV" is often used more broadly to refer to any text file that:

- Can be interpreted as tabular data
- Uses a delimiter to separate fields (columns)
- Uses line breaks to separate records (rows)
- Optionally includes a header in the first row


```csv
Name, Age, DateOfBirth
Alice, 30, 1993-05-14
Bob, 25, 1998-11-02
Charlie, 35, 1988-03-21
```

**Delimiters for Rows and Columns**

Rows are typically separated by line breaks (`\n` or `\r\n`), while columns (fields) are separated by a specific delimiter. Although commas are the most common delimiter for fields, other characters, such as semicolons (`;`), are commonly used in regions where commas are decimal separators (e.g., Germany). Tabs (`\t`) are also used as delimiters in some cases, with such files often referred to as "TSV" (Tab-Separated Values).

Example using semicolons as delimiter and comma for decimal separator:

```csv
Name; Age; Grade
Alice; 30; 50,50
Bob; 25; 45,75
Charlie; 35; 60,00
```

**Data Types**

CSV files do not inherently define data types. Numbers and dates are stored as plain text, and their interpretation depends on the software importing the file. Typically, data is interpreted as follows:

```csv
Data, Comment
100, Interpreted as a number (integer)
100.00, Interpreted as a number (floating-point)
2024-12-03, Interpreted as a date or a string (depending on the parser)
Hello World, Interpreted as text (string)
"1234", Interpreted as text instead of a number
```

**Quoting Strings and Special Characters**

Quoting strings is only required if the string contains the delimiter, special characters, or otherwise could be interpreted as a number. However, it is often considered good practice to quote all strings to enhance readability and robustness.

```csv
Quoting strings examples,
Unquoted string,
"Optionally quoted string (good practice)",
"If it contains the delimiter, it needs to be quoted",
"Also, if it contains special characters like \n newlines or \t tabs",
"The quoting "" character itself typically is escaped by doubling the quote ("")",
"or in some systems with a backslash \" (like other escapes)",
```

However, make sure that for one document, the quoting method is consistent. For example, the last two examples of quoting with either "" or \" would not be consistent and could cause problems.

**Encoding**

Different encodings are used. Most modern CSV files use UTF-8 encoding, but older systems might use others like ASCII or ISO-8859.

If the file is transferred or shared between different systems, it is a good practice to explicitly define the encoding used, to avoid issues with character misinterpretation.

### More Resources

+ [Wikipedia](https://en.wikipedia.org/wiki/Comma-separated_values)
+ [RFC 4180](https://datatracker.ietf.org/doc/html/rfc4180)
