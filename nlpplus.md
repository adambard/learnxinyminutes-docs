---
name: NLP++
filename: learnnlpplus.nlp
contributors:
    - ["David de Hilster", "https://github.com/ddehilster"]
---

NLP++ is a programming language for natural language processing. Amnon
Meyers and David de Hilster created it in 1998 at Text Analysis
International, and it is now open source as part of
[VisualText](https://visualtext.org).

An NLP++ program is called an analyzer. It turns text into a parse tree, then
runs a sequence of passes over the tree. Each pass matches pattern rules
against the nodes of the tree and rewrites it, for example by grouping
`$` `5` into a single `_money` node. Code attached to a rule runs when the
rule matches. Analyzers can also build and query a knowledge base, a
hierarchy of concepts with attributes.

An analyzer is a folder:

```text
myanalyzer/
  spec/analyzer.seq   the passes, in order
  spec/*.nlp          one file per pass
  kb/user/            dictionaries and knowledge base files
  input/              text to analyze
```

`spec/analyzer.seq` lists the passes. Each line has a pass type, a pass file
and a comment. The first pass tokenizes the text:

```text
tokenize	nil	# split the text into tokens
nlp	basics	# runs spec/basics.nlp
nlp	funcs
nlp	title
nlp	person
nlp	money
nlp	people
nlp	report
```

The examples below run on this text:

```text
Dr. Ada Lovelace paid $5 on 12/10/1843.
```

```nlpplus
###############################################
# spec/basics.nlp: the language
###############################################

# A comment runs from a hash to the end of the line.
/* Block comments
   work too. */

# A pass file is made of regions, each starting with @. @CODE runs once,
# before the pass looks at the tree. A pass has at most one @CODE region,
# and it comes first.
@CODE

# Variables need no declaration. A one-letter function picks the scope:
G("count") = 3;    # global: lasts for the whole analyzer run
L("name") = "Ada"; # local: lasts for this region or function call
L("pi") = 3.14;
# N("x"), S("x") and X("x") are variables stored on parse tree nodes.
# They come up with rules, below.

# An unset variable is 0 (and also the empty string).
G("nothing");      # => 0

# Arithmetic
7 / 2;             # => 3
7 % 2;             # => 1
7.0 / 2;           # => 3.5

# Strings join with +. Joining a string and a number is an error, so
# convert first.
L("s") = "abc" + "def";              # => "abcdef"
L("t") = L("name") + str(G("count")); # => "Ada3"
strlength(L("s"));                    # => 6
strtoupper(L("s"));                   # => "ABCDEF"
num("42") + 1;                        # => 43

# Any variable can hold an array.
L("days")[0] = "Mon";
L("days")[1] = "Tue";
L("days")[2] = "Wed";
arraylength(L("days"));  # => 3

# Comparisons and logic give 1 or 0: == != < > <= >= && || !
"abc" < "abd";     # => 1

# if, else and while work as in C. There is no for loop.
L("i") = 0;
while (L("i") < 3) {
  if (L("i") == 1)
    "learn.txt" << "one ";
  else if (!L("i"))
    "learn.txt" << "zero ";
  else
    "learn.txt" << "other ";
  L("i")++;
}

# << writes to a file. It is created in input/<text file>_log/.
"learn.txt" << "Hello, " << L("name") << "!\n";

# Semicolons are optional.
@@CODE

###############################################
# spec/funcs.nlp: functions
###############################################

# Functions go in a @DECL region, which comes before any @CODE region.
# Passes after this one can call them: square(4) => 16
@DECL
square(L("n")) {
  return L("n") * L("n");
}
@@DECL

###############################################
# spec/title.nlp: rules
###############################################

# @NODES picks where in the tree the rules look. _ROOT is the top of the
# tree. Node names start with an underscore.
@NODES _ROOT

# A rule has a suggested node, an arrow, the elements to match, and @@.
# With no @POST region, a match replaces the matched nodes with one node
# named after the suggested node: "Dr" "." becomes a _title node.
@RULES
_title <- _xWILD [one match=(Dr Mr Mrs Ms)] \. [opt] @@

# An element is a literal word, a node name, or an escaped character
# like \. or \$. The square brackets hold modifiers:
#   opt               optional
#   star, plus        zero or more, one or more
#   min=1 max=3       repetition bounds
#   one match=(a b)   with _xWILD: exactly one of these
#   s                 also match inside a node that has only one child
#
# _x names match classes of tokens:
#   _xALPHA _xNUM _xPUNCT _xWHITE _xCAP (capitalized word) _xANY
#   _xWILD matches any nodes, up to whatever the next element matches

###############################################
# spec/person.nlp
###############################################

# Each pass makes one left-to-right sweep. A rule can't use a node that
# another rule in the same pass has just built, so _title is used in the
# next pass.
@NODES _ROOT

# @POST runs when the rule matches. N("$text", 3) is the text of element 3.
# S("first") sets a variable on the node the rule builds. Once @POST has
# code, you call single() yourself to build the node.
@POST
  S("first") = N("$text", 3);
  S("last") = N("$text", 5);
  single();
@RULES
_person <-
    _title           # 1
    _xWHITE [star]   # 2  the tokenizer keeps spaces as nodes
    _xCAP            # 3
    _xWHITE [star]   # 4
    _xCAP            # 5
    @@

###############################################
# spec/money.nlp: checks
###############################################

@NODES _ROOT

@POST
  S("amount") = num(N("$text", 2));  # $5 => a _money node, amount 5
  single();
@RULES
_money <- \$ _xNUM @@

# @CHECK runs before @POST and can reject a match with fail().
@CHECK
  if (num(N("$text", 1)) > 31)
    fail();          # 45/10/1843 is not a date
@POST
  S("year") = num(N("$text", 5));
  single();
@RULES
_date <- _xNUM \/ _xNUM \/ _xNUM @@

###############################################
# spec/people.nlp: the knowledge base
###############################################

# The knowledge base is a tree of concepts with attributes. It lasts for
# the whole run, so passes can share what they find.
@CODE
G("people") = getconcept(findroot(), "people");  # finds or makes it
@@CODE

@NODES _ROOT

# N("first", 1) reads a variable from element 1, the _person node.
# group(1, 5, "_payment") builds a node from elements 1 to 5. The
# suggested node _xNIL means "build nothing" when the rule matches.
@POST
  L("who") = N("first", 1) + " " + N("last", 1);
  L("person") = getconcept(G("people"), L("who"));
  addstrval(L("person"), "paid", str(N("amount", 5)));
  group(1, 5, "_payment");
@RULES
_xNIL <-
    _person
    _xWHITE [star]
    paid             # a literal word matches that word
    _xWHITE [star]
    _money
    @@

###############################################
# spec/report.nlp
###############################################

@CODE
L("p") = down(G("people"));           # first child concept
while (L("p")) {
  "learn.txt" << conceptname(L("p"))  # Ada Lovelace
              << " paid "
              << strval(L("p"), "paid") # 5
              << "\n";
  L("p") = next(L("p"));              # next sibling
}
@@CODE
```

## Running an analyzer

[VisualText for VS Code][vscode] runs analyzers and shows the parse tree
after every pass. From the command line:

```bash
nlp -ANA myanalyzer -IN myanalyzer/input/text.txt -WORK <engine folder>
```

Add `-DEV` to write the parse tree after each pass to
`input/text.txt_log/`.

## Further reading

* [NLP++ textbook and help](https://github.com/VisualText/visualtext-files)
* [The NLP++ engine](https://github.com/VisualText/nlp-engine)
* [Analyzer template](https://github.com/VisualText/nlpplus-analyzer-template)
* [NLP++ on Rosetta Code](https://rosettacode.org/wiki/Category:NLP%2B%2B)

[vscode]: https://marketplace.visualstudio.com/items?itemName=dehilster.nlp
