---
name: CUE
category: language
language: CUE
filename: learncue.cue
contributors:
    - ["Daniel Cox", "https://github.com/danielpcox"]
    - ["Coleman McFarland", "https://github.com/dontlaugh"]
---

CUE is an expressive (but not Turing-complete) JSON superset, exportable to JSON or YAML. It supports optional types and many other conveniences for working with large configuration sets. The unification engine has roots in logic programming, and as such it provides a ready solution to modern configuration management problems.

When CUE is exported to JSON, values from every processed file are unified into one giant object. Consider these two files:

```yaml
//name.cue
name: "Daniel"
```

```yaml
//disposition.cue
disposition: "oblivious"
```

Now we can unify and export to JSON:

```bash
% cue export name.cue disposition.cue
{
    "name": "Daniel",
    "disposition": "oblivious"
}
```

Or YAML:

```bash
% cue export --out yaml name.cue disposition.cue 
name: Daniel
disposition: oblivious
```

Notice the C-style comments are not in the output. Also notice that the keys in CUE syntax did not require quotes. Some special characters do require quotes:

```yaml
works_fine: true
"needs-quotes": true
```

Unification doesn't just unify across files, it is also a *global merge* of all types and values. The following fails, because the *types* are different.

```yaml
//string_value.cue
foo: "baz"
```

```yaml
//integer_value.cue
foo: 100
```

```bash
% cue export string_value.cue integer_value.cue 
foo: conflicting values "baz" and 100 (mismatched types string and int):
    integer_value.cue:1:6
    string_value.cue:1:6
```

But even if we quote the integer, it still fails, because the *values* conflict and there is no way to unify everything into a top-level object.

```yaml
//string_value.cue
foo: "baz"
```

```yaml
//integer_value.cue
foo: "100"  // a string now
```

```bash
% cue export string_value.cue integer_value.cue
foo: conflicting values "100" and "baz":
    integer_value.cue:1:6
    string_value.cue:1:6
```

Types in CUE *are* values; special ones that the unification engine knows have certain behavior relative to other values. During unification it requires that values match the specified types, and when concrete values are required, you will get an error if there's only a type. So this is fine:

```yaml
street: "1 Infinite Loop"
street: string
```

While `cue export` produces YAML or JSON, `cue eval` produces CUE. This is useful for converting YAML or JSON to CUE, or for inspecting the unified output in CUE itself. It's fine to be missing concrete values in CUE (though it prefers concrete values when emitting CUE when both are available and match),

```yaml
//type-only.cue
amount: float
```

```bash
% cue eval type-only.cue
amount: float
```

but you *need* concrete values if you want to export (or if you tell `eval` to require them with `-c`):

```bash
% cue export type-only.cue
amount: incomplete value float
```

Give it a value that unifies with the type, and all is well.

```yaml
//concrete-value.cue
amount: 3.14
```

```bash
% cue export type-only.cue concrete-value.cue
{
    "amount": 3.14
}
```

The method of unifying concrete values with types that share a common syntax is very powerful, and much more compact than, e.g., JSON Schema. This way, schema, defaults, and data are all expressible in CUE.

Default values may be supplied with a type using an asterisk:

```yaml
// default-port.cue
port: int | *8080
```

```bash
% cue eval default-port.cue
port: 8080
```

Enum-style options ("disjunctions" in CUE) may be specified with an `|` separator:

```yaml
//severity-enum.cue
severity: "high" | "medium" | "low"
severity: "unknown"
```

```bash
% cue eval severity-enum.cue
severity: 3 errors in empty disjunction:
severity: conflicting values "high" and "unknown":
    ./severity-enum.cue:1:11
    ./severity-enum.cue:1:48
severity: conflicting values "low" and "unknown":
    ./severity-enum.cue:1:31
    ./severity-enum.cue:1:48
severity: conflicting values "medium" and "unknown":
    ./severity-enum.cue:1:20
    ./severity-enum.cue:1:48
```

You can even have disjunctions of structs (not shown, but it works like you'd expect).

CUE has "definitions", and you can use them like you would variable declarations in other languages. They are also for defining struct types. You can apply a struct of type definitions to some concrete value(s) with `&`. Also notice you can say "a list with type #Whatever" using `[...#Whatever]`.

```yaml
// definitions.cue

#DashboardPort: 1337

configs: {
    host: "localhost"
    port: #DashboardPort
}

#Address: {
    street: string
    city: string
    zip?: int  // ? makes zip optional
}

some_address: #Address & {
  street: "1 Rocket Rd"
  city: "Hawthorne"
}

more_addresses: [...#Address] & [
  {street: "1600 Amphitheatre Parkway", city: "Mountain View", zip: "94043"},
  {street: "1 Hacker Way", city: "Menlo Park"}
]
```

```bash
% cue export --out yaml definitions.cue
configs:
  host: localhost
  port: 1337
some_address:
  street: 1 Rocket Rd
  city: Hawthorne
more_addresses:
  - street: 1600 Amphitheatre Parkway
    city: Mountain View
    zip: "94043"
  - street: 1 Hacker Way
    city: Menlo Park
```

CUE supports more complex values and validation:

```yaml
#Country: {
  name: =~"^\\p{Lu}" // Must start with an upper-case letter
  pop: >800 & <9_000_000_000 // More than 800, fewer than 9 billion
}

vatican_city: #Country & {
  name: "Vatican City"
  pop: 825
}
```

CUE may save you quite a bit of time with all the sugar it provides on top of mere JSON. Here we're defining, "modifying", and validating a nested structure in three lines: (Notice the `[]` syntax used around `string` to signal to the engine that `string` is a constraint, not a string in this case.) 

```yaml
//paths.cue

// path-value pairs
outer: middle1: inner: 3
outer: middle2: inner: 7

// collection-constraint pair
outer: [string]: inner: int
```

```bash
% cue export paths.cue
{
    "outer": {
        "middle1": {
            "inner": 3
        },
        "middle2": {
            "inner": 7
        }
    }
}
```

In the same vein, CUE supports "templates", which are a bit like functions of a single argument. Here `Name` is bound to each string key immediately under `container` while the struct underneath *that* is evaluated.

```yaml
//templates.cue

container: [Name=_]: {
    name:     Name
    replicas: uint | *1
    command:  string
}

container: sidecar: command: "envoy"

container: service: {
    command:  "fibonacci"
    replicas: 2
}
```

```bash
% cue eval templates.cue
container: {
    sidecar: {
        name:     "sidecar"
        replicas: 1
        command:  "envoy"
    }
    service: {
        name:     "service"
        command:  "fibonacci"
        replicas: 2
    }
}
```

And while we're talking about references like that, CUE supports scoped references.

```yaml
//scopes-and-references.cue
v: "top-level v"
b: v // a reference
a: {
    b: v // matches the top-level v
}

let V = v
a: {
    v: "a's inner v"
    c: v // matches the inner v
    d: V // matches the top-level v now shadowed by a.v
}
av: a.v // matches a's v
```

```bash
% cue eval --out yaml scopes-and-references.cue
```

```yaml
v: top-level v
b: top-level v
a:
  b: top-level v
  v: a's inner v
  c: a's inner v
  d: top-level v
av: a's inner v
```

I changed the order of the keys in the output for clarity. Order doesn't actually matter, and notice that duplicate keys at a given level are *all* unified.

You can hide fields be prefixing them with `_` (quote the field if you need a `_` prefix in an emitted field)

```yaml
//hiddens.cue
"_foo": 2
_foo:   3
foo:    4
_#foo:  5
#foo : 6
```

```bash
% cue eval hiddens.cue
"_foo": 2
foo:    4
#foo:   6

% cue export hiddens.cue
{
    "_foo": 2,
    "foo": 4
}
```

Notice the difference between `eval` and `export` with respect to definitions. If you want to hide a definition in CUE, you can prefix *that* with `_`.

Interpolation of values and fields:

```yaml
//interpolation.cue

#expense: 90
#revenue: 100
message: "Your profit was $\( #revenue - #expense)"

cat: {
    type: "Cuddly"
    "is\(type)":    true
}
```

```bash
% cue export interpolation.cue
{
    "message": "Your profit was $10",
    "cat": {
        "type": "Cuddly",
        "isCuddly": true
    }
}
```

Operators, list comprehensions, conditionals, imports...:

```yaml
//getting-out-of-hand-now.cue
import "strings"  // we'll come back to this

// operators are nice
g: 5 / 3         // CUE can do math
h: 3 * "blah"    // and Python-like string repetition
i: 3 * [1, 2, 3] // with lists too
j: 8 < 10        // and supports boolean ops

// conditionals are also nice
price: number
// Require a justification if price is too high
if price > 100 {
	justification: string
}
price:         200
justification: "impulse buy"

// list comprehensions are powerful and compact
#items: [ 1, 2, 3, 4, 5, 6, 7, 8, 9]
comp: [ for x in #items if x rem 2 == 0 {x*x}]

// and... well you can do this too
#a: [ "Apple", "Google", "SpaceX"]
for k, v in #a {
	"\( strings.ToLower(v) )": {
		pos:     k + 1
		name:    v
		nameLen: len(v)
	}
}
```

```bash
% cue export getting-out-of-hand-now.cue
```

```json
{
    "g": 1.66666666666666666666667,
    "h": "blahblahblah",
    "i": [1, 2, 3, 1, 2, 3, 1, 2, 3],
    "j": true,
    "apple": {
        "pos": 1,
        "name": "Apple",
        "nameLen": 5
    },
    "google": {
        "pos": 2,
        "name": "Google",
        "nameLen": 6
    },
    "price": 200,
    "justification": "impulse buy",
    "comp": [
        4,
        16,
        36,
        64
    ],
    "spacex": {
        "pos": 3,
        "name": "SpaceX",
        "nameLen": 6
    }
}
```

At this point it's worth mentioning that CUE may not be Turing-complete, but it *is* powerful enough for you to shoot yourself in the foot, so do try to keep it clear. It's easy to go off the deep end and make your config *harder* to work with if you're not careful. Make use of those comments, at least, and/or...

To that end, CUE supports packages and modules. CUE files are standalone by default, but if you put a package clause at the top, you're saying that file is unifiable with other files "in" the same package.

```yaml
//a.cue
package config

foo: 100
bar: int
```

```yaml
//b.cue
package config

bar: 200
```

If you create these two files in a new directory and run `cue eval` (no arguments), it will unify them like you'd expect. It searches the current directory for .cue files, and if they all have the same package, they will be unified.

Packages are more clear in the context of "modules". Modules are the *largest* unit of organization. Basically every time you have a project that spans multiple files, you should create a module and name it with something that looks like the domain and path of a URL, e.g., `example.com/something`. When you import anything from this module, even from *within* the module, you must do so using the fully-qualified module path which will be prefixed with this module name.

You can create a new module like so:

```bash
mkdir mymodule && cd mymodule
cue mod init example.com/mymodule
```

This creates a `cue.mod/` subdirectory within that `mymodule` directory, and `cue.mod/` contains the following file and subdirectories:

- `module.cue`  (which defines your module name, in this case with `module: "example.com/mymodule"`)
- pkg/
- gen/
- usr/

For a different perspective on this and details about what's in there, see https://cuelang.org/docs/concepts/packages/. For my purposes here, I'll say you don't need to think about the contents of this directory *at all*, except that your module name will be the prefix for all imports within your module.

Where will your module file hierarchy go? All files and directories for your module are rooted in `mymodule/`, the directory that also contains `cue.mod/`. If you want to import a package, you'll prefix it with `example.com/mymodule`, followed by a relative path rooted in `mymodule/`.

To make it concrete, consider the following:

```
mymodule
├── config
│   ├── a.cue
│   └── b.cue
├── cue.mod
│   ├── module.cue
│   ├── pkg
│   └── usr
└── main.cue
```

`cue.mod/` and the files underneath it were created by `cue mod init example.com/mymodule`. I then created the `config/` subdirectory with `a.cue` and `b.cue` inside. Then I created `main.cue` to act as my top-level file to rule them all.

Running `eval` (no arguments) checks to see if there's only one package in all .cue files in the current directory, and if so, it unifies them and outputs the result. In this case, there's only main.cue with package `main` (nothing special about "main" there, it just seemed appropriate), so that's the one.

```bash
% cue eval
configuredBar: 200
```

The contents of `main.cue` is:

```yaml
//main.cue

package main
import "example.com/mymodule/config"

configuredBar: config.bar
```

`config/a.cue` and `config/b.cue` are files from earlier, except now they've both got `package config` at the top:

```yaml
//a.cue
package config

foo: 100
bar: int
```

```yaml
//b.cue
package config

bar: 200
```

So there you go. If you want to verify that it's actually unifying both files under `config/`, you can change `bar: int` to `bar: string` in `a.cue` and re-run `cue eval` to get a nice type error:

```
cue eval                                                                     2022-01-06 17:51:24
configuredBar: conflicting values string and 200 (mismatched types string and int):
    ./config/a.cue:4:6
    ./config/b.cue:3:6
    ./main.cue:5:16
```

That's it for now. I understand there are more package management features coming in the future and the design decisions around `cue.mod` are looking ahead to that.

Finally, CUE has built-in modules with powerful functionality. We saw one of these earlier, when we imported "strings" and used `strings.ToLower`. Imports without fully-qualified module names are assumed to be built-ins. The full list and documentation for each is here: https://pkg.go.dev/cuelang.org/go/pkg

This has been a condensation of the official docs and tutorials, so go give the source material some love: https://cuelang.org/docs/tutorials/
