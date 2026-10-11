---
name: Gooo
contributors:
    - ["asketeddy", "https://github.com/kimjooyoon"]
---

Gooo describes typed data and operations with stable semantic identities.
This tutorial uses its early native execution subset: fixed, pure activities
compiled to Go. No model or inference service is needed.

The program below clamps a score to 0..100 and returns a labelled record.
It returns data; it does not print from inside the Gooo source. The CLI writes
each returned value as JSON. Use a POSIX shell, Git and Go with toolchain
selection support. The first build may download Go and module dependencies.

## 1. Pin the compiler

Start in a directory that does not contain `learn-gooo`:

```sh
mkdir learn-gooo
cd learn-gooo
export GOTOOLCHAIN=go1.27.2
export GOWORK=off
git clone https://github.com/kimjooyoon/meta-ontology-go.git compiler
git -C compiler checkout 366ee6ef8f9d2b9b57331dfd26b6f908c0289851
cd compiler
go build -o ../gooo ./cmd/gooo
cd ..
./gooo version --build --json
```

The build record must show that exact `compiler_source_sha`, `go1.27.2`, and
`vcs_modified: "false"`. Keep the following files beside `gooo`. Run all later
commands from this `learn-gooo` directory, in the same shell.

## 2. Define types and activities

Save this complete file as `limits.gooo`:

```text
// A file declares one package and namespace.
package limits
namespace learn_gooo_limits

// These builtin identities select signed int64 and Boolean representations.
// An ID identifies a declaration; it is not a URL that the runtime fetches.
entity Integer id "urn:gooo:type:integer"
entity Boolean id "urn:gooo:type:boolean"

// One input is called input. An activity returns exactly one value.
activity InRange(Integer) -> Boolean id "urn:learn-gooo:in-range"
computes `return input >= 0 && input <= 100`

activity Clamp(Integer) -> Integer id "urn:learn-gooo:clamp"
computes `if input < 0 { return 0 }
if input > 100 { return 100 }
return input`
```

`entity` names a type. `activity` declares its input and output types.
`computes` supplies the executable body, here inside backticks. Keep the first
and last body tokens next to the backticks; this pin requires canonical bodies.
Comments start with `//`. Comparisons produce Booleans; `&&` short-circuits.
An `if` guard may return early. `else` branches are supported too.

The builtin scalar IDs are `urn:gooo:type:integer`, `urn:gooo:type:boolean`
and `urn:gooo:type:string`. Their native values are `int64`, `bool` and
`string`.
These IDs, rather than the chosen display names, establish the scalar kinds.

## 3. Import code and return a record

Save this complete file as `main.gooo`:

```text
package report
namespace learn_gooo_report
import limits "example/limits"

entity Integer id "urn:gooo:type:integer"
entity Text id "urn:gooo:type:string"

// Field names are used in source and JSON; each field also has its own ID.
// Field types use the lowercase keywords integer, boolean and string.
entity Summary id "urn:learn-gooo:summary" fields {
    field score id "urn:learn-gooo:score" type integer required one
    field remaining id "urn:learn-gooo:remaining" type integer required one
    field in_range id "urn:learn-gooo:in-range-field" type boolean required one
    field label id "urn:learn-gooo:label" type string required one
}

// Multiple inputs are called input0, input1, ... in declaration order.
activity Describe(Integer, Text) -> Summary id "urn:learn-gooo:describe"
computes `let score = limits.Clamp(input0)
let label = input1 + ": low"
if score >= 60 { label = input1 + ": high" }
return Summary{score: score, remaining: 100 - score,
    in_range: limits.InRange(input0), label: label}`
```

`let` creates an inferred local variable; it is not an immutability promise.
The assignment to `label` changes that local. Inputs cannot be assigned to.
`+` concatenates Text; `-` subtracts integers. Record constructors name fields.
A record-valued local can be read with a selector such as `result.score`.
The `required one` fields above each hold one scalar value.

`limits` is the local import alias. `example/limits` is a workspace package
path, not a Go module or a network address. A call runs the declared Gooo
activity; it does not invoke an arbitrary Go function.

## 4. Register the workspace

Save this as `gooo.workspace.json`:

```json
{
  "schema": "gooo/package-workspace-manifest/v1",
  "entry": {"package_path": "app/report", "activity": "Describe"},
  "packages": [
    {"path": "example/limits", "sources": ["limits.gooo"]},
    {"path": "app/report", "sources": ["main.gooo"]}
  ]
}
```

The manifest maps import paths to source files and selects the entry activity.
`app/report` is the manifest path; `report` is the source package name.
Imports do not download dependencies. Source paths are relative to the manifest
and must remain inside its workspace: do not use `../` to import a sibling.

## 5. Supply inputs

Save this as `inputs.json`:

```json
{
  "schema": "gooo/body-composition-inputs/v1",
  "inputs": [
    {"app/report:Describe.input0": -5,
     "app/report:Describe.input1": "sensor"},
    {"app/report:Describe.input0": 42,
     "app/report:Describe.input1": "sensor"},
    {"app/report:Describe.input0": 75,
     "app/report:Describe.input1": "sensor"},
    {"app/report:Describe.input0": 105,
     "app/report:Describe.input1": "sensor"}
  ]
}
```

Each object is one invocation. The keys identify the entry package, activity
and argument position. A single-input activity instead uses `package:Activity`
without `.input0`. Keep integers as exact JSON integers, even beyond
JavaScript's safe integer range. This file contains no expected outputs.

## 6. Check declarations, then run

```sh
./gooo check limits.gooo
./gooo check main.gooo
./gooo package execute --inputs inputs.json gooo.workspace.json
```

`check` validates declarations; it does not establish that the body can run.
`package execute` generates Go, compiles it and runs the supplied inputs.
Its output contains one JSON record per input. Ignoring object-key order:

```json
{"score":0,"remaining":100,"in_range":false,"label":"sensor: low"}
{"score":42,"remaining":58,"in_range":true,"label":"sensor: low"}
{"score":75,"remaining":25,"in_range":true,"label":"sensor: high"}
{"score":100,"remaining":0,"in_range":false,"label":"sensor: high"}
```

Change an input and execute again to compute a new result. There is no implicit
zero-argument `main`, and `gooo run` without input only resolves a declaration;
it does not execute this program's `computes` bodies.

## 7. Save an execution record and replay

```sh
./gooo package execute --json --inputs inputs.json gooo.workspace.json \
  > execution.json
./gooo package replay --json --receipt execution.json \
  --inputs inputs.json gooo.workspace.json > replay.json
```

`--json` returns the complete record instead of the entry values alone. The
record retains source identities, generated code and execution observations.
Actual values are at `result.runtime.traces[].deliveries[].actual`. The entry
delivery in this example has `activity_id: "urn:learn-gooo:describe"`.

Replay runs the saved program against the supplied inputs. Keep its source and
workspace unchanged. Replay is a new execution, not a display of cached output;
its metadata need not be byte-for-byte identical to the original record.

## 8. Read the evidence correctly

These input-only records have `decision: "OBSERVED"`: values were executed and
recorded, with no caller-supplied expected outputs to score. It is not a
Go-reference comparison or an all-input correctness proof.

The separate `--cases` mode compares against supplied expectations. A `PASS`
there describes those cases, not where the expectations came from. An
independent reference implementation is a separate responsibility. Never use
`check` success as evidence of native execution.

## 9. Know this subset's boundaries

Use arithmetic, Boolean expressions, Text values, flat scalar records, locals,
conditionals and fixed activity calls. Bodies currently require 1..16 inputs.
This pin does not execute loops, array/map collections, byte indexing, bitwise
operators, recursive calls or arbitrary host calls. There is no free `print`
operation in this body profile. A returned Text value is printed as a JSON
string, including its quotes.

Text length and slicing exist, but their offsets count UTF-8 bytes rather than
Unicode characters. They are not demonstrated here. This tutorial also leaves
construction choices, models and other declaration profiles out of scope.
The `namespace` and IDs describe meaning; they do not grant external effects.

Continue with the [Gooo source and documentation][gooo] or the existing
[native importing examples][ports]. Those ports are separate Gooo sources,
not access to the whole Go standard library.

This tutorial was prepared with OpenAI Codex assistance for Gooo maintainer
asketeddy.

[gooo]: https://github.com/kimjooyoon/meta-ontology-go
[ports]: https://github.com/kimjooyoon/gooo-go-ports
