---
name: Nextflow
contributors:
    - ["Ben Sherman", "https://github.com/bentsherman"]
filename: learnnextflow.nf
---

[Nextflow](https://www.nextflow.io/) is a workflow language for writing
data-driven computational pipelines, widely used in bioinformatics. A pipeline
is a set of *processes*, which run scripts written in any language, connected
by asynchronous *channels*. Nextflow runs tasks in parallel, caches results so
that failed runs can be resumed, and runs the same pipeline on a laptop, an
HPC cluster, or the cloud.

Nextflow runs on the JVM and its syntax is based on [Groovy](../groovy/). This
article assumes Nextflow 26.04 or later.

```groovy
/*
  Set yourself up:

  1) Install Java 17 or later
  2) Install Nextflow: curl -s https://get.nextflow.io | bash
  3) Run a script: nextflow run hello.nf

  A script with only statements (no processes or workflows) runs as-is,
  like the one below.
*/

// Single-line comments start with two slashes
/*
 * Multi-line comments look like this
 */

// Print to the console
println('Hello, World!')

////////////////////////////////////////////////////
// Variables and types
////////////////////////////////////////////////////

// Declare variables with `def`
def x = 42
def pi = 3.14
def name = 'Alice'
def happy = true
def nothing = null

// Optionally add a type annotation after the name
def count: Integer = 0
def sample: String = 'sample_1'

// Types ending in `?` can be null
def maybe: String? = null

// Some types are specific to pipelines
def mem = 8.GB                   // MemoryUnit
def time = 2.h                   // Duration
def reads = file('reads.fastq')  // Path

////////////////////////////////////////////////////
// Strings
////////////////////////////////////////////////////

// Single quotes are plain strings
def plain = 'no $interpolation here'

// Double quotes support interpolation
def greeting = "Hello, ${name}!"
def math = "2 + 2 = ${2 + 2}"

// Triple quotes span multiple lines
def text = """
    Dear ${name},
    How are you?
    """

// Regular expressions
assert 'hello world' =~ /hello/     // find
assert 'hello' ==~ /h.*o/           // exact match
def m = '2.7.3' =~ /(\d+)\.(\d+)\.(\d+)/
assert m[0][1] == '2'

////////////////////////////////////////////////////
// Collections
////////////////////////////////////////////////////

// Lists
def list = [1, 2, 3]
assert list[0] == 1
assert list.size() == 3
assert list + [4] == [1, 2, 3, 4]
assert 2 in list

// Sets have no order and no duplicates
assert [1, 2, 2, 3].toSet().size() == 3

// Maps
def scores = [alice: 100, bob: 92]
assert scores['alice'] == 100
assert scores.bob == 92

// `+` returns a new map, which is safer than mutating the original
def more = scores + [carol: 88]

////////////////////////////////////////////////////
// Records and tuples
////////////////////////////////////////////////////

// Records store named fields, like an immutable map
def person = record(name: 'Alice', age: 42)
assert person.name == 'Alice'
// person.foo       // error: unrecognized property `foo`
// person.age = 43  // error: records are immutable

// Use `+` to create a new record with updated fields
def older = person + record(age: 43)
assert older.age == 43

// Tuples store a fixed sequence of values, accessed by index
def pair = tuple('sample_1', 100)
assert pair[0] == 'sample_1'

// Tuples can be destructured
def (id, total) = pair

////////////////////////////////////////////////////
// Operators and control flow
////////////////////////////////////////////////////

assert 2 ** 8 == 256              // exponent
assert 7 % 2 == 1                 // modulo
assert (true && !false) == true   // logic

// if / else
if (x > 40) {
    println('big')
}
else {
    println('small')
}

// Ternary operator
def size = x > 40 ? 'big' : 'small'

// Elvis operator returns the right side if the left side is falsy
assert (scores['dave'] ?: 0) == 0

// Safe navigation returns null instead of raising an error
assert nothing?.size() == null

// Raise an error with `error`
if (x < 0) {
    error("Invalid value: ${x}")
}

////////////////////////////////////////////////////
// Closures
////////////////////////////////////////////////////

// A closure is a function that can be used as a value
def square = { v -> v * v }
assert square.call(3) == 9

// There are no `for` or `while` loops. Use higher-order functions instead
[1, 2, 3].each { v -> println(v) }
assert [1, 2, 3].collect { v -> v * v } == [1, 4, 9]
assert [1, 2, 3, 4].findAll { v -> v % 2 == 0 } == [2, 4]
assert [1, 2, 3].inject(0) { acc, v -> acc + v } == 6
assert [1, 2, 3].every { v -> v > 0 }

// Closures can destructure tuples
[tuple('a', 1), tuple('b', 2)].each { key, value ->
    println("${key} = ${value}")
}

// Maps can also be iterated
scores.each { key, value -> println("${key}: ${value}") }
```

## Channels and operators

A workflow does not compute values directly. It builds a *dataflow graph*
of channels, operators, and processes, which Nextflow then executes
asynchronously as data becomes available.

```groovy
// declare at the top of each script to enable static typing
nextflow.enable.types = true

workflow {
    // A channel is an asynchronous sequence of values
    nums = channel.of(1, 2, 3, 4)                // Channel<Integer>

    // A dataflow value is a single asynchronous value
    index = channel.value(file('genome.fa'))     // Value<Path>

    // Channel values can't be accessed directly -- use operators instead
    nums.map { v -> v * v }.view()               // 1, 4, 9, 16
    nums.filter { v -> v % 2 == 0 }.view()       // 2, 4
    nums.flatMap { v -> [v, v] }.view()          // 1, 1, 2, 2, ...
    nums.collect().view()                        // [1, 2, 3, 4]
    nums.reduce { acc, v -> acc + v }.view()     // 10
    nums.mix(channel.of(5, 6)).view()            // 1..6, in any order

    // Operators run asynchronously, so output order is not guaranteed

    // Operators are especially useful with records
    left = channel.of(
        record(id: 'A', reads: 100),
        record(id: 'B', reads: 200),
    )
    right = channel.of(
        record(id: 'B', bam: file('B.bam')),
        record(id: 'A', bam: file('A.bam')),
    )

    // Join two channels of records by a matching field
    left.join(right, by: 'id').view()
    // [id:A, reads:100, bam:/path/to/A.bam]
    // [id:B, reads:200, bam:/path/to/B.bam]

    // Add fields to every record (plain values or dataflow values)
    left.combine(genome: index).view()
    // [id:A, reads:100, genome:/path/to/genome.fa]
    // [id:B, reads:200, genome:/path/to/genome.fa]

    // Group values by key, given (key, value) tuples
    channel.of(tuple('x', 1), tuple('y', 2), tuple('x', 3))
        .groupBy()
        .view()
    // [x, [1, 3]]
    // [y, [2]]
}
```

## A complete pipeline

Save this as `main.nf`, write a `samples.csv` with `id` and `fastq`
columns, and run `nextflow run main.nf --input samples.csv`.

```groovy
#!/usr/bin/env nextflow

// Enable typed processes and workflows
nextflow.enable.types = true

// Include definitions from other scripts or from the Nextflow registry
// include { FASTQC } from './modules/fastqc'
// include { BWA_MEM } from 'nf-core/bwa/mem'

////////////////////////////////////////////////////
// Parameters
////////////////////////////////////////////////////

// Pipeline parameters are declared with a name, type, and optional default.
// They can be set on the command line, e.g. `--input samples.csv`
params {
    // Parameters without a default are required
    input: Path

    // Parameters with a default are optional
    min_reads: Integer = 2

    // Boolean parameters default to false
    save_summary: Boolean
}

////////////////////////////////////////////////////
// Record types and functions
////////////////////////////////////////////////////

// A record type specifies the fields that a record must have.
// Records are duck-typed: extra fields are allowed
record Sample {
    id: String
    fastq: Path
}

record QcSample {
    id: String
    fastq: Path
    num_reads: Integer
    gc: Path
    notes: String?      // optional field
}

// Enums define a fixed set of values
enum Strandedness {
    FORWARD,
    REVERSE,
    UNSTRANDED,
}

// Functions can declare parameter types and a return type
def isPassing(sample: QcSample, min_reads: Integer) -> Boolean {
    sample.num_reads >= min_reads
}

////////////////////////////////////////////////////
// Processes
////////////////////////////////////////////////////

// A process runs a script for each set of inputs. Each execution is a
// *task*, which runs in its own work directory, in parallel with other
// tasks, on any supported executor (local, HPC, cloud)
process COUNT_READS {
    // Directives control how each task is executed
    tag sample.id
    cpus 1
    memory 1.GB
    container 'ubuntu:24.04'

    input:
    // Each input has a name and a type.
    // `Path` inputs (and `Path` fields in records) are staged automatically
    sample: Sample

    output:
    // Outputs are plain values, built with output functions such as
    // `stdout()`, `file()`, `files()`, `env()`, and `eval()`
    record(
        id: sample.id,
        num_reads: stdout().trim().toInteger(),
    )

    script:
    // The script is a string executed by bash. Nextflow variables use `$`,
    // so escape Bash variables with `\$`
    """
    echo \$(( \$(wc -l < ${sample.fastq}) / 4 ))
    """
}

process GC_CONTENT {
    tag id

    input:
    // Records can be destructured into individual inputs
    record(
        id: String,
        fastq: Path
    )

    stage:
    // The stage section customizes how inputs are staged
    stageAs fastq, 'reads.fq'
    env 'SAMPLE_ID', id

    output:
    record(
        id: id,
        gc: file("${id}.gc.txt"),
    )

    script:
    """
    awk 'NR % 4 == 2 { n += length(\$0); gc += gsub(/[GC]/, "") }
         END { print gc / n }' reads.fq > \${SAMPLE_ID}.gc.txt
    """
}

process SUMMARIZE {
    input:
    // Use collection types for multiple files
    reports: Bag<Path>

    output:
    file('summary.txt')

    script:
    """
    cat ${reports.join(' ')} > summary.txt
    """
}

////////////////////////////////////////////////////
// Workflows
////////////////////////////////////////////////////

// A named workflow composes processes and operators, and can be called
// like a process. `Channel` and `Value` are *dataflow types*
workflow QC {
    take:
    samples: Channel<Sample>
    min_reads: Integer

    main:
    // Calling a process with a channel runs a task for each value.
    // Processes return channels of their outputs
    counts_ch = COUNT_READS(samples)
    gc_ch = GC_CONTENT(samples)

    // Join the results by sample ID, keeping every field
    qc_ch = samples
        .join(counts_ch, by: 'id')
        .join(gc_ch, by: 'id')

    passed_ch = qc_ch.filter { s -> isPassing(s, min_reads) }
    failed_ch = qc_ch.filter { s -> !isPassing(s, min_reads) }

    emit:
    // Each output has a name and optional type (unless there is only one)
    passed: Channel<QcSample> = passed_ch
    failed: Channel<QcSample> = failed_ch
}

// The entry workflow is the entrypoint of the pipeline. It can access
// pipeline parameters through the built-in `params` variable
workflow {
    main:
    // Load the samplesheet as a channel of records
    samples_ch = channel.of(params.input)
        .flatMap { csv -> csv.splitCsv(header: true) }
        .map { row -> record(id: row.id, fastq: file(row.fastq)) }

    qc = QC(samples_ch, params.min_reads)
    qc.failed.view { s -> "Sample ${s.id} has too few reads" }

    // `collect()` gathers all values into a single dataflow value,
    // so SUMMARIZE runs once with all of the reports
    summary = SUMMARIZE(qc.passed.map { s -> s.gc }.collect())

    publish:
    // Assign channels and values to workflow outputs
    samples = qc.passed
    summary = summary
}

////////////////////////////////////////////////////
// Outputs
////////////////////////////////////////////////////

// The output block declares the pipeline outputs, and how to publish them
// from the work directory to the output directory (`results` by default)
output {
    samples: Channel<QcSample> {
        // Publish files into a custom directory for each value
        path { s -> "gc/${s.id}/" }
        // Save the channel as a samplesheet, including published file paths
        index {
            path 'samples.csv'
            header true
        }
    }

    summary: Path {
        path '.'
        // Publish settings such as `mode` and `enabled` can also be set here
        mode 'copy'
        enabled params.save_summary
    }
}
```

## Configuration

```groovy
// nextflow.config is loaded automatically from the project directory

// Default settings for all processes
process {
    executor = 'local'
    cpus = 2

    // Override settings for specific processes
    withName: SUMMARIZE {
        memory = 4.GB
    }
    // ...or for processes with a given `label` directive
    withLabel: big_mem {
        memory = 64.GB
    }
}

// Settings for publishing workflow outputs
outputDir = 'results'
workflow.output.mode = 'copy'

// Profiles group settings that are selected with `-profile`
profiles {
    docker {
        docker.enabled = true
    }
    slurm {
        process.executor = 'slurm'
        process.queue = 'long'
    }
    test {
        params.input = "${projectDir}/data/samples.csv"
    }
}
```

## Running pipelines

```bash
# Run a local script, overriding a parameter
nextflow run main.nf --input samples.csv --min_reads 10

# Resume a run, reusing cached results for tasks that haven't changed
nextflow run main.nf --input samples.csv -resume

# Select config profiles and set the output directory
nextflow run main.nf -profile docker,test -output-dir my-results

# Load parameters from a JSON or YAML file
nextflow run main.nf -params-file params.yml

# Run a pipeline directly from a Git repository
nextflow run nextflow-io/hello

# Check scripts for errors, and format them
nextflow lint -format main.nf

# Search for and install modules from the Nextflow registry
nextflow module search bwa
nextflow module install nf-core/bwa/mem

# List previous runs
nextflow log
```

## Further Reading

* [Nextflow documentation](https://docs.seqera.io/nextflow/)
* [Nextflow training](https://training.nextflow.io/)
* [Source code on GitHub](https://github.com/nextflow-io/nextflow)
* [nf-core](https://nf-co.re/), a community collection of Nextflow pipelines
  and modules
* [VS Code extension](https://marketplace.visualstudio.com/items?itemName=nextflow.nextflow)
