---
language: Hjson
filename: learnhjson.hjson
contributors:
  - ["MrTeferi", "https://github.com/MrTeferi"]
lang: en-us
---

Hjson is an attempt to make [JSON](https://learnxinyminutes.com/docs/json/) more human readable.

Hjson is a syntax extension to JSON. 
It's NOT a proposal to replace JSON or to incorporate it into the JSON spec itself. 
It's intended to be used like a user interface for humans, 
to read and edit before passing the JSON data to the machine.

Let's take a look at examples to see the key syntax differences!

```
{
    # Comments are totally supported!
    
    // With forward slashes too!
    
    /*
        Even block style comments, neat!
    /*
    
    # Strings do not require quotes!
    # Just keep it to a single line
    human: readable
    quotes: "are fine too"
    
    # Notice that commas are also not required!
    # If using commas, strings DO require quotes!
    object: {
        name: Hjson
        properties: [
            readable
            exciting
            fun
        ]
        with_commas: [
            "quoted",
            "quoty",
            "quote"
        ]
        details: ["this", "is", "fine", "too"]
    }
    
    # Multiline quotes with proper whitespace handling are supported!
    diary:
        '''
        I wish JSON was more human readable.
        If only there was a JSON for my needs!
        Oh wait.. there is! It's called Hjson.
        '''
    
    # Backslashes are interpreted as an escape character ONLY in quoted strings
    slash: This will not have a new line\n
    slash-quoted: "This will definitely have a new line\n"

    # Make sure to use quotes when mixing whitespace with important punctuation
    example1: "If, you're, going, to, comma in a string, use, quotes!"
    example2: "Also if you want to use {} or [] or any JSON relevant punctuation!"
    example3: [because, this, is, totally, BROKEN!]
    example4: this is technically OK though: {}[],:
    
    # Enjoy working with Hjson!
    party-time: {
        Hjson-lovers: [
            me
            my mom
            "my dad"
        ]
        Hjson-power-level: 9000
        supported: {
            python: yes
            java: yes
            javascript: yes
            c++: yes
            Go: yes
            C#: yes
            Rust: yes
        }
        partial-support: ["C", "Kotlin", "Ruby", "Rust"]
    }
    
}
```

## Further Reading

* [Hjson.github.io](https://hjson.github.io/) Main Hjson site including editor support, how-to, etc.
* [Hjson Packages](https://github.com/hjson/) Various Hjson packages for different applications.
