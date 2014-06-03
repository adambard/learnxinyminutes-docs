---
language: swift
contributors:
    - ["Max Yankov", "https://github.com/golergka/"]
---

Swift is an OOP language that was created by Apple for iOS and OS X development and
released to the word at 2014 WWDC. Like Objective-C, it uses ARC, but also brings a
lot more of modern language features to the table.


```swift

// Comments look just like in C

/*
    Multiline comments too,
    /* but unlike C, they should be closed recursively */
*/

// You can declare variables:

var canChange = "Hello world!"

// And constants:

let cannotChange = "Hi, mom!"

// You can edit variables:

canChange += " Good to see you."

// But constants can't be changed after assignment:

cannotChange += " Come visit!" // Compile-time error

// Types are inferred automatically:

let apples = 5 // Int
let length = 5.4 // Double

// But never converted automatically:

let appleCertificate = "This person has " + apples + " apples"                  // Compile-time error!
let lengthMeasurement = "This apple tree is " + String(length) + " meters high" // This works

// Although in case of strings, there exists more convenient syntax:

let appleTreeSurvey = "This apple tree has \(apples) apples and is \(length) meters high"

// There are built-in arrays and dictionaries:

var fishes = ["starfish", "swordfish", "shark"]
fishes[3] = "whale"

let captains = [
    "Kirk": "Enterprise",
    "Picard": "Enterprise",
    "Solo" : "Falcon",
]
captains["Morgan"] = "rum bottle"
```
