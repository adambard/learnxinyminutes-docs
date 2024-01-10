---
language: Sieve
contributors:
    - ["thiagola92", "http://github.com/thiagola92"]
filename: learnsieve.sieve
---

Sieve was created in late 90s to help email filtering.

```sieve

# Single line comments start with a number symbol.

/*
Multi-line comments look like this.
Starts with forward slash and asterisk.
Ends with asterisk and forward slash.
*/

# Declare one extension.
require "fileinto";
# Must be at the top of script.

# Declare multiple extensions.
require ["fileinto", "reject"];
# A single string is accepted in any case where string-list is accepted.

####################################################
## Action commands
####################################################

# Send the message to the main mailbox.
keep;
# Is implicit called unless other action cancel it.

# Silently throw away the message.
discard;
# Will cancel the implicit keep.

# Redirect the message to other address.
redirect "bart@example.com";
# Will cancel the implicit keep.

# Send the message to an specific mailbox.
fileinto "job";
# Will cancel the implicit keep.
# Note: The email provider decide the action in the mailbox doesn't exist.

# The command fileinto must be extended.
require "fileinto";

fileinto "job";

####################################################
## Control commands
####################################################

# Control flow
if true {
    keep;
} elsif true {
    keep;
} else {
    keep;
}

# Ends all processing.
stop;
# Will call the implicit keep unless other action have cancelled it.

####################################################
## Test commands
####################################################

# "true" is a command that evaluates to true.
# "false" is a command that evaluates to false.
if true {
    keep;
} elsif false {
    keep;
}

# "not" is a command that invert the result of a test.
if not false {
    keep;
}

# Test message size.
if size :over 100 {
    keep;
} elsif size :under 100 {
    discard;
}
# In this case, exactly 100 wouldn't match any of the tests.

# Numbers can use suffix to represent large values.
if size :over 500K {
    discard;
}
# K for KB, M for MB, G for GB

# Test if all the following tests evalutes to true.
if allof (true, true, false) {
    keep;
}

# Test if any following test evalutes to true.
if anyof (false, false, true) {
    keep;
}

# Test if the header exists.
if exists "From" {
    keep;
}

# All headers in the string-list must exists to evalute to true.
if exists ["From", "Date", "Cc"] {
    keep;
}

# Test if message came from bob@example.org.
if header :is "From" "bob@example.org" {
    keep;
}

# Comparasion between string-list will attempt to match every combination.
if header :is ["From", "To"] ["bob@example.org", "alice@example.com"] {
    keep;
}
# If any combination is true, the test will evaluate to true.

# Test if message came from domain "example.org".
if address :domain :is "From" "example.org" {
    keep;
}

####################################################
## Match
####################################################

# When comparing size, 2 tagged arguments are accepeted.
#   :over
#   :under

# Check if size is more than 100 bytes.
if size :over 100 {
    keep;
}

# Check if size is less than 100 bytes.
if size :under 100 {
    keep;
}

# When comparing strings, 3 tagged arguments are accepeted.
#   :is
#   :contains
#   :matches

# Check if string is exactly "bob@example.org".
if header :is "From" "bob@example.org" {
    keep;
}

# Check if string contains "bob".
if header :contains "From" "bob" {
    keep;
}
# Will match "bob@example.org", "bob@example.com", "hibob@example.org".

# Check if string ends with ".org".
if header :contains "From" "?*.org" {
    keep;
}
# Will match "bob@example.org", "alice@example.org", "notbob@email.org".
# Can be escaped using two backslash ("\\?", "\\*").

# When comparing address, 3 tagged arguments are accepeted.
#   :localpart
#   :domain
#   :all
# Address syntax: local-part@domain

# Check if the local-part of address is "bob". 
if address :localpart :is "From" "bob" {
    keep;
}

# Check if the local-part of address is "bob". 
if address :domain :is "From" "example.org" {
    keep;
}

# Check if address is exactly "bob@example.org". 
if address :all :is "From" "bob@example.org" {
    keep;
}
```
