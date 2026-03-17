---
name: ANSI escape codes
contributors:
    - ["Yuxi Liu", "https://github.com/yuxi-liu-wired"]
filename: LearnANSI.sh
---

ANSI escape codes are special character sequences that control text formatting,
color, cursor position, and other display features in terminal emulators.
They date back to the 1970s ANSI X3.64 standard (now ECMA-48) and are
supported by virtually every modern terminal.

Run this file with `bash LearnANSI.sh` to see every example demonstrated live.

```bash
#!/usr/bin/env bash

###############################################################################
# ANSI ESCAPE CODES
###############################################################################

# The foundation of ANSI escape codes is the ESC character (byte 27 / 0x1b).
# In bash, you can write it three ways — all are equivalent:
#   \e      (bash/zsh shorthand)
#   \033    (octal)
#   \x1b    (hex)

# The most common sequence type is CSI (Control Sequence Introducer): ESC [
# Pattern: \e[ <parameters separated by ;> <letter>
# The letter determines the function; the numbers are its arguments.

# SGR (Select Graphic Rendition) sequences end with 'm' and control
# text appearance (formatting and color).

# We'll use printf throughout — it's more portable than echo -e.
# The %b format interprets backslash escapes like \e.

###############################################################################
# TEXT FORMATTING
###############################################################################

printf "\n=== Text Formatting ===\n\n"

# Each attribute has an "on" code and an "off" code.
# \e[0m resets ALL attributes at once.

printf "  \e[1mBold (1)\e[22m           — turn off with 22\n"
printf "  \e[2mDim (2)\e[22m            — turn off with 22 (same as bold-off)\n"
printf "  \e[3mItalic (3)\e[23m         — turn off with 23\n"
printf "  \e[4mUnderline (4)\e[24m      — turn off with 24\n"
printf "  \e[5mBlink (5)\e[25m          — turn off with 25 (rarely supported)\n"
printf "  \e[7mInverse (7)\e[27m        — turn off with 27 (swaps FG/BG)\n"
printf "  \e[8mHidden (8)\e[28m         — turn off with 28 (text invisible)\n"
printf "  \e[9mStrikethrough (9)\e[29m  — turn off with 29\n"

# You can combine multiple attributes with semicolons:
printf "\n  \e[1;3;4mBold + Italic + Underline\e[0m  — \\\\e[1;3;4m\n"

# Always reset when you're done, or the formatting bleeds into subsequent text.
printf "  \e[0m(reset all)\n"

###############################################################################
# STANDARD COLORS (8 colors)
###############################################################################

printf "\n=== Standard Colors (8 colors) ===\n\n"

# Foreground: 30–37    Background: 40–47    Default: 39 / 49
#
#   30/40  Black       34/44  Blue
#   31/41  Red         35/45  Magenta
#   32/42  Green       36/46  Cyan
#   33/43  Yellow      37/47  White

printf "  Foreground: "
for code in 30 31 32 33 34 35 36 37; do
    printf "\e[${code}m %3d \e[0m" "$code"
done

printf "\n  Background: "
for code in 40 41 42 43 44 45 46 47; do
    printf "\e[${code}m %3d \e[0m" "$code"
done
printf "\n"

# Example: red text on white background
printf "\n  \e[31;47mRed on White\e[0m — \\\\e[31;47m\n"

###############################################################################
# BRIGHT / HIGH-INTENSITY COLORS
###############################################################################

printf "\n=== Bright Colors ===\n\n"

# Bright foreground: 90–97    Bright background: 100–107
# These are non-standard (not in ECMA-48) but universally supported.

printf "  Foreground: "
for code in 90 91 92 93 94 95 96 97; do
    printf "\e[${code}m %3d \e[0m" "$code"
done

printf "\n  Background: "
for code in 100 101 102 103 104 105 106 107; do
    printf "\e[${code}m %3d \e[0m" "$code"
done
printf "\n"

# Note: older scripts use \e[1;31m (bold + color) as a substitute for bright
# red. The 90-97 range is cleaner and doesn't conflate boldness with brightness.

###############################################################################
# 256-COLOR MODE
###############################################################################

printf "\n=== 256-Color Mode ===\n\n"

# Foreground: \e[38;5;<N>m     Background: \e[48;5;<N>m
# N ranges from 0 to 255:
#   0–7:     Standard colors (same as 30–37)
#   8–15:    Bright colors (same as 90–97)
#   16–231:  6×6×6 RGB cube (index = 16 + 36*r + 6*g + b; r,g,b ∈ 0..5)
#   232–255: Grayscale ramp (24 shades, dark to light)

# Print the standard + bright colors (0–15)
printf "  Standard + Bright (0-15):\n  "
for i in $(seq 0 15); do
    printf "\e[48;5;${i}m %3d \e[0m" "$i"
    (( (i + 1) % 8 == 0 )) && printf "\n  "
done

# Print a slice of the 6×6×6 color cube (16–231)
printf "\n  Color cube sample (16-51):\n  "
for i in $(seq 16 51); do
    printf "\e[48;5;${i}m %3d \e[0m" "$i"
    (( (i - 15) % 6 == 0 )) && printf "\n  "
done

# Print the grayscale ramp (232–255)
printf "\n  Grayscale ramp (232-255):\n  "
for i in $(seq 232 255); do
    printf "\e[48;5;${i}m %3d \e[0m" "$i"
done
printf "\n"

###############################################################################
# 24-BIT TRUE COLOR (RGB)
###############################################################################

printf "\n=== 24-Bit True Color ===\n\n"

# Foreground: \e[38;2;<R>;<G>;<B>m
# Background: \e[48;2;<R>;<G>;<B>m
# R, G, B each range from 0 to 255.
# Supported by most modern terminals; NOT by the raw Linux console.

printf "  \e[38;2;255;100;0mOrange text (255;100;0)\e[0m\n"
printf "  \e[38;2;0;200;200mTeal text (0;200;200)\e[0m\n"
printf "  \e[48;2;50;0;100m\e[38;2;255;255;255m White on purple \e[0m\n"

# A smooth gradient to show off true color:
printf "\n  RGB gradient: "
for i in $(seq 0 4 255); do
    printf "\e[48;2;${i};$((255 - i));$(((i + 128) % 256))m \e[0m"
done
printf "\n"

###############################################################################
# CURSOR MOVEMENT
###############################################################################

printf "\n=== Cursor Movement ===\n\n"

# \e[<n>A  — Cursor Up n lines
# \e[<n>B  — Cursor Down n lines
# \e[<n>C  — Cursor Forward (right) n columns
# \e[<n>D  — Cursor Back (left) n columns
# \e[<n>E  — Cursor to beginning of line, n lines down
# \e[<n>F  — Cursor to beginning of line, n lines up
# \e[<n>G  — Cursor to column n (absolute, 1-indexed)
# \e[<n>;<m>H — Cursor to row n, column m (1-indexed)

# Demo: print text, move cursor back, overwrite
printf "  ABCDEF"
printf "\e[3D"   # move back 3 columns
printf "xyz"     # overwrite "DEF" with "xyz"
printf "\n"
printf "  ^ wrote 'ABCDEF', moved back 3, wrote 'xyz' → 'ABCxyz'\n"

# Demo: move to a specific column
printf "  \e[10Gcolumn 10\n"
printf "  \e[20Gcolumn 20\n"
printf "  \e[30Gcolumn 30\n"

###############################################################################
# SCREEN AND LINE CLEARING
###############################################################################

printf "\n=== Screen and Line Clearing ===\n\n"

# Erase in Display: \e[<n>J
#   0 (default): cursor to end of screen
#   1: beginning of screen to cursor
#   2: entire screen
#   3: entire screen + scrollback buffer (xterm extension)
#
# Erase in Line: \e[<n>K
#   0 (default): cursor to end of line
#   1: beginning of line to cursor
#   2: entire line

printf "  \\\\e[0J — clear cursor to end of screen\n"
printf "  \\\\e[1J — clear start of screen to cursor\n"
printf "  \\\\e[2J — clear entire screen\n"
printf "  \\\\e[0K — clear cursor to end of line\n"
printf "  \\\\e[2K — clear entire line\n"

# Live demo: write text then erase part of it
printf "\n  BEFORE_ERASE"
printf "\e[6D"       # move back 6 characters (over "ERASE")
printf "\e[0K"       # erase from cursor to end of line
printf "AFTER\n"
printf "  ^ wrote 'BEFORE_ERASE', moved back, erased to EOL, wrote 'AFTER'\n"

###############################################################################
# USING COLORS IN SCRIPTS — BEST PRACTICES
###############################################################################

printf "\n=== Best Practices for Scripts ===\n\n"

# 1. Always reset after colored output
printf "  1. Always reset: \e[32mgreen text\e[0m then normal text\n"

# 2. Use variables for readability
RED='\e[31m'
GREEN='\e[32m'
BOLD='\e[1m'
RESET='\e[0m'
printf "  2. Use variables: ${BOLD}${RED}bold red${RESET} and ${GREEN}green${RESET}\n"

# 3. Check if stdout is a terminal before using colors
if [ -t 1 ]; then
    printf "  3. stdout is a terminal — colors are safe\n"
else
    printf "  3. stdout is not a terminal — consider disabling colors\n"
fi

# 4. Respect NO_COLOR (https://no-color.org/)
if [ -n "$NO_COLOR" ]; then
    printf "  4. NO_COLOR is set — scripts should suppress color output\n"
else
    printf "  4. NO_COLOR is unset — colors are welcome\n"
fi

# 5. tput is the most portable way to generate escape codes:
#    tput bold       → bold
#    tput setaf 1    → red foreground
#    tput sgr0       → reset
#    It queries the terminfo database so it works even on exotic terminals.

printf "  5. For maximum portability, use tput:\n"
printf "     $(tput bold)bold$(tput sgr0) and $(tput setaf 1)red$(tput sgr0)\n"

printf "\n"
```

## Further Reading

- [ECMA-48 standard (PDF)](https://ecma-international.org/publications-and-standards/standards/ecma-48/) — the actual spec
- [ANSI escape code - Wikipedia](https://en.wikipedia.org/wiki/ANSI_escape_code)
- [XTerm control sequences](https://invisible-island.net/xterm/ctlseqs/ctlseqs.html) — the definitive xterm reference
- [console_codes(4) man page](https://man7.org/linux/man-pages/man4/console_codes.4.html) — Linux console specifics
- [Build your own Command Line with ANSI escape codes](https://www.lihaoyi.com/post/BuildyourownCommandLinewithANSIescapecodes.html)
- [NO_COLOR convention](https://no-color.org/) — standard for disabling color output
