---
category: tool
name: Rink
contributors:
  - ["Evan Hahn", "https://evanhahn.com/"]
filename: LearnRink.txt
---

[Rink](https://rinkcalc.app/about) is a unit-aware calculator for the command
line and [the browser](https://rinkcalc.app/).

Start the Rink REPL with `rink`, or run a calculation with
`rink '10 pounds to kilograms'`.

Rink values are numbers with units:

```
> 123 kilometers
123 kilometer (length)

> 1.5 radians
1.5 radian (angle)
```

You don't _need_ units, though. Values can be "dimensionless":

```
> 123
123  (dimensionless)

> -456.78
-456.78  (dimensionless)
```

Rink supports arbitrary precision:

```
> 2/3 * 1/6
0.[1]...  (dimensionless)

> 1e20000 / 2e20001
0.05  (dimensionless)
```

Convert units, including currencies:

```
> 10 pounds to kilograms
approx. 4.535923 kilogram (mass)

> 32 degF to degC
0 °C (temperature)

> 1 KiB to B
1024 byte (information)

> 2 halfnotes to wholenote
1 wholenote (musical_note_length)

> 1 EUR to INR
97.4838 INR (money)

> 52 weeks to seconds
31449600 second (time)
```

You can use `->` instead of `to`, which can be easier to read:

```
> 52 weeks -> seconds
31449600 second (time)
```

Do math with units:

```
> 1.5 hours + 30 minutes
2 hour, 0 second (time)

> 100 GB / 25 mbps
8 hour, 53 minute, 20 second (time)
```

Combine math with unit conversion:

```
> 12 megabytes + 34 kilobytes + 56 bytes -> bytes
12034056 byte (information)
```

Convert a time to another time zone:

```
> #2025-04-20T18:09 America/Chicago# -> "Africa/Lagos"
2025-04-21 00:09:00 WAT

> #01:23# -> "America/Sao_Paulo"
2019-01-23 05:23:00 -02

> now -> "Asia/Yangon"
2019-01-23 17:26:00 +0630
```

Compute time deltas:

```
> #2025-01-23# - #2024-02-10#
49 week, 5 day, 0 second (time)

> #2025-01-23# - #2024-02-10# -> day
348 day (time)

> #2025-01-23# - #2024-02-10# -> hour
8352 hour (time)

> now - #2025-01-01# -> day
109 day (time)
```

Math with time deltas:

```
> now + 3 days
2020-01-26 01:23:00 +00:00

> now - (four score years + seven years)
1933-01-22 23:40:20.182200 +00:00 (92 years ago)
```

Remember that _months and years are weird_ (at no fault of Rink).
Compare Rink's default `year` and `month` units to alternate ones:

```
> 1 year -> days
approx. 365.2421 day (time)

> 1 calendaryear -> days
365 day (time)

> 1 month -> days
approx. 30.43684 day (time)

> 1 lunarmonth -> days
approx. 29.53058 day (time)
```

Work with percentages:

```
> 0.25 -> percent
25 percent (dimensionless)

> 25 percent
0.25  (dimensionless)

> 100 meters * 25 percent
25 meter (length)
```

Use helpers like "billion" and "thousand":

```
> 1 billion / 1 million
1000  (dimensionless)

> 1 billion kilograms / 1 million pounds
approx. 2204.622  (dimensionless)
```

Convert numbers between bases, such as hexadecimal and binary:

```
> 0x45
69  (dimensionless)

> 0b1010011010
666  (dimensionless)

> 420 -> base 16
1a4  (dimensionless)

> 420 -> hex
1a4  (dimensionless)

> 420 -> base 2
110100100  (dimensionless)

> 420 -> bin
110100100  (dimensionless)
```

Use simple math functions, like square root.
_Note that these do not use arbitrary precision._

```
> sqrt(4)
approx. 2  (dimensionless)

> sqrt(4) meters
approx. 2 meter (length)
```

Rink supports a number of constants, such as the volume of celestial bodies:

```
> volume of uranus / volume of moon
approx. 3111.849  (dimensionless)
```

## Further Information

- [Rink homepage](https://rinkcalc.app/about)

- [`rink` manpage](https://rinkcalc.app/manual)
