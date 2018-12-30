# Advent of Code :evergreen_tree:

Here my answers to the [Advent of Code](https://adventofcode.com) challenge.

## Progress

```
              1111111111222222
     1234567890123456789012345
2015 XXXXXX···················
2016 ·························
2017 XXXXXXXXXXXXXXXXXXXXXX···
2018 XXXXXXXXXXXXXXXXXXXXXXXXX
```

`·`: todo, `/`: 1 star, `X`: 2 stars

## Launch

With your input in a file `inputXX.txt` (where `XX` is the #day).

```bash
./sbt "run aocYYYY.dayXX" < inputXX.txt
```

Where:
 * `YYYY` is the year
 * `XX` is the day

**Sample:**

For the first day of december 2018:

```bash
./sbt "run aoc2018.day01" < input01.txt
```

## Utility script

To create a new day:

```bash
./newday.sh [<year>] <day>
```

where:
 * _year_ is the year with 4 digits (2018)
 * _day_ is a two digits number (02)

**Sample:**

```bash
./newday.sh 12
./newday.sh 2017 02
```

Enjoy! :evergreen_tree:
