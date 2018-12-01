#!/bin/bash

function help {
  echo "use with as:"
  echo "  $0 [<year>] <day>"
  echo "sample:"
  echo "  $0 02"
  echo "  $0 2017 24"
}


function write {
  if [ ! -d src/main/scala/aoc$YEAR ]; then
    mkdir -p src/main/scala/aoc$YEAR
  fi
  cat > src/main/scala/aoc$YEAR/day$DAY.scala << EOF
package aoc${YEAR}

object day${DAY} extends App {

  def part1(input: String) = ???

  // def part2(input: String) = ???

  val input = io.Source.stdin.getLines.mkString
  println("part1 = " + part1(input))
  // println("part2 = " + part2(input))
}
EOF

  if [ ! -d src/test/scala/aoc$YEAR ]; then
    mkdir -p src/test/scala/aoc$YEAR
  fi
  cat > src/test/scala/aoc$YEAR/day${DAY}Spec.scala << EOF
package aoc${YEAR}
import common.AocSpec

class day${DAY}Spec extends AocSpec {

  "day${DAY} ${YEAR}" can {

    "part1" should {
      "example" in {
        //day${DAY}.part1("") shouldEqual 0
        pending
      }
    }

    "part2" should {
      "example" in {
        //day${DAY}.part2("") shouldEqual 0
        pending
      }
    }
  }
}

EOF
}

if [ $# -eq 1 ]; then
  YEAR=2018
  DAY=$1
elif [ $# -eq 2 ]; then
  YEAR=$1
  DAY=$2
else
  help
fi
write
