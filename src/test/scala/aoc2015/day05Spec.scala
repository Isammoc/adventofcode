package aoc2015
import common.AocSpec

class day05Spec extends AocSpec {

  "day05 2015" can {
    val sample = """""".stripMargin

    "part1" should {
      "example" in {
        day05.isNice1("ugknbfddgicrmopn") shouldBe true
        day05.isNice1("aaa") shouldBe true
        day05.isNice1("jchzalrnumimnmhp") shouldBe false
        day05.isNice1("haegwjzuvuyypxyu") shouldBe false
        day05.isNice1("dvszwmarrgswjxmb") shouldBe false
      }
    }

    "part2" should {
      "example" in {
        day05.isNice2("qjhvhtzxzqqjkmpb") shouldBe true
        day05.isNice2("xxyxx") shouldBe true
        day05.isNice2("uurcxstgmygtbstg") shouldBe false
        day05.isNice2("ieodomkazucvgmuy") shouldBe false
      }
    }
  }
}

