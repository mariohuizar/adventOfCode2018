import org.scalatest.{ Matchers, WordSpec }

class DayOneSpec extends WordSpec with Matchers {
  "DayOne" can {
    "part one" should {
      "sum all inputs from raw string" in {
        val rawInput = """|-2
                                |-6
                                |+2
                                |-10"""

        DayOne.sumAllInputs(rawInput) === -16
      }
    }

    "part two" should {
      "find the first repeated frequency 0" in {
        val rawInput =
          """|+1
             |-1"""

        DayOne.findFirstRepeatedFrequency(rawInput) shouldBe 1
      }

      "find the first repeated frequency 2" in {
        val rawInput =
          """|+1
             |-2
             |+3
             |+1"""

        DayOne.findFirstRepeatedFrequency(rawInput) shouldBe 2
      }

      "find the first repeated frequency 3" in {
        val rawInput =
          """|+3
             |+3
             |+4
             |-2
             |-4""".stripMargin

        DayOne.findFirstRepeatedFrequency(rawInput) shouldBe 10
      }

      "find the first repeated frequency 4" in {
        val rawInput =
          """|-6
             |+3
             |+8
             |+5
             |-6""".stripMargin

        DayOne.findFirstRepeatedFrequency(rawInput) shouldBe 5
      }

      "find the first repeated frequency 5" in {
        val rawInput =
          """+7
            |+7
            |-2
            |-7
            |-4""".stripMargin

        DayOne.findFirstRepeatedFrequency(rawInput) shouldBe 14
      }
    }
  }
}
