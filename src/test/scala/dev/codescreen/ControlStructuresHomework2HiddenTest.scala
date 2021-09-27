package dev.codescreen

import ControlStructuresHomework2._
import ControlStructuresHomework2.Command._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.EitherValues._

class ControlStructuresHomework2HiddenTest extends AnyFreeSpec {
  "parseCommand" - {
    "should return error for invalid amount of numbers in divide" - {
      "for input 'divide 4 5 3'" in {
        parseCommand("divide 4 5 3").isLeft shouldBe true
      }

      "for input 'divide 4'" in {
        parseCommand("divide 4").isLeft shouldBe true
      }
    }

    "should return error for words in input" - {
      "for input 'divide 1 two'" in {
        parseCommand("divide 1 two").isLeft shouldBe true
      }

      "for input 'sum 1 2 3 four 5'" in {
        parseCommand("sum 1 2 3 four 5").isLeft shouldBe true
      }

      "for input 'average 1 2 3 four 5'" in {
        parseCommand("average 1 2 3 four 5").isLeft shouldBe true
      }

      "for input 'min 1 2 3 four 5'" in {
        parseCommand("min 1 2 3 four 5").isLeft shouldBe true
      }

      "for input 'max 1 2 3 four 5'" in {
        parseCommand("max 1 2 3 four 5").isLeft shouldBe true
      }
    }

    "should return error for command without numbers list" - {
      "for input 'sum'" in {
        parseCommand("sum").isLeft shouldBe true
      }

      "for input 'divide'" in {
        parseCommand("divide").isLeft shouldBe true
      }
    }

    "should return error for unknown command" - {
      "for input 'unknown'" in {
        parseCommand("unknown").isLeft shouldBe true
      }

      "for input 'unknown 1 2 3'" in {
        parseCommand("unknown 1 2 3").isLeft shouldBe true
      }
    }
  }

  "process" - {
    "return string starting with 'Error:'" - {
      "for input 'divide 4'" in {
        process("divide 4") should startWith("Error: ")
      }

      "for input 'divide 4 5 6'" in {
        process("divide 4 5 6") should startWith("Error: ")
      }

      "for input 'divide 4 0'" in {
        process("divide 4 0") should startWith("Error: ")
      }

      "for input 'unknown'" in {
        process("unknown") should startWith("Error: ")
      }

      "for input 'sum'" in {
        process("sum") should startWith("Error: ")
      }
    }
  }
}
