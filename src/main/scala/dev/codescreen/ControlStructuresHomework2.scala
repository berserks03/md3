package dev.codescreen;

import dev.codescreen.ControlStructuresHomework2.Command.{Average, Divide, Max, Min, Sum}

import scala.io.Source

object ControlStructuresHomework2 {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  // Adjust `Result` and `ChangeMe` as you wish - you can turn Result into a `case class` and remove the `ChangeMe` if
  // you think it is the best model for your solution, or just have other `case class`-es implement `Result`
  final case class Result(value: String)

  def makeString(list: List[Double]): String = {
    list.foldLeft("")((output, num) => output + num + " ")
  }

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    // implement this method
    // Implementation hints:
    // You can use String#split, convert to List using .toList, then pattern match on:
    //   case x :: xs => ???

    // Consider how to handle extra whitespace gracefully (without errors).
    val rawCommand = x.split("\\s+").toList
    val command: Either[ErrorMessage, Command] = rawCommand match {
      case x :: xs if(x == "divide") => Right(Divide(xs.head.toDouble, xs.tail.head.toDouble))
      case x :: xs if(x == "sum") => Right(Sum(xs.map(_.toDouble)))
      case x :: xs if(x == "average") => Right(Average(xs.map(_.toDouble)))
      case x :: xs if(x == "min") => Right(Min(xs.map(_.toDouble)))
      case x :: xs if(x == "max") => Right(Max(xs.map(_.toDouble)))
      case _ => Left(ErrorMessage("Error: Not Valid Input"))
    }
    command
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    // implement this method
    val result: Either[ErrorMessage, Result] = x match {
      case x: Divide => {
       val a = x.dividend
       val b = x.divisor
        if(b == 0) Left(ErrorMessage("Error: Division by zero"))
        else Right(Result(s"${a} divided by ${b} is ${a/b}"))
      }
      case x: Sum => Right(Result(s"the sum of ${makeString(x.numbers)}is ${x.numbers.sum}"))
      case x: Average => Right(Result(s"the average of ${makeString(x.numbers)}is ${x.numbers.sum/x.numbers.length}"))
      case x: Min => Right(Result(s"the minimum of ${makeString(x.numbers)}is ${x.numbers.min}"))
      case x: Max => Right(Result(s"the maximum of ${makeString(x.numbers)}is ${x.numbers.max}"))
    }
    result
  }

  def renderResult(x: Result): String = x.value

  def process(x: String): String = {
    import cats.implicits._
    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.

    val command = parseCommand(x) // implement using a for-comprehension

    val result: String = command match {
      case Right(x) => {
        val calculation = calculate(x)
        calculation match {
          case Right(res) => renderResult(res)
          case Left(errMsg) => errMsg.value
        }
      }
      case Left(x) => x.value
    }
    println(result )
    result
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
