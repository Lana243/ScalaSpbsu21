package org.spbsu.mkn.scala

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.Random

object TheGame {

  sealed trait GuessResult
  case class Correct(numTries: Int) extends GuessResult
  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException
  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException


  def generateNumberString(length: Int): String = {
    val symbols = List(0,1,2,3,4,5,6,7,8,9)
    if (symbols.size < length)
      throw new RuntimeException("Too long number")
    val l = Random.shuffle(symbols)
    l.take(length).foldRight("")((c,str) => c + str)
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    if (secret.length != userInput.length)
      throw new WrongNumberLengthException(secret.length, userInput.length)
    if (secret.toSet.size != secret.length)
      throw new RepeatingDigitsException
    if (secret == userInput)
      return Correct(numTries)
    val bulls = secret.zip(userInput).count({ case (x, y) => x == y
                                              case _ => false })
    val cows = userInput.toSet.count(x => secret.contains(x)) - bulls
    Incorrect(bulls, cows)
  }

  def main(args: Array[String]): Unit = {
    print("Enter the number of digits: ")
    val n = readLine().toInt
    val secret = generateNumberString(n)
    var numTries = 0
    var res: GuessResult = null
    while (!res.isInstanceOf[TheGame.Correct]) {
      print("Enter your guess: ")
      val userInput = readLine()
      numTries += 1
      try {
        res = validate(secret,userInput, numTries)
        res match {
          case Incorrect(bulls, cows) => println(s"Incorrect. Bulls: $bulls, cows: $cows")
          case _ => println(s"You won using $numTries tries!")
        }
      } catch {
        case e: WrongNumberLengthException => println(s"Wrong number of digits")
      }
    }
  }
}
