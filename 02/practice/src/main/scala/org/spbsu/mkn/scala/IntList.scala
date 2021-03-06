package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

sealed trait IntList {
  def head: Int
  def tail: IntList
  def drop(n: Int): IntList
  def take(n: Int): IntList
  def map(f: Int => Int): IntList
  def ::(elem: Int): IntList
}

case object IntNil extends IntList {
  override def head: Int = undef
  override def tail: IntList = IntNil
  override def drop(n: Int): IntList = if (n <= 0) IntNil else undef
  override def take(n: Int): IntList = if (n <= 0) IntNil else undef
  override def map(f: Int => Int): IntList = IntNil
  override def ::(elem: Int): IntList = IntCons(elem, IntNil)
}

case class IntCons(head: Int, tail: IntList) extends IntList {
  override def drop(n: Int): IntList = if (n <= 0) head :: tail else tail.drop(n - 1)
  override def take(n: Int): IntList = if (n <= 0) IntNil else head :: tail.take(n - 1)
  override def map(f: Int => Int): IntList = f(head) :: tail.map(f)
  override def ::(elem: Int): IntList = IntCons(elem, head :: tail)
}

object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq(seq: Seq[Int]): IntList = if (seq.isEmpty) IntNil else seq.head :: fromSeq(seq.tail)
  def sum(intList: IntList): Int      = intList match {
                                                case IntNil => undef
                                                case _ => foldLeft(intList, 0)((m, n) => m + n)
                                              }
  def size(intList: IntList): Int     = foldLeft(intList, 0)((m, _) => m + 1)
  // extra task: implement sum using foldLeft
  def foldLeft(intList: IntList, ini: Int)(f: (Int, Int) => Int): Int = intList match {
                                                                                case IntNil => ini
                                                                                case IntCons(x, IntNil) => f(ini, x)
                                                                                case IntCons(x, xs) => foldLeft(xs, f(ini, x))(f)
                                                                              }
}
