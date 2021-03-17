package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._

import scala.math.Ordering
import scala.math.Numeric
import org.spbsu.mkn.scala

sealed trait MyGenericList[+T] {
  def head: T
  def tail: MyGenericList[T]
  def drop(n: Int): MyGenericList[T]
  def take(n: Int): MyGenericList[T]
  def map[N](f: T => N): MyGenericList[N]
  def ::[N >: T](elem: N): MyGenericList[N] = MyCons(elem, this)
}

case object MyNil extends MyGenericList[Nothing] {
  override def head: Nothing = undef
  override def tail: MyGenericList[Nothing] = MyNil
  override def drop(n: Int): MyGenericList[Nothing] = if (n <= 0) MyNil else undef
  override def take(n: Int): MyGenericList[Nothing] = if (n <= 0) MyNil else undef
  override def map[B](f: Nothing => B): MyGenericList[B] = MyNil
}

case class MyCons[T](head: T, tail: MyGenericList[T]) extends MyGenericList[T] {
  override def drop(n: Int): MyGenericList[T] = if (n <= 0) head :: tail else tail.drop(n - 1)
  override def take(n: Int): MyGenericList[T] = if (n <= 0) MyNil else head :: tail.take(n - 1)
  override def map[B](f: T => B): MyGenericList[B] = f(head) :: tail.map(f)
}

object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq[T](seq: Seq[T]): MyGenericList[T] = if (seq.isEmpty) MyNil else seq.head :: fromSeq(seq.tail)

  def size[T](myList: MyGenericList[T]): Int = foldLeft(myList, 0)((m, _) => m + 1)

  def sortInsert[T](elem: T, sortedList: MyGenericList[T])(comparator: Ordering[T]) : MyGenericList[T] = sortedList match {
    case MyNil => MyCons(elem, MyNil)
    case MyCons(x, xs) => if (comparator.gt(elem, x)) MyCons(x, sortInsert(elem, xs)(comparator))
                          else MyCons(elem, sortedList)
  }

  def sort[T](myList: MyGenericList[T])(implicit comparator: Ordering[T]): MyGenericList[T] =
    foldLeft(myList, MyNil: MyGenericList[T])((list, elem) => sortInsert(elem, list)(comparator))

  def sum[T](myList: MyGenericList[T])(implicit summator: Numeric[T]): T =
    foldLeft(myList, summator.fromInt(0))((sum, elem) => summator.plus(sum, elem))

  // extra task: implement sum using foldLeft
  def foldLeft[T, B](myList: MyGenericList[T], ini: B)(f: (B, T) => B): B = myList match {
    case MyNil => ini
    case MyCons(x, MyNil) => f(ini, x)
    case MyCons(x, xs) => foldLeft(xs, f(ini, x))(f)
  }
}