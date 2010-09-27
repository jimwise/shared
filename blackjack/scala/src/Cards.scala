/*
 * Main.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.draga.blackjack.Cards

sealed trait suit
final case object Hearts extends suit
final case object Diamonds extends suit
final case object Clubs extends suit
final case object Spades extends suit

sealed trait value
final case object Ace extends value
final case object Two extends value
final case object Three extends value
final case object Four extends value
final case object Five extends value
final case object Six extends value
final case object Seven extends value
final case object Eight extends value
final case object Nine extends value
final case object Ten extends value
final case object Jack extends value
final case object Queen extends value
final case object King extends value

abstract class AbstractCard (s: suit, v: value) {
  override def toString = v + " of " + s
  def value : Int
}

abstract class AbstractShoe[A <: AbstractCard] {
  val decksInShoe : Int
  // XXX should use List (see draw!), but need to rewrite shuffle
  private var cards : Array[A] = Array()
  private val rand = new scala.util.Random();

  val suits = Array(Hearts, Diamonds, Clubs, Spades)
  val values = Array(Ace, Two, Three, Four, Five, Six, Seven,
                             Eight, Nine, Ten, Jack, Queen, King)

  val onedeck : Array[A]  
  // XXX XXX XXX originally, this was a for-comprehension:
  //    for (s <- suits; v <- values) yield new A(s,v)
  // but scala doesn't seem to allow this to be parametrized
  // so child classes must override

  //    Shuffle due to John Lees-Miller at
  //      http://jdleesmiller.blogspot.com/2008/12/shuffles-surprises-and-scala.html

  private def swap[T](xs: Array[T], i: Int, j: Int) = {
    val t = xs(i)
    xs(i) = xs(j)
    xs(j) = t
  }

  private def fisherYatesShuffle[T](xs: Array[T]) = {
    for (i <- xs.indices.reverse) swap(xs, i, rand.nextInt(i + 1))
  }

  def shuffle : Unit = {
    var i : Int = 0
    cards = Array()
    while (i < decksInShoe) {
      cards = cards ++ onedeck
      i+=1
    }
    fisherYatesShuffle(cards)
  }

  def draw : A = {
    if (cards.isEmpty) shuffle

    val c = cards.first
    cards = cards.drop(1)
    c
  }
}
