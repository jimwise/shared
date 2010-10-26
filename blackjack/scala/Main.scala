/*
 * Main.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.draga.blackjack

import com.draga.blackjack.Cards._
import com.draga.blackjack.BlackJack._

object Main {
  // XXX make configurable
  val table_min = 5.00
  val table_limit = 1000.00
  var player_purse = 1000.00
  var player_last_bet = table_min

  var player_hand = new Hand(false)
  var dealer_hand = new Hand(true)

  def deal() {
    player_hand.add(Shoe.draw)
    player_hand.add(Shoe.draw)
    dealer_hand.add(Shoe.draw)
    dealer_hand.add(Shoe.draw)
    show_hands(false)
  }

  def show_hands(reveal : Boolean) {
    println("Dealer has:")
    dealer_hand.show(reveal)
    println
    println("Player has:")
    player_hand.show(true)
    printf("Bet: $%.2f\n", player_hand.bet)

  }

  def getchar : Char = {
    var resp = ' '
    var respln = Console.readLine
    if (respln != "")
      resp = respln(0)
    resp.toLower
  }
  def getresp(prompt1 : String, prompt2 : String, allowed : List[Char], 
	      default : Char) : Char = {
    print(prompt1 + " ")
    var resp = getchar

    var done = false
    while (!done) {
      if (resp == ' ') {
	if (default != ' ') {
	  resp = default
	  done = true
	} else {
	  print(prompt2 + " ")
	  resp = getchar
	}
      } else if (allowed.find(resp ==) != None) {
	done = true
      } else {
	print(prompt2 + " ")
	resp = getchar
      }
    }
    resp
  }

  def getbet(min: Double, table_limit: Double) : Double = {
    // XXX check actual min/table_limit rules
    // XXX XXX XXX number formatting
    val limit = math.min(table_limit, player_purse)
    var resp = Console.readLine(
      "Please enter a bet (min = $%.2f, limit = $%.2f) [$%.2f] ",
      min, limit, player_last_bet)
    var bet : Double = 0.0

    var done = false
    while (!done) {
      if (resp == "") {
	bet = player_last_bet
	done = true
      } else {
	if (resp(0) == '$') {
	  resp = resp.substring(1)
	}
	bet = try {
	  resp.toDouble
	} catch {
	  case _ => 0.0
	} 
	if (bet == 0.0) {
          resp = Console.readLine("Bet must be a number of dollars, try again: ")
	} else  if (bet < min) {
          resp = Console.readLine("Bet must be at least $" + min + ", try again: ")
	} else if (bet % min != 0) {
          resp = Console.readLine("Bet must be in an increment of $" + min + ", try again: ")
	} else if (bet > limit) {
	  // XXX might be nice to have different message for over limit vs. over purse
          resp = Console.readLine("Bet must be less than or equal to $" + limit + ", try again:")
	} else {
	  done = true
	}
      }
    }

    player_last_bet = bet
    bet
  }

  def playerplays : Int = {
    var first_draw = true
    var action : Char = ' '
    var done = false
    var surrendered = false
    var v = 0

    // XXX - insurance
    if (player_hand.blackjack) {
      println("[BLACKJACK]")
      21
    } else {
      while (!done) {	// actually, until we bust or stand
        // XXX - split
	// XXX - this is `late surrender', early surrender has to be
	//       handled at insurance time, if it is to be offered
        if (first_draw) {
          action = getresp(
	    "[H]it, [D]ouble down, [S]tand, or S[u]rrender (HDSU)?",
            "Please enter [H], [D], [S], or [U]:",
            List('h', 'd', 's', 'u'), ' ')
	} else {
          action = getresp(
            "[H]it or [S]tand (HS)?",
            "Please enter [H] or [S]:",
            List('h', 's'), ' ')
	}

	action match {
	  case 'h' => {
            print("You draw the ")
            if (player_hand.hit == 0) done = true
            first_draw = false
	  }
	  case 's' => {
            println("You stand")
	    done = true
	  }
	  case 'd' => {
            // XXX some casinos allow DD after split.  some don't (confirm)
	    if (player_purse < player_hand.bet) {
	      println("You cannot afford to double down!")
	    } else {
	      // XXX for some reason python code allowed player to choose new bet
	      player_purse -= player_hand.bet
	      player_hand.bet *= 2
              print("You draw the")
	      player_hand.hit
	      done = true
	    }
	  }
	  case 'u' => {
            println("You surrender.")
	    player_purse += 0.5 * player_hand.bet
	    surrendered = true
	    done = true
	  }
	}
      }
    }
    if (surrendered) 0 else player_hand.value
  }

 def dealerplays : Int = {
   if (dealer_hand.blackjack) {
     println("[BLACKJACK]")
     21
   } else {
     println("The dealer reveals the " + dealer_hand.cards(0))
     dealer_hand.showValue(true)

     var done = false
     while (!done) {
       // XXX XXX should dealer hit a soft 17?  should this be configurable?
       if (dealer_hand.value < 17) {
         print("Dealer draws the ")
         if (dealer_hand.hit == 0) {
	   done = true
	 }
       } else {
	 println("Dealer stands")
	 done = true
       }
     }
   }
   dealer_hand.value
 }

  def play_one_hand {
    player_hand.bet = getbet(table_min, table_limit)
    player_purse -= player_hand.bet

    deal
    val playersbest = playerplays
    if (playersbest == 0) {
      // (busted)
      println("Dealer wins")
    } else if (player_hand.blackjack && !dealer_hand.blackjack) {
	println("Player wins")
        // XXX XXX 3:2 (should be configurable) on blackjack
        player_purse += 2.5 * player_hand.bet
    } else {
      println

      val dealersbest = dealerplays
      if (dealersbest == 0) {
        println("Player wins")
        player_purse += 2 * player_hand.bet
      } else {
	println

	show_hands(true)

	println

	if (dealersbest > playersbest) {
	  println("Dealer wins")
	} else if (playersbest > dealersbest) {
	  println("Player wins")
	  player_purse += 2 * player_hand.bet
	} else {
	  println("Push")
	  player_purse += player_hand.bet
	}
      }
    }
  }

  def main(args: Array[String]) {
    var done = false
    printf("You have: $%.2f\n", player_purse)
    while (!done) {
      player_hand.muck
      dealer_hand.muck

      play_one_hand

      if (player_purse < table_min) {
        println("You're out of money!")
	done = true
      }
      printf("You have: $%.2f\n", player_purse)
      
      if (!done) {
	var cont = getresp(
          "Continue ([Y]es or [N]o) ([Y]N)?",
          "Please anser [Y]es or [N]o (default Y):",
          List('y', 'n'), 'y')
	if (cont == 'n') done = true
      }
    }
  }
}
