package com.draga.blackjack

object IO {
  var last_bet = 5.00

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

  def getbet(min: Double, limit: Double) : Double = {
    // XXX check actual min/table_limit rules
    // XXX XXX XXX number formatting
    var resp = Console.readLine(
      "Please enter a bet (min = $%.2f, limit = $%.2f) [$%.2f] ",
      min, limit, last_bet)
    var bet : Double = 0.0

    var done = false
    while (!done) {
      if (resp == "") {
	bet = last_bet
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

    last_bet = bet
    bet
  }
}
