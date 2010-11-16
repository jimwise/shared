#!/opt/csw/bin/perl

use strict;
use warnings;

use feature qw(say switch);

use Cards;
use Blackjack;

my $table_min = 5;
my $table_limit = 1000;

# XXX make configurable
my $player_purse = 1000.00;
my $player_last_bet = $table_min;

my $player_hand = makehand();
my $dealer_hand = makehand();

sub show_hands {
  my ($reveal) = @_;
  say "";
  say "Dealer has:";
  show($dealer_hand, $reveal);
  say "";
  say "Player has:";
  show($player_hand, 1);
  say "";
}

sub deal {
  add($player_hand, draw());
  add($player_hand, draw());
  add($dealer_hand, draw());
  add($dealer_hand, draw());
  showhands();
}

sub playerplays {
  my $first = 1;
  # XXX - insurance
  if (blackjack($player_hand)) {
    say "[BLACKJACK]";
    return 21;
  }

  while(1) {			#  actually, until we bust, surrender, or stand
    # XXX - split
    # XXX - this is `late surrender', early surrender has to be
    # handled at insurance time, if it is to be offered
    my $action;
    if ($first) {
      $action = getresp(
			"[H]it, [D]ouble down, [S]tand, or S[u]rrender (HDSU)? ",
			"Please enter [H], [D], [S], or [U]: ",
			["h", "d", "s", "u"], "" );
    } else {
      $action = getresp(
			"[H]it or [S]tand (HS)? ",
			"Please enter [H] or [S]: ",
			["h", "s"], "");
    }

    given ($action) {
      when ("h") {
		print "You draw the ";
		unless (hit($player_hand)) {
		  return 0
		};
		# XXX some casinos allow DD after split.  some don't (confirm)
		$first = 0;
	       }
      when ("s") {
		say "You stand";
		return handvalue($player_hand);
	       }
      when ("d") {
		if ($player_purse < $table_min) {
		  say "You cannot afford to double down!";
		  next;
		}
		my $newbet = getbet($table_min, $player_hand->{bet});
		$player_hand->{bet} += $newbet;
		$player_purse -= $newbet;
		print "You draw the ";
		return(hit($player_hand));
	       }
      when ("u") {
		say "You surrender";
		$player_purse += 0.5 * $player_hand->{bet};
		return 0;
	       }
    }
  }
}

# def dealerplays():
#     if dealer_hand.blackjack():
#         print("[BLACKJACK]")
#         return 21

#     print("The dealer reveals the", dealer_hand.cards[0].name())
#     dealer_hand.showvalue(reveal=True)

#     while True:
#         # XXX XXX should dealer hit a soft 17?  should this be configurable?
#         if dealer_hand.value() < 17:
#             print("Dealer draws the", end=' ')
#             if dealer_hand.hit() == 0:
#                 return 0
#         else:
#             print("Dealer stands")
#             return dealer_hand.value()

# def play_one_hand():
#     global player_purse
#     player_hand.bet = getbet(table_min, table_limit)
#     player_purse -= player_hand.bet

#     deal()
#     playersbest = playerplays()
#     if playersbest == 0:
#         print("Dealer wins")
#         return
#     if player_hand.blackjack() and not dealer_hand.blackjack():
#         print("Player wins")
#         # XXX XXX 3:2 (configurable) on blackjack
#         player_purse += 2.5 * player_hand.bet
#         return

#     print()

#     dealersbest = dealerplays()
#     if dealersbest == 0:
#         print("Player wins")
#         player_purse += 2 * player_hand.bet
#         return
    
#     print()
#     print("Dealer has:")
#     dealer_hand.show(reveal)
#     print()
#     print("Player has:")
#     player_hand.show()
#     print()

#     if dealersbest > playersbest:
#         print("Dealer wins")
#         return
#     elif playersbest > dealersbest:
#         print("Player wins")
#         player_purse += 2 * player_hand.bet
#         return
#     else:
#         print("Push")
#         player_purse += player_hand.bet
#         return


# def getbet(table_min, table_limit):
#     global player_last_bet, player_purse
#     # XXX check actual min/table_limit rules
#     print(("Please enter a bet (min = $%.2f, limit = $%.2f) [%.2f]: " % (table_min, min(player_purse, table_limit), player_last_bet)), end=' ')
#     while True:
#         resp = sys.stdin.readline()[:-1]
#         if resp == '':
#             bet = player_last_bet
#         else:
#             if resp[0] == '$':
#                 resp = resp[1:]
#             try:
#                 bet = float(resp)
#             except ValueError:
#                 print("Bet must be a number of dollars, try again: ", end=' ')
#                 continue

#         if bet < table_min:
#             print("Bet must be at least $%.2f, try again: ", end=' ')
#             continue
#         if bet % table_min != 0:
#             print("Bet must be in an increment of $%d.00, try again: " % min, end=' ')
#             continue
#         if bet > table_limit:
#             print("Bet must be less than or equal to $%d.00, try again: " % table_limit, end=' ')
#             continue

#         # if we get here, bet is good
#         player_last_bet = bet
#         return bet

# def getresp(prompt1, prompt2, allowed, default):
#     print(prompt1, end=' ')
#     while True:
#         resp = sys.stdin.readline()
#         if resp[-1] == "\n":
#             resp = resp[:-1]
#         resp = resp.lower()
#         if resp in allowed:
#             return resp
#         if resp == '' and default != '':
#             return default
#         print(prompt2, end=' ')

# if __name__ == "__main__":
#     print("You have: $%.2f" % player_purse)
#     while True:
#         player_hand.muck()
#         dealer_hand.muck()

#         play_one_hand()

#         if player_purse < table_min:
#             print("You're out of money!")
#             sys.exit(0)

#         print("You have: $%.2f" % player_purse)

#         cont = getresp(
#             "Continue ([Y]es or [N]o) ([Y]N)? ",
#             "Please anser [Y]es or [N]o (default Y): ",
#             ["y", "n"], "y")
#         if cont == "n":
#             sys.exit(0)
