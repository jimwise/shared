#!/opt/csw/bin/perl

use strict;
use warnings;

use feature qw(say switch);
use List::Util qw(min);

use Cards;
use Blackjack;

my $table_min = 5;
my $table_limit = 1000;

# XXX make configurable
my $player_purse = 1000.00;
my $player_last_bet = $table_min;

my $player_hand = makehand();
my $dealer_hand = makehand();

sub showhands {
  my ($reveal) = @_;
  say "";
  say "Dealer has:";
  show($dealer_hand, $reveal);
  say "";
  say "Player has:";
  show($player_hand, 1);
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
		my $newbet = getbet();
		$player_hand->{bet} += $newbet;
		$player_purse -= $newbet;
		print "You draw the ";
		return hit($player_hand);
	       }
      when ("u") {
		say "You surrender";
		$player_purse += 0.5 * $player_hand->{bet};
		return 0;
	       }
    }
  }
}

sub dealerplays {
  if (blackjack($dealer_hand)) {
    say "[BLACKJACK]";
    return 21;
  }

  say "The dealer reveals the ", name($dealer_hand->{cards}->[0]);
  showvalue($dealer_hand, 1);

  while (handvalue($dealer_hand) < 17) {
    # XXX XXX should dealer hit a soft 17?  should this be configurable?
    print "Dealer draws the ";
    if (hit($dealer_hand) == 0) {
      return 0;
    }
  }
  say "Dealer stands";
  return handvalue($dealer_hand);
}

sub play_one_hand {
  $player_hand->{bet} = getbet($table_min, $table_limit);
  $player_purse -= $player_hand->{bet};

  deal();
  my $playersbest = playerplays();

  unless ($playersbest) {
    say "Dealer wins";
    return;
  }

  if (blackjack($player_hand) and not blackjack($dealer_hand)) {
    say "Player wins";
    # XXX XXX 3:2 (should this be configurable?) on blackjack
    $player_purse += 2.5 * $player_hand->{bet};
    return;
  }

  say "";

  my $dealersbest = dealerplays();
  unless ($dealersbest) {
    say "Player wins";
    $player_purse += 2 * $player_hand->{bet};
    return;
  }

  say "";
  showhands(1);

  if ($dealersbest > $playersbest) {
    say "Dealer wins";
    return;
  } elsif ($playersbest > $dealersbest) {
    say "Player wins";
    $player_purse += 2 * $player_hand->{bet};
    return;
  } else {
    say "Push";
    $player_purse += $player_hand->{bet};
    return;
  }
}

sub getbet {
  # XXX check actual min/table_limit rules
  my $maxbet = min ($player_purse, $table_limit);
  my $bet = 0;

  printf "Please enter a bet (min = \$%0.2f, limit = \$%0.2f) [\$%0.2f]: ", $table_min, $maxbet, $player_last_bet;

  while (<STDIN>) {
    chomp;
    if (/^$/) {
      return $player_last_bet;
    } else {
      s/^\$//;
      $bet = eval {return $_ + 0};
      if ($bet == 0) {
	print "Bet must be a number of dollars, try again: ";
	next;
      }

      if ($bet < $table_min) {
	print "Bet must be at least $table_min, try again: ";
	next;
      }
      if ($bet % $table_min != 0) {
	print "Bet must be in an increment of $table_min, try again: ";
	next;
      }
      if ($bet > $table_limit) {
	print "Bet must be less than or equal to $table_limit, try again: ";
	next;
      }

      # if we get here, bet is good
      $player_last_bet = $bet;
      return $bet;
    }
  }

}

sub getresp {
  my ($prompt1, $prompt2, $allowed, $default) = @_;
  print $prompt1;
  while (<STDIN>) {
    chomp;
    my $resp = lc;
    if ($resp eq "" and $default ne "") {
      return $default;
    }
    if (grep {/^$resp$/} @{$allowed}) {
      return $resp;
    }
    print $prompt2;
  }
}

printf "You have: \$%0.2f\n", $player_purse;
while (1) {
  muck($player_hand);
  muck($dealer_hand);

  play_one_hand();

  if ($player_purse < $table_min) {
    say "You're out of money!";
    exit 0;
  }

  printf "You have: \$%0.2f\n", $player_purse;

  exit 0 if ("n" eq getresp(
	      "Continue ([Y]es or [N]o) ([Y]N)? ",
	      "Please anser [Y]es or [N]o (default Y): ",
	      ["y", "n"], "y"));
}
