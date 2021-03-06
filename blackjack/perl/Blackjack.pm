#!/opt/csw/bin/perl
package Blackjack;

use strict;
use warnings;
use 5.010;
use feature qw(say);
use List::Util qw(max);

use Cards;

use Exporter 'import';
use base 'Exporter';
our  @EXPORT = qw(
		add
		blackjack
		busted
		handvalue
		hit
		makehand
		muck
		show
		showvalue
	     );
our @EXPORT_OK = qw();

# yes, ace needs special treatment
my @vals = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10);
my %cardvals;

foreach my $i (0 .. 12) { $cardvals{$Cards::cards[$i]} = $vals[$i];}

sub cardval {
  my ($card) = @_;
  return $cardvals{$card->{card}};
}

sub makehand {
  return {'bet' => 0, cards => []};
}

sub add {
  my ($hand, $card) = @_;
  push @{$hand->{cards}}, $card;
}

sub muck {
  my ($hand) = @_;
  $hand->{cards} = [];
}

sub hit {
  my ($hand) = @_;
  my $card = draw();
  say name($card);
  add($hand, $card);
  showvalue($hand, 1);
  if (busted($hand)) {
    say "[BUST]";
    return 0;
  } else {
    return handvalue($hand);
  }
}

sub show {
  my ($hand, $reveal) = @_;
  # XXX copies, but this is cheap
  my @cards = @{$hand->{cards}};

  if (scalar(@cards) == 0) {
    say "[no cards]";
    return;
  } else {
    if ($reveal) {
      say " ", name($cards[0]);
    } else {
      say " one face down card"
    }
  }
  shift @cards;
  foreach my $card (@cards) {
    say " ", name($card);
  }
  showvalue($hand, $reveal);
  if ($hand->{bet}) {
    printf "Bet: %0.2f\n", $hand->{bet};
  }
}

sub showvalue {
  my ($hand, $reveal) = @_;
  if ($reveal) {
    say "Total value: ", join("/", handvalues($hand));
  } else {
    say "Total value: ???";
  }
}

sub busted {return handvalue(shift) == 0}

sub blackjack {
  my ($hand) = @_;
  return (handvalue($hand) == 21 and scalar(@{$hand->{cards}} == 2));
}

sub handvalues {
  my ($hand) = @_;
  my ($val, $aces) = (0, 0);

  foreach my $card (@{$hand->{cards}}) {
    if (cardval($card) == 1) {
      $aces++
    } else {
      $val += cardval($card);
    }
  }

  my @handval = ($val);

  for (1..$aces) {
    my @newhandval = ();
    grep {return ($_+1, $_+11)} @handval;
    @handval = @newhandval;
  }
  return @handval;
}

sub handvalue {
  my ($hand) = @_;
  my @good = (0, grep {$_ <= 21} handvalues($hand));
  return max @good;
}

1;
