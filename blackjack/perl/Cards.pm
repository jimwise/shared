#!/opt/csw/bin/perl
package Cards;

use strict;
use warnings;

BEGIN {
  use Exporter ();
  our (@ISA, @EXPORT, @EXPORT_OK);

  @ISA = qw(Exporter);
  @EXPORT = qw(
		draw
		name
		shuffle
	     );
  @EXPORT_OK = qw(
		   @cards
		   @suits
		);
}

use feature qw(say);
use List::Util qw();

my $decksinshoe = 6;

our @suits = ('Hearts', 'Diamonds', 'Clubs', 'Spades');
our @cards = ('Ace', 'Two', 'Three', 'Four', 'Five', 'Six', 'Seven',
	      'Eight', 'Nine', 'Ten', 'Jack', 'Queen', 'King');

my @onedeck;
foreach my $s (@suits) { foreach my $c (@cards) {push @onedeck, {suit => $s, card => $c};}}

sub name {
  my ($card) = @_;
  return "$card->{card} of $card->{suit}";
}

my @shoe = ();

sub shuffle {
  for (1..$decksinshoe) {
    push @shoe, @onedeck;
  }
  # XXX XXX this is probably an ideal shuffle, which even a good shoe
  # doesn't provide...
  Lists::Util::shuffle(@shoe);
}

sub draw {
  unless (scalar @shoe) {
    say "Refilling shoe with $decksinshoe decks.";
    shuffle();
  }
  return shift(@shoe);
}

1;
