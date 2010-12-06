#!/opt/csw/bin/perl
package BlackIO;

use strict;
use warnings;
use 5.010;

BEGIN {
  use Exporter ();
  our (@ISA, @EXPORT);

  @ISA = qw(Exporter);
  @EXPORT = qw(
		getbet
		getresp
	     );
}

use feature qw(say);

my $last_bet = 5.00;

sub getbet {
  my ($min, $limit) = @_;
  # XXX check actual min/table_limit rules
  my $bet = 0;

  printf "Please enter a bet (min = \$%0.2f, limit = \$%0.2f) [\$%0.2f]: ",
    $min, $limit, $last_bet;

  while (<STDIN>) {
    chomp;
    if (/^$/) {
      return $last_bet;
    } else {
      s/^\$//;
      $bet = eval {return $_ + 0};
      if ($bet == 0) {
	print "Bet must be a number of dollars, try again: ";
	next;
      }

      if ($bet < $min) {
	print "Bet must be at least $min, try again: ";
	next;
      }
      if ($bet % $min != 0) {
	print "Bet must be in an increment of $min, try again: ";
	next;
      }
      if ($bet > $limit) {
	print "Bet must be less than or equal to $limit, try again: ";
	next;
      }

      # if we get here, bet is good
      $last_bet = $bet;
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
