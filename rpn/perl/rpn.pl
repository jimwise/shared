#!/usr/bin/perl

use strict;
use warnings;
use 5.010;

use Scalar::Util qw(looks_like_number);

my %ops;
my $stack = [];

sub make_op {
  my ($name, %op) = @_;
  $ops{$name} = \%op;
}

make_op('.',
	arity => 1,
	doc => "show the top value on the stack",
	op => sub {
	  my ($x, $s) = @_;
	  say $x;
	  push @$s, $x;
	  $s;
	});

make_op('+',
	arity => 2,
	doc => "replace the top two values on the stack with their sum",
	op => sub {
	  my ($x, $y, $s) = @_;
	  push @$s, $x + $y;
	  $s;
	});

make_op('-',
	arity => 2,
	doc => "replace the top two values on the stack with their difference",
	op => sub {
	  my ($x, $y, $s) = @_;
	  push @$s, $x - $y;
	  $s;
	});

make_op('*',
	arity => 2,
	doc => "replace the top two values on the stack with their product",
	op => sub {
	  my ($x, $y, $s) = @_;
	  push @$s, $x * $y;
	  $s;
	});

make_op('/',
	arity => 2,
	doc => "replace the top two values on the stack with their quotient",
	op => sub {
	  my ($x, $y, $s) = @_;
	  push @$s, $x / $y;
	  $s;
	});

make_op('^',
	arity => 2,
	doc => "replace the top two values on the stack, x and y, with x to the yth power",
	op => sub {
	  my ($x, $y, $s) = @_;
	  push @$s, $x ** $y;
	  $s;
	});

make_op('^',
	arity => 2,
	doc => "replace the top two values on the stack, x and y, with x^y",
	op => sub {
	  my ($x, $y, $s) = @_;
	  push @$s, $x ** $y;
	  $s;
	});

make_op('drop',
	arity => 1,
	doc => "remove the top value from the stack",
	op => sub {
	  my ($x, $s) = @_;
	  $s;
	});

make_op('dup',
	arity => 1,
	doc => "duplicate the top value on the stack",
	op => sub {
	  my ($x, $s) = @_;
	  push @$s, $x, $x;
	  $s;
	});

make_op('swap',
	arity => 2,
	doc => "swap the top two values on the stack",
	op => sub {
	  my ($x, $y, $s) = @_;
	  push @$s, $y, $x;
	  $s;
	});

make_op('help',
	arity => 0,
	doc => "display this help",
	op => sub {
	  my ($s) = @_;
	  my @k = sort keys %ops;
	  say scalar(@k), " Commands:";
	  foreach my $k (@k) {
	    say "$k -- $ops{$k}->{doc}"
	  }
	  $s;
	});

sub signal_error {
  my ($str) = @_;
  say "ERROR: $str";
}

sub act {
  my ($s) = @_;
  if (exists $ops{$s}) {
    my $op = $ops{$s};
    if ((scalar @$stack) < $op->{arity}) {
      signal_error("stack underflow")
    } else {
      my @args;
      foreach (1 .. $op->{arity}) {
	unshift @args, pop(@$stack);
      }
      $stack = $op->{op}->(@args, $stack);
    }
  } elsif (looks_like_number($s)) {
    push @$stack, $s;
  } else {
    signal_error("unknown operation")
  }
}

print "> ";
while (<>) {
  chomp;
  act($_);
  print "> ";
}

