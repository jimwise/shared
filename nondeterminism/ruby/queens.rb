#!/usr/bin/ruby

require 'nondeterminism'

# This solution to the N queens problem is inspired by that given
#
# Sterling, Leon and Ehud Shapiro, The Art of Prolog, MIT Press, 1994
#   http://www.amazon.com/Art-Prolog-Second-Programming-Techniques/dp/0262193388
#
# but is less elegant, as this is not prolog (and I am not Sterling or Shapiro)
#
# we want to place N queens on an NxN chess board.  Since we know no two queens
# can be in the same row, an array of N integers between 0 and N-1 will do to
# represent the placement.  Since we know no two queens can be in the same column,
# each number from 1 .. N will appear once in this array;  this means the solution
# is a permutation of 1 .. N

def board_to_s board
  s = ""
  board.each_with_index do |x, i|
    r = ( i.odd? ? '. ' : ' .') * ((board.size+1).div 2)
    r[-1, 1] = "" if board.size.odd?
    r[x-1] = "Q";
    s << r << "\n";
  end
  s
end

def show_board board
  print board_to_s board
end

# tests:
# show_board (1..8).to_a
# puts ""
# show_board [1, 3, 5, 7, 2, 4, 6, 8]
raise "board_to_s failed" unless board_to_s([1,2]) == "Q.\n.Q\n";

# board the first M columns of an NxN board, and is valid so far.
# piece is a proposed piece for the M+1th row of the board.
# returns true if piece is a valid placement, false otherwise
def safe board, piece
  board.each_with_index do |c, r|
    return false if c == piece  # same column
    # they're on the same diagonal if the distance in columns == the distance in rows
    rdist = board.size - r;
    cdist = (piece - c).abs
    return false if rdist == cdist
  end
  true
end

# tests:
raise "safe failed" if safe([1, 3, 5], 3);
raise "safe failed" unless safe([1, 3, 5], 2);
raise "safe failed" if safe([1, 3, 5], 4);

$nd = Nondeterminism::Generator.new
def queens n, board = []
  if board.size == n
    board
  else
    c = $nd.choose(1..n)
    $nd.fail! unless safe board, c
    queens n, board + [c]
  end
end

# to run one board
#show_board queens 8

# to show all valid 8x8 boards:
ARGV = ["8"] if ARGV.empty?
ARGV.each do |a|
  begin
    n = a.to_i
    count = 0
    show_board queens n
    count += 1
    puts ""
    $nd.fail!
  rescue Nondeterminism::ChoicesExhausted
    puts "#{count} #{n}x#{n} boards found, not accounting for symmetry"
  end
end
