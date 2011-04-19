#!/usr/bin/ruby

require 'nondeterminism'

# This solution to the N queens problem is inspired by that given
#
# Sterling, Leon and Ehud Shapiro, The Art of Prolog, MIT Press, 1994
#   http://www.amazon.com/Art-Prolog-Second-Programming-Techniques/dp/0262193388
#
# but is less elegant, as this is not prolog
#
# we want to place N queens on an NxN chess board.  Since we know no two queens
# can be in the same row, an array of N integers between 0 and N-1 will do to
# represent the placement.  Since we know no two queens can be in the same column,
# each number from 1 .. N will appear once in this array;  this means the solution
# is a permutation of 1 .. N

def board_to_s board
  s = ""
  board.each do |x|
    s << "." * (x-1)
    s << "Q"
    s <<  "." * (board.size - x)
    s << "\n"
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
raise unless board_to_s([1,2]) == "Q.\n.Q\n";

# board the first M columns of an NxN board, and is valid so far.
# piece is a proposed piece for the M+1th row of the board.
# returns true if piece is a valid placement, false otherwise
def safe board, piece
  board.each_index do |x|
    q = board[x]
    return false if q == piece  # same column
    # they're on the same diagonal if the distance in columns == the distance in rows
    # or the distance in columns == 0 - the distance in rows
    xdist = board.size - x;
    ydist = (piece - q).abs
    return false if xdist == ydist
  end
  true
end

# tests:
raise if safe([1, 3, 5], 3);
raise unless safe([1, 3, 5], 2);
raise if safe([1, 3, 5], 4);

$nd = Nondeterminism::Generator.new
def queens n, board = []
  cols = (1..n).to_a
  if board.size == n
    board
  else
    c = $nd.choose(1..n)
    $nd.fail! unless safe board, c
    queens n, board + [c]
  end
end

show_board queens 8

