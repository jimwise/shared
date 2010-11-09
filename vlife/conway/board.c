/*
 * board.c -- abstraction for boards
 *
 * life is copyright (c) 1995-2010, Jim Wise
 *
 * You may redistribute this code freely.  You may modify and redistribute
 * this code freely as long as you retain this paragraph and an indication
 * that the code has been modified.  life comes with absolutely NO WARRANTY.
 */

#include "life.h"

/* visible world runs from 1..?MAX+1, with fenceposts all around */
typedef unsigned char Board[YMAX+3][XMAX+3];
Board	world[2];

#define CELL(row, col)	world[current][row][col]

int	current = 0, rows = YMAX+1, cols = XMAX+1;
int	max_row = YMAX, max_col = XMAX;

/*
 * get_cell(), set_cell() -- generic board interface; hides that there are two boards
 */

int get_cell (int row, int col) {
  if ((row < 0) || (row > max_row) ||
      (col < 0) || (col > max_col))
    return 0;

  return world[current][row][col];
}

int set_cell (int row, int col, char val) {
  if ((row < 0) || (row > max_row) ||
      (col < 0) || (col > max_row))
    return val;

  return world[current][row][col] = val;
}

/*
 * clear_board() -- clear a board
 */

void
clear_board (void) {
  int	index, xedni;
	
  for (index=0; index<=YMAX+1; index++)
    for (xedni=0; xedni<=XMAX+1; xedni++)
      CELL(index, xedni) = 0;
}

/*
 * generation() -- given two board selectors, generate the next generation
 * from the first board into the second board.
 *
 * on reflection, this is the only code which ever needs to know there are two
 * boards (though the file code might want to)
 */

void
generation (void) {
  int index, xedni, to = OTHER(current);
	
  for (index=1; index<=YMAX; index++)
    for (xedni=1; xedni<=XMAX; xedni++)
      world[to][index][xedni] = determine(index, xedni);

  current = to;
}

/*
 * determine() -- given a board selector, and the coordinates of a square,
 * return the state of the square in the next generation.
 */

int
determine (int row, int col) {
  int	count=0;
  count = CELL(row-1, col-1) + CELL(row-1, col) + CELL(row-1, col+1) +
    CELL(row, col-1) + CELL(row, col+1) +
    CELL(row+1, col-1) + CELL(row+1, col) + CELL(row+1, col+1);
	
  if (CELL(row, col))	{
    if ((count == 2) || (count == 3))
      return 1;
    else
      return 0;
  } else {
    if (count == 3)
      return 1;
    else
      return 0;
  }
}