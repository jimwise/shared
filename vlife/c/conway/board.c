/*
 * board.c -- abstraction for boards
 *
 * life is copyright (c) 1995-2010, Jim Wise
 *
 * You may redistribute this code freely.  You may modify and redistribute
 * this code freely as long as you retain this paragraph and an indication
 * that the code has been modified.  life comes with absolutely NO WARRANTY.
 */

#include <stdlib.h>
#include "life.h"

typedef unsigned char **Board;
Board	world[2];

int	current = 0;		/* which of the two boards in world is active */
int	rows, cols, max_row, max_col;

/*
 * make_board() -- allocate board, given size
 */

void
make_board (int nr, int nc) {
  int i;

  rows = nr;
  cols = nc;
  max_row = nr - 1;
  max_col = nc - 1;

  world[0] = malloc(rows * sizeof(char *));
  world[1] = malloc(rows * sizeof(char *));

  for (i=0; i<rows; i++) {
    world[0][i] = malloc(cols * sizeof(char));
    world[1][i] = malloc(cols * sizeof(char));
  }
}

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
      (col < 0) || (col > max_col))
    return val;

  return world[current][row][col] = val;
}

/*
 * clear_board() -- clear a board
 */

void
clear_board (void) {
  int	index, xedni;
	
  for (index=0; index<rows; index++)
    for (xedni=0; xedni<cols; xedni++)
      set_cell(index, xedni, 0);
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
	
  for (index=0; index<rows; index++)
    for (xedni=0; xedni<cols; xedni++)
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

  count = get_cell(row-1, col-1) + get_cell(row-1, col) + get_cell(row-1, col+1) +
    get_cell(row, col-1) + get_cell(row, col+1) +
    get_cell(row+1, col-1) + get_cell(row+1, col) + get_cell(row+1, col+1);
	
  if (get_cell(row, col))	{
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
