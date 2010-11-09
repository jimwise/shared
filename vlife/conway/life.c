/*
 * life.c -- main routine and game logic for Conway's Life
 *
 * life is copyright (c) 1995-2010, Jim Wise
 *
 * You may redistribute this code freely.  You may modify and redistribute
 * this code freely as long as you retain this paragraph and an indication
 * that the code has been modified.  life comes with absolutely NO WARRANTY.
 */

#include <stdlib.h>
#include <unistd.h>
#include <curses.h>
#include "life.h"

Board	world[2];
int	current = BOARD_A, turn = 1;

int
main (int argc, char **argv) {
  WINDOW *win;

  win = initscr();
  cbreak();
  noecho();
  clear();
  refresh();
  nodelay(win, 1);

  prompt("Welcome to Life, Version %s, Copyright 1995, Jim Wise", VERSION_STR);
  clear_board(BOARD_A);
	
  while (1) {	
    if (!edit(current))
      break;
    run();
  }

  endwin();
  exit(0);
}

/*
 * clear_board() -- clear a board
 */

void
clear_board (int which) {
  int	index, xedni;
	
  for (index=0; index<=YMAX+1; index++)
    for (xedni=0; xedni<=XMAX+1; xedni++)
      CELL(index, xedni) = 0;
}

/*
 * run() -- run life cycle until interrupted
 */

void
run (void) {
  while (1) {
    message("Turn : %6d ; <Press any key to interrupt>", turn);
    clrtoeol();
    display(current);
		
    if(getch() != ERR)
      break;
		
    generation(current, OTHER(current));
    current = OTHER(current);
		
    turn++;
    sleep(1);
  }
}


/*
 * generation() -- given two board selectors, generate the next generation
 * from the first board into the second board.
 */

void
generation (int from, int to) {
  int index, xedni;
	
  for (index=1; index<=YMAX; index++)
    for (xedni=1; xedni<=XMAX; xedni++)
      world[to][index][xedni] = determine(from, index, xedni);
}

/*
 * determine() -- given a board selector, and the coordinates of a square,
 * return the state of the square in the next generation.
 */

int
determine (int which, int row, int col) {
  int	count=0;
  /* XXX XXX XXX pseudo-dynamic scope --depends on which naming the paramter here*/
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
