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

int turn = 1;

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
  clear_board();
	
  while (1) {	
    if (!edit())
      break;
    run();
  }

  endwin();
  exit(0);
}

/*
 * run() -- run life cycle until interrupted
 */

void
run (void) {
  while (1) {
    message("Turn : %6d ; <Press any key to interrupt>", turn);
    clrtoeol();
    display();
		
    if(getch() != ERR)
      break;
		
    generation();
		
    turn++;
    sleep(1);
  }
}


