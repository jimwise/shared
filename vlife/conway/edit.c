/*
 * thinkedit.c -- mac-specific (console) board editing code for Conway's Life
 *
 * please see the file life.c for more information
 *
 * life is copyright © 1995, Jim Wise
 * You may redistribute this code freely.
 * You may modify and redistribute this code freely as long as you retain
 * this paragraph and an indication that the code has been modified.
 * life comes with absolutely NO WARRANTY.
 */

#include <stdio.h>
#include <curses.h>
#include "life.h"

#define	MOVECURSOR(delta_x, delta_y)		\
  xcur = MAX(MIN(xcur + delta_x, XMAX), 0);	\
  ycur = MAX(MIN(ycur + delta_y, YMAX), 0);	
					
/*
 * edit() -- allow the user to set up the board.
 * Returns 1 if the user wants to run, 0 if the user wants to quit...
 * This version, for the Think C console environment, implements a simple
 * text-based screen editing, using the commands described in life.c
 */

int
edit (int which) {
  char	c;
  int	xcur = 0, ycur = 0;
	
  display(which);
			
  /* Don't use message() to avoid pause */
  mvprintw(YMAX+1, 0, EDIT_INSTSTR);
  clrtoeol();
	
  while (1) {
    move(ycur, xcur);
		
    c = getch();
		
    switch (c) {
    case 'h':
    case '4':
      MOVECURSOR(-1, 0);
    break;
    case '7':
      MOVECURSOR(-1, -1);
      break;
    case 'k':
    case '8':
      MOVECURSOR(0, -1);
    break;
    case '9':
      MOVECURSOR(1, -1);
      break;
    case 'l':
    case '6':
      MOVECURSOR(1, 0);
    break;
    case '3':
      MOVECURSOR(1, 1);
      break;
    case 'j':
    case '2':
      MOVECURSOR(0, 1);
    break;
    case '1':
      MOVECURSOR(-1, 1);
      break;
    case ' ':
    case '5':
      CELL(ycur+1, xcur+1) = OTHER(CELL(ycur+1, xcur+1));
      addch(CHAR(CELL(ycur+1, xcur+1)));
      break;
    case 'f':
      filemenu(which);
      mvprintw(YMAX+1, 0, EDIT_INSTSTR);
      clrtoeol();
      break;
    case 'q':
      return(0);
      break;
    case 'g':
      return(1);
      break;
    case 'c':
      clear_board(which);
      display(which);
      break;
    default:			/* e.g. ERR if no char ready */
      break;
    }
  }
}
