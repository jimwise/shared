/*
 * edit.c -- curses based editing ui for life
 *
 * life is copyright (c) 1995-2010, Jim Wise
 *
 * You may redistribute this code freely.  You may modify and redistribute
 * this code freely as long as you retain this paragraph and an indication
 * that the code has been modified.  life comes with absolutely NO WARRANTY.
 */

#include <stdio.h>
#include <string.h>
#include <curses.h>
#include "life.h"

static void	filemenu (void);

#define	MOVECURSOR(delta_x, delta_y)		\
  xcur = MAX(MIN(xcur + delta_x, max_col), 0);	\
  ycur = MAX(MIN(ycur + delta_y, max_row), 0);	
					
/*
 * edit() -- allow the user to set up the board.
 * Returns 1 if the user wants to run, 0 if the user wants to quit...
 * This version, for the Think C console environment, implements a simple
 * text-based screen editing, using the commands described in life.c
 */

int
edit (void) {
  int	c, xcur = 0, ycur = 0;
	
  display();
			
  message(EDIT_INSTSTR);
	
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
      set_cell(ycur, xcur, OTHER(get_cell(ycur, xcur)));
      addch(CHAR(get_cell(ycur, xcur)));
      move(ycur,xcur);
      break;
    case 'f':
      filemenu();
      message(EDIT_INSTSTR);
      break;
    case 'q':
      return(0);
      break;
    case 'g':
      return(1);
      break;
    case 'c':
      clear_board();
      display();
      break;
    default:			/* e.g. ERR if no char ready */
      break;
    }
  }
}

/*
 * filemenu() -- let the user load or save a file
 */
 
static void
filemenu (void) {
  int	c;
  char	*fname;

  message(MENUSTR);

  while (1) {
    c = getch();
		
    switch (c) {
    case 'l':
      clear_board();
      if ((fname = prompt_string("Enter FileName: ")) != NULL) {
	if (load(fname)) {
	  prompt("Could not load board from file %s", fname);
	  clear_board();
	} else {
	  prompt("Board loaded from file %s", fname);
	}
      }
      display();
      return;
      break;
    case 's':
      if ((fname = prompt_string("Enter FileName: ")) != NULL) {
	if (save(fname))
	  prompt("Could not save board to file %s", fname);
	else
	  prompt("Board saved to file %s", fname);
      }
      return;
      break;
    case 'r':
      return;
      break;
    default:			/* e.g. ERR, i.e. no char ready */
      break;
    }
  }
}
