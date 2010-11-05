/*
 * thinkdisplay.c -- mac-specific (console) display code for Conway's Life
 *
 * please see the file life.c for more information
 *
 * life is copyright © 1995, Jim Wise
 * You may redistribute this code freely.
 * You may modify and redistribute this code freely as long as you retain
 * this paragraph and an indication that the code has been modified.
 * life comes with absolutely NO WARRANTY.
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <curses.h>

#include "life.h"

/*
 * display() -- given a board selector, show that board to the user.
 * this version, for the Think C console environment, simply ascii-arts
 * it out.
 */
 
void
display (int which) {
  int index, xedni;

  for (index=1; index<=YMAX; index++) {
    move (index-1, 0);
    for (xedni=1; xedni<=XMAX; xedni++)
      addch(CHAR(world[which][xedni][index]));
  }
  refresh();
}

/*
 * message() -- given a printf string, output the string
 */

void
message(char *format, ...) {
  va_list args;
  char line[XMAX+1];
	
  va_start(args, format);
  
  vsnprintf(line, sizeof(line), format, args);
  strlcat(line, "; <<Press any key>>", sizeof(line));
  mvprintw(YMAX+1, 0, line);
  clrtoeol();
  refresh();

  getch();

  va_end(args);
}


/*
 * filemenu() -- let the user load or save a file
 */
 
void
filemenu (int which) {
  int		flag = 1;
  char	c, fname[NAMELEN];

  mvprintw(YMAX+1, 0, MENUSTR);
  clrtoeol();
  refresh();

  while (flag) {
    c = getch();
		
    switch (c) {
    case 'l':
      clear_board(which);
      if (!getname(fname)) {
	if (load(which, fname))	{
	  message("Could not load board from file %s", fname);
	  clear_board(which);
	} else {
	  message("Board loaded from file %s", fname);
	}
      }
      display(which);
      mvprintw(YMAX+1, 0, MENUSTR);
      clrtoeol();
      break;
    case 's':
      if (!getname(fname)) {
	if (save(which, fname))
	  message("Could not save board to file %s", fname);
	else
	  message("Board saved to file %s", fname);
      }
      mvprintw(YMAX+1, 0, MENUSTR);
      clrtoeol();
      break;
    case 'r':
      flag = 0;
      break;
    default:
      break;
    }
  }
}

/*
 * getname() -- get file name from user and store in given char *
 * returns 0 on success, non-zero if user entered zero-length string
 */

int
getname(char *name) {
  mvprintw(YMAX+1, 0, "Enter FileName: ");
  clrtoeol();
  refresh();
  
  echo();
  nocrmode();
  getstr(name);		/* XXX XXX real curses doesn't have getnstr, use wgetnstr once we have a win */
  crmode();
  noecho();

  return(!strlen(name));
}


/*
 * callback() -- called every turn
 * Returns 1 to keep running, 0 to stop
 */
 
int
callback (int turn, int current) {
  char c;
	
  /* Don't use message() to avoid pause */
  mvprintw(YMAX+1, 0, "Turn : %6d ; <Press any key to interrupt>", turn);
  clrtoeol();
  refresh();
	
  c = getch();
  if (c == EOF)
    return(1);
  else
    return(0);
}
