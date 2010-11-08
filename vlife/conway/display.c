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
 */
 
void
display (int which) {
  int index, xedni;

  for (index=1; index<=YMAX; index++) {
    move (index-1, 0);
    for (xedni=1; xedni<=XMAX; xedni++)
      addch(CHAR(CELL(index, xedni)));
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
  
  while (getch() == ERR)	/* XXX when we have a win, control nodelay better */
    ;

  va_end(args);
}
