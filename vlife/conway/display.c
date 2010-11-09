/*
 * display.c -- curses-based display code for Conway's Life
 *
 * life is copyright (c) 1995-2010, Jim Wise
 *
 * You may redistribute this code freely.  You may modify and redistribute
 * this code freely as long as you retain this paragraph and an indication
 * that the code has been modified.  life comes with absolutely NO WARRANTY.
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <curses.h>
#include "life.h"

static void vmessage(char *format, va_list args);

/*
 * display() -- given a board selector, show that board to the user.
 */
 
void
display (int which) {
  int index, xedni;

  for (index=1; index<=YMAX+1; index++) {
    move (index-1, 0);
    for (xedni=1; xedni<=XMAX+1; xedni++)
      addch(CHAR(CELL(index, xedni)));
  }
  refresh();
}

/*
 * prompt() -- display a printf string and wait for keypress
 */

void
prompt(char *fmt, ...)
{
  char line[XMAX+2];

  va_list args;
  va_start(args, fmt);

  strlcpy(line, fmt, sizeof(line));
  strlcat(line, "; <<Press any key>>", sizeof(line));

  vmessage(line, args);
  while (getch() == ERR)	/* XXX when we have a win, control nodelay better */
    ;

  va_end(args);
}

/*
 * message() -- display a printf string and don't ait for keypress
 */

void
message(char *fmt, ...)
{
  va_list args;
  va_start(args, fmt);

  vmessage(fmt, args);
  
  va_end(args);
}

/*
 * vmessage() -- driver for message() and prompt()
 */

static void
vmessage(char *fmt, va_list args)
{
  char line[XMAX+2];

  vsnprintf(line, sizeof(line), fmt, args);
  mvprintw(YMAX+1, 0, line);
  clrtoeol();
  refresh();
}
