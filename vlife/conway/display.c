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

static WINDOW *win;

/*
 * begin_display(), end_display() -- curses setup and teardown
 */

void
begin_display (int *r, int *c) {
  win = initscr();
  getmaxyx(win, *r, *c);
  cbreak();
  noecho();
  clear();
  refresh();
  nodelay(win, 1);
}

void end_display (void) {
  endwin();
}


/*
 * key_pressed() -- return whether user has pressed a key (and discard that key) 
 * depends on nodelay being in effect.
 */
int
key_pressed (void) {
  return (getch() != ERR);
}

/*
 * display() -- given a board selector, show that board to the user.
 */
 
void
display (void) {
  int index, xedni;

  for (index=0; index<rows; index++) {
    move (index, 0);
    for (xedni=0; xedni<cols; xedni++)
      addch(CHAR(get_cell(index, xedni)));
  }
  refresh();
}

/*
 * prompt() -- display a printf string and wait for keypress
 */

void
prompt(char *fmt, ...)
{
  char line[1024];

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
  char line[1024];

  vsnprintf(line, sizeof(line), fmt, args);
  attron(A_REVERSE);
  mvprintw(msg_row, 0, line);
  clrtoeol();
  attroff(A_REVERSE);
  refresh();
}


/*
 * get_string() -- get string from user.  returns NULL if blank.
 */

static char gsbuf[NAMELEN];

char *
prompt_string(char *prompt) {
  gsbuf[0] = '\0';
  
  message(prompt);
  echo();
  nodelay(win, 0);
  wgetnstr(win, gsbuf, sizeof(gsbuf));
  nodelay(win, 1);
  noecho();

  if (strlen(gsbuf))
    return(gsbuf);
  else
    return(NULL);
}
