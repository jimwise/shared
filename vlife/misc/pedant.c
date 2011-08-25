/*
 * pedant.c -- a simple random walk generator based on the walk
 * simulator described in _Science_News_ Vol.148,No.18 (10/28/95)
 * Right now, this is Think C specific... perhaps later that will change.
 * 
 * The concept, taken from the work of Christopher G. Langton at SFI, is
 * of an ant wandering in a 2-D bitfield.  As the ant enters a square, it
 * toggles the value of the square.  It then turns 90¡ to the left if the
 * squares original value was zero, or to the right if it was 1.
 *
 * Although no rules govern system-wide behavior, or span more than the
 * current turn, suddenly, at about the 10,000th turn, the ant will begin
 * to build a 'highway' towards the northeast...
 *
 * Usage: pedant [-t] <<Number-of-Turns>>
 * causes pedant to run for <<Number-of-Turns>> turns.  If -t is specified,
 * pedant runs in trace mode, stopping after each turn to report, and wait
 * for a key press.
 *
 * pedant is copyright (c) 1995, Jim Wise
 *
 * You may redistribute this code freely.
 * You may modify and redistribute this code freely as long as you retain
 * this paragraph and an indication that the code has been modified.
 * pedant comes with absolutely NO WARRANTY.
 */

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <curses.h>

/* don't touch these */
typedef enum {LEFT, UP, RIGHT, DOWN} dir_t;

const char	usage[] = "usage: %s [-ht] [<number-of-steps>]\n";
const char	help[] =
  "  -h	show this help\n"
  "  -t	turn on trace mode\n";

char	**world;
dir_t	dir = LEFT;
int	xcur, ycur, xmax, ymax;

int	step (void);
void	report (int turn);

int
main (int argc, char **argv) {
  char 	*end;
  int	trace = 0;
  int 	c, x, y;
  unsigned long	n, count;
  WINDOW *win;

  while ((c = getopt(argc, argv, "ht")) != -1) {
    switch(c) {
    case 'h':
      printf(usage, argv[0]);
      printf(help);
      exit(0);
      break;
    case 't':
      trace = 1;
      break;
    case '?':
      fprintf(stderr, usage, argv[0]);
      exit(1);
    }
  }

  if (optind == argc) {
    count = ULONG_MAX;
  } else {
    count = strtoul(argv[optind], &end, 0);
    if ((count == 0) || (count == ULONG_MAX)) {
      fprintf(stderr, "Unable to parse count of %s\n", argv[1]);
      exit(1);
    }
  }
  
  win = initscr();
  getmaxyx(win, ymax, xmax);
  ycur = ymax / 2;
  xcur = xmax / 2;
  cbreak();
  noecho();
  clear();
  refresh();

  world = malloc((xmax + 1) * sizeof(char *));
  for (x = 0; x <= xmax; x++) {
    world[x] = malloc((ymax + 1) * sizeof(char));
    for (y = 0; y <= ymax; y++)
      world[x][y] = ' ';
  }
	
  for (n=1; n<count; n++) {
    if (step()) {
      mvprintw(0, 0, "Fell off edge of world on turn %lu. <<Press any key>>", n);
      move(ycur, xcur);
      refresh();
      getch();
      break;
    }
    if (trace)
      report(n);
  }
  if (n == count) {
    mvprintw(0, 0, "Simulation ended on turn %lu. <<Press any key>>", n);
    getch();
  }
  endwin();
  exit(0);
}

#define TOGGLE(c)	(((c)==' ') ? '*' : ' ')
#define RTURN(x)	(((x)==DOWN) ? LEFT : (x) + 1)
#define	LTURN(x)	(((x)==LEFT) ? DOWN : (x) - 1)

/*
 * step() -- take one step, return TRUE if we fell off the edge of the world
 */
 
int
step (void)
{
  /* First, the current square is toggled */
  world[xcur][ycur] = TOGGLE(world[xcur][ycur]);
  move(ycur, xcur);
  addch(world[xcur][ycur]);
  refresh();
	
  /* Second, the ant moves one square in the current direction */
  switch (dir) {
  case LEFT:
    if (--xcur < 0)
      return TRUE;
    break;
  case UP:
    if (--ycur < 0)
      return TRUE;
    break;
  case RIGHT:
    if (++xcur > xmax)
      return TRUE;
    break;
  default:
    if (++ycur > ymax)
      return TRUE;
    break;
  }
	
  /* Finally, direction is changed based on the current square */
	
  if (world[xcur][ycur] == ' ')
    dir = LTURN(dir);
  else
    dir = RTURN(dir);
		
  return FALSE;
}

/*
 * report() -- given turn #, report status and wait for a keypress
 */
void
report(int turn)
{
  mvprintw(0, 0, "Turn: %d; Location: %d, %d ; Direction: %d. <<Press any Key>>",
	   turn, xcur, ycur, dir);
  move(ycur, xcur);
  refresh();
  getch();
}
