/*
 * onedee.c -- a simple two dimensional cellular automoton with a variable
 * ruleset, based on the work of Stephen Wolfram.
 *
 * onedee is copyright (c) 1995, Jim Wise
 *
 * You may redistribute this code freely.
 * You may modify and redistribute this code freely as long as you retain
 * this paragraph and an indication that the code has been modified.
 * pedant comes with absolutely NO WARRANTY.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <curses.h>

void	getrules (void);
void	getboard (void);
void	run (void);
void	putline (void);

#define	OTHER(a)	((a) ? 0 : 1)
#define	CHAR(x)		((x) ? '*' : ' ')

int	rows, cols;

/*
 * the rules structure is an array indexed by the values of each of three relevant cells.
 */
 
int	rules[2][2][2];
int	*world[2];
int	current = 0;
int	allrules = 0;

const char	usage[] = "usage: %s [-ha]\n";
const char	help[] =
  "  -h	show this help\n"
  "  -a	run all 256 possible rulesets, instead of prompting for one\n";

void	runall (void);
void	setrules (unsigned int);

int
main (int argc, char **argv)
{
  int c;
  WINDOW *win;

  while ((c = getopt(argc, argv, "ha")) != -1) {
    switch(c) {
    case 'h':
      printf(usage, argv[0]);
      printf(help);
      exit(0);
      break;
    case 'a':
      allrules = 1;
      break;
    case '?':
      fprintf(stderr, usage, argv[0]);
      exit(1);
    }
  }

  if (!allrules)
    getrules();

  win = initscr();
  getmaxyx(win, rows, cols);
  curs_set(0);
  cbreak();
  noecho();
  clear();
  refresh();
	
  if (allrules) {
    runall();
  } else {
    getboard();
    run();
    getch();
  }

  endwin();
  exit(0);
}

/*
 * getrules() -- get the ruleset from the user by listing each state and prompting for
 * a next state
 */

void
getrules (void)
{
  int l, m, r, x = -1;

  /* this is silly, but allows easy expansion to more relevant squares... */
  for (l=0;l<=1;l++)
    for (m=0;m<=1;m++)
      for (r=0;r<=1;r++) {
  	do {
  	  printf("%d%d%d --> ", l, m, r);
  	  scanf("%d", &x);
  	} while ((x != 0) && (x != 1));
  	rules[l][m][r] = x;
      }
}

/*
 * getboard() -- for now, set the board to blank except for one speck in the middle
 */

void
getboard (void)
{
  /* two fencepost cells to allow for normal treatment of edges */
  world[0] = calloc((cols + 2), sizeof(int));
  world[1] = calloc((cols + 2), sizeof(int));

  world[0][cols/2 + 1] = 1;
}

/*
 * run() -- run the simulation
 */
 
void
run (void)
{
  int		x, y;

  clrtobot();
  refresh();
	
  for (x=2; x<(rows); x++) {
    move(x, 0);
    putline();
    for (y=1; y<=cols; y++)
      /* here's the guts of it... */
      world[OTHER(current)][y] =
	rules[world[current][y-1]][world[current][y]][world[current][y+1]];
    current = OTHER(current);
  }
}

/*
 * putline() -- output a single line
 */

void
putline (void)
{
  int		x;

  for (x=1; x<cols+2; x++)
    addch(CHAR(world[current][x]));
  refresh();
}

/*
 * runall() -- run all possible combinations
 */

void
runall (void)
{
  unsigned int	ruleno;
	
  for (ruleno = 0; ruleno <= 255; ruleno++) {
    setrules(ruleno);
    getboard();
    run();
    getch();
  }
}

/*
 * setrules() -- given an int, convert it into a rule and store it in rules[][][]
 */

void
setrules (unsigned int ruleno)
{
  int				l, m, r;
  unsigned char	mask = 1;

  attron(A_REVERSE);
  mvprintw(0, 0, "01234567");
  attroff(A_REVERSE);
  move(1, 0);
  for (l=0;l<=1;l++)
    for (m=0;m<=1;m++)
      for (r=0;r<=1;r++) {
	rules[l][m][r] = (ruleno & mask) > 0;
	mask <<= 1;
	printw(rules[l][m][r] ? "*" : " ");
      }
  refresh();
}
