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
#include <curses.h>

#define ALL_RULES

#define	XMAX		90
#define	TURNS		30

void	getrules (void);
void	getboard (void);
void	run (void);
void	putline (void);

#define	OTHER(a)	((a) ? 0 : 1)
#define	CHAR(x)		((x) ? '*' : ' ')

/*
 * the rules structure is an array indexed by the values of each of three relevant cells.
 */
 
int		rules[2][2][2];
int		world[2][XMAX+2];	/* two fencepost cells to allow for normal treatment of edges */
int		current = 0;

#ifdef	ALL_RULES
void	runall (void);
void	setrules (unsigned int);
FILE	*rulewin;
#endif

int
main (int argc, char **argv)
{
#ifndef	ALL_RULES
  getrules();
#endif

  initscr();
  cbreak();
  noecho();
  clear();
  refresh();
	
#ifdef	ALL_RULES
  attron(A_REVERSE);
  mvprintw(0, 0, "01234567");
  attroff(A_REVERSE);
  runall();
#else
  getboard();
  run();
#endif

  exit(0);
}

/*
 * getrules() -- get the ruleset from the user by listing each state and prompting for
 * a next state
 */

void
getrules (void)
{
  int		l, m, r, x = -1;
	
  /* this is silly, but allows easy expansion to more relevant squares... */
  for (l=0;l<=1;l++)
    for (m=0;m<=1;m++)
      for (r=0;r<=1;r++)
	{
	  do
	    {
	      printf("%d%d%d --> ", l, m, r);
	      scanf("%d", &x);
	    }
	  while ((x != 0) && (x != 1));
	  rules[l][m][r] = x;
	}
}

/*
 * getboard() -- for now, set the board to blank except for one speck in the middle
 */

void
getboard (void)
{
  int		x;
	
  for (x=0; x<=XMAX+2; x++)
    world[current][x] = 0;
	
  world[current][XMAX/2] = 1;
}

/*
 * run() -- run the simulation
 */
 
void
run (void)
{
  int		x, y;

  clear();
  refresh();
	
  for (x=0; x<TURNS; x++)
    {
      putline();
      for(y=1; y<=XMAX; y++)
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
	
  for (x=1; x<=XMAX; x++)
    putc(CHAR(world[current][x]), stdout);
}

#ifdef	ALL_RULES
/*
 * runall() -- run all possible combinations
 */

void
runall (void)
{
  unsigned int	ruleno;
	
  for (ruleno = 0; ruleno <= 255; ruleno++)
    {
      setrules(ruleno);
      getboard();
      run();
      getc(stdin);
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

  move(0,1);
  for (l=0;l<=1;l++)
    for (m=0;m<=1;m++)
      for (r=0;r<=1;r++)
	{
	  rules[l][m][r] = (ruleno & mask) > 0;
	  mask <<= 1;
	  printf("%1d", rules[l][m][r]);
	}
  fflush(rulewin);
}
#endif
