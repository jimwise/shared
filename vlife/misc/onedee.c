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

#include	<stdio.h>
#include	<console.h>

#ifdef	SMALL
#define	XMAX		200
#define	TURNS		160
#define	FONTSIZE	1
#define	INVERSE		1
#define	GLYPH		(' '|0x80)
#else
#define	XMAX		90
#define	TURNS		30
#define	FONTSIZE	9
#define	INVERSE		0
#define	GLYPH		'+'
#endif

#define	TOP		120
#define	LEFT	20

void	getrules (void);
void	getboard (void);
void	run (void);
void	putline (void);

#define	OTHER(a)	((a) ? 0 : 1)
#define	CHAR(x)		((x) ? GLYPH : ' ')

/*
 * the rules structure is an array indexed by the values of each of three relevant cells.
 */
 
int		rules[2][2][2];
int		world[2][XMAX+2];	/* two fencepost cells to allow for normal treatment of edges */
int		current = 0;

#ifdef	ALL_RULES
void	runall (void);
void	setrules (unsigned char);
FILE	*rulewin;
#endif

void
main (void)
{
#ifndef	ALL_RULES
  getrules();
#endif

#ifdef	ALL_RULES
  console_options.top = TOP - 70;
  console_options.left = LEFT;
  console_options.nrows = 2;
  console_options.ncols = 8;
  console_options.title = "\pRules";
  console_options.txSize = 12;
  console_options.procID = 5;
	
  rulewin = fopenc();
  cinverse(1, rulewin);
#endif
  console_options.top = TOP;
  console_options.left = LEFT;
  console_options.nrows = TURNS;
  console_options.ncols = XMAX;
  console_options.title = "\pTwoDee";
  console_options.txSize = FONTSIZE;
  console_options.procID = 5;
	
  freopenc(NULL, stdout);
  freopenc(stdout, stdin);
  cinverse(INVERSE, stdout);
	
#ifdef	ALL_RULES
  cgotoxy(1,1, rulewin);
  fprintf(rulewin, "%c%c%c%c%c%c%c%c", '0'|0x80, '1'|0x80, '2'|0x80, '3'|0x80,
	  '4'|0x80, '5'|0x80, '6'|0x80, '7'|0x80);
  runall();
#else
  getboard();
  run();
#endif
}

/*
 * getrules() -- get the ruleset from the user by listing each state and prompting for
 * a next state
 */

void
getrules (void)
{
  int		l, m, r, x = -1;
  char	c;
	
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
	
  cgotoxy(1,1, stdout);
  ccleos(stdout);
	
  for (x=1; x<=TURNS; x++)
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
  unsigned char	ruleno;
	
  for (ruleno = 0; ruleno <= 256; ruleno++)
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
setrules (unsigned char ruleno)
{
  int				l, m, r;
  unsigned char	mask = 1;
	
  cgotoxy(1,2, rulewin);
	
  for (l=0;l<=1;l++)
    for (m=0;m<=1;m++)
      for (r=0;r<=1;r++)
	{
	  rules[l][m][r] = (ruleno & mask) > 0;
	  mask <<= 1;
	  fprintf(rulewin, "%1d", rules[l][m][r]);
	}
  fflush(rulewin);
}
#endif
