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
 * To build this code, include it and the <ANSI> library in a Think C project.
 * It has only been tested with Think C 5.0, but should work with any
 * version thereafter, and possibly versions before.  A port to any other
 * compiler or platform would require a change in the code which uses the
 * Think C console interface.
 *
 * pedant is copyright © 1995, Jim Wise
 * You may redistribute this code freely.
 * You may modify and redistribute this code freely as long as you retain
 * this paragraph and an indication that the code has been modified.
 * pedant comes with absolutely NO WARRANTY.
 */

#include	<stdlib.h>
#include	<stdio.h>
#include	<limits.h>
#include	<console.h>

/*
 * play with these to change the size and location of the world.
 * the default is good for a mac SE
 */
#define	XMAX		60
#define	YMAX		44
#define	XLOC		100
#define	YLOC		40
#define	FONTSIZE	6

/* don't touch these */
#define	LEFT	1
#define	UP		2
#define	RIGHT	3
#define DOWN	4
#define STARTX	(XMAX / 2)
#define	STARTY	(YMAX / 2)
#define	TRUE	1
#define	FALSE	0

char	world[XMAX+1][YMAX+1];	/* an extra of each to allow true indexing */
int		dir = LEFT, xcur = STARTX, ycur = STARTY;

int		step (void);
void	report (int turn);

void
main (void)
{
	int 	argc;
	char 	**argv, *end;
	int		trace = 0;
	unsigned long int	index, xedni, count;
	
	/* get and parse argv */
	argc = ccommand(&argv);
	
	if ((argc > 3) || (argc < 2))
	{
		printf("usage: pedant [-t] <Number-of-Steps>\n");
		exit(1);
	}
	else if (argc > 2)
	{
		if (strcmp(argv[1], "-t"))
		{
			printf("usage: pedant [-t] <Number-of-Steps>\n");
			exit(1);
		}
		trace = 1;
		count = strtoul(argv[2], &end, 0);
	}
	else
	{
		count = strtoul(argv[1], &end, 0);
	}
	
	if ((count == 0) || (count == ULONG_MAX))
	{
		printf("Unable to parse count of %s\n", argv[1]);
		exit(2);
	}
	
	/* get ready */
	chide(stdout);
	
	console_options.top = YLOC;
	console_options.left = XLOC;
	console_options.nrows = YMAX;
	console_options.ncols = XMAX;
	console_options.title = "\pPedAnt";
	console_options.txSize = FONTSIZE;
	console_options.procID = 5;
	
	freopenc(NULL, stdout);
	freopenc(stdout, stdin);
	csetmode(C_RAW, stdin);
	
	for (xedni = 0; xedni <= YMAX; xedni++)
		for (index = 0; index <= XMAX; index++);
			world[index][xedni] = ' ';
	
	/* go */
	for (index=1; index<=count; index++)
	{
		if (trace)
			report(index);
		if (step())
		{
			cgotoxy(1, 1, stdout);
			printf("Fell off edge of world on turn %lu.\n", index);
			break;
		}
	}
}

#define TOGGLE(c)	(((c)==' ') ? '¥' : ' ')
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
	cgotoxy(xcur, ycur, stdout);
	putchar(world[xcur][ycur]);
	
	/* Second, the ant moves one square in the current direction */
	switch (dir)
	{
		case LEFT:
			if (xcur-- < 1)
				return TRUE;
			break;
		case UP:
			if (ycur-- < 1)
				return TRUE;
			break;
		case RIGHT:
			if (xcur++ > XMAX)
				return TRUE;
			break;
		default:
			if (ycur++ > YMAX)
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
	cgotoxy(1,1, stdout);
	printf("Turn: %d; Location: %d, %d ; Direction: %d. <<Press any Key>>",
				turn, xcur, ycur, dir);
	while(getc(stdin) == EOF)
		;
}