/*
 * life.c -- main routine for Conway's Life
 *
 * (This information is replicated in the file README)
 *
 * This and the accompanying files are a very simple implementation of
 * the game of Life, as described by Elwyn R. Berklekamp, John H.
 * Conway, and Richard Guy in _Winning_Ways_For_Your_Mathematical_Plays_
 * (New York:  Academic Press, 1982):
 *
 *		"Life Occurs on a virtual checkerboard.  The squares are called
 *		cells.  They are in one of two states:  alive or dead.  Each cell
 *		has eight possible neighbors, the cells which touch its sides or
 *		its corners.
 *		If a cell on the checkerboard is alive, it will survive in the
 *		next time step (or generation) if there are either two or three
 *		neighbors also alive.  It will die of overcrowding if there are
 *		more than three live neighbors, and it will die of exposure if
 *		there are fewer than two.
 *		If a cell on the checkerboard is dead, it will remain dead in the
 *		next generation unless exactly three of its eight neighbors are
 *		alive.  In that case, the cell will be born in the next
 *		generation."
 *
 * From this very simple set of rules, amazingly complex behavior develops.
 * 
 * The mac version of life has no command line options.
 *
 * Life starts in edit mode.  Use the following keys to move the '%'
 * cursor:
 *                          
 *                7  8  9                       k
 *                 \ | /                        |
 *                  \l/                         |
 *                4--+--l         or         h--+--l
 *                  /l\                         |
 *                 / | \                        |
 *                1  2  3                       j
 *
 * Press <space> to toggle the current square.
 * To quit life, type 'q'.  To clear the screen, type 'c'.
 * To exit edit mode, type 'g'.  Life will then run until a key is pressed.
 * After being interrupted by a keypress, life will return to edit mode. 
 *
 * To build this code for the macintosh, include the files generation.c,
 * life.c, thinkdisplay.c, thinkedit.c, thinkinit.c, thinkcallback.c,
 * and the <ANSI> library in a Think C project.
 * I recommend adding the lines:
 *		#define XMAX 90
 *		#define YMAX 30
 * to the project prefix (under options) as well.  See below for details.
 *
 * This version uses the Think C console interface.
 * It has only been tested with Think C 5.0, but should work with any
 * version thereafter, and possibly versions before.  A port to any other
 * compiler or platform would require a change in the code which uses the
 * Think C console interface.  All such code is isolated in the files
 * beginning with 'think'.  To produce a version for another platform or
 * compiler, you will need to code the following functions:
 *
 *				void	init (int *, char ***);
 *				void	clear_board (int);
 *				int		callback (int, int);
 *				void	display (int);
 *				int		edit (int);
 *				void	message(char *, ...);
 *
 * See the files think* for details.
 *
 * In addition, if you want file support, you must code the following routines.
 * Otherwise, you should exclude file.c and #define NO_FILE
 *
 *				int		openfile (char *, int);
 *				int		closefile (void);
 *				int		putstring (char *, ...);
 *				int		checkstring (char *, ...);
 *				int		getsize (int *, int *);
 *				int		putsize (int, int);
 *				int		getcell (void);
 *				int		putcell (int);
 *
 * Note that ansifile.c contains versions of these routines that should work with any
 * working ANSI standard libc, so you can probably just include it or at worst clone
 * and hack...
 *
 * Also note that the size configuration constants in life.h are designed to
 * be overridden by each specific build.  See above for good values for the
 * Think C console...
 *
 * life is copyright © 1995, Jim Wise
 * You may redistribute this code freely.
 * You may modify and redistribute this code freely as long as you retain
 * this paragraph and an indication that the code has been modified.
 * life comes with absolutely NO WARRANTY.
 */

#include <stdlib.h>
#include "life.h"

Board	world[3];
int		current = BOARD_A, turn = 1;

int
main (int argc, char **argv)
{
	init(&argc, &argv);
	clear_board(BOARD_A);
	
	while ( 1 )
	{	
		if ( !edit(current) )
			break;
		run();
	}

	exit(0);
}

/*
 * clear_board() -- clear a board
 */

void
clear_board (int which)
{
	int	index, xedni;
	
	for (index=1; index<=YMAX; index++)
		for (xedni=1; xedni<=XMAX; xedni++)
			world[which][xedni][index] = 0;
}

/*
 * run() -- run life cycle until interrupted
 */

void
run (void)
{
	while ( 1 )
	{
		display(current);
			
		if ( !callback(turn, current) )
			break;
		
		generation(current, OTHER(current));
		current = OTHER(current);
		
		turn++;
	}
}
