/*
 * generation.c -- single generation routine for Conway's Life
 *
 * please see the file life.c for more information
 *
 * life is copyright © 1995, Jim Wise
 * You may redistribute this code freely.
 * You may modify and redistribute this code freely as long as you retain
 * this paragraph and an indication that the code has been modified.
 * life comes with absolutely NO WARRANTY.
 */

#include	"life.h"

/*
 * generation() -- given two board selectors, generate the next generation
 * from the first board into the second board.
 */

void
generation (int from, int to)
{
	int index, xedni;
	
	for (index=1; index<=YMAX; index++)
		for (xedni=1; xedni<=XMAX; xedni++)
			world[to][xedni][index] = determine(from, xedni, index);
}

/*
 * determine() -- given a board selector, and the coordinates of a square,
 * return the state of the square in the next generation.
 */

int
determine (int which, int x, int y)
{
	int	count=0;
	
	count = world[which][x-1][y-1]
			+ world[which][x][y-1]
			+ world[which][x+1][y-1]
			+ world[which][x-1][y]
			+ world[which][x+1][y]
			+ world[which][x-1][y+1]
			+ world[which][x][y+1]
			+ world[which][x+1][y+1];
	
	if (world[which][x][y])
	{
		if ((count == 2) || (count == 3))
			return 1;
		else
			return 0;
	}
	else
	{
		if (count == 3)
			return 1;
		else
			return 0;
	}
}