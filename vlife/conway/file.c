/*
 * file.c -- generic Load and Save routines for Conways Life
 *
 * Note that this file is pretty grossly inefficient in that a lot of things should
 * really be calls to the standard library.  It _is_ however, 100% generic in terms
 * of all sorts of file systems, since all actual calls to the file system go through
 * a seperate platform-specific set of routines.
 *
 * Worth noting, I suppose, that this code does more error checking than the rest of
 * the system combined, mostly to take care of the number of recoverable things that
 * can go wrong here.
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

static int		x_min, x_max, y_min, y_max;

/*
 * save() -- save board in standard ASCII format
 * returns 0 on successful save, non-zero otherwise.
 */

int
save(int which, char *name)
{
	int		index, xedni;
	
	findbounds(which);
	
	if (openfile(name, WRITEFILE))
	{
		message("Could not open file %s", name);
		return(1);
	}

	if ( putstring(FILE_HEADERSTRING)
		 || putstring(FILE_SIZESTRING)
		 || putsize(x_max - x_min + 1, y_max - y_min + 1)
		 || putstring(FILE_SEPSTRING) )
	{
		message("Could not write header to file %s", name);
		closefile();
		return(1);
	}
	
	if ( putboard(which)
		 || putstring(FILE_SEPSTRING))
	{
		message("Could not write board to file %s", name);
		closefile();
		return(1);
	}
	
	if (closefile())
	{
		message("Failed to close file %s", name);
		return(1);
	}
	
	return(0);
}

/*
 * load() -- load board in standard ASCII format
 * Return 0 after a successful load, non-zero otherwise...
 */

int
load (int which, char *name)
{
	int index, xedni, x_size, y_size;
	
	if (openfile(name, READFILE))
	{
		message("Could not open file %s", name);
		return(1);
	}
		
	if ( checkstring(FILE_HEADERSTRING)
		 || checkstring(FILE_SIZESTRING)
		 || getsize(&x_size, &y_size)
		 || checkstring(FILE_SEPSTRING) )
	{
		message("Bad header information in file %s", name);
		closefile();
		return(1);
	}
	
	if (getboard(which, x_size, y_size))
	{
		message("Invalid board in file %s", name);
		closefile();
		return(1);
	}
	
	if (checkstring(FILE_SEPSTRING))
	{
		message("Incomplete file %s", name);
		closefile();
		return(1);
	}
	
	if (closefile())
	{
		message("Failed to close file %s", name);
		return(1);
	}
		
	return(0);
}

/*
 * findbounds() -- determine the minimum and maximum x and y bounds of a life
 * board.  This allows boards saved in one version of life to open boards it can
 * manage, even if saved with a larger version.
 * If there are no live cells, we treat the board as an empty board of size 1x1.
 */

void
findbounds (int which)
{
	int		index, xedni;

	x_min = XMAX, x_max = 0, y_min = YMAX, y_max = 0;

	/* this is ugly but simple... */
	for (index=1; index<=YMAX; index++)
		for (xedni=1; xedni<=XMAX; xedni++)
			if ( world[which][xedni][index] )
			{
				x_min = MIN(x_min, xedni);
				x_max = MAX(x_max, xedni);
				y_min = MIN(y_min, index);
				y_max = MAX(y_max, index);
			}

	/* if x_max is still zero, we never found a live cell */
	if (!x_max)
		x_min = x_max = y_min = y_max = 1;
}

/*
 * putboard() -- save a board, given a selector of which board to save
 * Return 0 on success, non-zero on failure.
 */

int
putboard(int which)
{
	int		index, xedni;
	
	for (index=y_min; index<=y_max; index++)
	{
		for (xedni=x_min; xedni<=x_max; xedni++)
			if (putcell(world[which][xedni][index]))
				return(1);
		if (putstring("\n"))
			return(1);
	}
	
	return(0);
}

/*
 * getboard() -- get a board, given its size and a selector of which board to load it
 * into.  Return 0 on success, non-zero on failure.
 */

int
getboard(int which, int x_size, int y_size)
{
	int index, xedni, curr;
	
	x_min = XMAX/2 - x_size/2;
	y_min = YMAX/2 - y_size/2;
	x_max = x_min + x_size - 1;
	y_max = y_min + y_size - 1;
	
	if (x_min < 0 || y_min < 0 || x_max > XMAX || y_max > YMAX) /* Overly thorough */
	{
		message("Board is too large (Board is %d x %d, I can handle %d x %d)",
					x_size, y_size, XMAX, YMAX);
		return(1);
	}
	
	for (index=y_min; index<=y_max; index++)
	{
		for (xedni=x_min; xedni<=x_max; xedni++)
		{	
			curr = getcell();
			if (curr < 0)
				return(1);
			world[which][xedni][index] = curr;
		}
		if (checkstring("\n"))
			return(1);
	}
	
	return(0);
}