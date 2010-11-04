/*
 * ansifile.c -- ANSI standard libc Load and Save routines for Conways Life
 *
 * please see the file life.c for more information
 *
 * life is copyright © 1995, Jim Wise
 * You may redistribute this code freely.
 * You may modify and redistribute this code freely as long as you retain
 * this paragraph and an indication that the code has been modified.
 * life comes with absolutely NO WARRANTY.
 */

#include	<stdlib.h>
#include	<stdio.h>
#include	<stdarg.h>
#include	<string.h>
#include	"life.h"

/* size of buffer for checkstring() */
#define		CHECK_LEN	1024

static FILE		*boardfile;

/*
 * openfile() -- open a file as the current board file.
 * Takes a file name and a selector of whether the file is to be read or written.
 * returns 0 on success, non-zero on failure.
 */
 
int
openfile (char *name, int mode)
{
	if (mode)
		boardfile = fopen(name, "rb");
	else
		boardfile = fopen(name, "wb");
		
	return(boardfile == NULL);
}

/*
 * closefile() -- close the current	board file
 * returns 0 on success, non-zero on failure.
 */

int
closefile (void)
{
	return(fclose(boardfile));
}	

/*
 * putstring() -- output a printf string to the current board file
 * returns 0 on success, non-zero on failure.
 */
 
int
putstring (char *format, ...)
{
	va_list args;
	int		retval;
	
	va_start(args, format);
	
	retval = !(vfprintf(boardfile, format, args));
	
	va_end(args);
	
	return(retval);
}

/*
 * checkstring() -- check to see if a given printf string occurs at the current
 * point in the current file.
 * returns 0 on success, non-zero on failure.
 */
 
int
checkstring (char *format, ...)
{
	va_list args;
	char	checkstring[CHECK_LEN + 1];
	
	/* This isn't adequate, but is better than nothing */
	if (strlen(format) > CHECK_LEN)
	{
		message("Cannot read, data chunk too large");
		return(1);
	}
	
	va_start(args, format);
	vsprintf(checkstring, format, args);
	va_end(args);	
	
	if (fscanf(boardfile, checkstring) == EOF)
		return(1);
	else
		return(0);
}
 
/*
 * putsize() -- save size to current file, given size in two ints
 * this could be killed, but a good compiler will anyways...
 * returns 0 on success, non-zero on failure.
 */

int
putsize (int x_size, int y_size)
{
	return(putstring(FILE_SIZEFMT, x_size, y_size));
}

/*
 * getsize() -- read size from current file, given pointers to two ints to put it in
 * returns 0 on success, non-zero on failure.
 */
 
int
getsize (int *x_size, int *y_size)
{
	if (fscanf(boardfile, FILE_SIZEFMT, x_size, y_size) != 2)
		return(1);
	else
		return(0);
}

/*
 * putcell() -- output a given cell value to the current file
 * returns 0 on success, non-zero on failure.
 */

int
putcell (int value)
{
	int 	outc = value ? 'X' : ' ';
	
	return (fputc(outc, boardfile) == EOF);
}

/*
 * getcell() -- get the value of the next cell in the current file
 * returns value of cell on success, <0 on failure.
 */

int
getcell (void)
{
	char	c;
	
	c = getc(boardfile);
	
	switch(c)
	{
		case 'X':
			return(1);
			break;
		case ' ':
			return(0);
			break;
		default:
			return(-1);
			break;
	}
}
