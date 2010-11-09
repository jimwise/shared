/*
 * file.c -- stdio-based load and save routines for life
 *
 * XXX this file has more indirection than it needs, as it contains the
 * XXX remnants of an attempt at a common api between macos 7 and stdio file
 * XXX routines
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
#include "life.h"

static int	checkstring (char *, ...);
static void	findbounds (void);
static int	getboard (int, int);
static int	getcell (void);
static int	getsize (int *, int *);
static int	putboard (void);
static int	putcell (int);
static int	putsize (int, int);
static int	putstring (char *, ...);

/* size of buffer for checkstring() */
#define	CHECK_LEN	1024

static FILE	*boardfile;
static int x_min, x_max, y_min, y_max;

/*
 * save() -- save board in standard ASCII format
 * returns 0 on successful save, non-zero otherwise.
 */

int
save(char *name) {
  findbounds();
	
  if ((boardfile = fopen(name, "w")) == NULL) {
    prompt("Could not open file %s", name);
    return(1);
  }

  if ( putstring(FILE_HEADERSTRING) ||
       putstring(FILE_SIZESTRING) ||
       putsize(x_max - x_min + 1, y_max - y_min + 1) ||
       putstring(FILE_SEPSTRING) ) {
    prompt("Could not write header to file %s", name);
    fclose(boardfile);
    return(1);
  }
	
  if ( putboard() || putstring(FILE_SEPSTRING)) {
    prompt("Could not write board to file %s", name);
    fclose(boardfile);
    return(1);
  }
	
  if (fclose(boardfile)) {
    prompt("Failed to write file %s", name);
    return(1);
  }
	
  return(0);
}

/*
 * load() -- load board in standard ASCII format
 * Return 0 after a successful load, non-zero otherwise...
 */

int
load (char *name) {
  int x_size, y_size;

  if ((boardfile = fopen(name, "r")) == NULL) {
    prompt("Could not open file %s", name);
    return(1);
  }
		
  if ( checkstring(FILE_HEADERSTRING) ||
       checkstring(FILE_SIZESTRING) ||
       getsize(&x_size, &y_size) ||
       checkstring(FILE_SEPSTRING) ) {
    prompt("Bad header information in file %s", name);
    fclose(boardfile);
    return(1);
  }
	
  if (getboard(x_size, y_size)) {
    prompt("Invalid board in file %s", name);
    fclose(boardfile);
    return(1);
  }
	
  if (checkstring(FILE_SEPSTRING)) {
    prompt("Incomplete file %s", name);
    fclose(boardfile);
    return(1);
  }
	
  fclose(boardfile);
		
  return(0);
}

/*
 * findbounds() -- determine the minimum and maximum x and y bounds of a life
 * board.  This allows any run of life to open boards it can manage, even if
 * saved with a larger version.
 * If there are no live cells, we treat the board as an empty board of size 1x1.
 */

static void
findbounds (void) {
  int		index, xedni;

  x_min = max_row+1; x_max = -1; y_min = max_col+1; y_max = -1;

  /* this is ugly but simple... */
  for (index=0; index<rows; index++)
    for (xedni=0; xedni<cols; xedni++)
      if (get_cell(index, xedni)) {
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
 * putboard() -- save a board
 * Return 0 on success, non-zero on failure.
 */

static int
putboard (void) {
  int		index, xedni;
  for (index=y_min; index<=y_max; index++) {
    for (xedni=x_min; xedni<=x_max; xedni++)
      if (putcell(get_cell(index,xedni)))
	return(1);
    if (putstring("\n"))
      return(1);
  }
	
  return(0);
}

/*
 * getboard() -- get a board, given its max size
 * Return 0 on success, non-zero on failure.
 */

static int
getboard(int rows_needed, int cols_needed) {
  int start_row, start_col, stop_row, stop_col;
  int index, xedni, curr;

  if (rows_needed > rows || cols_needed > cols) {
    prompt("Board is too large (Board is %d x %d, I can handle %d x %d)",
	    rows_needed, cols_needed, rows, cols);
    return(1);
  }
	
  start_col = (cols - cols_needed)/2;
  start_row = (rows - rows_needed)/2;
  stop_col = start_col + cols_needed;
  stop_row = start_row + rows_needed;

  prompt("starting: loading %d x %d at (%d,%d) (out of %d x %d)",
	 rows_needed, cols_needed, start_row, start_col, rows, cols);
  for (index=start_row; index<stop_row; index++) {
    for (xedni=start_col; xedni<stop_col; xedni++) {	
      if ((curr = getcell()) == -1)
	return(1);
      /* prompt("set_cell(%d, %d, %d)", index, xedni, curr); */
      set_cell(index, xedni, curr);
    }
    if (checkstring("\n"))
      return(1);
  }
  display();

  return(0);
}

/*
 * putstring() -- output a printf string to the current board file
 * returns 0 on success, non-zero on failure.
 */
 
static int
putstring (char *format, ...) {
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
 
static int
checkstring (char *format, ...) {
  va_list args;
  char	checkstring[CHECK_LEN + 1];
	
  /* This isn't adequate, but is better than nothing */
  if (strlen(format) > CHECK_LEN)
    {
      prompt("Cannot read, data chunk too large");
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

static int
putsize (int x_size, int y_size) {
  return(putstring(FILE_SIZEFMT, x_size, y_size));
}

/*
 * getsize() -- read size from current file, given pointers to two ints to put it in
 * returns 0 on success, non-zero on failure.
 */
 
static int
getsize (int *x_size, int *y_size) {
  if (fscanf(boardfile, FILE_SIZEFMT, x_size, y_size) != 2)
    return(1);
  else
    return(0);
}

/*
 * putcell() -- output a given cell value to the current file
 * returns 0 on success, non-zero on failure.
 */

static int
putcell (int value) {
  int 	outc = value ? '*' : ' ';
	
  return (fputc(outc, boardfile) == EOF);
}

/*
 * getcell() -- get the value of the next cell in the current file
 * returns value of cell on success, <0 on failure.
 */

static int
getcell (void) {
  int	c;
	
  c = getc(boardfile);
  /* prompt("|%c|", c); */
	
  switch(c) {
  case ' ':
    return(0);
    break;
  case '*':
  case 'X':
    return(1);
    break;
  default:
    return(-1);
    break;
  }
}
