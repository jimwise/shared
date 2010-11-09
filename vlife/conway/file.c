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

/* size of buffer for checkstring() */
#define	CHECK_LEN	1024

static FILE	*boardfile;
static int x_min, x_max, y_min, y_max;

/*
 * save() -- save board in standard ASCII format
 * returns 0 on successful save, non-zero otherwise.
 */

int
save(int which, char *name) {
  findbounds(which);
	
  if (openfile(name, WRITEFILE)) {
    prompt("Could not open file %s", name);
    return(1);
  }

  if ( putstring(FILE_HEADERSTRING) ||
       putstring(FILE_SIZESTRING) ||
       putsize(x_max - x_min + 1, y_max - y_min + 1) ||
       putstring(FILE_SEPSTRING) ) {
    prompt("Could not write header to file %s", name);
    closefile();
    return(1);
  }
	
  if ( putboard(which) || putstring(FILE_SEPSTRING)) {
    prompt("Could not write board to file %s", name);
    closefile();
    return(1);
  }
	
  if (closefile()) {
    prompt("Failed to close file %s", name);
    return(1);
  }
	
  return(0);
}

/*
 * load() -- load board in standard ASCII format
 * Return 0 after a successful load, non-zero otherwise...
 */

int
load (int which, char *name) {
  int x_size, y_size;
	
  if (openfile(name, READFILE)) {
    prompt("Could not open file %s", name);
    return(1);
  }
		
  if ( checkstring(FILE_HEADERSTRING) ||
       checkstring(FILE_SIZESTRING) ||
       getsize(&x_size, &y_size) ||
       checkstring(FILE_SEPSTRING) ) {
    prompt("Bad header information in file %s", name);
    closefile();
    return(1);
  }
	
  if (getboard(which, x_size, y_size)) {
    prompt("Invalid board in file %s", name);
    closefile();
    return(1);
  }
	
  if (checkstring(FILE_SEPSTRING)) {
    prompt("Incomplete file %s", name);
    closefile();
    return(1);
  }
	
  if (closefile()) {
    prompt("Failed to close file %s", name);
    return(1);
  }
		
  return(0);
}

/*
 * findbounds() -- determine the minimum and maximum x and y bounds of a life
 * board.  This allows any run of life to open boards it can manage, even if
 * saved with a larger version.
 * If there are no live cells, we treat the board as an empty board of size 1x1.
 */

void
findbounds (int which) {
  int		index, xedni;

  x_min = XMAX, x_max = 0, y_min = YMAX, y_max = 0;

  /* this is ugly but simple... */
  for (index=1; index<=YMAX; index++)
    for (xedni=1; xedni<=XMAX; xedni++)
      if (CELL(index, xedni)) {
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
putboard(int which) {
  int		index, xedni;
  /* XXX XXX cheats, depending on which parameter overriding global due to macro */
  for (index=y_min; index<=y_max; index++) {
    for (xedni=x_min; xedni<=x_max; xedni++)
      if (putcell(CELL(index,xedni)))
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
getboard(int which, int x_size, int y_size) {
  int index, xedni, curr;
	
  x_min = XMAX/2 - x_size/2;
  y_min = YMAX/2 - y_size/2;
  x_max = x_min + x_size - 1;
  y_max = y_min + y_size - 1;
	
  if (x_min < 0 || y_min < 0 || x_max > XMAX || y_max > YMAX) { /* Overly thorough */
    prompt("Board is too large (Board is %d x %d, I can handle %d x %d)",
	    x_size, y_size, XMAX, YMAX);
    return(1);
  }
	
  for (index=y_min; index<=y_max; index++) {
    for (xedni=x_min; xedni<=x_max; xedni++) {	
      curr = getcell();
      if (curr < 0)
	return(1);
      /* XXX XXX cheats, depending on which parameter overriding global due to macro */
      CELL(index,xedni) = curr;
    }
    if (checkstring("\n"))
      return(1);
  }
	
  return(0);
}

/*
 * openfile() -- open a file as the current board file.
 * Takes a file name and a selector of whether the file is to be read or written.
 * returns 0 on success, non-zero on failure.
 */
 
int
openfile (char *name, int mode) {
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
closefile (void) {
  return(fclose(boardfile));
}	

/*
 * putstring() -- output a printf string to the current board file
 * returns 0 on success, non-zero on failure.
 */
 
int
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
 
int
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

int
putsize (int x_size, int y_size) {
  return(putstring(FILE_SIZEFMT, x_size, y_size));
}

/*
 * getsize() -- read size from current file, given pointers to two ints to put it in
 * returns 0 on success, non-zero on failure.
 */
 
int
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

int
putcell (int value) {
  int 	outc = value ? 'X' : ' ';
	
  return (fputc(outc, boardfile) == EOF);
}

/*
 * getcell() -- get the value of the next cell in the current file
 * returns value of cell on success, <0 on failure.
 */

int
getcell (void) {
  char	c;
	
  c = getc(boardfile);
	
  switch(c) {
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
