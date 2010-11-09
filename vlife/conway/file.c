/*
 * file.c -- stdio-based load and save routines for life
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

static void	findbounds (void);
static int	getboard (int rows_needed, int cols_needed);
static int	getcell (void);

static FILE	*boardfile;
static int	x_min, x_max, y_min, y_max;

/*
 * save() -- save board in standard ASCII format
 * returns 0 on successful save, non-zero otherwise.
 */

int
save(char *name) {
  int	index, xedni;

  findbounds();
	
  if ((boardfile = fopen(name, "w")) == NULL) {
    prompt("Could not open file %s", name);
    return(1);
  }
  clearerr(boardfile);

  fputs(FILE_HEADERSTRING, boardfile);
  fputs(FILE_SIZESTRING, boardfile);

  fprintf(boardfile, FILE_SIZEFMT, x_max - x_min + 1, y_max - y_min + 1);

  fputs(FILE_SEPSTRING, boardfile);

  for (index=y_min; index<=y_max; index++) {
    for (xedni=x_min; xedni<=x_max; xedni++)
      fputc(CHAR(get_cell(index, xedni)), boardfile);
    putc('\n', boardfile);
  }

  fputs(FILE_SEPSTRING, boardfile);
  fclose(boardfile);

  if (ferror(boardfile)) {
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
  int r, c;

  if ((boardfile = fopen(name, "r")) == NULL) {
    prompt("Could not open file %s", name);
    return(1);
  }
		
  if ((fscanf(boardfile, FILE_HEADERSTRING) == EOF) ||
      (fscanf(boardfile, FILE_SIZESTRING) == EOF) ||
      (fscanf(boardfile, FILE_SIZEFMT, &r, &c) != 2) ||
      (fscanf(boardfile, FILE_SEPSTRING) == EOF)) {
    prompt("Bad header information in file %s", name);
    fclose(boardfile);
    return(1);
  }
	
  if (getboard(r, c)) {
    prompt("Invalid board in file %s", name);
    fclose(boardfile);
    return(1);
  }
	
  if (fscanf(boardfile, FILE_SEPSTRING) == EOF) {
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
      prompt("set_cell(%d, %d, %d)", index, xedni, curr);
      set_cell(index, xedni, curr);
    }
    if (getc(boardfile) != '\n')
      return(1);
  }
  display();

  return(0);
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
