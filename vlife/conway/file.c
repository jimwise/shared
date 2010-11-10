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
static int	getboard (FILE *f, int rows_needed, int cols_needed);

static int	x_min, x_max, y_min, y_max;

#define MAGIC	"LIFE FILE FORMAT 1.1\n"
#define SIZE	"rows %d cols %d\n"
/*
 * save() -- save board in standard ASCII format
 * returns 0 on successful save, non-zero otherwise.
 */

int
save(char *name) {
  int	index, xedni;
  FILE *f;

  findbounds();
	
  if ((f = fopen(name, "w")) == NULL) {
    prompt("Could not open file %s", name);
    return(1);
  }
  clearerr(f);

  fputs(MAGIC, f);
  fprintf(f, SIZE, x_max - x_min + 1, y_max - y_min + 1);

  for (index=y_min; index<=y_max; index++) {
    for (xedni=x_min; xedni<=x_max; xedni++)
      fputc(CHAR(get_cell(index, xedni)), f);
    putc('\n', f);
  }

  fclose(f);

  if (ferror(f)) {
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
  FILE *f;
  char line[1024];

  if ((f = fopen(name, "r")) == NULL) {
    prompt("Could not open file %s", name);
    return(1);
  }

  line[0] = '\0';

  fgets(line, sizeof(line), f);
  if (strcmp(line, MAGIC)) {
    prompt("Bad header information in file %s", name);
    fclose(f);
    return(1);
  }

  fgets(line, sizeof(line), f);
  if (sscanf(line, SIZE, &r, &c) != 2) {
    prompt("Bad size information in file %s", name);
    fclose(f);
    return(1);
  }

  if (getboard(f, r, c)) {
    prompt("Invalid board in file %s", name);
    fclose(f);
    return(1);
  }
	
  fclose(f);
		
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
getboard(FILE *f, int rows_needed, int cols_needed) {
  int start_row, start_col, stop_row, stop_col;
  int index, xedni, c;
  unsigned char curr;

  if (rows_needed > rows || cols_needed > cols) {
    prompt("Board is too large (Board is %d x %d, I can handle %d x %d)",
	    rows_needed, cols_needed, rows, cols);
    return(1);
  }
	
  start_col = (cols - cols_needed)/2;
  start_row = (rows - rows_needed)/2;
  stop_col = start_col + cols_needed;
  stop_row = start_row + rows_needed;

  /* prompt("starting: loading %d x %d at (%d,%d) (out of %d x %d)", */
  /* 	 rows_needed, cols_needed, start_row, start_col, rows, cols); */

  for (index=start_row; index<stop_row; index++) {
    for (xedni=start_col; xedni<stop_col; xedni++) {	
      switch(c = getc(f)) {
      case ' ':
      case '.':
	curr = 0;
      break;
      case '*':
      case 'X':
	curr = 1;
      break;
      default:
	prompt("Board description contains illegal character '%c'", c); 
	return(1);
      }

      set_cell(index, xedni, curr);
    }
    if (getc(f) != '\n')
      return(1);
  }
  display();

  return(0);
}
