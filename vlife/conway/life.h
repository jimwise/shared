/*
 * life.h -- common include file for conway's life
 *
 * life is copyright (c) 1995-2010, Jim Wise
 *
 * You may redistribute this code freely.  You may modify and redistribute
 * this code freely as long as you retain this paragraph and an indication
 * that the code has been modified.  life comes with absolutely NO WARRANTY.
 */

#ifndef LIFE_H
#define LIFE_H

#define	VERSION_STR	"1.3"

/* (abstract) file open modes */
#define WRITEFILE	0
#define	READFILE	1

/* shared routines */

void	begin_display (int *r, int *c);
void	clear_board (void);
int	determine (int row, int col);
void	display (void);
int	edit (void);
void	end_display (void);
void	generation (void);
int	get_cell (int row, int col);
char	*prompt_string(char *prompt);
int	key_pressed (void);
int	load (char *fname);
void	make_board(int nr, int nc);
void	message(char *fmt, ...);
void	prompt(char *fmt, ...);
void	run (void);
int	save (char *fname);
int	set_cell (int row, int col, char val);

#define	OTHER(a)	((a) ? 0 : 1)
#define MIN(x,y)	(((x)<=(y)) ? (x) : (y))
#define MAX(x,y)	(((x)>=(y)) ? (x) : (y))

extern int rows, cols, max_row, max_col, msg_row;

#define	NAMELEN		512
#define	MENUSTR		"[L]oad; [S]ave; [R]eturn"
#define	EDIT_INSTSTR	"Move [keypad/hjkl]; Toggle [5/space]; [F]ile; [C]lear; [G]o; [Q]uit"

#define	CHAR(x)	((x) ? '*' : ' ')

#endif
