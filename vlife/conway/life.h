/*
 * life.h -- common include file for conway's life
 *
 * please see the file life.c for more information
 *
 * life is copyright © 1995, Jim Wise
 * You may redistribute this code freely.
 * You may modify and redistribute this code freely as long as you retain
 * this paragraph and an indication that the code has been modified.
 * life comes with absolutely NO WARRANTY.
 */

#ifndef LIFE_H
#define LIFE_H

#define	VERSION_STR		"1.3"

/* default settings for these two... should be overridden by specific builds */
#ifndef	XMAX
#define	XMAX	80
#endif
#ifndef	YMAX
#define	YMAX	23
#endif

/* board selectors */
#define	BOARD_A	0
#define	BOARD_B	1

/* (abstract) file open modes */
#define WRITEFILE	0
#define	READFILE	1

/* board file magic words */
#define FILE_HEADERSTRING	"Life, copyright 1995, Jim Wise\nFile Format Version 1.0\n\n"
#define FILE_SIZESTRING		"Board Size "
#define	FILE_SEPSTRING		"----------\n"
#define FILE_SIZEFMT		"%d x %d\n"

typedef unsigned char Board [XMAX+1][YMAX+1];

/* shared routines */

void	run (void);
void	generation (int, int);
int	determine (int, int, int);
#ifndef	NO_FILE
int	save (int, char *);
int	load (int, char *);
void	findbounds (int);
int	putboard (int);
int	getboard (int, int, int);
#endif

/* machine-dependent routines */

void	init (void);
void	clear_board (int);
int	callback (int, int);
void	display (int);
int	edit (int);
void	message(char *, ...);
#ifndef	NO_FILE
int	openfile (char *, int);
int	closefile (void);
int	putstring (char *, ...);
int	checkstring (char *, ...);
int	getsize (int *, int *);
int	putsize (int, int);
int	getcell (void);
int	putcell (int);
#endif

extern Board	world[3];

#define	OTHER(a)	((a) ? 0 : 1)
#define MIN(x,y)	(((x)<=(y)) ? (x) : (y))
#define MAX(x,y)	(((x)>=(y)) ? (x) : (y))

#ifdef NO_FILE
#define	EDIT_INSTSTR	"Move [keypad/hjkl]; Toggle [5/space]; [C]lear; [G]o; [Q]uit"
#else
#define	NAMELEN		512
#define	MENUSTR		"[L]oad; [S]ave; [R]eturn"
#define	EDIT_INSTSTR	"Move [keypad/hjkl]; Toggle [5/space]; [F]ile; [C]lear; [G]o; [Q]uit"
#endif

#define	CHAR(x)	((x) ? '¥' : ' ')

#ifndef NO_FILE
void	filemenu (int which);
int	getname (char *);
#endif

#endif
