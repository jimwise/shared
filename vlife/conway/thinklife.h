/*
 * thinklife.h -- mac specific (console) header for Conway's Life
 *
 * please see the file life.c for more information
 *
 * life is copyright © 1995, Jim Wise
 * You may redistribute this code freely.
 * You may modify and redistribute this code freely as long as you retain
 * this paragraph and an indication that the code has been modified.
 * life comes with absolutely NO WARRANTY.
 */
 
#define	FONTSIZE	9

#define	XLOC		10
#define	YLOC		50

#ifdef NO_FILE
#define	EDIT_INSTSTR	"Move [keypad/hjkl]; Toggle [5/space]; [C]lear; [G]o; [Q]uit"
#else
#define	NAMELEN			512
#define	MENUSTR			"[L]oad; [S]ave; [R]eturn"
#define	EDIT_INSTSTR	"Move [keypad/hjkl]; Toggle [5/space]; [F]ile; [C]lear; [G]o; [Q]uit"
#endif

#define	CHAR(x)		((x) ? '¥' : ' ')

#ifndef NO_FILE
void	filemenu (int which);
int		getname (char *);
#endif