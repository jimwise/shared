/*
 * thinkinit.c -- mac-specific (console) initialization code for Conway's Life
 *
 * please see the file life.c for more information
 *
 * life is copyright © 1995, Jim Wise
 * You may redistribute this code freely.
 * You may modify and redistribute this code freely as long as you retain
 * this paragraph and an indication that the code has been modified.
 * life comes with absolutely NO WARRANTY.
 */

#include	<console.h>
#include	"life.h"

extern int		count, trace;

void
init (int *Argc, char ***Argv)
{	
	console_options.top = YLOC;
	console_options.left = XLOC;
	console_options.nrows = YMAX+1; /* extra for status bar */
	console_options.ncols = XMAX;
	console_options.title = "\pConway's Life";
	console_options.txSize = FONTSIZE;
	console_options.pause_atexit = 0;
	console_options.procID = 5;
	
	freopenc(NULL, stdout);
	freopenc(stdout, stdin);
	csetmode(C_RAW, stdin);
	
	/* Published interface?  We don't need no steenkin' published interface */
	/* WHACK!  Ok...  500 times... */
	/*		"I will not use unpublished internal variables." */
	/*		"I will not use unpublished internal variables." */
	/*		... 	*/
	_ftype = 'TEXT';
	
	message("Welcome to Life, Version %s, Copyright 1995, Jim Wise", VERSION_STR);
}
