/*
 * thinkdisplay.c -- mac-specific (console) display code for Conway's Life
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
#include	<console.h>
#include	"life.h"
#include	"thinklife.h"

/*
 * display() -- given a board selector, show that board to the user.
 * this version, for the Think C console environment, simply ascii-arts
 * it out.
 */
 
void
display (int which)
{
	int		index, xedni;
	
	cgotoxy(1, 1, stdout);
	
	for (index=1; index<=YMAX; index++)
		for (xedni=1; xedni<=XMAX; xedni++)
			putc(CHAR(world[which][xedni][index]), stdout);
}

/*
 * message() -- given a printf string, output the string
 */

void
message(char *format, ...)
{
	va_list		args;
	char		*newfmt;
	
	newfmt = malloc(strlen(format) + 19);
	sprintf(newfmt, "%s; <<Press any Key>>", format);
	free(newfmt);
	
	va_start(args, format);
	
	cgotoxy(1, YMAX+1, stdout);
	vprintf(newfmt, args);
	ccleol(stdout);
	
	va_end(args);
	
	while(getc(stdin) == EOF)
		;
}