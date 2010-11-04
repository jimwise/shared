/*
 * thinkcallback.c -- mac-specific (console) callback code for Conway's Life
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
#include	<stdio.h>
#include	"life.h"
#include	"thinklife.h"

/*
 * callback() -- called every turn
 * Returns 1 to keep running, 0 to stop
 */
 
int
callback (int turn, int current)
{
	char c;
	
	/* Don't use message() to avoid pause */
	cgotoxy(1, YMAX+1, stdout);
	printf("Turn : %6d ; <Press any key to interrupt>", turn);
	ccleol(stdout);
	
	c = getc(stdin);
	if (c == EOF)
		return(1);
	else
		return(0);
}