/*
 * thinkfile.c -- mac-specific (console) file routines for Conway's Life
 *
 * note that this simply pulls in ansifile.c for the standard file routines,
 * then defines a few of its own for internal use (from thinkedit.c)
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
#include	<string.h>
#include	<console.h>
#include	"life.h"
#include	"thinklife.h"

/*
 * filemenu() -- let the user load or save a file
 */
 
void
filemenu (int which)
{
	int		flag = 1;
	char	c, fname[NAMELEN];

	cgotoxy(1, YMAX+1, stdout);
	printf(MENUSTR);
	ccleol(stdout);
	
	while (flag)
	{
		c = getc(stdin);
		
		switch (c)
		{
			case 'l':
				clear(which);
				if (!getname(fname))
				{
					if (load(which, fname))
					{
						message("Could not load board from file %s", fname);
						clear(which);
					}
					else
						message("Board loaded from file %s", fname);
				}
				display(which);
				cgotoxy(1, YMAX+1, stdout);
				printf(MENUSTR);
				ccleol(stdout);
				break;
			case 's':
				if (!getname(fname))
				{
					if (save(which, fname))
						message("Could not save board to file %s", fname);
					else
						message("Board saved to file %s", fname);
				}
				cgotoxy(1, YMAX+1, stdout);
				printf(MENUSTR);
				ccleol(stdout);
				break;
			case 'r':
				flag = 0;
				break;
			default:
				break;
		}
	}
}

/*
 * getname() -- get file name from user and store in given char *
 * returns 0 on success, non-zero if user entered zero-length string
 */

int
getname(char *name)
{
	int		counter = 0;
	char	c;
	
	cgotoxy(1, YMAX+1, stdout);
	printf("Enter FileName: ");
	ccleol(stdout);
	
	while ( 1 )
	{
		c = getc(stdin);
		if (c == EOF)
			continue;
		if (c == '\r')
			break;
		if (c == '\b')
		{
			putc('\b', stdout);
			putc(' ', stdout);
			putc('\b', stdout);
			counter--;
			continue;
		}
		
		putc(c, stdout);
		name[counter] = c;
		
		counter++;
		if (counter == NAMELEN)
		{
			message("Name is too long");
			counter = 0;
			cgotoxy(1, YMAX+1, stdout);
			printf("Enter FileName: ");
			ccleol(stdout);
			continue;
		}
	}
	
	name[counter] = '\0';
	DEBUG("Got Name");
	return(!strlen(name));
}