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
				clear_board(which);
				if (!getname(fname))
				{
					if (load(which, fname))
					{
						message("Could not load board from file %s", fname);
						clear_board(which);
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
