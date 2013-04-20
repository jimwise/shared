#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static double last_bet = 5.00L;
static char buf[255];

double
getbet(double min, double limit) {
  /* XXX check actual min/table_limit rules */
  double bet = 0.0L;
  char *s = buf;

  printf("Please enter a bet (min = $%0.2f, limit = $%0.2f) [$%0.2f]: ", min, limit, last_bet);

  while (1) {
    s = buf;
    fgets(buf, sizeof(buf), stdin);
    if (buf[0] == '\n')
      return last_bet;

    if (s[0] == '$')
      s++;

    errno = 0;
    bet = atof(s);

    if (errno) {
      printf("Bet must be a number of dollars, try again: ");
      continue;
    }

    if (bet < min) {
      printf("Bet must be at least $%0.2f, try again: ", min);
      continue;
    }

    if (fmod(bet, min) != 0.0L) {
      printf("Bet must be in an increment of $%0.2f, try again: ", min);
      continue;
    }

    if (bet > limit) {
      printf("Bet must be less than or equal to $%0.2f, try again: ", limit);
      continue;
    }

    /* if we get here, bet is good */
    last_bet = bet;
    return bet;
  }
}

char
getresp(char *ps1, char *ps2, char *allowed, char def) {
  char c;
  fputs(ps1, stdout);
  while (1) {
    fgets(buf, sizeof(buf), stdin);
    if ((buf[0] == '\n') && def)
      return def;

    c = tolower(buf[0]);
    if (strchr(allowed, c))
	return c;

    fputs(ps2, stdout);
  }
}
