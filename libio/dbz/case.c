/*
 * case-mapping stuff
 *
 * We exploit the fact that we are dealing only with headers here, and
 * headers are limited to the ASCII characters by RFC822.  It is barely
 * possible that we might be dealing with a translation into another
 * character set, but in particular it's very unlikely for a header
 * character to be outside -128..255.
 *
 * Life would be a whole lot simpler if tolower() could safely and portably
 * be applied to any char.
 */
#include <stdio.h>
#include "string.h"
#include "case.h"

/* note that case.h knows the value of OFFSET */
#define	OFFSET	128		/* avoid trouble with negative chars */
#define	MAPSIZE	(256+OFFSET)
char casemap[MAPSIZE];		/* relies on init to '\0' */
static int primed = 0;		/* has casemap been set up? */

/*
 - prime - set up case-mapping stuff
 */
static void
prime()
{
	register char *lp;
	register char *up;
	register int c;
	register int i;
	static char lower[] = "abcdefghijklmnopqrstuvwxyz";
	static char upper[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

	for (lp = lower, up = upper; *lp != '\0'; lp++, up++) {
		c = *lp;
		casemap[c+OFFSET] = c;
		casemap[*up+OFFSET] = c;
	}
	for (i = 0; i < MAPSIZE; i++)
		if (casemap[i] == '\0')
			casemap[i] = (char)(i-OFFSET);
	primed = 1;
}

/*
 - cistrncmp - case-independent strncmp
 */
int				/* < == > 0 */
cistrncmp(s1, s2, len)
char *s1;
char *s2;
int len;
{
	register char *p1;
	register char *p2;
	register int n;

	if (!primed)
		prime();

	p1 = s1;
	p2 = s2;
	n = len;
	while (--n >= 0 && *p1 != '\0' && TOLOW(*p1) == TOLOW(*p2)) {
		p1++;
		p2++;
	}
	if (n < 0)
		return(0);

	/*
	 * The following case analysis is necessary so that characters
	 * which look negative collate low against normal characters but
	 * high against the end-of-string NUL.
	 */
	if (*p1 == '\0' && *p2 == '\0')
		return(0);
	else if (*p1 == '\0')
		return(-1);
	else if (*p2 == '\0')
		return(1);
	else
		return(TOLOW(*p1) - TOLOW(*p2));
}

/*
 - rfc822ize - do the bizarre case conversion needed for rfc822 message-ids
 *
 * Actually, this is not quite complete.  Absolute, total, full RFC822
 * compliance requires a horrible parsing job, because of the arcane
 * quoting conventions -- abc"def"ghi is not equivalent to abc"DEF"ghi,
 * for example.  There are three or four things that might occur in the
 * domain part of a message-id that are case-sensitive.  They don't seem
 * to ever occur in real news, thank Cthulhu.  (What?  You were expecting
 * a merciful and forgiving deity to be invoked in connection with RFC822?
 * Forget it; none of them would come near it.)
 */
char *				/* returns the argument */
rfc822ize(s)
char *s;
{
	register char *p;
	static char post[] = "postmaster";
	static int postlen = sizeof(post)-1;

	if (!primed)
		prime();

	p = strrchr(s, '@');
	if (p == NULL)			/* no local/domain split */
		p = "";			/* assume all local */
	else if	(p - (s+1) == postlen && CISTREQN(s+1, post, postlen)) {
		/* crazy special case -- "postmaster" is case-insensitive */
		p = s;
	}
#ifdef NONSTANDARD
#ifdef RFCVIOLATION
#ifdef B_2_11_MISTAKE
	p = s;				/* all case-insensitive */
#endif
#endif
#endif
	for (; *p != '\0'; p++)
		*p = TOLOW(*p);

	return(s);
}
