extern int cistrncmp();
extern char *rfc822ize();

extern char casemap[];

/* must call cistrncmp before invoking TOLOW... */
#define	TOLOW(c)	(casemap[(c)+128])	/* see case.c for why 128 */

/* ...but the use of it in CISTREQN is safe without the preliminary call (!) */
/* CISTREQN is an optimised case-insensitive strncmp(a,b,n)==0; n > 0 */
#define CISTREQN(a, b, n) \
	(TOLOW((a)[0]) == TOLOW((b)[0]) && cistrncmp(a, b, n) == 0)
