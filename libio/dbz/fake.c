/*
 * fake - make up random lines resembling history-file entries, reproducibly
 *
 * -Log-
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

#define	MAXSTR	500		/* For sizing strings -- DON'T use BUFSIZ! */
#define	STREQ(a, b)	(*(a) == *(b) && strcmp((a), (b)) == 0)

#ifndef lint
static char RCSid[] = "$Header: /rel/cvsfiles/devo/libio/dbz/fake.c,v 1.2 1993/10/25 20:02:42 bothner Exp $";
#endif

int midonly = 0;		/* just message ids, rest not realistic */
int tag = 0;			/* tag lines with random digit for later use */
int expired = -1;		/* percentage of lines to be expired */

int debug = 0;
char *progname;

char *inname;				/* filename for messages etc. */
long lineno;				/* line number for messages etc. */

void doline();
void addchars();
void seed();

/*
 - main - parse arguments and handle options
 */
int
main(argc, argv)
int argc;
char *argv[];
{
	int c;
	int errflg = 0;
	FILE *in;
	struct stat statbuf;
	extern int optind;
	extern char *optarg;
	void process();
	register long no;
	extern long atol();
	char line[MAXSTR];

	progname = argv[0];

	while ((c = getopt(argc, argv, "ms:te:d")) != EOF)
		switch (c) {
		case 'm':	/* message-ids only */
			midonly = 1;
			break;
		case 's':	/* seed */
			seed(atol(optarg));
			break;
		case 't':	/* tag lines with a random digit */
			tag = 1;
			break;
		case 'e':	/* percentage to be expired */
			expired = atoi(optarg);
			break;
		case 'd':	/* Debugging. */
			debug++;
			break;
		case '?':
		default:
			errflg++;
			break;
		}
	if (errflg || optind != argc - 1) {
		fprintf(stderr, "usage: %s ", progname);
		fprintf(stderr, "[-m] [-s seed] length\n");
		exit(2);
	}

	for (no = atol(argv[optind]); no > 0; no--) {
		doline(line);
		puts(line);
	}
#ifdef DBZ_FINISH
	DBZ_FINISH;
#endif
	exit(0);
}

/*
 - doline - generate random history pseudo-line
 */
void
doline(buf)
char *buf;
{
	char tagch[2];

	(void) strcpy(buf, "<");
	addchars(buf, range(4, 20));
	(void) strcat(buf, "@");
	addchars(buf, range(8, 20));
	if (midonly)
		(void) strcat(buf, ">\tx");
	else {
		if (tag) {
			tagch[0] = "1234567890"[range(0,9)];
			tagch[1] = '\0';
			(void) strcat(buf, ">\t");
			(void) strcat(buf, tagch);
			(void) strcat(buf, "00000000~-");
		} else
			(void) strcat(buf, ">\t1234567890~-");
	}
	if (range(1, 100) > expired) {
		if (midonly)
			(void) strcat(buf, "\tx");
		else {
			(void) strcat(buf, "\t");
			addchars(buf, range(10, 30));
		}
	}
}

/*
 - addchars - generate n random characters suitable for history file
 */
void
addchars(buf, len)
char *buf;
int len;
{
	register int i;
	register char *p = buf + strlen(buf);
	static char vocab[] = "1234567890.abcde.fghij.klmno.pqrst.uvwxyz.\
1234567890.ABCDE.FGHIJ.KLMNO.PQRST.UVWXYZ.1234567890.\
1234567890.abcde.fghij.klmno.pqrst.uvwxyz.1234567890";

	for (i = len; i > 0; i--)
		*p++ = vocab[range(0, sizeof(vocab)-2)];
	*p++ = '\0';
}
