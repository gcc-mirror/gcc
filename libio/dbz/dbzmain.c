/*
 * dbz - use and test dbz in various ways
 *
 * -Log-
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <stdlib.h>
#include <dbz.h>

#ifdef FUNNYSEEKS
#include <unistd.h>
#else
#define	SEEK_SET	0
#endif

#define	STREQ(a, b)	(*(a) == *(b) && strcmp((a), (b)) == 0)

char *progname;

char *inname = "(no file)";		/* filename for messages etc. */
long lineno;				/* line number for messages etc. */

char *my_basename;
char *pagname;
char *dir_name;
char *str2dup();
FILE *base;

int op = 'b';			/* what to do, default build a new table */
int baseinput = 1;		/* is the base file also the input? */

char *from = NULL;		/* old table to use for dbzagain() */
int omitzero = 0;		/* omit lines tagged with 0 */
long every = 0;			/* report every n lines */
int syncs = 0;			/* dbzsync() on each report */
int quick = 0;			/* quick checking, not too thorough */
int sweep = 0;			/* sweep file checking all offsets */
int useincore = 1;		/* should we use incore facility? */
long xxx = 0;			/* debugging variable */
int printx = 0;			/* print xxx after all is done */
int unique = 1;			/* before store(), check with fetch() */
int usefresh = 0;		/* use dbzfresh? */
long siz = 0;			/* -p size */
char map = 'C';			/* -p map */
long tag = 0;			/* -p tag mask */
int exact = 0;			/* do not run dbzsize(siz) */
int dbzint = 1;			/* use new interface? */
char fs = '\t';			/* field separator, default tab */
int unopen = 0;			/* make base unopenable during dbminit? */
char *change = NULL;		/* chdir here before dbmclose */

#define	DEFBUF	1024		/* default line-buffer size */
int buflen = DEFBUF;		/* line length limit */
char lbuf[DEFBUF];
char *line = lbuf;
char cbuf[DEFBUF];
char *cmp = cbuf;

void fail();
void dofile();
void runs();
void dosweep();
void mkfiles();
void crfile();
void doline();
void process();

#ifdef HAVERFCIZE
extern char *rfc822ize();
#else
#define	rfc822ize(n)	(n)
#endif

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
	extern int optind;
	extern char *optarg;
	int doruns = 0;
	extern long atol();

	progname = argv[0];

	while ((c = getopt(argc, argv, "axcmt:l:R0E:SqOiX:Yuf:p:eMUC:d")) != EOF)
		switch (c) {
		case 'a':	/* append to existing table */
			if (op != 'b')
				fail("only one of -a -x -c -m can be given", "");
			op = 'a';
			baseinput = 0;
			break;
		case 'x':	/* extract from existing table */
			if (op != 'b')
				fail("only one of -a -x -c -m can be given", "");
			op = 'x';
			baseinput = 0;
			break;
		case 'c':	/* check existing table */
			if (op != 'b')
				fail("only one of -a -x -c -m can be given", "");
			op = 'c';
			break;
		case 'm':	/* extract missing (complement of -x) */
			if (op != 'b')
				fail("only one of -a -x -c -m can be given", "");
			op = 'm';
			baseinput = 0;
			break;
		case 't':	/* set field separator */
			if (strlen(optarg) > 1)
				fail("only one field separator allowed", "");
			fs = *optarg;
			break;
		case 'l':	/* override line-length limit */
			buflen = atoi(optarg) + 1;
			if (buflen <= 2)
				fail("bad -l value `%s'", optarg);
			line = malloc(buflen);
			cmp = malloc(buflen);
			if (line == NULL || cmp == NULL)
				fail("cannot allocate %s-byte buffers", optarg);
			break;
		case 'R':	/* print run statistics */
			doruns = 1;
			break;
		case '0':	/* omit lines tagged (by fake -t) with 0 */
			omitzero = 1;
			break;
		case 'E':	/* report every n items */
			every = atol(optarg);
			break;
		case 'S':	/* dbzsync() on each -E report */
			syncs = 1;
			break;
		case 'q':	/* quick check or extract */
			quick = 1;
			break;
		case 'O':	/* sweep file checking all offsets */
			sweep = 1;
			break;
		case 'i':	/* don't use incore */
			useincore = 0;
			break;
		case 'X':	/* set xxx */
			xxx = atoi(optarg);
			break;
		case 'Y':	/* print xxx afterward */
			printx = 1;
			break;
		case 'u':	/* don't check uniqueness */
			unique = 0;
			break;
		case 'f':	/* init from existing table's parameters */
			from = optarg;
			break;
		case 'p':	/* parameters for dbzfresh */
			if (sscanf(optarg, "%ld %1s %lx", &siz, &map, &tag) != 3) {
				map = '?';
				tag = 0;
				if (sscanf(optarg, "%ld", &siz) != 1)
					fail("bad -n value `%s'", optarg);
			}
			usefresh = 1;
			break;
		case 'e':	/* -p size is exact, don't dbzsize() it */
			exact = 1;
			break;
		case 'M':	/* use old dbm interface + rfc822ize */
			dbzint = 0;
			break;
		case 'U':	/* make base unopenable during init */
			unopen = 1;
			break;
		case 'C':	/* change directories before dbmclose */
			change = optarg;
			break;
		case 'd':	/* Debugging. */
			if (dbzdebug(1) < 0)
				fail("dbz debugging not available", "");
			break;
		case '?':
		default:
			errflg++;
			break;
		}
	if (errflg || optind >= argc || (optind+1 < argc && baseinput)) {
		fprintf(stderr, "usage: %s ", progname);
		fprintf(stderr, "[-a] [-x] [-c] database [file] ...\n");
		exit(2);
	}

	(void) dbzincore(useincore);
	my_basename = argv[optind];
	pagname = str2dup(my_basename, ".pag");
	dir_name = str2dup(my_basename, ".dir");
	mkfiles();
	optind++;

	if (baseinput)		/* implies no further arguments */
		process(base, my_basename);
	else if (optind >= argc)
		process(stdin, "stdin");
	else
		for (; optind < argc; optind++)
			dofile(argv[optind]);

	if (change != NULL)
		(void) chdir(change);
	if (dbmclose() < 0)
		fail("dbmclose failed", "");
	if (doruns)
		runs(pagname);
	if (sweep)
		dosweep(my_basename, pagname);
	if (printx)
		printf("%ld\n", xxx);
#ifdef DBZ_FINISH
	DBZ_FINISH;
#endif
	exit(0);
}

/*
 - dofile - open a file and invoke process()
 */
void
dofile(name)
char *name;
{
	register FILE *in;

	if (STREQ(name, "-"))
		process(stdin, "-");
	else {
		in = fopen(name, "r");
		if (in == NULL)
			fail("cannot open `%s'", name);
		process(in, name);
		(void) fclose(in);
	}
}

/*
 - mkfiles - create empty files and open them up
 */
void
mkfiles()
{
	if (op == 'b' && !dbzint) {
		crfile(dir_name);
		crfile(pagname);
	}

	base = fopen(my_basename, (op == 'a') ? "a" : "r");
	if (base == NULL)
		fail("cannot open `%s'", my_basename);
	if (unopen)
		(void) chmod(my_basename, 0);
	if (from != NULL) {
		if (dbzagain(my_basename, from) < 0)
			fail("dbzagain(`%s'...) failed", my_basename);
	} else if (op == 'b' && dbzint) {
		if (!exact)
			siz = dbzsize(siz);
		if (dbzfresh(my_basename, siz, (int)fs, map, (off_t)tag) < 0)
			fail("dbzfresh(`%s'...) failed", my_basename);
	} else if (dbminit(my_basename) < 0)
		fail("dbminit(`%s') failed", my_basename);
	if (unopen)
		(void) chmod(my_basename, 0600);	/* hard to restore original */
}

/*
 - crfile - create a file
 */
void
crfile(name)
char *name;
{
	register int f;

	f = creat(name, 0666);
	if (f < 0)
		fail("cannot create `%s'", name);
	(void) close(f);
}

/*
 - process - process input file
 */
void
process(in, name)
FILE *in;
char *name;
{
	register off_t place;

	inname = name;
	lineno = 0;

	for (;;) {
		place = ftell(in);
		if (fgets(line, buflen, in) == NULL)
			return;
		lineno++;
		if (every > 0 && lineno%every == 0) {
			fprintf(stderr, "%ld\n", lineno);
			if (dbzsync() < 0)
				fail("dbzsync failed", "");
		}
		doline(line, place);
	}
	/* NOTREACHED */
}

/*
 - doline - process input line
 */
void
doline(lp, inoffset)
char *lp;
off_t inoffset;
{
	register char *p;
	register char pc;
	datum key, value;
	off_t place = inoffset;
	register int shouldfind;
	register int llen;
	char keytext[DBZMAXKEY+1];

	p = NULL;
	if (fs != '\0')
		p = strchr(lp, fs);
	if (p == NULL)
		p = lp + strlen(lp);
	if (p > lp && *(p-1) == '\n')
		p--;
	if (p - lp > DBZMAXKEY)
		fail("key of `%.40s...' too long", lp);
	pc = *p;
	*p = '\0';
	(void) strcpy(keytext, lp);
	*p = pc;
	key.dptr = (dbzint) ? keytext : rfc822ize(keytext);
	key.dsize = strlen(keytext)+1;

	switch (op) {
	case 'a':
		place = ftell(base);
		llen = strlen(lp);
		if (fwrite(lp, 1, llen, base) != llen)
			fail("write error in `%s'", my_basename);
		/* FALLTHROUGH */
	case 'b':
		if (omitzero && p != NULL && *(p+1) == '0')
			return;
		if (unique) {
			value = (dbzint) ? dbzfetch(key) : fetch(key);
			if (value.dptr != NULL)
				fail("`%.40s...' already present", lp);
		}
		value.dptr = (char *)&place;
		value.dsize = (int)sizeof(off_t);
		if (((dbzint) ? dbzstore(key, value) : store(key, value)) < 0)
			fail("store failed on `%.40s...'", lp);
		break;
	case 'c':
		value = (dbzint) ? dbzfetch(key) : fetch(key);
		shouldfind = (omitzero && p != NULL && *(p+1) == '0') ? 0 : 1;
		if (!shouldfind && (value.dptr != NULL || value.dsize != 0))
			fail("`%.40s...' found, shouldn't be", lp);
		if (shouldfind && (value.dptr == NULL ||
					value.dsize != sizeof(off_t)))
			fail("can't find `%.40s...'", lp);
		if (shouldfind && !quick) {
			(void) memcpy((char *)&place, value.dptr, sizeof(off_t));
			if (place != inoffset)
				fail("offset mismatch on `%.40s...'", lp);
			if (fseek(base, place, SEEK_SET) == -1)
				fail("fseek failed on `%.40s...'", lp);
			if (fgets(cmp, buflen, base) == NULL)
				fail("can't read line for `%.40s...'", lp);
			if (!STREQ(lp, cmp))
				fail("compare failed on `%.40s...'", lp);
		}
		break;
	case 'x':
		value = (dbzint) ? dbzfetch(key) : fetch(key);
		if (value.dptr != NULL && !quick) {
			(void) memcpy((char *)&place, value.dptr, sizeof(off_t));
			if (fseek(base, place, SEEK_SET) == -1)
				fail("fseek failed on `%.40s...'", lp);
			if (fgets(cmp, buflen, base) == NULL)
				fail("can't read line for `%.40s...'", lp);
			fputs(cmp, stdout);
		} else if (value.dptr != NULL)
			fputs(lp, stdout);
		break;
	case 'm':
		value = (dbzint) ? dbzfetch(key) : fetch(key);
		if (value.dptr == NULL) {
			fputs(keytext, stdout);
			putchar('\n');
		}
		break;
	default:
		fail("unknown operator -- can't happen", "");
		break;
	}
}

/*
 - runs - print run statistics
 */
void
runs(file)
char *file;
{
	register FILE *fd;
	off_t it;
	register long run;

	fd = fopen(file, "r");
	if (fd == NULL)
		fail("cannot reopen `%s'", file);
	run = 0;
	while (fread((char *)&it, sizeof(off_t), 1, fd) == 1) {
		if (it != 0)
			run++;
		else if (run > 0) {
			printf("%ld\n", run);
			run = 0;
		}
	}
	(void) fclose(fd);
}

/*
 - dosweep - sweep pag file checking for valid offsets
 */
void
dosweep(fn, pn)
char *fn;
char *pn;
{
	register FILE *pf;
	off_t it;
	char nl;
	register FILE *hf;

	hf = fopen(fn, "r");
	if (hf == NULL)
		fail("cannot reopen `%s'", fn);
	pf = fopen(pn, "r");
	if (pf == NULL)
		fail("cannot reopen `%s'", pn);
	while (fread((char *)&it, sizeof(off_t), 1, pf) == 1) {
		it = (it & ((off_t)0x80000000)) ? (it&~((off_t)0xff000000)) : it;
		if (it != 0 && it != 1) {	/* 0 empty, 1 known okay */
			it--;		/* get rid of bias */
			(void) fseek(hf, it-1, SEEK_SET);
			nl = getc(hf);
			if (nl != '\n')
				fprintf(stderr, "offset 0%lo does not point to line\n",
								(long)it);
		}
	}
	(void) fclose(hf);
	(void) fclose(pf);
}

/*
 - fail - complain and die
 */
void
fail(s1, s2)
char *s1;
char *s2;
{
	fprintf(stderr, "%s: (file `%s', line %ld) ", progname, inname, lineno);
	fprintf(stderr, s1, s2);
	fprintf(stderr, "\n");
	exit(1);
}

/*
 - str2dup - concatenate strings and malloc result
 */
char *
str2dup(s1, s2)
char *s1;
char *s2;
{
	register char *p;

	p = malloc((size_t)strlen(s1) + strlen(s2) + 1);
	if (p == NULL)
		fail("can't allocate space for strings", "");
	(void) strcpy(p, s1);
	(void) strcat(p, s2);
	return(p);
}
