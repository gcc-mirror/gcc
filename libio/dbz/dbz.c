/*

dbz.c  V3.2

Copyright 1988 Jon Zeeff (zeeff@b-tech.ann-arbor.mi.us)
You can use this code in any manner, as long as you leave my name on it
and don't hold me responsible for any problems with it.

Hacked on by gdb@ninja.UUCP (David Butler); Sun Jun  5 00:27:08 CDT 1988

Various improvments + INCORE by moraes@ai.toronto.edu (Mark Moraes)

Major reworking by Henry Spencer as part of the C News project.

These routines replace dbm as used by the usenet news software
(it's not a full dbm replacement by any means).  It's fast and
simple.  It contains no AT&T code.

In general, dbz's files are 1/20 the size of dbm's.  Lookup performance
is somewhat better, while file creation is spectacularly faster, especially
if the incore facility is used.

*/

#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#ifndef __STDC__
extern int errno;
#endif
#include <dbz.h>

/*
 * #ifdef index.  "LIA" = "leave it alone unless you know what you're doing".
 *
 * FUNNYSEEKS	SEEK_SET is not 0, get it from <unistd.h>
 * INDEX_SIZE	backward compatibility with old dbz; avoid using this
 * NMEMORY	number of days of memory for use in sizing new table (LIA)
 * INCORE	backward compatibility with old dbz; use dbzincore() instead
 * DBZDEBUG	enable debugging
 * DEFSIZE	default table size (not as critical as in old dbz)
 * OLDBNEWS	default case mapping as in old B News; set NOBUFFER
 * BNEWS	default case mapping as in current B News; set NOBUFFER
 * DEFCASE	default case-map algorithm selector
 * NOTAGS	fseek offsets are strange, do not do tagging (see below)
 * NPAGBUF	size of .pag buffer, in longs (LIA)
 * SHISTBUF	size of ASCII-file buffer, in bytes (LIA)
 * MAXRUN	length of run which shifts to next table (see below) (LIA)
 * OVERFLOW	long-int arithmetic overflow must be avoided, will trap
 * NOBUFFER	do not buffer hash-table i/o, B News locking is defective
 */

#ifdef FUNNYSEEKS
#include <unistd.h>
#else
#define	SEEK_SET	0
#endif
#ifdef OVERFLOW
#include <limits.h>
#endif

static int dbzversion = 3;	/* for validating .dir file format */

/*
 * The dbz database exploits the fact that when news stores a <key,value>
 * tuple, the `value' part is a seek offset into a text file, pointing to
 * a copy of the `key' part.  This avoids the need to store a copy of
 * the key in the dbz files.  However, the text file *must* exist and be
 * consistent with the dbz files, or things will fail.
 *
 * The basic format of the database is a simple hash table containing the
 * values.  A value is stored by indexing into the table using a hash value
 * computed from the key; collisions are resolved by linear probing (just
 * search forward for an empty slot, wrapping around to the beginning of
 * the table if necessary).  Linear probing is a performance disaster when
 * the table starts to get full, so a complication is introduced.  The
 * database is actually one *or more* tables, stored sequentially in the
 * .pag file, and the length of linear-probe sequences is limited.  The
 * search (for an existing item or an empty slot) always starts in the
 * first table, and whenever MAXRUN probes have been done in table N,
 * probing continues in table N+1.  This behaves reasonably well even in
 * cases of massive overflow.  There are some other small complications
 * added, see comments below.
 *
 * The table size is fixed for any particular database, but is determined
 * dynamically when a database is rebuilt.  The strategy is to try to pick
 * the size so the first table will be no more than 2/3 full, that being
 * slightly before the point where performance starts to degrade.  (It is
 * desirable to be a bit conservative because the overflow strategy tends
 * to produce files with holes in them, which is a nuisance.)
 */

/*
 * The following is for backward compatibility.
 */
#ifdef INDEX_SIZE
#define	DEFSIZE	INDEX_SIZE
#endif

/*
 * ANSI C says the offset argument to fseek is a long, not an off_t, for some
 * reason.  Let's use off_t anyway.
 */
#define	SOF	(sizeof(off_t))

/*
 * We assume that unused areas of a binary file are zeros, and that the
 * bit pattern of `(off_t)0' is all zeros.  The alternative is rather
 * painful file initialization.  Note that okayvalue(), if OVERFLOW is
 * defined, knows what value of an offset would cause overflow.
 */
#define	VACANT		((off_t)0)
#define	BIAS(o)		((o)+1)		/* make any valid off_t non-VACANT */
#define	UNBIAS(o)	((o)-1)		/* reverse BIAS() effect */

/*
 * In a Unix implementation, or indeed any in which an off_t is a byte
 * count, there are a bunch of high bits free in an off_t.  There is a
 * use for them.  Checking a possible hit by looking it up in the base
 * file is relatively expensive, and the cost can be dramatically reduced
 * by using some of those high bits to tag the value with a few more bits
 * of the key's hash.  This detects most false hits without the overhead of
 * seek+read+strcmp.  We use the top bit to indicate whether the value is
 * tagged or not, and don't tag a value which is using the tag bits itself.
 * We're in trouble if the off_t representation wants to use the top bit.
 * The actual bitmasks and offset come from the configuration stuff,
 * which permits fiddling with them as necessary, and also suppressing
 * them completely (by defining the masks to 0).  We build pre-shifted
 * versions of the masks for efficiency.
 */
static off_t tagbits;		/* pre-shifted tag mask */
static off_t taghere;		/* pre-shifted tag-enable bit */
static off_t tagboth;		/* tagbits|taghere */
#define	HASTAG(o)	((o)&taghere)
#define	TAG(o)		((o)&tagbits)
#define	NOTAG(o)	((o)&~tagboth)
#define	CANTAG(o)	(((o)&tagboth) == 0)
#define	MKTAG(v)	(((v)<<conf.tagshift)&tagbits)

/*
 * A new, from-scratch database, not built as a rebuild of an old one,
 * needs to know table size, casemap algorithm, and tagging.  Normally
 * the user supplies this info, but there have to be defaults.
 */
#ifndef DEFSIZE
#define	DEFSIZE	120011		/* 300007 might be better */
#endif
#ifdef OLDBNEWS
#define	DEFCASE	'0'		/* B2.10 -- no mapping */
#define	NOBUFFER		/* B News locking is defective */
#endif
#ifdef BNEWS
#define	DEFCASE	'='		/* B2.11 -- all mapped */
#define	NOBUFFER		/* B News locking is defective */
#endif
#ifndef DEFCASE			/* C News compatibility is the default */
#define	DEFCASE	'C'		/* C News -- RFC822 mapping */
#endif
#ifndef NOTAGS
#define	TAGENB	0x80		/* tag enable is top bit, tag is next 7 */
#define	TAGMASK	0x7f
#define	TAGSHIFT	24
#else
#define	TAGENB	0		/* no tags */
#define	TAGMASK	0
#define	TAGSHIFT	0
#endif

/*
 * We read configuration info from the .dir file into this structure,
 * so we can avoid wired-in assumptions for an existing database.
 *
 * Among the info is a record of recent peak usages, so that a new table
 * size can be chosen intelligently when rebuilding.  10 is a good
 * number of usages to keep, since news displays marked fluctuations
 * in volume on a 7-day cycle.
 */
struct dbzconfig {
	int olddbz;		/* .dir file empty but .pag not? */
	off_t tsize;		/* table size */
#	ifndef NMEMORY
#	define	NMEMORY	10	/* # days of use info to remember */
#	endif
#	define	NUSEDS	(1+NMEMORY)
	off_t used[NUSEDS];	/* entries used today, yesterday, ... */
	int valuesize;		/* size of table values, == SOF */
	int bytemap[SOF];	/* byte-order map */
	char casemap;		/* case-mapping algorithm (see cipoint()) */
	char fieldsep;		/* field separator in base file, if any */
	off_t tagenb;		/* unshifted tag-enable bit */
	off_t tagmask;		/* unshifted tag mask */
	int tagshift;		/* shift count for tagmask and tagenb */
};
static struct dbzconfig conf;
static int getconf();
static long getno();
static int putconf();
static void mybytemap();
static off_t bytemap();

/* 
 * For a program that makes many, many references to the database, it
 * is a large performance win to keep the table in core, if it will fit.
 * Note that this does hurt robustness in the event of crashes, and
 * dbmclose() *must* be called to flush the in-core database to disk.
 * The code is prepared to deal with the possibility that there isn't
 * enough memory.  There *is* an assumption that a size_t is big enough
 * to hold the size (in bytes) of one table, so dbminit() tries to figure
 * out whether this is possible first.
 *
 * The preferred way to ask for an in-core table is to do dbzincore(1)
 * before dbminit().  The default is not to do it, although -DINCORE
 * overrides this for backward compatibility with old dbz.
 *
 * We keep only the first table in core.  This greatly simplifies the
 * code, and bounds memory demand.  Furthermore, doing this is a large
 * performance win even in the event of massive overflow.
 */
#ifdef INCORE
static int incore = 1;
#else
static int incore = 0;
#endif

/*
 * Stdio buffer for .pag reads.  Buffering more than about 16 does not help
 * significantly at the densities we try to maintain, and the much larger
 * buffers that most stdios default to are much more expensive to fill.
 * With small buffers, stdio is performance-competitive with raw read(),
 * and it's much more portable.
 */
#ifndef NPAGBUF
#define	NPAGBUF	16
#endif
#ifndef NOBUFFER
#ifdef _IOFBF
static off_t pagbuf[NPAGBUF];	/* only needed if !NOBUFFER && _IOFBF */
#endif
#endif

/*
 * Stdio buffer for base-file reads.  Message-IDs (all news ever needs to
 * read) are essentially never longer than 64 bytes, and the typical stdio
 * buffer is so much larger that it is much more expensive to fill.
 */
#ifndef SHISTBUF
#define	SHISTBUF	64
#endif
#ifdef _IOFBF
static char basebuf[SHISTBUF];		/* only needed if _IOFBF exists */
#endif

/*
 * Data structure for recording info about searches.
 */
struct searcher {
	off_t place;		/* current location in file */
	int tabno;		/* which table we're in */
	int run;		/* how long we'll stay in this table */
#		ifndef MAXRUN
#		define	MAXRUN	100
#		endif
	long hash;		/* the key's hash code (for optimization) */
	off_t tag;		/* tag we are looking for */
	int seen;		/* have we examined current location? */
	int aborted;		/* has i/o error aborted search? */
};
static void start();
#define	FRESH	((struct searcher *)NULL)
static off_t search();
#define	NOTFOUND	((off_t)-1)
static int okayvalue();
static int set();

/*
 * Arguably the searcher struct for a given routine ought to be local to
 * it, but a fetch() is very often immediately followed by a store(), and
 * in some circumstances it is a useful performance win to remember where
 * the fetch() completed.  So we use a global struct and remember whether
 * it is current.
 */
static struct searcher srch;
static struct searcher *prevp;	/* &srch or FRESH */

/* byte-ordering stuff */
static int mybmap[SOF];			/* my byte order (see mybytemap()) */
static int bytesame;			/* is database order same as mine? */
#define	MAPIN(o)	((bytesame) ? (o) : bytemap((o), conf.bytemap, mybmap))
#define	MAPOUT(o)	((bytesame) ? (o) : bytemap((o), mybmap, conf.bytemap))

/*
 * The double parentheses needed to make this work are ugly, but the
 * alternative (under most compilers) is to pack around 2K of unused
 * strings -- there's just no way to get rid of them.
 */
static int debug;			/* controlled by dbzdebug() */
#ifdef DBZDEBUG
#define DEBUG(args) if (debug) { (void) printf args ; }
#else
#define	DEBUG(args)	;
#endif

/* externals used */
extern char *malloc();
extern char *calloc();
extern void free();		/* ANSI C; some old implementations say int */
extern int atoi();
extern long atol();

/* misc. forwards */
static long hash();
static void crcinit();
static char *cipoint();
static char *mapcase();
static int isprime();
static FILE *latebase();

/* file-naming stuff */
static char dir[] = ".dir";
static char pag[] = ".pag";
static char *enstring();

/* central data structures */
static FILE *basef;		/* descriptor for base file */
static char *basefname;		/* name for not-yet-opened base file */
static FILE *dirf;		/* descriptor for .dir file */
static int dirronly;		/* dirf open read-only? */
static FILE *pagf = NULL;	/* descriptor for .pag file */
static off_t pagpos;		/* posn in pagf; only search may set != -1 */
static int pagronly;		/* pagf open read-only? */
static off_t *corepag;		/* incore version of .pag file, if any */
static FILE *bufpagf;		/* well-buffered pagf, for incore rewrite */
static off_t *getcore();
static int putcore();
static int written;		/* has a store() been done? */

/*
 - dbzfresh - set up a new database, no historical info
 */
int				/* 0 success, -1 failure */
dbzfresh(name, size, fs, cmap, tagmask)
char *name;			/* base name; .dir and .pag must exist */
long size;			/* table size (0 means default) */
int fs;				/* field-separator character in base file */
int cmap;			/* case-map algorithm (0 means default) */
off_t tagmask;			/* 0 default, 1 no tags */
{
	register char *fn;
	struct dbzconfig c;
	register off_t m;
	register FILE *f;

	if (pagf != NULL) {
		DEBUG(("dbzfresh: database already open\n"));
		return(-1);
	}
	if (size != 0 && size < 2) {
		DEBUG(("dbzfresh: preposterous size (%ld)\n", size));
		return(-1);
	}

	/* get default configuration */
	if (getconf((FILE *)NULL, (FILE *)NULL, &c) < 0)
		return(-1);	/* "can't happen" */

	/* and mess with it as specified */
	if (size != 0)
		c.tsize = size;
	c.fieldsep = fs;
	switch (cmap) {
	case 0:
	case '0':
	case 'B':		/* 2.10 compat */
		c.casemap = '0';	/* '\0' nicer, but '0' printable! */
		break;
	case '=':
	case 'b':		/* 2.11 compat */
		c.casemap = '=';
		break;
	case 'C':
		c.casemap = 'C';
		break;
	case '?':
		c.casemap = DEFCASE;
		break;
	default:
		DEBUG(("dbzfresh case map `%c' unknown\n", cmap));
		return(-1);
		break;
	}
	switch (tagmask) {
	case 0:			/* default */
		break;
	case 1:			/* no tags */
		c.tagshift = 0;
		c.tagmask = 0;
		c.tagenb = 0;
		break;
	default:
		m = tagmask;
		c.tagshift = 0;
		while (!(m&01)) {
			m >>= 1;
			c.tagshift++;
		}
		c.tagmask = m;
		c.tagenb = (m << 1) & ~m;
		break;
	}

	/* write it out */
	fn = enstring(name, dir);
	if (fn == NULL)
		return(-1);
	f = fopen(fn, "w");
	free(fn);
	if (f == NULL) {
		DEBUG(("dbzfresh: unable to write config\n"));
		return(-1);
	}
	if (putconf(f, &c) < 0) {
		(void) fclose(f);
		return(-1);
	}
	if (fclose(f) == EOF) {
		DEBUG(("dbzfresh: fclose failure\n"));
		return(-1);
	}

	/* create/truncate .pag */
	fn = enstring(name, pag);
	if (fn == NULL)
		return(-1);
	f = fopen(fn, "w");
	free(fn);
	if (f == NULL) {
		DEBUG(("dbzfresh: unable to create/truncate .pag file\n"));
		return(-1);
	} else
		(void) fclose(f);

	/* and punt to dbminit for the hard work */
	return(dbminit(name));
}

/*
 - dbzsize - what's a good table size to hold this many entries?
 */
long
dbzsize(contents)
long contents;			/* 0 means what's the default */
{
	register long n;

	if (contents <= 0) {	/* foulup or default inquiry */
		DEBUG(("dbzsize: preposterous input (%ld)\n", contents));
		return(DEFSIZE);
	}
	n = (contents/2)*3;	/* try to keep table at most 2/3 full */
	if (!(n&01))		/* make it odd */
		n++;
	DEBUG(("dbzsize: tentative size %ld\n", n));
	while (!isprime(n))	/* and look for a prime */
		n += 2;
	DEBUG(("dbzsize: final size %ld\n", n));

	return(n);
}

/*
 - isprime - is a number prime?
 *
 * This is not a terribly efficient approach.
 */
static int			/* predicate */
isprime(x)
register long x;
{
	static int quick[] = { 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 0 };
	register int *ip;
	register long div;
	register long stop;

	/* hit the first few primes quickly to eliminate easy ones */
	/* this incidentally prevents ridiculously small tables */
	for (ip = quick; (div = *ip) != 0; ip++)
		if (x%div == 0) {
			DEBUG(("isprime: quick result on %ld\n", (long)x));
			return(0);
		}

	/* approximate square root of x */
	for (stop = x; x/stop < stop; stop >>= 1)
		continue;
	stop <<= 1;

	/* try odd numbers up to stop */
	for (div = *--ip; div < stop; div += 2)
		if (x%div == 0)
			return(0);

	return(1);
}

/*
 - dbzagain - set up a new database to be a rebuild of an old one
 */
int				/* 0 success, -1 failure */
dbzagain(name, oldname)
char *name;			/* base name; .dir and .pag must exist */
char *oldname;			/* base name; all must exist */
{
	register char *fn;
	struct dbzconfig c;
	register int i;
	register long top;
	register FILE *f;
	register int newtable;
	register off_t newsize;

	if (pagf != NULL) {
		DEBUG(("dbzagain: database already open\n"));
		return(-1);
	}

	/* pick up the old configuration */
	fn = enstring(oldname, dir);
	if (fn == NULL)
		return(-1);
	f = fopen(fn, "r");
	free(fn);
	if (f == NULL) {
		DEBUG(("dbzagain: cannot open old .dir file\n"));
		return(-1);
	}
	i = getconf(f, (FILE *)NULL, &c);
	(void) fclose(f);
	if (i < 0) {
		DEBUG(("dbzagain: getconf failed\n"));
		return(-1);
	}

	/* tinker with it */
	top = 0;
	newtable = 0;
	for (i = 0; i < NUSEDS; i++) {
		if (top < c.used[i])
			top = c.used[i];
		if (c.used[i] == 0)
			newtable = 1;	/* hasn't got full usage history yet */
	}
	if (top == 0) {
		DEBUG(("dbzagain: old table has no contents!\n"));
		newtable = 1;
	}
	for (i = NUSEDS-1; i > 0; i--)
		c.used[i] = c.used[i-1];
	c.used[0] = 0;
	newsize = dbzsize(top);
	if (!newtable || newsize > c.tsize)	/* don't shrink new table */
		c.tsize = newsize;

	/* write it out */
	fn = enstring(name, dir);
	if (fn == NULL)
		return(-1);
	f = fopen(fn, "w");
	free(fn);
	if (f == NULL) {
		DEBUG(("dbzagain: unable to write new .dir\n"));
		return(-1);
	}
	i = putconf(f, &c);
	(void) fclose(f);
	if (i < 0) {
		DEBUG(("dbzagain: putconf failed\n"));
		return(-1);
	}

	/* create/truncate .pag */
	fn = enstring(name, pag);
	if (fn == NULL)
		return(-1);
	f = fopen(fn, "w");
	free(fn);
	if (f == NULL) {
		DEBUG(("dbzagain: unable to create/truncate .pag file\n"));
		return(-1);
	} else
		(void) fclose(f);

	/* and let dbminit do the work */
	return(dbminit(name));
}

/*
 - dbminit - open a database, creating it (using defaults) if necessary
 *
 * We try to leave errno set plausibly, to the extent that underlying
 * functions permit this, since many people consult it if dbminit() fails.
 */
int 				/* 0 success, -1 failure */
dbminit(name)
char *name;
{
	register int i;
	register size_t s;
	register char *dirfname;
	register char *pagfname;

	if (pagf != NULL) {
		DEBUG(("dbminit: dbminit already called once\n"));
		errno = 0;
		return(-1);
	}

	/* open the .dir file */
	dirfname = enstring(name, dir);
	if (dirfname == NULL)
		return(-1);
	dirf = fopen(dirfname, "r+");
	if (dirf == NULL) {
		dirf = fopen(dirfname, "r");
		dirronly = 1;
	} else
		dirronly = 0;
	free(dirfname);
	if (dirf == NULL) {
		DEBUG(("dbminit: can't open .dir file\n"));
		return(-1);
	}

	/* open the .pag file */
	pagfname = enstring(name, pag);
	if (pagfname == NULL) {
		(void) fclose(dirf);
		return(-1);
	}
	pagf = fopen(pagfname, "r+b");
	if (pagf == NULL) {
		pagf = fopen(pagfname, "rb");
		if (pagf == NULL) {
			DEBUG(("dbminit: .pag open failed\n"));
			(void) fclose(dirf);
			free(pagfname);
			return(-1);
		}
		pagronly = 1;
	} else if (dirronly)
		pagronly = 1;
	else
		pagronly = 0;
#ifdef NOBUFFER
	/*
	 * B News does not do adequate locking on its database accesses.
	 * Why it doesn't get into trouble using dbm is a mystery.  In any
	 * case, doing unbuffered i/o does not cure the problem, but does
	 * enormously reduce its incidence.
	 */
	(void) setbuf(pagf, (char *)NULL);
#else
#ifdef _IOFBF
	(void) setvbuf(pagf, (char *)pagbuf, _IOFBF, sizeof(pagbuf));
#endif
#endif
	pagpos = -1;
	/* don't free pagfname, need it below */

	/* open the base file */
	basef = fopen(name, "r");
	if (basef == NULL) {
		DEBUG(("dbminit: basefile open failed\n"));
		basefname = enstring(name, "");
		if (basefname == NULL) {
			(void) fclose(pagf);
			(void) fclose(dirf);
			free(pagfname);
			pagf = NULL;
			return(-1);
		}
	} else
		basefname = NULL;
#ifdef _IOFBF
	if (basef != NULL)
		(void) setvbuf(basef, basebuf, _IOFBF, sizeof(basebuf));
#endif

	/* pick up configuration */
	if (getconf(dirf, pagf, &conf) < 0) {
		DEBUG(("dbminit: getconf failure\n"));
		(void) fclose(basef);
		(void) fclose(pagf);
		(void) fclose(dirf);
		free(pagfname);
		pagf = NULL;
		errno = EDOM;	/* kind of a kludge, but very portable */
		return(-1);
	}
	tagbits = conf.tagmask << conf.tagshift;
	taghere = conf.tagenb << conf.tagshift;
	tagboth = tagbits | taghere;
	mybytemap(mybmap);
	bytesame = 1;
	for (i = 0; i < SOF; i++)
		if (mybmap[i] != conf.bytemap[i])
			bytesame = 0;

	/* get first table into core, if it looks desirable and feasible */
	s = (size_t)conf.tsize * SOF;
	if (incore && (off_t)(s/SOF) == conf.tsize) {
		bufpagf = fopen(pagfname, (pagronly) ? "rb" : "r+b");
		if (bufpagf != NULL)
			corepag = getcore(bufpagf);
	} else {
		bufpagf = NULL;
		corepag = NULL;
	}
	free(pagfname);

	/* misc. setup */
	crcinit();
	written = 0;
	prevp = FRESH;
	DEBUG(("dbminit: succeeded\n"));
	return(0);
}

/*
 - enstring - concatenate two strings into a malloced area
 */
static char *			/* NULL if malloc fails */
enstring(s1, s2)
char *s1;
char *s2;
{
	register char *p;

	p = malloc((size_t)strlen(s1) + (size_t)strlen(s2) + 1);
	if (p != NULL) {
		(void) strcpy(p, s1);
		(void) strcat(p, s2);
	} else {
		DEBUG(("enstring(%s, %s) out of memory\n", s1, s2));
	}
	return(p);
}

/*
 - dbmclose - close a database
 */
int
dbmclose()
{
	register int ret = 0;

	if (pagf == NULL) {
		DEBUG(("dbmclose: not opened!\n"));
		return(-1);
	}

	if (fclose(pagf) == EOF) {
		DEBUG(("dbmclose: fclose(pagf) failed\n"));
		ret = -1;
	}
	pagf = basef;		/* ensure valid pointer; dbzsync checks it */
	if (dbzsync() < 0)
		ret = -1;
	if (bufpagf != NULL && fclose(bufpagf) == EOF) {
		DEBUG(("dbmclose: fclose(bufpagf) failed\n"));
		ret = -1;
	}
	if (corepag != NULL)
		free((char *)corepag);
	corepag = NULL;
	if (fclose(basef) == EOF) {
		DEBUG(("dbmclose: fclose(basef) failed\n"));
		ret = -1;
	}
	if (basefname != NULL)
		free(basefname);
	basef = NULL;
	pagf = NULL;
	if (fclose(dirf) == EOF) {
		DEBUG(("dbmclose: fclose(dirf) failed\n"));
		ret = -1;
	}

	DEBUG(("dbmclose: %s\n", (ret == 0) ? "succeeded" : "failed"));
	return(ret);
}

/*
 - dbzsync - push all in-core data out to disk
 */
int
dbzsync()
{
	register int ret = 0;

	if (pagf == NULL) {
		DEBUG(("dbzsync: not opened!\n"));
		return(-1);
	}
	if (!written)
		return(0);

	if (corepag != NULL) {
		if (putcore(corepag, bufpagf) < 0) {
			DEBUG(("dbzsync: putcore failed\n"));
			ret = -1;
		}
	}
	if (!conf.olddbz)
		if (putconf(dirf, &conf) < 0)
			ret = -1;

	DEBUG(("dbzsync: %s\n", (ret == 0) ? "succeeded" : "failed"));
	return(ret);
}

/*
 - dbzcancel - cancel writing of in-core data
 * Mostly for use from child processes.
 * Note that we don't need to futz around with stdio buffers, because we
 * always fflush them immediately anyway and so they never have stale data.
 */
int
dbzcancel()
{
	if (pagf == NULL) {
		DEBUG(("dbzcancel: not opened!\n"));
		return(-1);
	}

	written = 0;
	return(0);
}

/*
 - dbzfetch - fetch() with case mapping built in
 */
datum
dbzfetch(key)
datum key;
{
	char buffer[DBZMAXKEY + 1];
	datum mappedkey;
	register size_t keysize;

	DEBUG(("dbzfetch: (%s)\n", key.dptr));

	/* Key is supposed to be less than DBZMAXKEY */
	keysize = key.dsize;
	if (keysize >= DBZMAXKEY) {
		keysize = DBZMAXKEY;
		DEBUG(("keysize is %d - truncated to %d\n", key.dsize, DBZMAXKEY));
	}

	mappedkey.dptr = mapcase(buffer, key.dptr, keysize);
	buffer[keysize] = '\0';	/* just a debug aid */
	mappedkey.dsize = keysize;

	return(fetch(mappedkey));
}

/*
 - fetch - get an entry from the database
 *
 * Disgusting fine point, in the name of backward compatibility:  if the
 * last character of "key" is a NUL, that character is (effectively) not
 * part of the comparison against the stored keys.
 */
datum				/* dptr NULL, dsize 0 means failure */
fetch(key)
datum key;
{
	char buffer[DBZMAXKEY + 1];
	static off_t key_ptr;		/* return value points here */
	datum output;
	register size_t keysize;
	register size_t cmplen;
	register char *sepp;

	DEBUG(("fetch: (%s)\n", key.dptr));
	output.dptr = NULL;
	output.dsize = 0;
	prevp = FRESH;

	/* Key is supposed to be less than DBZMAXKEY */
	keysize = key.dsize;
	if (keysize >= DBZMAXKEY) {
		keysize = DBZMAXKEY;
		DEBUG(("keysize is %d - truncated to %d\n", key.dsize, DBZMAXKEY));
	}

	if (pagf == NULL) {
		DEBUG(("fetch: database not open!\n"));
		return(output);
	} else if (basef == NULL) {	/* basef didn't exist yet */
		basef = latebase();
		if (basef == NULL)
			return(output);
	}

	cmplen = keysize;
	sepp = &conf.fieldsep;
	if (key.dptr[keysize-1] == '\0') {
		cmplen--;
		sepp = &buffer[keysize-1];
	}
	start(&srch, &key, FRESH);
	while ((key_ptr = search(&srch)) != NOTFOUND) {
		DEBUG(("got 0x%lx\n", key_ptr));

		/* fetch the key */
		if (fseek(basef, key_ptr, SEEK_SET) != 0) {
			DEBUG(("fetch: seek failed\n"));
			return(output);
		}
		if (fread(buffer, 1, keysize, basef) != keysize) {
			DEBUG(("fetch: read failed\n"));
			return(output);
		}

		/* try it */
		buffer[keysize] = '\0';		/* terminated for DEBUG */
		(void) mapcase(buffer, buffer, keysize);
		DEBUG(("fetch: buffer (%s) looking for (%s) size = %d\n", 
						buffer, key.dptr, keysize));
		if (memcmp(key.dptr, buffer, cmplen) == 0 &&
				(*sepp == conf.fieldsep || *sepp == '\0')) {
			/* we found it */
			output.dptr = (char *)&key_ptr;
			output.dsize = SOF;
			DEBUG(("fetch: successful\n"));
			return(output);
		}
	}

	/* we didn't find it */
	DEBUG(("fetch: failed\n"));
	prevp = &srch;			/* remember where we stopped */
	return(output);
}

/*
 - latebase - try to open a base file that wasn't there at the start
 */
static FILE *
latebase()
{
	register FILE *it;

	if (basefname == NULL) {
		DEBUG(("latebase: name foulup\n"));
		return(NULL);
	}
	it = fopen(basefname, "r");
	if (it == NULL) {
		DEBUG(("latebase: still can't open base\n"));
	} else {
		DEBUG(("latebase: late open succeeded\n"));
		free(basefname);
		basefname = NULL;
#ifdef _IOFBF
		(void) setvbuf(it, basebuf, _IOFBF, sizeof(basebuf));
#endif
	}
	return(it);
}

/*
 - dbzstore - store() with case mapping built in
 */
int
dbzstore(key, data)
datum key;
datum data;
{
	char buffer[DBZMAXKEY + 1];
	datum mappedkey;
	register size_t keysize;

	DEBUG(("dbzstore: (%s)\n", key.dptr));

	/* Key is supposed to be less than DBZMAXKEY */
	keysize = key.dsize;
	if (keysize >= DBZMAXKEY) {
		DEBUG(("dbzstore: key size too big (%d)\n", key.dsize));
		return(-1);
	}

	mappedkey.dptr = mapcase(buffer, key.dptr, keysize);
	buffer[keysize] = '\0';	/* just a debug aid */
	mappedkey.dsize = keysize;

	return(store(mappedkey, data));
}

/*
 - store - add an entry to the database
 */
int				/* 0 success, -1 failure */
store(key, data)
datum key;
datum data;
{
	off_t value;

	if (pagf == NULL) {
		DEBUG(("store: database not open!\n"));
		return(-1);
	} else if (basef == NULL) {	/* basef didn't exist yet */
		basef = latebase();
		if (basef == NULL)
			return(-1);
	}
	if (pagronly) {
		DEBUG(("store: database open read-only\n"));
		return(-1);
	}
	if (data.dsize != SOF) {
		DEBUG(("store: value size wrong (%d)\n", data.dsize));
		return(-1);
	}
	if (key.dsize >= DBZMAXKEY) {
		DEBUG(("store: key size too big (%d)\n", key.dsize));
		return(-1);
	}

	/* copy the value in to ensure alignment */
	(void) memcpy((char *)&value, data.dptr, SOF);
	DEBUG(("store: (%s, %ld)\n", key.dptr, (long)value));
	if (!okayvalue(value)) {
		DEBUG(("store: reserved bit or overflow in 0x%lx\n", value));
		return(-1);
	}

	/* find the place, exploiting previous search if possible */
	start(&srch, &key, prevp);
	while (search(&srch) != NOTFOUND)
		continue;

	prevp = FRESH;
	conf.used[0]++;
	DEBUG(("store: used count %ld\n", conf.used[0]));
	written = 1;
	return(set(&srch, value));
}

/*
 - dbzincore - control attempts to keep .pag file in core
 */
int				/* old setting */
dbzincore(value)
int value;
{
	register int old = incore;

	incore = value;
	return(old);
}

/*
 - getconf - get configuration from .dir file
 */
static int			/* 0 success, -1 failure */
getconf(df, pf, cp)
register FILE *df;		/* NULL means just give me the default */
register FILE *pf;		/* NULL means don't care about .pag */
register struct dbzconfig *cp;
{
	register int c;
	register int i;
	int err = 0;

	c = (df != NULL) ? getc(df) : EOF;
	if (c == EOF) {		/* empty file, no configuration known */
		cp->olddbz = 0;
		if (df != NULL && pf != NULL && getc(pf) != EOF)
			cp->olddbz = 1;
		cp->tsize = DEFSIZE;
		cp->fieldsep = '\t';
		for (i = 0; i < NUSEDS; i++)
			cp->used[i] = 0;
		cp->valuesize = SOF;
		mybytemap(cp->bytemap);
		cp->casemap = DEFCASE;
		cp->tagenb = TAGENB;
		cp->tagmask = TAGMASK;
		cp->tagshift = TAGSHIFT;
		DEBUG(("getconf: defaults (%ld, %c, (0x%lx/0x%lx<<%d))\n",
			cp->tsize, cp->casemap, cp->tagenb, 
			cp->tagmask, cp->tagshift));
		return(0);
	}
	(void) ungetc(c, df);

	/* first line, the vital stuff */
	if (getc(df) != 'd' || getc(df) != 'b' || getc(df) != 'z')
		err = -1;
	if (getno(df, &err) != dbzversion)
		err = -1;
	cp->tsize = getno(df, &err);
	cp->fieldsep = getno(df, &err);
	while ((c = getc(df)) == ' ')
		continue;
	cp->casemap = c;
	cp->tagenb = getno(df, &err);
	cp->tagmask = getno(df, &err);
	cp->tagshift = getno(df, &err);
	cp->valuesize = getno(df, &err);
	if (cp->valuesize != SOF) {
		DEBUG(("getconf: wrong off_t size (%d)\n", cp->valuesize));
		err = -1;
		cp->valuesize = SOF;	/* to protect the loops below */
	}
	for (i = 0; i < cp->valuesize; i++)
		cp->bytemap[i] = getno(df, &err);
	if (getc(df) != '\n')
		err = -1;
	DEBUG(("size %ld, sep %d, cmap %c, tags 0x%lx/0x%lx<<%d, ", cp->tsize,
			cp->fieldsep, cp->casemap, cp->tagenb, cp->tagmask,
			cp->tagshift));
	DEBUG(("bytemap (%d)", cp->valuesize));
	for (i = 0; i < cp->valuesize; i++) {
		DEBUG((" %d", cp->bytemap[i]));
	}
	DEBUG(("\n"));

	/* second line, the usages */
	for (i = 0; i < NUSEDS; i++)
		cp->used[i] = getno(df, &err);
	if (getc(df) != '\n')
		err = -1;
	DEBUG(("used %ld %ld %ld...\n", cp->used[0], cp->used[1], cp->used[2]));

	if (err < 0) {
		DEBUG(("getconf error\n"));
		return(-1);
	}
	return(0);
}

/*
 - getno - get a long
 */
static long
getno(f, ep)
FILE *f;
int *ep;
{
	register char *p;
#	define	MAXN	50
	char getbuf[MAXN];
	register int c;

	while ((c = getc(f)) == ' ')
		continue;
	if (c == EOF || c == '\n') {
		DEBUG(("getno: missing number\n"));
		*ep = -1;
		return(0);
	}
	p = getbuf;
	*p++ = c;
	while ((c = getc(f)) != EOF && c != '\n' && c != ' ')
		if (p < &getbuf[MAXN-1])
			*p++ = c;
	if (c == EOF) {
		DEBUG(("getno: EOF\n"));
		*ep = -1;
	} else
		(void) ungetc(c, f);
	*p = '\0';

	if (strspn(getbuf, "-1234567890") != strlen(getbuf)) {
		DEBUG(("getno: `%s' non-numeric\n", getbuf));
		*ep = -1;
	}
	return(atol(getbuf));
}

/*
 - putconf - write configuration to .dir file
 */
static int			/* 0 success, -1 failure */
putconf(f, cp)
register FILE *f;
register struct dbzconfig *cp;
{
	register int i;
	register int ret = 0;

	if (fseek(f, 0, SEEK_SET) != 0) {
		DEBUG(("fseek failure in putconf\n"));
		ret = -1;
	}
	fprintf(f, "dbz %d %ld %d %c %ld %ld %d %d", dbzversion,
		(long)cp->tsize,
		cp->fieldsep, cp->casemap, (long)cp->tagenb,
		(long)cp->tagmask, cp->tagshift,
		cp->valuesize);

	for (i = 0; i < cp->valuesize; i++)
		fprintf(f, " %d", cp->bytemap[i]);
	fprintf(f, "\n");
	for (i = 0; i < NUSEDS; i++)
		fprintf(f, "%ld%c",
			(long)cp->used[i], (i < NUSEDS-1) ? ' ' : '\n');


	(void) fflush(f);
	if (ferror(f))
		ret = -1;

	DEBUG(("putconf status %d\n", ret));
	return(ret);
}

/*
 - getcore - try to set up an in-core copy of .pag file
 */
static off_t *			/* pointer to copy, or NULL */
getcore(f)
FILE *f;
{
	register off_t *p;
	register size_t i;
	register size_t nread;
	register char *it;

	it = malloc((size_t)conf.tsize * SOF);
	if (it == NULL) {
		DEBUG(("getcore: malloc failed\n"));
		return(NULL);
	}

	nread = fread(it, SOF, (size_t)conf.tsize, f);
	if (ferror(f)) {
		DEBUG(("getcore: read failed\n"));
		free(it);
		return(NULL);
	}

	p = (off_t *)it + nread;
	i = (size_t)conf.tsize - nread;
	while (i-- > 0)
		*p++ = VACANT;
	return((off_t *)it);
}

/*
 - putcore - try to rewrite an in-core table
 */
static int			/* 0 okay, -1 fail */
putcore(tab, f)
off_t *tab;
FILE *f;
{
	if (fseek(f, 0, SEEK_SET) != 0) {
		DEBUG(("fseek failure in putcore\n"));
		return(-1);
	}
	(void) fwrite((char *)tab, SOF, (size_t)conf.tsize, f);
	(void) fflush(f);
	return((ferror(f)) ? -1 : 0);
}

/*
 - start - set up to start or restart a search
 */
static void
start(sp, kp, osp)
register struct searcher *sp;
register datum *kp;
register struct searcher *osp;		/* may be FRESH, i.e. NULL */
{
	register long h;

	h = hash(kp->dptr, kp->dsize);
	if (osp != FRESH && osp->hash == h) {
		if (sp != osp)
			*sp = *osp;
		DEBUG(("search restarted\n"));
	} else {
		sp->hash = h;
		sp->tag = MKTAG(h / conf.tsize);
		DEBUG(("tag 0x%lx\n", sp->tag));
		sp->place = h % conf.tsize;
		sp->tabno = 0;
		sp->run = (conf.olddbz) ? conf.tsize : MAXRUN;
		sp->aborted = 0;
	}
	sp->seen = 0;
}

/*
 - search - conduct part of a search
 */
static off_t			/* NOTFOUND if we hit VACANT or error */
search(sp)
register struct searcher *sp;
{
	register off_t dest;
	register off_t value;
	off_t val;		/* buffer for value (can't fread register) */
	register off_t place;

	if (sp->aborted)
		return(NOTFOUND);

	for (;;) {
		/* determine location to be examined */
		place = sp->place;
		if (sp->seen) {
			/* go to next location */
			if (--sp->run <= 0) {
				sp->tabno++;
				sp->run = MAXRUN;
			}
			place = (place+1)%conf.tsize + sp->tabno*conf.tsize;
			sp->place = place;
		} else
			sp->seen = 1;	/* now looking at current location */
		DEBUG(("search @ %ld\n", place));

		/* get the tagged value */
		if (corepag != NULL && place < conf.tsize) {
			DEBUG(("search: in core\n"));
			value = MAPIN(corepag[place]);
		} else {
			/* seek, if necessary */
			dest = place * SOF;
			if (pagpos != dest) {
				if (fseek(pagf, dest, SEEK_SET) != 0) {
					DEBUG(("search: seek failed\n"));
					pagpos = -1;
					sp->aborted = 1;
					return(NOTFOUND);
				}
				pagpos = dest;
			}

			/* read it */
			if (fread((char *)&val, sizeof(val), 1, pagf) == 1)
				value = MAPIN(val);
			else if (ferror(pagf)) {
				DEBUG(("search: read failed\n"));
				pagpos = -1;
				sp->aborted = 1;
				return(NOTFOUND);
			} else
				value = VACANT;

			/* and finish up */
			pagpos += sizeof(val);
		}

		/* vacant slot is always cause to return */
		if (value == VACANT) {
			DEBUG(("search: empty slot\n"));
			return(NOTFOUND);
		};

		/* check the tag */
		value = UNBIAS(value);
		DEBUG(("got 0x%lx\n", value));
		if (!HASTAG(value)) {
			DEBUG(("tagless\n"));
			return(value);
		} else if (TAG(value) == sp->tag) {
			DEBUG(("match\n"));
			return(NOTAG(value));
		} else {
			DEBUG(("mismatch 0x%lx\n", TAG(value)));
		}
	}
	/* NOTREACHED */
}

/*
 - okayvalue - check that a value can be stored
 */
static int			/* predicate */
okayvalue(value)
off_t value;
{
	if (HASTAG(value))
		return(0);
#ifdef OVERFLOW
	if (value == LONG_MAX)	/* BIAS() and UNBIAS() will overflow */
		return(0);
#endif
	return(1);
}

/*
 - set - store a value into a location previously found by search
 */
static int			/* 0 success, -1 failure */
set(sp, value)
register struct searcher *sp;
off_t value;
{
	register off_t place = sp->place;
	register off_t v = value;

	if (sp->aborted)
		return(-1);

	if (CANTAG(v) && !conf.olddbz) {
		v |= sp->tag | taghere;
		if (v != UNBIAS(VACANT))	/* BIAS(v) won't look VACANT */
#ifdef OVERFLOW
			if (v != LONG_MAX)	/* and it won't overflow */
#endif
			value = v;
	}
	DEBUG(("tagged value is 0x%lx\n", value));
	value = BIAS(value);
	value = MAPOUT(value);

	/* If we have the index file in memory, use it */
	if (corepag != NULL && place < conf.tsize) {
		corepag[place] = value;
		DEBUG(("set: incore\n"));
		return(0);
	}

	/* seek to spot */
	pagpos = -1;		/* invalidate position memory */
	if (fseek(pagf, place * SOF, SEEK_SET) != 0) {
		DEBUG(("set: seek failed\n"));
		sp->aborted = 1;
		return(-1);
	}

	/* write in data */
	if (fwrite((char *)&value, SOF, 1, pagf) != 1) {
		DEBUG(("set: write failed\n"));
		sp->aborted = 1;
		return(-1);
	}
	/* fflush improves robustness, and buffer re-use is rare anyway */
	if (fflush(pagf) == EOF) {
		DEBUG(("set: fflush failed\n"));
		sp->aborted = 1;
		return(-1);
	}

	DEBUG(("set: succeeded\n"));
	return(0);
}

/*
 - mybytemap - determine this machine's byte map
 *
 * A byte map is an array of ints, sizeof(off_t) of them.  The 0th int
 * is the byte number of the high-order byte in my off_t, and so forth.
 */
static void
mybytemap(map)
int map[];			/* -> int[SOF] */
{
	union {
		off_t o;
		char c[SOF];
	} u;
	register int *mp = &map[SOF];
	register int ntodo;
	register int i;

	u.o = 1;
	for (ntodo = (int)SOF; ntodo > 0; ntodo--) {
		for (i = 0; i < SOF; i++)
			if (u.c[i] != 0)
				break;
		if (i == SOF) {
			/* trouble -- set it to *something* consistent */
			DEBUG(("mybytemap: nonexistent byte %d!!!\n", ntodo));
			for (i = 0; i < SOF; i++)
				map[i] = i;
			return;
		}
		DEBUG(("mybytemap: byte %d\n", i));
		*--mp = i;
		while (u.c[i] != 0)
			u.o <<= 1;
	}
}

/*
 - bytemap - transform an off_t from byte ordering map1 to map2
 */
static off_t			/* transformed result */
bytemap(ino, map1, map2)
off_t ino;
int *map1;
int *map2;
{
	union oc {
		off_t o;
		char c[SOF];
	};
	union oc in;
	union oc out;
	register int i;

	in.o = ino;
	for (i = 0; i < SOF; i++)
		out.c[map2[i]] = in.c[map1[i]];
	return(out.o);
}

/*
 * This is a simplified version of the pathalias hashing function.
 * Thanks to Steve Belovin and Peter Honeyman
 *
 * hash a string into a long int.  31 bit crc (from andrew appel).
 * the crc table is computed at run time by crcinit() -- we could
 * precompute, but it takes 1 clock tick on a 750.
 *
 * This fast table calculation works only if POLY is a prime polynomial
 * in the field of integers modulo 2.  Since the coefficients of a
 * 32-bit polynomial won't fit in a 32-bit word, the high-order bit is
 * implicit.  IT MUST ALSO BE THE CASE that the coefficients of orders
 * 31 down to 25 are zero.  Happily, we have candidates, from
 * E. J.  Watson, "Primitive Polynomials (Mod 2)", Math. Comp. 16 (1962):
 *	x^32 + x^7 + x^5 + x^3 + x^2 + x^1 + x^0
 *	x^31 + x^3 + x^0
 *
 * We reverse the bits to get:
 *	111101010000000000000000000000001 but drop the last 1
 *         f   5   0   0   0   0   0   0
 *	010010000000000000000000000000001 ditto, for 31-bit crc
 *	   4   8   0   0   0   0   0   0
 */

#define POLY 0x48000000L	/* 31-bit polynomial (avoids sign problems) */

static long CrcTable[128];

/*
 - crcinit - initialize tables for hash function
 */
static void
crcinit()
{
	register int i, j;
	register long sum;

	for (i = 0; i < 128; ++i) {
		sum = 0L;
		for (j = 7 - 1; j >= 0; --j)
			if (i & (1 << j))
				sum ^= POLY >> j;
		CrcTable[i] = sum;
	}
	DEBUG(("crcinit: done\n"));
}

/*
 - hash - Honeyman's nice hashing function
 */
static long
hash(name, size)
register char *name;
register int size;
{
	register long sum = 0L;

	while (size--) {
		sum = (sum >> 7) ^ CrcTable[(sum ^ (*name++)) & 0x7f];
	}
	DEBUG(("hash: returns (%ld)\n", sum));
	return(sum);
}

/*
 * case-mapping stuff
 *
 * Borrowed from C News, by permission of the authors.  Somewhat modified.
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

#define	OFFSET	128		/* avoid trouble with negative chars */

/* must call casencmp before invoking TOLOW... */
#define	TOLOW(c)	(cmap[(c)+OFFSET])

/* ...but the use of it in CISTREQN is safe without the preliminary call (!) */
/* CISTREQN is an optimised case-insensitive strncmp(a,b,n)==0; n > 0 */
#define CISTREQN(a, b, n) \
	(TOLOW((a)[0]) == TOLOW((b)[0]) && casencmp(a, b, n) == 0)

#define	MAPSIZE	(256+OFFSET)
static char cmap[MAPSIZE];	/* relies on init to '\0' */
static int mprimed = 0;		/* has cmap been set up? */

/*
 - mapprime - set up case-mapping stuff
 */
static void
mapprime()
{
	register char *lp;
	register char *up;
	register int c;
	register int i;
	static char lower[] = "abcdefghijklmnopqrstuvwxyz";
	static char upper[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

	for (lp = lower, up = upper; *lp != '\0'; lp++, up++) {
		c = *lp;
		cmap[c+OFFSET] = c;
		cmap[*up+OFFSET] = c;
	}
	for (i = 0; i < MAPSIZE; i++)
		if (cmap[i] == '\0')
			cmap[i] = (char)(i-OFFSET);
	mprimed = 1;
}

/*
 - casencmp - case-independent strncmp
 */
static int			/* < == > 0 */
casencmp(s1, s2, len)
char *s1;
char *s2;
int len;
{
	register char *p1;
	register char *p2;
	register int n;

	if (!mprimed)
		mapprime();

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
 - mapcase - do case-mapped copy
 */
static char *			/* returns src or dst */
mapcase(dst, src, siz)
char *dst;			/* destination, used only if mapping needed */
char *src;			/* source; src == dst is legal */
size_t siz;
{
	register char *s;
	register char *d;
	register char *c;	/* case break */
	register char *e;	/* end of source */


	c = cipoint(src, siz);
	if (c == NULL)
		return(src);

	if (!mprimed)
		mapprime();
	s = src;
	e = s + siz;
	d = dst;

	while (s < c)
		*d++ = *s++;
	while (s < e)
		*d++ = TOLOW(*s++);

	return(dst);
}

/*
 - cipoint - where in this message-ID does it become case-insensitive?
 *
 * The RFC822 code is not quite complete.  Absolute, total, full RFC822
 * compliance requires a horrible parsing job, because of the arcane
 * quoting conventions -- abc"def"ghi is not equivalent to abc"DEF"ghi,
 * for example.  There are three or four things that might occur in the
 * domain part of a message-id that are case-sensitive.  They don't seem
 * to ever occur in real news, thank Cthulhu.  (What?  You were expecting
 * a merciful and forgiving deity to be invoked in connection with RFC822?
 * Forget it; none of them would come near it.)
 */
static char *			/* pointer into s, or NULL for "nowhere" */
cipoint(s, siz)
char *s;
size_t siz;
{
	register char *p;
	static char post[] = "postmaster";
	static int plen = sizeof(post)-1;

	switch (conf.casemap) {
	case '0':		/* unmapped, sensible */
		return(NULL);
		break;
	case 'C':		/* C News, RFC 822 conformant (approx.) */
		p = memchr(s, '@', siz);
		if (p == NULL)			/* no local/domain split */
			return(NULL);		/* assume all local */
		else if	(p - (s+1) == plen && CISTREQN(s+1, post, plen)) {
			/* crazy -- "postmaster" is case-insensitive */
			return(s);
		} else
			return(p);
		break;
	case '=':		/* 2.11, neither sensible nor conformant */
		return(s);	/* all case-insensitive */
		break;
	}

	DEBUG(("cipoint: unknown case mapping `%c'\n", conf.casemap));
	return(NULL);		/* just leave it alone */
}

/*
 - dbzdebug - control dbz debugging at run time
 */
int				/* old value */
dbzdebug(value)
int value;
{
#ifdef DBZDEBUG
	register int old = debug;

	debug = value;
	return(old);
#else
	return(-1);
#endif
}
