/* --------------------------------- SHS.H ------------------------------- */

/*
 * NIST proposed Secure Hash Standard. 
 *
 * Written 2 September 1992, Peter C. Gutmann.
 * This implementation placed in the public domain. 
 *
 * Comments to pgut1@cs.aukuni.ac.nz 
 */

/* Useful defines/typedefs */

#ifndef SHS_H
#define SHS_H

typedef unsigned char BYTE;
typedef unsigned int LONG; /* A 32-bit type */

/* The SHS block size and message digest sizes, in bytes */

#define SHS_BLOCKSIZE	64
#define SHS_DIGESTSIZE	20

/* The structure for storing SHS info */

typedef struct {
	LONG digest [5];	/* Message digest */
	LONG countLo, countHi;	/* 64-bit bit count */
	LONG data [16];		/* SHS data buffer */
} SHS_INFO;

/* Turn off prototypes if requested */
#if (defined(NOPROTO) && defined(PROTO))
#	undef PROTO
#endif

/* Used to remove arguments in function prototypes for non-ANSI C */
#ifdef PROTO
#	define OF(a) a
#else	/* !PROTO */
#	define OF(a) ()
#endif	/* ?PROTO */

#define	local	static

void shsInit OF((SHS_INFO *shsInfo));
void shsUpdate OF((SHS_INFO *shsInfo, BYTE *buffer, int count));
void shsFinal OF((SHS_INFO *shsInfo));

#endif
