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

#include<config.h>
#if HAVE_INTTYPES_H
# include <inttypes.h>
#else
# if HAVE_STDINT_H
#  include <stdint.h>
# else
typedef unsigned int uint8_t __attribute__((mode(QI)));
/* This is a blatant hack: on Solaris 2.5, pthread.h defines uint32_t
   in pthread.h, which we sometimes include.  We protect our
   definition the same way Solaris 2.5 does, to avoid redefining it.  */
#  ifndef _UINT32_T
typedef unsigned int uint32_t __attribute__((mode(SI)));
#  endif
# endif
#endif

/* The SHS block size and message digest sizes, in bytes */

#define SHS_BLOCKSIZE	64
#define SHS_DIGESTSIZE	20

/* The structure for storing SHS info */

typedef struct {
       uint32_t digest [5];    /* Message digest */
       uint32_t countLo, countHi;      /* 64-bit bit count */
       uint32_t data [16];             /* SHS data buffer */
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
void shsUpdate OF((SHS_INFO *shsInfo, uint8_t *buffer, int count));
void shsFinal OF((SHS_INFO *shsInfo));

#endif
