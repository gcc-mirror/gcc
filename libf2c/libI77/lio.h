/*	copy of ftypes from the compiler */
/* variable types
 * numeric assumptions:
 *	int < reals < complexes
 *	TYDREAL-TYREAL = TYDCOMPLEX-TYCOMPLEX
 */

/* 0-10 retain their old (pre LOGICAL*1, etc.) */
/* values to allow mixing old and new objects. */

#define TYUNKNOWN 0
#define TYADDR 1
#define TYSHORT 2
#define TYLONG 3
#define TYREAL 4
#define TYDREAL 5
#define TYCOMPLEX 6
#define TYDCOMPLEX 7
#define TYLOGICAL 8
#define TYCHAR 9
#define TYSUBR 10
#define TYINT1 11
#define TYLOGICAL1 12
#define TYLOGICAL2 13
#ifdef Allow_TYQUAD
#undef TYQUAD
#define TYQUAD 14
#endif

#define	LINTW	24
#define	LINE	80
#define	LLOGW	2
#ifdef Old_list_output
#define	LLOW	1.0
#define	LHIGH	1.e9
#define	LEFMT	" %# .8E"
#define	LFFMT	" %# .9g"
#else
#define	LGFMT	"%.9G"
#endif
/* LEFBL 20 should suffice; 24 overcomes a NeXT bug. */
#define	LEFBL	24

typedef union
{
  signed char flchar;
  short flshort;
  ftnint flint;
#ifdef Allow_TYQUAD
  longint fllongint;
#endif
  real flreal;
  doublereal fldouble;
}
flex;
extern int f__scale;
extern int (*f__lioproc) (ftnint *, char *, ftnlen, ftnint);
extern int l_write (ftnint *, char *, ftnlen, ftnint);
extern void x_wsne (cilist *);
extern int c_le (cilist *), (*l_getc) (void), (*l_ungetc) (int, FILE *);
extern int l_read (ftnint *, char *, ftnlen, ftnint);
extern integer e_rsle (void), e_wsle (void), s_wsne (cilist *);
extern int z_rnew (void);
extern ftnint L_len;
