/* "Normal" configuration for alloca.  */

#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* ! defined (__GNUC__) */
#if defined (sparc) && defined (sun)
#include <alloca.h>
#ifdef __STDC__
extern void *__builtin_alloca();
#else /* ! defined (__STDC__) */
extern char *__builtin_alloca();  /* Stupid include file doesn't declare it */
#endif /* ! defined (__STDC__) */
#else /* ! defined (sparc) || ! defined (sun) */
#ifdef __STDC__
PTR alloca (size_t);
#else /* ! defined (__STDC__) */
PTR alloca ();			/* must agree with functions.def */
#endif /* ! defined (__STDC__) */
#endif /* ! defined (sparc) || ! defined (sun) */
#ifdef _WIN32
#include <malloc.h>
#endif
#endif /* ! defined (__GNUC__) */
