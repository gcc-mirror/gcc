/* You may need to adjust the definition of signal1 to supply a */
/* cast to the correct argument type.  This detail is system- and */
/* compiler-dependent.   The #define below assumes signal.h declares */
/* type SIG_PF for the signal function's second argument. */

/* For some C++ compilers, "#define Sigarg_t ..." may be appropriate. */

#include <signal.h>

#ifndef Sigret_t
#define Sigret_t void
#endif
#ifndef Sigarg_t
#ifdef KR_headers
#define Sigarg_t
#else
#define Sigarg_t int
#endif
#endif /*Sigarg_t*/

#ifdef USE_SIG_PF	/* compile with -DUSE_SIG_PF under IRIX */
#define sig_pf SIG_PF
#else
typedef Sigret_t (*sig_pf)(Sigarg_t);
#endif

#define signal1(a,b) signal(a,(sig_pf)b)

#ifdef __cplusplus
#define Sigarg ...
#define Use_Sigarg
#else
#define Sigarg Int n
#define Use_Sigarg n = n	/* shut up compiler warning */
#endif
