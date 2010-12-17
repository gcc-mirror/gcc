dnl Get type kind from filename.
define(kind,regexp(file, `_.\([0-9]+\).c$', `\1'))dnl
define(complex_type, `GFC_COMPLEX_'kind)dnl
define(real_type, `GFC_REAL_'kind)dnl
define(`upcase', `translit(`$*', `a-z', `A-Z')')dnl
define(q,ifelse(kind,4,f,ifelse(kind,8,`',ifelse(kind,10,l,ifelse(kind,16,l,`_'kind)))))dnl
define(Q,translit(q,`a-z',`A-Z'))dnl
define(hasmathfunc,`ifelse(kind,4,`defined (HAVE_'upcase($1)`F)',ifelse(kind,8,`defined (HAVE_'upcase($1)`)',ifelse(kind,10,`defined (HAVE_'upcase($1)`L)',ifelse(kind,16,`(defined(GFC_REAL_16_IS_FLOAT128) || defined(HAVE_'upcase($1)`L))',`error out'))))')
define(mathfunc_macro,`ifelse(kind,16,`#if defined(GFC_REAL_16_IS_FLOAT128)
#define MATHFUNC(funcname) funcname ## q
#else
#define MATHFUNC(funcname) funcname ## l
#endif',ifelse(kind,8,``#''`define MATHFUNC(funcname) funcname',```#'''`define MATHFUNC(funcname) funcname '```#'''```#'''` 'q))')dnl
