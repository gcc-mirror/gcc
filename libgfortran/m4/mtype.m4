dnl Get type kind from filename.
define(kind,regexp(file, `_.\([0-9]+\).c$', `\1'))dnl
define(complex_type, `GFC_COMPLEX_'kind)dnl
define(real_type, `GFC_REAL_'kind)dnl
define(`upcase', `translit(`$*', `a-z', `A-Z')')dnl
define(q,ifelse(kind,4,f,ifelse(kind,8,`',ifelse(kind,10,l,ifelse(kind,16,l,`_'kind)))))dnl
define(Q,translit(q,`a-z',`A-Z'))dnl
define(hasmathfunc,dnl
`ifelse(kind,4,`defined (HAVE_'upcase($1)`F)',dnl
ifelse(kind,8,`defined (HAVE_'upcase($1)`)',dnl
ifelse(kind,10,`defined (HAVE_'upcase($1)`L)',dnl
ifelse(kind,16,`(defined(GFC_REAL_16_IS_FLOAT128) || defined(HAVE_'upcase($1)`L))',dnl
ifelse(kind,17,`1 /* FIXME: figure this out later.  */',dnl
`error out')))))')
define(mathfunc_macro,`ifelse(kind,17,dnl
`#if defined(POWER_IEEE128)
#define MATHFUNC(funcname) __ ## funcname ## ieee128
#elif defined(GFC_REAL_17_USE_IEC_60559)
#define MATHFUNC(funcname) funcname ## f128
#else
#define MATHFUNC(funcname) funcname ## q
#endif',dnl
`ifelse(kind,16,dnl
`#if defined(GFC_REAL_16_IS_FLOAT128)
#if defined(GFC_REAL_16_USE_IEC_60559)
#define MATHFUNC(funcname) funcname ## f128
#else
#define MATHFUNC(funcname) funcname ## q
#endif
#else
#define MATHFUNC(funcname) funcname ## l
#endif',dnl
ifelse(kind,8,``#''`define MATHFUNC(funcname) funcname',dnl
```#'''`define MATHFUNC(funcname) funcname '```#'''```#'''` 'q))')')dnl
