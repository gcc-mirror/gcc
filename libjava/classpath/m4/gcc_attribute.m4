dnl CACHED_TRY_COMPILE(<description>,<cachevar>,<include>,<program>,<ifyes>,<ifno>)
AC_DEFUN([CACHED_TRY_COMPILE],[
 AC_MSG_CHECKING($1)
 AC_CACHE_VAL($2,[
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[$3]], [[$4]])],[$2=yes],[$2=no])
 ])
 if test "x$$2" = xyes; then
  true
  $5
 else
  true
  $6
 fi
])

dnl GCC_ATTRIBUTE(<short-label>,<cachevar>,<func-params>,<attribute>,<HAVE>,<desc>,[<true-cmds>],[<false-cmds>])
AC_DEFUN([GCC_ATTRIBUTE],[
  CACHED_TRY_COMPILE(__attribute__(($1)),gcc_cv_c_gcc_attribute_$2,,
   [extern int testfunction($3) __attribute__(($4))],
   AC_MSG_RESULT(yes)
   AC_DEFINE(HAVE_GNUC25_$5,,$6)
   $7,
   AC_MSG_RESULT(no)
   $8)
])


AC_DEFUN([GCC_ATTRIBUTE_SUPPORTED],[
 GCC_ATTRIBUTE([,,],supported,[int x],[,,],ATTRIB,[Define if function attributes a la GCC 2.5 and higher are available.])
 AH_BOTTOM([/* GNU C attributes. */
#ifndef FUNCATTR
#ifdef HAVE_GNUC25_ATTRIB
#define FUNCATTR(x) __attribute__(x)
#else
#define FUNCATTR(x)
#endif
#endif])

])
AC_DEFUN([GCC_ATTRIBUTE_CONST],[
 AC_REQUIRE([GCC_ATTRIBUTE_SUPPORTED])
 GCC_ATTRIBUTE(const,const,[int x],const,CONST,[Define if constant functions a la GCC 2.5 and higher are available.])
 AH_BOTTOM([/* GNU C constant functions, or null. */
#ifndef ATTRCONST
#ifdef HAVE_GNUC25_CONST
#define ATTRCONST const
#else
#define ATTRCONST
#endif
#endif
#ifndef CONSTANT
#define CONSTANT FUNCATTR((ATTRCONST))
#endif])
])
AC_DEFUN([GCC_ATTRIBUTE_NORETURN],[
 AC_REQUIRE([GCC_ATTRIBUTE_SUPPORTED])
 GCC_ATTRIBUTE(noreturn,noreturn,[int x],noreturn,NORETURN,[Define if nonreturning functions a la GCC 2.5 and higher are available.])
 AH_BOTTOM([/* GNU C nonreturning functions, or null. */
#ifndef ATTRNORETURN
#ifdef HAVE_GNUC25_NORETURN
#define ATTRNORETURN noreturn
#else /* ! HAVE_GNUC25_NORETURN */
#define ATTRNORETURN
#endif /* HAVE_GNUC25_NORETURN */
#endif /* ATTRNORETURN */
#ifndef NONRETURNING
#define NONRETURNING FUNCATTR((ATTRNORETURN))
#endif /* NONRETURNING */])
])
AC_DEFUN([GCC_ATTRIBUTE_UNUSED],[
 AC_REQUIRE([GCC_ATTRIBUTE_SUPPORTED])
 GCC_ATTRIBUTE(unused,unused,[int x],unused,UNUSED,[Define if unused variables la GCC 2.5 and higher are available.])
 AH_BOTTOM([/* GNU C unused functions, or null. */
#ifndef ATTRUNUSED
#ifdef HAVE_GNUC25_UNUSED
#define ATTRUNUSED unused
#else
#define ATTRUNUSED
#endif
#endif
#ifndef UNUSED
#define UNUSED FUNCATTR((ATTRUNUSED))
#endif])
])
AC_DEFUN([GCC_ATTRIBUTE_FORMAT],[
 AC_REQUIRE([GCC_ATTRIBUTE_SUPPORTED])
 GCC_ATTRIBUTE(format...,format,[char *y, ...],[format(printf,1,2)],PRINTFFORMAT,[Define if printf-format argument lists a la GCC are available.])
 AH_BOTTOM([/* GNU C printf formats, or null. */
#ifndef ATTRPRINTF
#ifdef HAVE_GNUC25_PRINTFFORMAT
#define ATTRPRINTF(si,tc) format(printf,si,tc)
#else
#define ATTRPRINTF(si,tc)
#endif
#endif
#ifndef PRINTFFORMAT
#define PRINTFFORMAT(si,tc) FUNCATTR((ATTRPRINTF(si,tc)))
#endif

#ifndef NONRETURNPRINTFFORMAT
#define NONRETURNPRINTFFORMAT(si,tc) FUNCATTR((ATTRPRINTF(si,tc),ATTRNORETURN))
#endif])
])
AC_DEFUN([GCC_ATTRIBUTE_ALWAYS_INLINE],[
 AC_REQUIRE([GCC_ATTRIBUTE_SUPPORTED])
 GCC_ATTRIBUTE(always_inline,always_inline,[int x],always_inline,ALWAYS_INLINE,[Define if unconditional inlining of functions a la GCC 3.1 and higher are available.])
 AH_BOTTOM([/* GNU C constant functions, or null. */
#ifndef ATTRALWAYS_INLINE
#ifdef HAVE_GNUC25_ALWAYS_INLINE
#define ATTRALWAYS_INLINE always_inline
#else
#define ATTRALWAYS_INLINE
#endif
#endif
#ifndef ALWAYS_INLINE
#define ALWAYS_INLINE FUNCATTR((ATTRALWAYS_INLINE))
#endif])
])
AC_DEFUN([GCC_ATTRIBUTE_PACKED],[
 AC_REQUIRE([GCC_ATTRIBUTE_SUPPORTED])
 GCC_ATTRIBUTE(packed,packed,[int x],packed,PACKED,[Define if packing of struct members a la GCC 2.5 and higher is available.])
 AH_BOTTOM([/* GNU C constant functions, or null. */
#ifndef ATTRPACKED
#ifdef HAVE_GNUC25_PACKED
#define ATTRPACKED packed
#else
#define ATTRPACKED
#endif
#endif
#ifndef PACKED
#define PACKED FUNCATTR((ATTRPACKED))
#endif])
])
