/* Some of these are defined here, not in configure.in, because
   they're AC_DEFINEd in two different places, which causes two
   defines to appear.  Some C compilers might now appreciate it...  */

/* Define if you have the libdl library or equivalent.  */
#undef HAVE_LIBDL

/* Define if you have the GNU dld library.  */
#undef HAVE_DLD

/* Define if you have the shl_load function.  */
#undef HAVE_SHL_LOAD

/* Define if you are using the Boehm GC.  */
#undef HAVE_BOEHM_GC
