/* Name of this package.  */
#undef PACKAGE

/* Version number of this package.  */
#undef VERSION

/* Define this if you want runtime debugging enabled.  */
#undef DEBUG

/* Define if using POSIX threads that have the mutexattr functions.  */
#undef HAVE_PTHREAD_MUTEXATTR_INIT

/* Define this if you prefer size over speed for java.lang.Character.  */
#undef COMPACT_CHARACTER

/* Define if you have memcpy.  */
#undef HAVE_MEMCPY

/* Define if you have memmove.  */
#undef HAVE_MEMMOVE

/* Define if you have strerror.  */
#undef HAVE_STRERROR

/* Define if you have fsync.  */
#undef HAVE_FSYNC

/* Define if you have sleep.  */
#undef HAVE_SLEEP

/* Define if you have int32_t and uint32_t. */
#undef HAVE_INT32_DEFINED

/* Define if you have u_int32_t */
#undef HAVE_BSD_INT32_DEFINED

/* Define if you're running eCos. */
#undef ECOS

/* */
#undef HAVE_LOCALTIME

/* */
#undef HAVE_MKTIME

/* Define if using POSIX threads on Linux.  */
#undef LINUX_THREADS

/* Define if you have the `gmtime_r' function.  */
#undef HAVE_GMTIME_R

/* Define if you have the `localtime_r' function.  */
#undef HAVE_LOCALTIME_R

/* Define to `int' if `ssize_t' is not defined.  */
#undef ssize_t

/* Define to 1 if `in_addr_t' is defined in sys/types.h or
   netinet/in.h.  */
#undef HAVE_IN_ADDR_T

/* Define if inet6 structures are defined in netinet/in.h.  */
#undef HAVE_INET6

/* Define if struct ip_mreq is defined in netinet/in.h.  */
#undef HAVE_STRUCT_IP_MREQ

/* Define it socklen_t typedef is in sys/socket.h.  */
#undef HAVE_SOCKLEN_T

/* Define if Boehm GC in use.  */
#undef HAVE_BOEHM_GC

/* Define if gethostname is declared in <unistd.h>.  */
#undef HAVE_GETHOSTNAME_DECL

/* Define if gethostbyname_r returns `int'.  */
#undef GETHOSTBYNAME_R_RETURNS_INT

/* Define if gethostbyaddr_r returns `int'.  */
#undef GETHOSTBYADDR_R_RETURNS_INT

/* Define if struct tm has tm_gmtoff field.  */
#undef STRUCT_TM_HAS_GMTOFF

/* Define if global `timezone' exists.  */
#undef HAVE_TIMEZONE

/* Define to version of GCJ in use.  */
#undef GCJVERSION

/* Define if you have the appropriate function.  */
#undef HAVE_ACCESS
#undef HAVE_STAT
#undef HAVE_MKDIR
#undef HAVE_RENAME
#undef HAVE_RMDIR
#undef HAVE_UNLINK
#undef HAVE_REALPATH
#undef HAVE_READDIR_R
#undef HAVE_GETHOSTBYNAME_R
#undef HAVE_GETHOSTBYADDR_R

/* Define if you want a bytecode interpreter.  */
#undef INTERPRETER

/* Define if pthread_mutex_t has m_count member.  */
#undef PTHREAD_MUTEX_HAVE_M_COUNT

/* Define if pthread_mutex_t has __m_count member.  */
#undef PTHREAD_MUTEX_HAVE___M_COUNT

/* Define if java.net native functions should be stubbed out.  */
#undef DISABLE_JAVA_NET

/* Define if system properties shouldn't be read from
   getenv("GCJ_PROPERTIES").  */
#undef DISABLE_GETENV_PROPERTIES

/* Define if using setjmp/longjmp exceptions.  */
#undef SJLJ_EXCEPTIONS

/* Define if you have /proc/self/exe */
#undef HAVE_PROC_SELF_EXE
 
/* Define if getuid() and friends are missing.  */
#undef NO_GETUID
