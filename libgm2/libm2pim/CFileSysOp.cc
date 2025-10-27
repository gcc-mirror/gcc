

/* A C++ implementation for CFileSysOp.def.  This file will use
   autoconf to test for the presence of operating system facilities.  */

#include <config.h>
#include <m2rts.h>

#define EXPORT(FUNC) m2pim ## _CFileSysOp_ ## FUNC
#define M2EXPORT(FUNC) m2pim ## _M2_CFileSysOp_ ## FUNC
#define M2LIBNAME "m2pim"

#if defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif

#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif

#if defined(HAVE_SYS_STAT_H)
#include <sys/stat.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#if defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

/* Define a generic NULL if one hasn't already been defined.  */

#if !defined(NULL)
#define NULL 0
#endif

#define A_FAIL (1<<5)


extern "C" int
EXPORT(Unlink) (char *filename)
{
#if defined(HAVE_UNLINK)
  return unlink (filename);
#else
  return -1;
#endif
}


/* Access test access to a path or file.  The behavior is
   the same as defined in access(2).  Except that A_FAIL
   is only used during the return result indicating the
   underlying C access has returned -1 (and errno can be
   checked).  */

extern "C" int
EXPORT(Access) (char *pathname, int mode)
{
#if defined(HAVE_ACCESS)
  int result = access (pathname, mode);

  if (result == -1)
    return A_FAIL;
  return result;
#else
  return A_FAIL;
#endif
}


/* Exists return true if pathname exists.  */

extern "C" bool
EXPORT(Exists) (char *pathname)
{
#if defined(HAVE_ACCESS)
  return (access (pathname, F_OK) == 0);
#else
  return false;
#endif
}

/* IsDir return true if filename is a regular directory.  */

extern "C" bool
EXPORT(IsDir) (char *dirname)
{
#if defined(HAVE_SYS_STAT_H) && defined(HAVE_STRUCT_STAT)
  struct stat dir_stat;
  int res = stat (dirname, (struct stat *)&dir_stat);
  if (res == 0)
    return (dir_stat.st_mode & S_IFMT) == S_IFDIR;
  return false;
#else
  return false;
#endif
}

/* IsFile return true if filename is a regular file.  */

extern "C" bool
EXPORT(IsFile) (char *filename)
{
#if defined(HAVE_SYS_STAT_H) && defined(HAVE_STRUCT_STAT)
  struct stat file_stat;
  int res = stat (filename, (struct stat *)&file_stat);
  if (res == 0)
    return (file_stat.st_mode & S_IFMT) == S_IFREG;
  return false;
#else
  return false;
#endif
}

/* GNU Modula-2 linking hooks.  */

extern "C" void
M2EXPORT(init) (int, char **, char **)
{
}

extern "C" void
M2EXPORT(fini) (int, char **, char **)
{
}

extern "C" void
M2EXPORT(dep) (void)
{
}

extern "C" void __attribute__((__constructor__))
M2EXPORT(ctor) (void)
{
  m2pim_M2RTS_RegisterModule ("CfileSysOp", M2LIBNAME,
			      M2EXPORT(init), M2EXPORT(fini),
			      M2EXPORT(dep));
}
