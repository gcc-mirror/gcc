/* Emulate getcwd using getwd.
   This function is in the public domain. */

/*
NAME
	getcwd -- get absolute pathname for current working directory

SYNOPSIS
	char *getcwd (char pathname[len], len)

DESCRIPTION
	Copy the absolute pathname for the current working directory into
	the supplied buffer and return a pointer to the buffer.  If the 
	current directory's path doesn't fit in LEN characters, the result
	is NULL and errno is set.

BUGS
	Emulated via the getwd() call, which is reasonable for most
	systems that do not have getcwd().

*/

#ifndef NO_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <errno.h>

extern char *getwd ();
extern int errno;

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

char *
getcwd (buf, len)
  char *buf;
  int len;
{
  char ourbuf[MAXPATHLEN];
  char *result;

  result = getwd (ourbuf);
  if (result) {
    if (strlen (ourbuf) >= len) {
      errno = ERANGE;
      return 0;
    }
    strcpy (buf, ourbuf);
  }
  return buf;
}
