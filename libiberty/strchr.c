/* Portable version of strchr()
   This function is in the public domain.  */

/*
NAME
	strchr -- return pointer to first occurance of a character

SYNOPSIS
	char *strchr (const char *s, int c)

DESCRIPTION
	Returns a pointer to the first occurance of character C in
	string S, or a NULL pointer if no occurance is found.
	
BUGS
	Behavior when character is the null character is implementation
	dependent.
*/

#include <ansidecl.h>

char *
strchr (s, c)
  register CONST char *s;
  int c;
{
  do {
    if (*s == c)
      {
	return (char*)s;
      }
  } while (*s++);
  return (0);
}
