/* Simple implementation of strstr for systems without it.
   This function is in the public domain.  */

/*

NAME

	strstr -- locate first occurance of a substring

SYNOPSIS

	#include <string.h>

	char *strstr (char *s1, char *s2)

DESCRIPTION

	Locates the first occurance in the string pointed to by S1 of
	the string pointed to by S2.  Returns a pointer to the substring
	found, or a NULL pointer if not found.  If S2 points to a string
	with zero length, the function returns S1.
	
BUGS

*/


/* FIXME:  The above description is ANSI compiliant.  This routine has not
   been validated to comply with it.  -fnf */

char *
strstr (s1, s2)
  char *s1, *s2;
{
  register char *p = s1;
  extern char *strchr ();
  extern int strncmp ();
#if __GNUC__==2
  extern __SIZE_TYPE__ strlen ();
#endif
  register int len = strlen (s2);

  for (; (p = strchr (p, *s2)) != 0; p++)
    {
      if (strncmp (p, s2, len) == 0)
	{
	  return (p);
	}
    }
  return (0);
}
