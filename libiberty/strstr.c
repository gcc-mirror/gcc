/* Simple implementation of strstr for systems without it.
   This function is in the public domain.  */

/*

@deftypefn Supplemental char* strstr (const char *@var{string}, const char *@var{sub})

This function searches for the substring @var{sub} in the string
@var{string}, not including the terminating null characters.  A pointer
to the first occurrence of @var{sub} is returned, or @code{NULL} if the
substring is absent.  If @var{sub} points to a string with zero
length, the function returns @var{string}.

@end deftypefn


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
#if __GNUC__ >= 2
  extern __SIZE_TYPE__ strlen (const char *);
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
