/*

@deftypefn Supplemental char* strdup (const char *@var{s})

Returns a pointer to a copy of @var{s} in memory obtained from
@code{malloc}, or @code{NULL} if insufficient memory was available.

@end deftypefn

*/

char *
strdup(s)
     char *s;
{
    char *result = (char*)malloc(strlen(s) + 1);
    if (result == (char*)0)
	return (char*)0;
    strcpy(result, s);
    return result;
}
