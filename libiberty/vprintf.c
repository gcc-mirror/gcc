#include <stdio.h>
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include <ansidecl.h>
#undef vprintf
int
vprintf (format, ap)
     const char *format;
     va_list ap;
{
  return vfprintf (stdout, format, ap);
}
