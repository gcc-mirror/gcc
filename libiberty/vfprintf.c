#include <stdio.h>
#include <varargs.h>
#include <ansidecl.h>
#undef vfprintf

int
vfprintf (file, format, ap)
     FILE *file;
     const char *format;
     va_list ap;
{
   return _doprnt (format, ap, file);
}
