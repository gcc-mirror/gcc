#include <stdio.h>
#include <stdint.h>

#include "config.h"  /* For HAVE_INTTYPES_H.  */
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>  /* For PRId64.  */
#endif

void
gomp_print_string (const char *msg, const char *value)
{
  printf ("%s%s\n", msg, value);
}

void
gomp_print_integer (const char *msg, int64_t value)
{
#ifdef HAVE_INTTYPES_H
  printf ("%s%" PRId64 "\n", msg, value);
#else
  printf ("%s%ld\n", msg, (long) value);
#endif
}

void
gomp_print_double (const char *msg, double value)
{
  printf ("%s%f\n", msg, value);
}
