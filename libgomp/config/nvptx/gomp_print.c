#include <stdio.h>
#include <stdint.h>

void
gomp_print_string (const char *msg, const char *value)
{
  printf ("%s%s\n", msg, value);
}

void
gomp_print_integer (const char *msg, int64_t value)
{
  printf ("%s%ld\n", msg, value);
}

void
gomp_print_double (const char *msg, double value)
{
  printf ("%s%f\n", msg, value);
}
