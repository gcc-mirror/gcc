#include "config.h"
#include "f2c.h"
#include "fio.h"

static FILE *
unit_chk (integer Unit, char *who)
{
  if (Unit >= MXUNIT || Unit < 0)
    f__fatal (101, who);
  return f__units[Unit].ufd;
}

integer
G77_ftell_0 (integer * Unit)
{
  FILE *f;
  return (f = unit_chk (*Unit, "ftell")) ? (integer) FTELL (f) : -1L;
}

integer
G77_fseek_0 (integer * Unit, integer * offset, integer * xwhence)
{
  FILE *f;
  int w = (int) *xwhence;
#ifdef SEEK_SET
  static int wohin[3] = { SEEK_SET, SEEK_CUR, SEEK_END };
#endif
  if (w < 0 || w > 2)
    w = 0;
#ifdef SEEK_SET
  w = wohin[w];
#endif
  return !(f = unit_chk (*Unit, "fseek"))
    || FSEEK (f, (off_t) * offset, w) ? 1 : 0;
}
