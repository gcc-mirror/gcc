#include "config.h"
#include "f2c.h"
#include "fio.h"
#include "fmt.h"
#include "lio.h"
#include "string.h"

integer
s_wsle (cilist * a)
{
  int n;
  if ((n = c_le (a)))
    return (n);
  f__reading = 0;
  f__external = 1;
  f__formatted = 1;
  f__putn = x_putc;
  f__lioproc = l_write;
  L_len = LINE;
  f__donewrec = x_wSL;
  if (f__curunit->uwrt != 1 && f__nowwriting (f__curunit))
    err (a->cierr, errno, "list output start");
  return (0);
}

integer
e_wsle (void)
{
  int n;
  f__init = 1;
  n = f__putbuf ('\n');
  f__recpos = 0;
#ifdef ALWAYS_FLUSH
  if (!n && fflush (f__cf))
    err (f__elist->cierr, errno, "write end");
#endif
  return (n);
}
