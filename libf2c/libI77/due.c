#include "config.h"
#include "f2c.h"
#include "fio.h"

int
c_due (cilist * a)
{
  if (f__init != 1)
    f_init ();
  f__init = 3;
  if (a->ciunit >= MXUNIT || a->ciunit < 0)
    err (a->cierr, 101, "startio");
  f__sequential = f__formatted = f__recpos = 0;
  f__external = 1;
  f__curunit = &f__units[a->ciunit];
  if (a->ciunit >= MXUNIT || a->ciunit < 0)
    err (a->cierr, 101, "startio");
  f__elist = a;
  if (f__curunit->ufd == NULL && fk_open (DIR, UNF, a->ciunit))
    err (a->cierr, 104, "due");
  f__cf = f__curunit->ufd;
  if (f__curunit->ufmt)
    err (a->cierr, 102, "cdue");
  if (!f__curunit->useek)
    err (a->cierr, 104, "cdue");
  if (f__curunit->ufd == NULL)
    err (a->cierr, 114, "cdue");
  if (a->cirec <= 0)
    err (a->cierr, 130, "due");
  FSEEK (f__cf, (off_t) (a->cirec - 1) * f__curunit->url, SEEK_SET);
  f__curunit->uend = 0;
  return (0);
}

integer
s_rdue (cilist * a)
{
  int n;
  f__reading = 1;
  if ((n = c_due (a)))
    return (n);
  if (f__curunit->uwrt && f__nowreading (f__curunit))
    err (a->cierr, errno, "read start");
  return (0);
}

integer
s_wdue (cilist * a)
{
  int n;
  f__reading = 0;
  if ((n = c_due (a)))
    return (n);
  if (f__curunit->uwrt != 1 && f__nowwriting (f__curunit))
    err (a->cierr, errno, "write start");
  return (0);
}

integer
e_rdue (void)
{
  f__init = 1;
  if (f__curunit->url == 1 || f__recpos == f__curunit->url)
    return (0);
  FSEEK (f__cf, (off_t) (f__curunit->url - f__recpos), SEEK_CUR);
  if (FTELL (f__cf) % f__curunit->url)
    err (f__elist->cierr, 200, "syserr");
  return (0);
}

integer
e_wdue (void)
{
  f__init = 1;
#ifdef ALWAYS_FLUSH
  if (fflush (f__cf))
    err (f__elist->cierr, errno, "write end");
#endif
  return (e_rdue ());
}
