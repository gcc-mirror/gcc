#include "config.h"
#include "f2c.h"
#include "fio.h"
#include "lio.h"

#define MAX_NL_CACHE 3		/* maximum number of namelist hash tables to cache */
#define MAXDIM 20		/* maximum number of subscripts */

struct dimen
{
  ftnlen extent;
  ftnlen curval;
  ftnlen delta;
  ftnlen stride;
};
typedef struct dimen dimen;

struct hashentry
{
  struct hashentry *next;
  char *name;
  Vardesc *vd;
};
typedef struct hashentry hashentry;

struct hashtab
{
  struct hashtab *next;
  Namelist *nl;
  int htsize;
  hashentry *tab[1];
};
typedef struct hashtab hashtab;

static hashtab *nl_cache;
static int n_nlcache;
static hashentry **zot;
static int colonseen;
extern ftnlen f__typesize[];

extern flag f__lquit;
extern int f__lcount, nml_read;
extern int t_getc (void);

#undef abs
#undef min
#undef max
#include <stdlib.h>
#include <string.h>

#ifdef ungetc
static int
un_getc (int x, FILE * f__cf)
{
  return ungetc (x, f__cf);
}
#else
#define un_getc ungetc
extern int ungetc (int, FILE *);	/* for systems with a buggy stdio.h */
#endif

static Vardesc *
hash (hashtab * ht, register char *s)
{
  register int c, x;
  register hashentry *h;
  char *s0 = s;

  for (x = 0; (c = *s++); x = x & 0x4000 ? ((x << 1) & 0x7fff) + 1 : x << 1)
    x += c;
  for (h = *(zot = ht->tab + x % ht->htsize); h; h = h->next)
    if (!strcmp (s0, h->name))
      return h->vd;
  return 0;
}

hashtab *
mk_hashtab (Namelist * nl)
{
  int nht, nv;
  hashtab *ht;
  Vardesc *v, **vd, **vde;
  hashentry *he;

  hashtab **x, **x0, *y;
  for (x = &nl_cache; (y = *x); x0 = x, x = &y->next)
    if (nl == y->nl)
      return y;
  if (n_nlcache >= MAX_NL_CACHE)
    {
      /* discard least recently used namelist hash table */
      y = *x0;
      free ((char *) y->next);
      y->next = 0;
    }
  else
    n_nlcache++;
  nv = nl->nvars;
  if (nv >= 0x4000)
    nht = 0x7fff;
  else
    {
      for (nht = 1; nht < nv; nht <<= 1);
      nht += nht - 1;
    }
  ht = (hashtab *) malloc (sizeof (hashtab) + (nht - 1) * sizeof (hashentry *)
			   + nv * sizeof (hashentry));
  if (!ht)
    return 0;
  he = (hashentry *) & ht->tab[nht];
  ht->nl = nl;
  ht->htsize = nht;
  ht->next = nl_cache;
  nl_cache = ht;
  memset ((char *) ht->tab, 0, nht * sizeof (hashentry *));
  vd = nl->vars;
  vde = vd + nv;
  while (vd < vde)
    {
      v = *vd++;
      if (!hash (ht, v->name))
	{
	  he->next = *zot;
	  *zot = he;
	  he->name = v->name;
	  he->vd = v;
	  he++;
	}
    }
  return ht;
}

static char Alpha[256], Alphanum[256];

static void
nl_init (void)
{
  register char *s;
  register int c;

  for (s = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"; (c = *s++);)
    Alpha[c]
      = Alphanum[c] = Alpha[c + 'a' - 'A'] = Alphanum[c + 'a' - 'A'] = c;
  for (s = "0123456789_"; (c = *s++);)
    Alphanum[c] = c;
}

#define GETC(x) (x=(*l_getc)())
#define Ungetc(x,y) (*l_ungetc)(x,y)

static int
getname (register char *s, int slen)
{
  register char *se = s + slen - 1;
  register int ch;

  GETC (ch);
  if (!(*s++ = Alpha[ch & 0xff]))
    {
      if (ch != EOF)
	ch = 115;
      errfl (f__elist->cierr, ch, "namelist read");
    }
  while ((*s = Alphanum[GETC (ch) & 0xff]))
    if (s < se)
      s++;
  if (ch == EOF)
    err (f__elist->cierr, EOF, "namelist read");
  if (ch > ' ')
    Ungetc (ch, f__cf);
  return *s = 0;
}

static int
getnum (int *chp, ftnlen * val)
{
  register int ch, sign;
  register ftnlen x;

  while (GETC (ch) <= ' ' && ch >= 0);
  if (ch == '-')
    {
      sign = 1;
      GETC (ch);
    }
  else
    {
      sign = 0;
      if (ch == '+')
	GETC (ch);
    }
  x = ch - '0';
  if (x < 0 || x > 9)
    return 115;
  while (GETC (ch) >= '0' && ch <= '9')
    x = 10 * x + ch - '0';
  while (ch <= ' ' && ch >= 0)
    GETC (ch);
  if (ch == EOF)
    return EOF;
  *val = sign ? -x : x;
  *chp = ch;
  return 0;
}

static int
getdimen (int *chp, dimen * d, ftnlen delta, ftnlen extent, ftnlen * x1)
{
  register int k;
  ftnlen x2, x3;

  if ((k = getnum (chp, x1)))
    return k;
  x3 = 1;
  if (*chp == ':')
    {
      if ((k = getnum (chp, &x2)))
	return k;
      x2 -= *x1;
      if (*chp == ':')
	{
	  if ((k = getnum (chp, &x3)))
	    return k;
	  if (!x3)
	    return 123;
	  x2 /= x3;
	  colonseen = 1;
	}
      if (x2 < 0 || x2 >= extent)
	return 123;
      d->extent = x2 + 1;
    }
  else
    d->extent = 1;
  d->curval = 0;
  d->delta = delta;
  d->stride = x3;
  return 0;
}

#ifndef No_Namelist_Questions
static void
print_ne (cilist * a)
{
  flag intext = f__external;
  int rpsave = f__recpos;
  FILE *cfsave = f__cf;
  unit *usave = f__curunit;
  cilist t;
  t = *a;
  t.ciunit = 6;
  s_wsne (&t);
  fflush (f__cf);
  f__external = intext;
  f__reading = 1;
  f__recpos = rpsave;
  f__cf = cfsave;
  f__curunit = usave;
  f__elist = a;
}
#endif

static char where0[] = "namelist read start ";

int
x_rsne (cilist * a)
{
  int ch, got1, k, n, nd, quote, readall;
  Namelist *nl;
  static char where[] = "namelist read";
  char buf[64];
  hashtab *ht;
  Vardesc *v;
  dimen *dn, *dn0, *dn1;
  ftnlen *dims, *dims1;
  ftnlen b, b0, b1, ex, no, nomax, size, span;
  ftnint no1, type;
  char *vaddr;
  long iva, ivae;
  dimen dimens[MAXDIM], substr;
  int dollarsign_delimited;

  if (!Alpha['a'])
    nl_init ();
  f__reading = 1;
  f__formatted = 1;
  got1 = 0;
top:
  dollarsign_delimited = 0;
  for (;;)
    switch (GETC (ch))
      {
      case EOF:
      eof:
	err (a->ciend, (EOF), where0);
      case '$':
        dollarsign_delimited = 1;
      case '&':
	goto have_amp;
#ifndef No_Namelist_Questions
      case '?':
	print_ne (a);
	continue;
#endif
      default:
	if (ch <= ' ' && ch >= 0)
	  continue;
#ifndef No_Namelist_Comments
	while (GETC (ch) != '\n')
	  if (ch == EOF)
	    goto eof;
#else
	errfl (a->cierr, 115, where0);
#endif
      }
have_amp:
  if ((ch = getname (buf, sizeof (buf))))
    return ch;
  nl = (Namelist *) a->cifmt;
  if (strcmp (buf, nl->name))
#ifdef No_Bad_Namelist_Skip
    errfl (a->cierr, 118, where0);
#else
    {
      fprintf (stderr,
	       "Skipping namelist \"%s\": seeking namelist \"%s\".\n",
	       buf, nl->name);
      fflush (stderr);
      for (;;)
	switch (GETC (ch))
	  {
	  case EOF:
	    err (a->ciend, EOF, where0);
	  case '/':
            if (dollarsign_delimited)
               continue;
	  case '&':
	  case '$':
	    if (f__external)
	      e_rsle ();
	    else
	      z_rnew ();
	    goto top;
	  case '"':
	  case '\'':
	    quote = ch;
	  more_quoted:
	    while (GETC (ch) != quote)
	      if (ch == EOF)
		err (a->ciend, EOF, where0);
	    if (GETC (ch) == quote)
	      goto more_quoted;
	    Ungetc (ch, f__cf);
	  default:
	    continue;
	  }
    }
#endif
  ht = mk_hashtab (nl);
  if (!ht)
    errfl (f__elist->cierr, 113, where0);
  for (;;)
    {
      for (;;)
	switch (GETC (ch))
	  {
	  case EOF:
	    if (got1)
	      return 0;
	    err (a->ciend, EOF, where0);
	  case '/':
	  case '$':
	  case '&':
	    return 0;
	  default:
	    if ((ch <= ' ' && ch >= 0) || ch == ',')
	      continue;
	    Ungetc (ch, f__cf);
	    if ((ch = getname (buf, sizeof (buf))))
	      return ch;
	    goto havename;
	  }
    havename:
      v = hash (ht, buf);
      if (!v)
	errfl (a->cierr, 119, where);
      while (GETC (ch) <= ' ' && ch >= 0);
      vaddr = v->addr;
      type = v->type;
      if (type < 0)
	{
	  size = -type;
	  type = TYCHAR;
	}
      else
	size = f__typesize[type];
      ivae = size;
      iva = readall = 0;
      if (ch == '(' /*) */ )
	{
	  dn = dimens;
	  if (!(dims = v->dims))
	    {
	      if (type != TYCHAR)
		errfl (a->cierr, 122, where);
	      if ((k = getdimen (&ch, dn, (ftnlen) size, (ftnlen) size, &b)))
		errfl (a->cierr, k, where);
	      if (ch != ')')
		errfl (a->cierr, 115, where);
	      b1 = dn->extent;
	      if (--b < 0 || b + b1 > size)
		return 124;
	      iva += b;
	      size = b1;
	      while (GETC (ch) <= ' ' && ch >= 0);
	      goto scalar;
	    }
	  nd = (int) dims[0];
	  nomax = span = dims[1];
	  ivae = iva + size * nomax;
	  colonseen = 0;
	  if ((k = getdimen (&ch, dn, size, nomax, &b)))
	    errfl (a->cierr, k, where);
	  no = dn->extent;
	  b0 = dims[2];
	  dims1 = dims += 3;
	  ex = 1;
	  for (n = 1; n++ < nd; dims++)
	    {
	      if (ch != ',')
		errfl (a->cierr, 115, where);
	      dn1 = dn + 1;
	      span /= *dims;
	      if ((k = getdimen (&ch, dn1, dn->delta ** dims, span, &b1)))
		errfl (a->cierr, k, where);
	      ex *= *dims;
	      b += b1 * ex;
	      no *= dn1->extent;
	      dn = dn1;
	    }
	  if (ch != ')')
	    errfl (a->cierr, 115, where);
	  readall = 1 - colonseen;
	  b -= b0;
	  if (b < 0 || b >= nomax)
	    errfl (a->cierr, 125, where);
	  iva += size * b;
	  dims = dims1;
	  while (GETC (ch) <= ' ' && ch >= 0);
	  no1 = 1;
	  dn0 = dimens;
	  if (type == TYCHAR && ch == '(' /*) */ )
	    {
	      if ((k = getdimen (&ch, &substr, size, size, &b)))
		errfl (a->cierr, k, where);
	      if (ch != ')')
		errfl (a->cierr, 115, where);
	      b1 = substr.extent;
	      if (--b < 0 || b + b1 > size)
		return 124;
	      iva += b;
	      b0 = size;
	      size = b1;
	      while (GETC (ch) <= ' ' && ch >= 0);
	      if (b1 < b0)
		goto delta_adj;
	    }
	  if (readall)
	    goto delta_adj;
	  for (; dn0 < dn; dn0++)
	    {
	      if (dn0->extent != *dims++ || dn0->stride != 1)
		break;
	      no1 *= dn0->extent;
	    }
	  if (dn0 == dimens && dimens[0].stride == 1)
	    {
	      no1 = dimens[0].extent;
	      dn0++;
	    }
	delta_adj:
	  ex = 0;
	  for (dn1 = dn0; dn1 <= dn; dn1++)
	    ex += (dn1->extent - 1) * (dn1->delta *= dn1->stride);
	  for (dn1 = dn; dn1 > dn0; dn1--)
	    {
	      ex -= (dn1->extent - 1) * dn1->delta;
	      dn1->delta -= ex;
	    }
	}
      else if ((dims = v->dims))
	{
	  no = no1 = dims[1];
	  ivae = iva + no * size;
	}
      else
      scalar:
	no = no1 = 1;
      if (ch != '=')
	errfl (a->cierr, 115, where);
      got1 = nml_read = 1;
      f__lcount = 0;
    readloop:
      for (;;)
	{
	  if (iva >= ivae || iva < 0)
	    {
	      f__lquit = 1;
	      goto mustend;
	    }
	  else if (iva + no1 * size > ivae)
	    no1 = (ivae - iva) / size;
	  f__lquit = 0;
	  if ((k = l_read (&no1, vaddr + iva, size, type)))
	    return k;
	  if (f__lquit == 1)
	    return 0;
	  if (readall)
	    {
	      iva += dn0->delta;
	      if (f__lcount > 0)
		{
		  ftnint no2 = (ivae - iva) / size;
		  if (no2 > f__lcount)
		    no2 = f__lcount;
		  if ((k = l_read (&no2, vaddr + iva, size, type)))
		    return k;
		  iva += no2 * dn0->delta;
		}
	    }
	mustend:
	  GETC (ch);
	  if (readall)
	    {
	      if (iva >= ivae)
		readall = 0;
	      else
		for (;;)
		  {
		    switch (ch)
		      {
		      case ' ':
		      case '\t':
		      case '\n':
			GETC (ch);
			continue;
		      }
		    break;
		  }
	    }
	  if (ch == '/' || ch == '$' || ch == '&')
	    {
	      f__lquit = 1;
	      return 0;
	    }
	  else if (f__lquit)
	    {
	      while (ch <= ' ' && ch >= 0)
		GETC (ch);
	      Ungetc (ch, f__cf);
	      if (!Alpha[ch & 0xff] && ch >= 0)
		errfl (a->cierr, 125, where);
	      break;
	    }
	  Ungetc (ch, f__cf);
	  if (readall && !Alpha[ch & 0xff])
	    goto readloop;
	  if ((no -= no1) <= 0)
	    break;
	  for (dn1 = dn0; dn1 <= dn; dn1++)
	    {
	      if (++dn1->curval < dn1->extent)
		{
		  iva += dn1->delta;
		  goto readloop;
		}
	      dn1->curval = 0;
	    }
	  break;
	}
    }
}

integer
s_rsne (cilist * a)
{
  extern int l_eof;
  int n;

  f__external = 1;
  l_eof = 0;
  if ((n = c_le (a)))
    return n;
  if (f__curunit->uwrt && f__nowreading (f__curunit))
    err (a->cierr, errno, where0);
  l_getc = t_getc;
  l_ungetc = un_getc;
  f__doend = xrd_SL;
  n = x_rsne (a);
  nml_read = 0;
  if (n)
    return n;
  return e_rsle ();
}
