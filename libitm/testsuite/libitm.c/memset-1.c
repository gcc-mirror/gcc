/* This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.  */

/* Verify memcpy operation.  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <libitm.h>

#define BEG_TRANSACTION \
  _ITM_beginTransaction (pr_instrumentedCode | pr_hasNoAbort \
			 | pr_hasNoIrrevocable)
#define END_TRANSACTION \
  _ITM_commitTransaction ()

#define MEMSET _ITM_memsetW

static unsigned char *buf;
static size_t bufsize, page_size;
static int fail;

#ifndef MAP_ANONYMOUS
#  ifdef MAP_ANON
#    define MAP_ANONYMOUS MAP_ANON
#  endif
#endif

static void
do_test (size_t align, size_t len)
{
  size_t i, j;
  unsigned char c1, c2;

  if (align + len >= bufsize)
    return;

  c1 = random () >> 8;
  c2 = random () >> 8;
  if (c1 == c2)
    c1++;
  memset (buf, c1, bufsize);

  BEG_TRANSACTION;
  MEMSET (buf + align, c2, len);
  END_TRANSACTION;

  i = (align > 64 ? align - 64 : 0);
  for (; i < align; ++i)
    if (buf[i] != c1)
      {
	printf ("Garbage before: ofs %zd\n", i);
	fail = 1;
	break;
      }
  for (; i < align + len; ++i)
    if (buf[i] != c2)
      {
	printf ("Wrong result: ofs %zd\n", i);
	fail = 1;
	break;
      }
  for (j = i + 64 < bufsize ? i + 64 : bufsize; i < j; ++i)
    if (buf[i] != c1)
      {
	printf ("Garbage after: ofs %zd\n", i);
	fail = 1;
	break;
      }
}

int main()
{
  size_t i, j;

  page_size = getpagesize ();
  bufsize = 2 * page_size;

  buf = mmap (NULL, bufsize + 2*page_size, PROT_READ | PROT_WRITE,
	      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (buf == MAP_FAILED)
    return 1;

  if (mprotect (buf, page_size, PROT_NONE))
    return 1;
  buf += page_size;
  if (mprotect (buf + bufsize, page_size, PROT_NONE))
    return 1;

  for (i = 0; i < 18; ++i)
    {
      size_t len = 1 << i;
      do_test (0, len);
      do_test (bufsize - len, len);
    }

  for (i = 0; i < 32; ++i)
    for (j = 0; j < 32; ++j)
      do_test (j, i);

  for (i = 3; i < 32; ++i)
    {
      if ((i & (i - 1)) == 0)
	continue;
      do_test (0, 16 * i);
      do_test (i, 16 * i);
    }

  return fail;
}
