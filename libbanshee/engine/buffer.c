/*
 * Copyright (c) 2000-2001
 *      The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "buffer.h"

/* Invariant: buffer always null-terminated */
struct growbuf
{
  region r;
  unsigned int maxsize, cursize;
  char *buffer;
};

/* Make a new buffer with initial size */
growbuf growbuf_new(region r, int size)
{
  growbuf b = ralloc(r, struct growbuf);

  assert(size > 0);
  b->r = r;
  b->maxsize = size; /* Force some growth! */
  b->cursize = 1;
  b->buffer = rstralloc(r, size);
  b->buffer[0] = '\0';
  return b;
}

/* Empty a buffer */
void growbuf_reset(growbuf b)
{
  assert(b->maxsize > 0);
  b->cursize = 1;
  b->buffer[0] = '\0';
}

/* Print to a buffer */
int gprintf(growbuf b, const char *fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  return gvprintf(b, fmt, args);
}

/* Print to a buffer */
int gvprintf(growbuf b, const char *fmt, va_list args)
{
  int nchars;

  if (!fmt) /* Bug (?)/feature of vnsprintf -- printing \0 returns -1,
	       goes into infinite loop. */
    return 0;
  while (1)
    {
      char *bufStart;
      int sizeLeft;

      bufStart = b->buffer + b->cursize - 1; /* chop trailing \0 */
      sizeLeft = b->maxsize - b->cursize + 1; /* +1 size we're chooping
						 the trailing \0 */
      assert(*bufStart == '\0');
      nchars = vsnprintf(bufStart, sizeLeft, fmt, args);
      if (nchars > -1 && nchars < sizeLeft)
	{
	  b->cursize += nchars; /* nchars doesn't include \0,
				   but we overwrote our \0 */
	  break;
	}
      else
	{
	  /* How much room do we need?  In the new glibc, nchars
	     tells us how much (not including the trailing null).
	     So we need the current size, -1 since we'll remove the null,
	     plus the new size, plus 1 for the new null. */
	  int newSize = (nchars > -1) ? b->cursize - 1 + nchars + 1
	                              : b->maxsize * 2;
	  char *newBuf;

	  /* fprintf(stderr, "Reallocating buffer, newSize=%d\n", newSize); */
	  newBuf = rstralloc(b->r, newSize);
	  memcpy(newBuf, b->buffer, b->cursize);
	  newBuf[b->cursize-1] = '\0';  /* vsnprintf has printed something! */
	  b->buffer = newBuf;
	  b->maxsize = newSize;
	  /* b->cursize unchanged */
	}
    }
  return nchars;
}

/* Get the contents of a buffer */
char *growbuf_contents(growbuf b)
{
  return b->buffer;
}

bool growbuf_empty(growbuf b)
{
  return b->cursize == 1; /* Buffer always null terminated */
}
