// natCollator.cc - Native code for collation.

/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

// Written by Tom Tromey <tromey@cygnus.com>.

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <java/text/Collator.h>
#include <java/lang/StringBuffer.h>

#include <java-chardecomp.h>

void
java::text::Collator::decomposeCharacter (jchar c,
					  java::lang::StringBuffer *buf)
{
  if (decmp == NO_DECOMPOSITION)
    {
      buf->append(c);
      return;
    }

  const struct decomp_entry *base;
  int high;

  if (decmp == FULL_DECOMPOSITION)
    {
      base = full_decomposition;
      high = sizeof (full_decomposition) / sizeof (struct decomp_entry);
    }
  else
    {
      base = canonical_decomposition;
      high = sizeof (canonical_decomposition) / sizeof (struct decomp_entry);
    }

  // FIXME: this is probably a bit slow for the task at hand.
  int i = high / 2;
  int low = 0;
  while (true)
    {
      if (c < base[i].key)
	high = i;
      else if (c > base[i].key)
	low = i;
      else
	break;

      int old = i;
      i = (high + low) / 2;
      if (i == old)
	{
	  // Not in table, so it expands to itself.
	  buf->append(c);
	  return;
	}
    }

  for (int j = 0; base[i].value[j] != '\0'; j += 2)
    {
      jchar x = (base[i].value[j] << 8) | (base[i].value[j + 1]);
      buf->append (x);
    }
}
