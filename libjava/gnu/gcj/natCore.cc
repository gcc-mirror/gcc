// natCore -- C++ side of Core

/* Copyright (C) 2001, 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author: Anthony Green <green@redhat.com>.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <string.h>
#include <stdlib.h>

#include <java/lang/NullPointerException.h>
#include <java/io/IOException.h>
#include <gnu/gcj/Core.h>

typedef struct core_chain_struct
{
  int name_length;
  const char *name;
  int data_length;
  const void *data;
  
  struct core_chain_struct *next;
} core_chain;

static core_chain *root;

void _Jv_RegisterResource (void *vptr)
{
  char *rptr = (char *)vptr;

  // These are permanent data structures for now.  This routine is
  // called from a static constructor, so we shouldn't depend on too
  // much existing infrastructure.
  core_chain *cc = (core_chain *) _Jv_Malloc (sizeof (core_chain));

  cc->name_length = ((int *)rptr)[0];
  cc->data_length = ((int *)rptr)[1];
  cc->name = rptr + 2*sizeof(int);
  cc->data = cc->name + cc->name_length;

  // Add this new item to the chain...
  core_chain *old_root = root;
  cc->next = old_root;
  root = cc;
}

gnu::gcj::Core *
gnu::gcj::Core::create (jstring name)
{
  char *buf = (char *) __builtin_alloca (JvGetStringUTFLength (name) + 1);
  jsize total = JvGetStringUTFRegion (name, 0, name->length(), buf);
  buf[total] = '\0';

  // Usually requests here end up as an absolute URL.  We strip the
  // initial `/'.
  if (buf[0] == '/')
    {
      ++buf;
      --total;
    }

  core_chain *node = root;

  while (node)
    {
      if (total == node->name_length
	  && strncmp (buf, node->name, total) == 0)
	{
	  gnu::gcj::Core *core = 
	    (gnu::gcj::Core *) _Jv_AllocObject(&gnu::gcj::Core::class$,
					       sizeof (gnu::gcj::Core));
	  core->ptr = (gnu::gcj::RawData *) node->data;
	  core->length = node->data_length;
	  return core;
	}
      else
	node = node->next;
    }

  throw new java::io::IOException (JvNewStringLatin1 ("can't open core"));
}
