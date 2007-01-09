// natCore -- C++ side of Core

/* Copyright (C) 2001, 2002, 2003, 2005, 2006  Free Software Foundation

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

// List of global core values.
static _Jv_core_chain *root;

static void
default_register_resource (_Jv_core_chain *node)
{
  node->next = root;
  root = node;
}

// This is set only when a lock is held on java.lang.Class.
// This function is called to handle a new core node.
void (*_Jv_RegisterCoreHook) (_Jv_core_chain *) = default_register_resource;

void
_Jv_RegisterResource (void *vptr)
{
  char *rptr = (char *) vptr;

  _Jv_core_chain *cc = (_Jv_core_chain *) _Jv_Malloc (sizeof (_Jv_core_chain));

  cc->name_length = ((int *)rptr)[0];
  cc->data_length = ((int *)rptr)[1];
  cc->name = rptr + 2 * sizeof (int);
  cc->data = cc->name + cc->name_length;
  cc->next = NULL;

  (*_Jv_RegisterCoreHook) (cc);
}

void
_Jv_FreeCoreChain (_Jv_core_chain *chain)
{
  while (chain != NULL)
    {
      _Jv_core_chain *next = chain->next;
      _Jv_Free (chain);
      chain = next;
    }
}

_Jv_core_chain *
_Jv_FindCore (_Jv_core_chain *node, jstring name)
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

  while (node)
    {
      if (total == node->name_length
	  && strncmp (buf, node->name, total) == 0)
	return node;
      node = node->next;
    }

  return NULL;
}

gnu::gcj::Core *
_Jv_create_core (_Jv_core_chain *node, jstring name)
{
  node = _Jv_FindCore (node, name);

  gnu::gcj::Core *core = NULL;
  if (node)
    {
      core = new gnu::gcj::Core ();
      core->ptr = (gnu::gcj::RawData *) node->data;
      core->length = node->data_length;
    }
  return core;
}

gnu::gcj::Core *
gnu::gcj::Core::find (jstring name)
{
  gnu::gcj::Core *core = _Jv_create_core (root, name);
  return core;
}

gnu::gcj::Core *
gnu::gcj::Core::create (jstring name)
{
  gnu::gcj::Core *core = _Jv_create_core (root, name);
  if (core == NULL)
    throw new ::java::io::IOException (JvNewStringLatin1 ("can't open core"));
  return core;
}
