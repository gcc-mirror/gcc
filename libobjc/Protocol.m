/* This file contains the implementation of class Protocol.
   Copyright (C) 1993, 2004 Free Software Foundation, Inc.

This file is part of GCC. 

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */
 
/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#include "objc/Protocol.h"
#include "objc/objc-api.h"

/* Method description list */
struct objc_method_description_list {
        int count;
        struct objc_method_description list[1];
};


@implementation Protocol
{
@private
        char *protocol_name;
        struct objc_protocol_list *protocol_list;
        struct objc_method_description_list *instance_methods, *class_methods; 
}

/* Obtaining attributes intrinsic to the protocol */

- (const char *)name
{
  return protocol_name;
}

/* Testing protocol conformance */

- (BOOL) conformsTo: (Protocol *)aProtocolObject
{
  size_t i;
  struct objc_protocol_list* proto_list;

  if (aProtocolObject == nil)
    return NO;

  if (!strcmp(aProtocolObject->protocol_name, self->protocol_name))
    return YES;

  for (proto_list = protocol_list; proto_list; proto_list = proto_list->next)
    {
      for (i=0; i < proto_list->count; i++)
	{
	  if ([proto_list->list[i] conformsTo: aProtocolObject])
	    return YES;
	}
    }

  return NO;
}

/* Looking up information specific to a protocol */

- (struct objc_method_description *) descriptionForInstanceMethod:(SEL)aSel
{
  int i;
  struct objc_protocol_list* proto_list;
  const char* name = sel_get_name (aSel);
  struct objc_method_description *result;

  if (instance_methods)
    for (i = 0; i < instance_methods->count; i++)
      {
	if (!strcmp ((char*)instance_methods->list[i].name, name))
	  return &(instance_methods->list[i]);
      }

  for (proto_list = protocol_list; proto_list; proto_list = proto_list->next)
    {
      size_t j;
      for (j=0; j < proto_list->count; j++)
	{
	  if ((result = [proto_list->list[j]
			 descriptionForInstanceMethod: aSel]))
	    return result;
	}
    }

  return NULL;
}

- (struct objc_method_description *) descriptionForClassMethod:(SEL)aSel;
{
  int i;
  struct objc_protocol_list* proto_list;
  const char* name = sel_get_name (aSel);
  struct objc_method_description *result;

  if (class_methods)
    for (i = 0; i < class_methods->count; i++)
      {
	if (!strcmp ((char*)class_methods->list[i].name, name))
	  return &(class_methods->list[i]);
      }

  for (proto_list = protocol_list; proto_list; proto_list = proto_list->next)
    {
      size_t j;
      for (j=0; j < proto_list->count; j++)
	{
	  if ((result = [proto_list->list[j]
			 descriptionForClassMethod: aSel]))
	    return result;
	}
    }

  return NULL;
}

- (unsigned) hash
{
  /* Compute a hash of the protocol_name; use the same hash algorithm
   * that we use for class names; protocol names and class names are
   * somewhat similar types of string spaces.
   */
  int hash = 0, index;
  
  for (index = 0; protocol_name[index] != '\0'; index++)
    {
      hash = (hash << 4) ^ (hash >> 28) ^ protocol_name[index];
    }

  hash = (hash ^ (hash >> 10) ^ (hash >> 20));

  return hash;
}

/*
 * Equality between formal protocols is only formal (nothing to do
 * with actually checking the list of methods they have!).  Two formal
 * Protocols are equal if and only if they have the same name.
 *
 * Please note (for comparisons with other implementations) that
 * checking the names is equivalent to checking that Protocol A
 * conforms to Protocol B and Protocol B conforms to Protocol A,
 * because this happens iff they have the same name.  If they have
 * different names, A conforms to B if and only if A includes B, but
 * the situation where A includes B and B includes A is a circular
 * dependency between Protocols which is forbidden by the compiler, so
 * A conforms to B and B conforms to A with A and B having different
 * names is an impossible case.
 */
- (BOOL) isEqual: (id)obj
{
  if (obj == self)
    return YES;

  if ([obj isKindOf: [Protocol class]])
    {
      if (strcmp (protocol_name, ((Protocol *)obj)->protocol_name) == 0)
	return YES;
    }

  return NO;
}
@end

