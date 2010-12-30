/* This file contains the implementation of class Protocol.
   Copyright (C) 1993, 2004, 2009, 2010 Free Software Foundation, Inc.

This file is part of GCC. 

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */ 

#include "objc-private/common.h"
#include "objc/runtime.h"
#include "objc-private/module-abi-8.h"
#include "objc/Protocol.h"

@implementation Protocol
- (BOOL) isEqual: (id)obj
{
  return protocol_isEqual (self, obj);
}
@end

@implementation Protocol (Deprecated)

- (const char *)name
{
  return protocol_name;
}

- (BOOL) conformsTo: (Protocol *)aProtocolObject
{
  return protocol_conformsToProtocol (self, aProtocolObject);
}

- (struct objc_method_description *) descriptionForInstanceMethod:(SEL)aSel
{
  int i;
  struct objc_protocol_list* proto_list;
  struct objc_method_description *result;

  if (instance_methods)
    for (i = 0; i < instance_methods->count; i++)
      {
	if (sel_isEqual (instance_methods->list[i].name, aSel))
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
  struct objc_method_description *result;

  if (class_methods)
    for (i = 0; i < class_methods->count; i++)
      {
	if (sel_isEqual (class_methods->list[i].name, aSel))
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
     that we use for class names; protocol names and class names are
     somewhat similar types of string spaces.  */
  int hash = 0, index;
  
  for (index = 0; protocol_name[index] != '\0'; index++)
    {
      hash = (hash << 4) ^ (hash >> 28) ^ protocol_name[index];
    }

  hash = (hash ^ (hash >> 10) ^ (hash >> 20));

  return hash;
}

@end
