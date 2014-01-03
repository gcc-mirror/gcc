/* GNU Objective C Runtime protocol related functions.
   Copyright (C) 2010-2014 Free Software Foundation, Inc.
   Contributed by Nicola Pero

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 3, or (at your option) any later version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "objc-private/common.h"
#include "objc/runtime.h"
#include "objc-private/module-abi-8.h" /* For runtime structures  */
#include "objc/thr.h"
#include "objc-private/runtime.h"      /* the kitchen sink */
#include "objc-private/hash.h"         /* For the hash table of protocols.  */
#include "objc-private/protocols.h"    /* For __objc_protocols_init() and
                                          __objc_protocols_add_protocol().  */
#include <stdlib.h>                    /* For malloc.  */

/* This is a table that maps a name to a Protocol instance with that
   name.  Because there may be multiple Protocol instances with the
   same name (no harm in that) the table records only one
   instance.  */
static cache_ptr __protocols_hashtable;

/* A mutex protecting the protocol_hashtable.  */
static objc_mutex_t __protocols_hashtable_lock = NULL;

/* Called at startup by init.c.  */
void
__objc_protocols_init (void)
{
  __protocols_hashtable_lock = objc_mutex_allocate ();

  /* The keys in the table are strings, and the values are Protocol
     objects.  */
  __protocols_hashtable = objc_hash_new (64, (hash_func_type) objc_hash_string,
					 (compare_func_type) objc_compare_strings);
}

/* Add a protocol to the hashtable.  */
void
__objc_protocols_add_protocol (const char *name, struct objc_protocol *object)
{
  objc_mutex_lock (__protocols_hashtable_lock);

  /* If we find a protocol with the same name already in the
     hashtable, we do not need to add the new one, because it will be
     identical to it.  This in the reasonable assumption that two
     protocols with the same name are identical, which is expected in
     any sane program.  If we are really paranoid, we would compare
     the protocols and abort if they are not identical.
     Unfortunately, this would slow down the startup of all
     Objective-C programs while trying to catch a problem that has
     never been seen in practice, so we don't do it.  */
  if (! objc_hash_is_key_in_hash (__protocols_hashtable, name))
    objc_hash_add (&__protocols_hashtable, name, object);

  objc_mutex_unlock (__protocols_hashtable_lock);
}

Protocol *
objc_getProtocol (const char *name)
{
  Protocol *protocol;

  if (name == NULL)
    return NULL;

  objc_mutex_lock (__protocols_hashtable_lock);
  protocol = (Protocol *)(objc_hash_value_for_key (__protocols_hashtable, name));
  objc_mutex_unlock (__protocols_hashtable_lock);

  return protocol;
}

Protocol **
objc_copyProtocolList (unsigned int *numberOfReturnedProtocols)
{
  unsigned int count = 0;
  Protocol **returnValue = NULL;
  node_ptr node;

  objc_mutex_lock (__protocols_hashtable_lock);

  /* Count how many protocols we have.  */
  node = objc_hash_next (__protocols_hashtable, NULL);
  while (node)
    {
      count++;
      node = objc_hash_next (__protocols_hashtable, node);
    }

  if (count != 0)
    {
      unsigned int i = 0;

      /* Allocate enough memory to hold them.  */
      returnValue = (Protocol **)(malloc (sizeof (Protocol *) * (count + 1)));
      
      /* Copy the protocols.  */
      node = objc_hash_next (__protocols_hashtable, NULL);
      while (node)
	{
	  returnValue[i] = node->value;
	  i++;
	  node = objc_hash_next (__protocols_hashtable, node);
	}

      returnValue[i] = NULL;
    }
  objc_mutex_unlock (__protocols_hashtable_lock);

  if (numberOfReturnedProtocols)
    *numberOfReturnedProtocols = count;

  return returnValue;
}

BOOL 
class_addProtocol (Class class_, Protocol *protocol)
{
  struct objc_protocol_list *protocols;

  if (class_ == Nil  ||  protocol == NULL)
    return NO;

  if (class_conformsToProtocol (class_, protocol))
    return NO;

  /* Check that it is a Protocol object before casting it to (struct
     objc_protocol *).  */
  if (protocol->class_pointer != objc_lookUpClass ("Protocol"))
    return NO;

  objc_mutex_lock (__objc_runtime_mutex);

  /* Create the objc_protocol_list.  */
  protocols = malloc (sizeof (struct objc_protocol_list));
  protocols->count = 1;
  protocols->list[0] = (struct objc_protocol *)protocol;

  /* Attach it to the list of class protocols.  */
  protocols->next = class_->protocols;
  class_->protocols = protocols;

  objc_mutex_unlock (__objc_runtime_mutex);

  return YES;
}

BOOL 
class_conformsToProtocol (Class class_, Protocol *protocol)
{
  struct objc_protocol_list* proto_list;

  if (class_ == Nil  ||  protocol == NULL)
    return NO;

  /* Check that it is a Protocol object before casting it to (struct
     objc_protocol *).  */
  if (protocol->class_pointer != objc_lookUpClass ("Protocol"))
    return NO;

  /* Acquire the runtime lock because the list of protocols for a
     class may be modified concurrently, for example if another thread
     calls class_addProtocol(), or dynamically loads from a file a
     category of the class.  */
  objc_mutex_lock (__objc_runtime_mutex);
  proto_list = class_->protocols;

  while (proto_list)
    {
      size_t i;
      for (i = 0; i < proto_list->count; i++)
	{
	  if (proto_list->list[i] == (struct objc_protocol *)protocol
	      || protocol_conformsToProtocol ((Protocol *)proto_list->list[i],
					      protocol))
	    {
	      objc_mutex_unlock (__objc_runtime_mutex);
	      return YES;
	    }
	}
      proto_list = proto_list->next;
    }
  
  objc_mutex_unlock (__objc_runtime_mutex);
  return NO;
}

Protocol **
class_copyProtocolList (Class class_, unsigned int *numberOfReturnedProtocols)
{
  unsigned int count = 0;
  Protocol **returnValue = NULL;
  struct objc_protocol_list* proto_list;

  if (class_ == Nil)
    {
      if (numberOfReturnedProtocols)
	*numberOfReturnedProtocols = 0;
      return NULL;
    }

  /* Lock the runtime mutex because the class protocols may be
     concurrently modified.  */
  objc_mutex_lock (__objc_runtime_mutex);

  /* Count how many protocols we have.  */
  proto_list = class_->protocols;

  while (proto_list)
    {
      count = count + proto_list->count;
      proto_list = proto_list->next;
    }

  if (count != 0)
    {
      unsigned int i = 0;
      
      /* Allocate enough memory to hold them.  */
      returnValue = (Protocol **)(malloc (sizeof (Protocol *) * (count + 1)));
      
      /* Copy the protocols.  */
      proto_list = class_->protocols;
      
      while (proto_list)
	{
	  size_t j;
	  for (j = 0; j < proto_list->count; j++)
	    {
	      returnValue[i] = (Protocol *)proto_list->list[j];
	      i++;
	    }
	  proto_list = proto_list->next;
	}
      
      returnValue[i] = NULL;
    }
  objc_mutex_unlock (__objc_runtime_mutex);

  if (numberOfReturnedProtocols)
    *numberOfReturnedProtocols = count;

  return returnValue;
}

BOOL 
protocol_conformsToProtocol (Protocol *protocol, Protocol *anotherProtocol)
{
  struct objc_protocol_list* proto_list;

  if (protocol == NULL  ||  anotherProtocol == NULL)
    return NO;

  if (protocol == anotherProtocol)
    return YES;
    
  /* Check that the objects are Protocol objects before casting them
     to (struct objc_protocol *).  */
  if (protocol->class_pointer != anotherProtocol->class_pointer)
    return NO;
  
  if (protocol->class_pointer != objc_lookUpClass ("Protocol"))
    return NO;

  if (strcmp (((struct objc_protocol *)protocol)->protocol_name,
	      ((struct objc_protocol *)anotherProtocol)->protocol_name) == 0)
    return YES;

  /* We do not acquire any lock because protocols are currently
     immutable.  We can freely iterate over a protocol structure.  */
  proto_list = ((struct objc_protocol *)protocol)->protocol_list;
  while (proto_list)
    {
      size_t i;
      
      for (i = 0; i < proto_list->count; i++)
	{
	  if (protocol_conformsToProtocol ((Protocol *)proto_list->list[i], anotherProtocol))
	    return YES;
	}
      proto_list = proto_list->next;
    }

  return NO;
}

BOOL 
protocol_isEqual (Protocol *protocol, Protocol *anotherProtocol)
{
  if (protocol == anotherProtocol)
    return YES;

  if (protocol == NULL  ||  anotherProtocol == NULL)
    return NO;
  
  /* Check that the objects are Protocol objects before casting them
     to (struct objc_protocol *).  */
  if (protocol->class_pointer != anotherProtocol->class_pointer)
    return NO;
  
  if (protocol->class_pointer != objc_lookUpClass ("Protocol"))
    return NO;

  /* Equality between formal protocols is only formal (nothing to do
     with actually checking the list of methods they have!).  Two
     formal Protocols are equal if and only if they have the same
     name.

     Please note (for comparisons with other implementations) that
     checking the names is equivalent to checking that Protocol A
     conforms to Protocol B and Protocol B conforms to Protocol A,
     because this happens iff they have the same name.  If they have
     different names, A conforms to B if and only if A includes B, but
     the situation where A includes B and B includes A is a circular
     dependency between Protocols which is forbidden by the compiler,
     so A conforms to B and B conforms to A with A and B having
     different names is an impossible case.  */
  if (strcmp (((struct objc_protocol *)protocol)->protocol_name,
	      ((struct objc_protocol *)anotherProtocol)->protocol_name) == 0)
    return YES;
  
  return NO;
}

const char *
protocol_getName (Protocol *protocol)
{
  /* Check that it is a Protocol object before casting it to (struct
     objc_protocol *).  */
  if (protocol->class_pointer != objc_lookUpClass ("Protocol"))
    return NULL;

  return ((struct objc_protocol *)protocol)->protocol_name;
}

struct objc_method_description protocol_getMethodDescription (Protocol *protocol, 
							      SEL selector,
							      BOOL requiredMethod,
							      BOOL instanceMethod)
{
  struct objc_method_description no_result = { NULL, NULL };
  struct objc_method_description_list *methods;
  int i;

  /* TODO: New ABI.  */
  /* The current ABI does not have any information on optional protocol methods.  */
  if (! requiredMethod)
    return no_result;

  /* Check that it is a Protocol object before casting it to (struct
     objc_protocol *).  */
  if (protocol->class_pointer != objc_lookUpClass ("Protocol"))
    return no_result;

  if (instanceMethod)
    methods = ((struct objc_protocol *)protocol)->instance_methods;
  else
    methods = ((struct objc_protocol *)protocol)->class_methods;

  if (methods)
    {
      for (i = 0; i < methods->count; i++)
	{
	  if (sel_isEqual (methods->list[i].name, selector))
	    return methods->list[i];
	  /*
	  if (strcmp (sel_getName (methods->list[i].name), selector_name) == 0)
	    return methods->list[i];
	  */
	}
    }

  return no_result;
}

struct objc_method_description *protocol_copyMethodDescriptionList (Protocol *protocol,
								    BOOL requiredMethod,
								    BOOL instanceMethod,
								    unsigned int *numberOfReturnedMethods)
{
  struct objc_method_description_list *methods;
  unsigned int count = 0;
  struct objc_method_description *returnValue = NULL;

  /* TODO: New ABI */
  /* The current ABI does not have any information on optional protocol methods.  */
  if (! requiredMethod)
    {
      if (numberOfReturnedMethods)
	*numberOfReturnedMethods = 0;

      return NULL;
    }

  /* Check that it is a Protocol object before casting it to (struct
     objc_protocol *).  */
  if (protocol == NULL  ||  protocol->class_pointer != objc_lookUpClass ("Protocol"))
    {
      if (numberOfReturnedMethods)
	*numberOfReturnedMethods = 0;

      return NULL;
    }
  
  /* We do not acquire any lock because protocols are currently
     immutable.  We can freely iterate over a protocol structure.  */

  if (instanceMethod)
    methods = ((struct objc_protocol *)protocol)->instance_methods;
  else
    methods = ((struct objc_protocol *)protocol)->class_methods;

  if (methods)
    {
      unsigned int i;
      count = methods->count;

      /* Allocate enough memory to hold them.  */
      returnValue = (struct objc_method_description *)(malloc (sizeof (struct objc_method_description) * (count + 1)));

      /* Copy them.  */
      for (i = 0; i < count; i++)
	{
	  returnValue[i].name = methods->list[i].name;
	  returnValue[i].types = methods->list[i].types;
	}
      returnValue[i].name = NULL;
      returnValue[i].types = NULL;
    }

  if (numberOfReturnedMethods)
    *numberOfReturnedMethods = count;

  return returnValue;
}

Property protocol_getProperty (Protocol *protocol, const char *propertyName, 
			       BOOL requiredProperty, BOOL instanceProperty)
{
  if (protocol == NULL  ||  propertyName == NULL)
    return NULL;

  if (!requiredProperty  ||  !instanceProperty)
    return NULL;

  /* Check that it is a Protocol object before casting it to (struct
     objc_protocol *).  */
  if (protocol->class_pointer != objc_lookUpClass ("Protocol"))
    return NULL;

  /* TODO: New ABI.  */
  /* The current ABI does not have any information on protocol properties.  */
  return NULL;
}

Property *protocol_copyPropertyList (Protocol *protocol, unsigned int *numberOfReturnedProperties)
{
  unsigned int count = 0;
  Property *returnValue = NULL;

  /* Check that it is a Protocol object before casting it to (struct
     objc_protocol *).  */
  if (protocol == NULL  ||  protocol->class_pointer != objc_lookUpClass ("Protocol"))
    {
      if (numberOfReturnedProperties)
	*numberOfReturnedProperties = 0;

      return NULL;
    }
  
  /* We do not acquire any lock because protocols are currently
     immutable.  We can freely iterate over a protocol structure.  */

  /* TODO: New ABI.  */
  /* The current ABI does not have any information on protocol properties.  */
  if (numberOfReturnedProperties)
    *numberOfReturnedProperties = count;

  return returnValue;
}

Protocol **protocol_copyProtocolList (Protocol *protocol, unsigned int *numberOfReturnedProtocols)
{
  unsigned int count = 0;
  Protocol **returnValue = NULL;
  struct objc_protocol_list* proto_list;

  /* Check that it is a Protocol object before casting it to (struct
     objc_protocol *).  */
  if (protocol == NULL  ||  protocol->class_pointer != objc_lookUpClass ("Protocol"))
    {
      if (numberOfReturnedProtocols)
	*numberOfReturnedProtocols = 0;

      return NULL;
    }
  
  /* We do not acquire any lock because protocols are currently
     immutable.  We can freely iterate over a protocol structure.  */

  /* Count how many protocols we have.  */
  proto_list = ((struct objc_protocol *)protocol)->protocol_list;

  while (proto_list)
    {
      count = count + proto_list->count;
      proto_list = proto_list->next;
    }

  if (count != 0)
    {
      unsigned int i = 0;
      
      /* Allocate enough memory to hold them.  */
      returnValue = (Protocol **)(malloc (sizeof (Protocol *) * (count + 1)));
      
      /* Copy the protocols.  */
      proto_list = ((struct objc_protocol *)protocol)->protocol_list;
      
      while (proto_list)
	{
	  size_t j;
	  for (j = 0; j < proto_list->count; j++)
	    {
	      returnValue[i] = (Protocol *)proto_list->list[j];
	      i++;
	    }
	  proto_list = proto_list->next;
	}

      returnValue[i] = NULL;
    }

  if (numberOfReturnedProtocols)
    *numberOfReturnedProtocols = count;

  return returnValue;
}
