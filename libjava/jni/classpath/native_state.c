/* Magical NSA API -- Associate a C ptr with an instance of an object
   Copyright (C) 1998, 2002 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

#include <stdlib.h>
#include <jni.h>
#include "native_state.h"

#define DEFAULT_TABLE_SIZE 97

struct state_table *
init_state_table_with_size (JNIEnv *env, jclass clazz, jint size)
{
  struct state_table *table;
  jfieldID hash;
  jclass clazz_g;

  hash = (*env)->GetFieldID (env, clazz, "native_state", "I");
  if (hash == NULL)
    return NULL;

  clazz_g = (*env)->NewGlobalRef (env, clazz);
  if (clazz_g == NULL)
    return NULL;

  table = (struct state_table *) malloc (sizeof (struct state_table));
  table->size = size;
  table->head = (struct state_node **) calloc (sizeof (struct state_node *),
					       table->size);
  table->hash = hash;
  table->clazz = clazz_g; 

  return table;
}

struct state_table *
init_state_table (JNIEnv *env, jclass clazz)
{
  return init_state_table_with_size (env, clazz, DEFAULT_TABLE_SIZE);
}

static void *
remove_node (struct state_node **head, jint obj_id)
{
  struct state_node *back_ptr = NULL;
  struct state_node *node = *head;

  while (node != NULL)
    {
      if (node->key == obj_id)
	{
	  void *return_value;
	  if (back_ptr == NULL)
	    *head = node->next;
	  else
	    back_ptr->next = node->next;
	  return_value = node->c_state;
	  free (node);
	  return return_value;
	}
      back_ptr = node;
      node = node->next;
    }

  return NULL;
}
	    
static void *
get_node (struct state_node **head, jint obj_id)
{
  struct state_node *back_ptr = NULL;
  struct state_node *node = *head;

  while (node != NULL)
    {
      if (node->key == obj_id)
	{
	  /* Move the node we found to the front of the list.  */
	  if (back_ptr != NULL)
	    {
	      back_ptr->next = node->next;
	      node->next = *head;
	      *head = node;
	    }

	  /* Return the match.  */
	  return node->c_state;
	}
  
      back_ptr = node;
      node = node->next;
    }

  return NULL;
}

static void 
add_node (struct state_node **head, jint obj_id, void *state)
{
  struct state_node *node = *head;
  struct state_node *back_ptr = NULL;

  struct state_node *new_node;

  if (node != NULL)
    {
      while (node->next != NULL && obj_id != node->key) 
	{
	  back_ptr = node;
	  node = node->next;
	}

      if (node->key == obj_id)
	{
	  /* If we're updating a node, move it to the front of the
	     list.  */
	  if (back_ptr != NULL)
	    {
	      back_ptr->next = node->next;
	      node->next = *head;
	    }
	  node->c_state = state;
	  return;
	}
    }

  new_node = (struct state_node *) malloc (sizeof (struct state_node));
  new_node->key = obj_id;
  new_node->c_state = state;
  new_node->next = *head;
  *head = new_node;
}

void 
set_state_oid (JNIEnv *env, jobject lock, struct state_table *table, 
	       jint obj_id, void *state)
{
  jint hash;
  
  hash = obj_id % table->size;

  (*env)->MonitorEnter (env, lock);
  add_node (&table->head[hash], obj_id, state);
  (*env)->MonitorExit (env, lock);
}

void *
get_state_oid (JNIEnv *env, jobject lock, struct state_table *table,
	       jint obj_id)
{
  jint hash;
  void *return_value;
  
  hash = obj_id % table->size;

  (*env)->MonitorEnter (env, lock);
  return_value = get_node (&table->head[hash], obj_id);
  (*env)->MonitorExit (env, lock);

  return return_value;
}

void *
remove_state_oid (JNIEnv *env, jobject lock, struct state_table *table,
		  jint obj_id)
{
  jint hash;
  void *return_value;
  
  hash = obj_id % table->size;

  (*env)->MonitorEnter (env, lock);
  return_value = remove_node (&table->head[hash], obj_id);
  (*env)->MonitorExit (env, lock);

  return return_value;
}

int
set_state (JNIEnv *env, jobject obj, struct state_table *table, void *state)
{
  jint obj_id;
  obj_id = (*env)->GetIntField (env, obj, table->hash);

  if ((*env)->ExceptionOccurred (env) != NULL)
    return -1;

  set_state_oid (env, table->clazz, table, obj_id, state);
  return 0;
}

void *
get_state (JNIEnv *env, jobject obj, struct state_table *table)
{
  jint obj_id;
  obj_id = (*env)->GetIntField (env, obj, table->hash);

  if ((*env)->ExceptionOccurred (env) != NULL)
    return NULL;

  return get_state_oid (env, table->clazz, table, obj_id);
}

void *
remove_state_slot (JNIEnv *env, jobject obj, struct state_table *table)
{
  jint obj_id;
  obj_id = (*env)->GetIntField (env, obj, table->hash);

  if ((*env)->ExceptionOccurred (env) != NULL)
    return NULL;

  return remove_state_oid (env, table->clazz, table, obj_id);
}
