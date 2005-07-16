/* Magical NSA API -- Associate a C ptr with an instance of an object
   Copyright (C) 1998 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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

#ifndef JCL_NATIVE_STATE
#define JCL_NATIVE_STATE

#include <jni.h>

struct state_table
{
  jint size;			/* number of slots, should be prime */
  jfieldID hash;		/* field containing System.identityHashCode(this) */
  jclass clazz;			/* lock aquired for reading/writing nodes */
  struct state_node **head;
};

struct state_node
{
  jint key;
  void *c_state;
  struct state_node *next;
};

struct state_table *cp_gtk_init_state_table_with_size (JNIEnv *, jclass, jint);
struct state_table *cp_gtk_init_state_table (JNIEnv *, jclass);

/* lowlevel api */
void cp_gtk_set_state_oid (JNIEnv *, jobject, struct state_table *, jint, void *);
void *cp_gtk_get_state_oid (JNIEnv *, jobject, struct state_table *, jint);
void *cp_gtk_remove_state_oid (JNIEnv *, jobject, struct state_table *, jint);

/* highlevel api */
int cp_gtk_set_state (JNIEnv *, jobject, struct state_table *, void *);
void *cp_gtk_get_state (JNIEnv *, jobject, struct state_table *);
void *cp_gtk_remove_state_slot (JNIEnv *, jobject, struct state_table *);

#endif
