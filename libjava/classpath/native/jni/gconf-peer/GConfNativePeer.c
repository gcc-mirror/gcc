/* GConfNativePeer.c -- Implements native methods for class GConfNativePeer
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.
   
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

#include <stdio.h>
#include <string.h>

#include <jni.h>

#include <glib.h>
#include <gdk/gdk.h>
#include <gconf/gconf-client.h>

#include "jcl.h"

#include "gnu_java_util_prefs_gconf_GConfNativePeer.h"

/*
 * Cached id, methods and objects
 */

/** Reference count */
static int reference_count = 0;

/** GConfEngine backend */
static GConfEngine *engine = NULL;

/** java.util.ArrayList class */
static jclass jlist_class = NULL;

/** java.util.ArrayList constructor id */
static jmethodID jlist_init_id = NULL;

/** ava.util.ArrayList add id */
static jmethodID jlist_add_id = NULL;

/* ***** PRIVATE FUNCTIONS DELCARATION ***** */

/**
 * Gets the reference of the default GConfEngine..
 * The client reference should be released with g_object_unref after use.
 */
static void init_gconf (void);

/**
 * Throws a new runtime exception after a failure, with the given message.
 */
static void throw_exception (JNIEnv * env, const char *msg);

/**
 * Throws the given exception after a failure, with the given message.
 */
static void
throw_exception_by_name (JNIEnv * env, const char *name, const char *msg);

/**
 * Return a reference to a java.util.ArrayList class.
 */
static gboolean set_jlist_class (JNIEnv * env);

/**
 * Builds a new reference to a new java.util.ArrayList instace.
 * The instance should be freed by the caller after use.
 */
static jclass get_jlist_reference (JNIEnv * env, jclass jlist_class);

/* ***** END: PRIVATE FUNCTIONS DELCARATION ***** */

/* ***** NATIVE FUNCTIONS ***** */

/*
 * Class:     gnu_java_util_prefs_gconf_GConfNativePeer
 * Method:    init_class
 * Signature: ()V
 */
JNIEXPORT void JNICALL
Java_gnu_java_util_prefs_gconf_GConfNativePeer_init_1class
  (JNIEnv *env, jclass clazz)
{
  if (reference_count == 0)
    {
      Java_gnu_java_util_prefs_gconf_GConfNativePeer_init_1id_1cache
		(env, clazz);
      return;
    }

  reference_count++;
}

/*
 * Class:     gnu_java_util_prefs_gconf_GConfNativePeer
 * Method:    init_id_chache
 * Signature: ()V
 */
JNIEXPORT void JNICALL
Java_gnu_java_util_prefs_gconf_GConfNativePeer_init_1id_1cache
  (JNIEnv *env, jclass clazz __attribute__ ((unused)))
{
  reference_count++;

  init_gconf ();

  /* if engine is null, there is probably an out of memory */
  if (engine == NULL)
    {
      /* release the string and throw a runtime exception */
      throw_exception (env,
      		"Unable to initialize GConfEngine in native code\n");
      return;
    }

  /* ***** java.util.ArrayList ***** */
  if (set_jlist_class (env) == FALSE)
    {
      throw_exception (env,
      		"Unable to get valid reference to java.util.List in native code\n");
      return;
    }
}

/*
 * Class:     gnu_java_util_prefs_gconf_GConfNativePeer
 * Method:    gconf_all_keys
 * Signature: (Ljava/lang/String;)Ljava/util/List;
 */
JNIEXPORT jobject JNICALL
Java_gnu_java_util_prefs_gconf_GConfNativePeer_gconf_1all_1keys
  (JNIEnv *env, jclass clazz __attribute__ ((unused)), jstring node)
{
  /* TODO: check all the calls to gdk_threads_enter/leave */
  
  const char *dir = NULL;
  const char *_val = NULL;
  const char *_val_unescaped = NULL;
  
  GError *err = NULL;
  GSList *entries = NULL;
  GSList *tmp;

  /* java.util.ArrayList */
  jobject jlist = NULL;

  dir = JCL_jstring_to_cstring (env, node);
  if (dir == NULL)
    {
      return NULL;
    }

  entries = gconf_engine_all_entries (engine, dir, &err);
  if (err != NULL)
    {
      throw_exception_by_name (env, "java/util/prefs/BackingStoreException",
                               err->message);
      g_error_free (err);
      err = NULL;

      JCL_free_cstring (env, node, dir);
      return NULL;
    }

  jlist = get_jlist_reference (env, jlist_class);
  if (jlist == NULL)
    {
      throw_exception_by_name (env, "java/util/prefs/BackingStoreException",
			       "Unable to get java.util.List reference in native code\n");
      JCL_free_cstring (env, node, dir);
      g_slist_foreach (entries, (GFunc) gconf_entry_free, NULL);
      g_slist_free (entries);
      return NULL;
    }

  tmp = entries;
  while (tmp != NULL)
    {
      _val = gconf_entry_get_key (tmp->data);
      _val = strrchr (_val, '/');
      ++_val;
      
      _val_unescaped = gconf_unescape_key (_val, strlen (_val));
      
      (*env)->CallBooleanMethod (env, jlist, jlist_add_id,
				 (*env)->NewStringUTF (env, _val_unescaped));
         
      tmp = g_slist_next (tmp);
      
      g_free ((gpointer) _val_unescaped);
    }

  /* clean up things */
  JCL_free_cstring (env, node, dir);
  g_slist_foreach (entries, (GFunc) gconf_entry_free, NULL);
  g_slist_free (entries);
  
  return jlist;
}

/*
 * Class:     gnu_java_util_prefs_gconf_GConfNativePeer
 * Method:    gconf_all_nodes
 * Signature: (Ljava/lang/String;)Ljava/util/List;
 */
JNIEXPORT jobject JNICALL
Java_gnu_java_util_prefs_gconf_GConfNativePeer_gconf_1all_1nodes
  (JNIEnv *env, jclass clazz __attribute__ ((unused)), jstring node)
{
  const char *dir = NULL;
  const char *_val = NULL;
  const char *_val_unescaped = NULL;
  
  GError *err = NULL;
  GSList *entries = NULL;
  GSList *tmp;

  /* java.util.ArrayList */
  jobject jlist = NULL;

  dir = JCL_jstring_to_cstring (env, node);
  if (dir == NULL)
    {
      return NULL;
    }

  entries = gconf_engine_all_dirs (engine, dir, &err);
  if (err != NULL)
    {
      throw_exception_by_name (env, "java/util/prefs/BackingStoreException",
                               err->message);
      g_error_free (err);
      err = NULL;
      JCL_free_cstring (env, node, dir);
      return NULL;
    }

  jlist = get_jlist_reference (env, jlist_class);
  if (jlist == NULL)
    {
      throw_exception_by_name (env, "java/util/prefs/BackingStoreException",
			       "Unable to get java.util.List reference in native code\n");
      JCL_free_cstring (env, node, dir);
      g_slist_foreach (entries, (GFunc) gconf_entry_free, NULL);
      g_slist_free (entries);
      return NULL;
    }

  tmp = entries;
  while (tmp != NULL)
    {
      _val = tmp->data;
      
      _val = strrchr (_val, '/');
      ++_val;
      
      _val_unescaped = gconf_unescape_key (_val, strlen (_val));
      
      (*env)->CallBooleanMethod (env, jlist, jlist_add_id,
				 (*env)->NewStringUTF (env, _val_unescaped));
      
      tmp = g_slist_next (tmp);
      
      g_free ((gpointer) _val_unescaped);
    }

  /* clean up things */
  JCL_free_cstring (env, node, dir);
  g_slist_foreach (entries, (GFunc) gconf_entry_free, NULL);
  g_slist_free (entries);

  return jlist;
}

/*
 * Class:     gnu_java_util_prefs_gconf_GConfNativePeer
 * Method:    gconf_suggest_sync
 * Signature: ()V
 */
JNIEXPORT void JNICALL
Java_gnu_java_util_prefs_gconf_GConfNativePeer_gconf_1suggest_1sync
  (JNIEnv *env, jclass clazz __attribute__ ((unused)))
{
  GError *err = NULL;

  gconf_engine_suggest_sync (engine, &err);
  if (err != NULL)
    {
      throw_exception_by_name (env, "java/util/prefs/BackingStoreException",
			       			   err->message);
      g_error_free (err);
      err = NULL;
    }
}

/*
 * Class:     gnu_java_util_prefs_gconf_GConfNativePeer
 * Method:    gconf_unset
 * Signature: (Ljava/lang/String;)Z
 */
JNIEXPORT jboolean JNICALL
Java_gnu_java_util_prefs_gconf_GConfNativePeer_gconf_1unset
  (JNIEnv *env, jclass clazz __attribute__ ((unused)), jstring key)
{
  const char *_key = NULL;
  gboolean result = JNI_FALSE;
  GError *err = NULL;

  _key = JCL_jstring_to_cstring (env, key);
  if (_key == NULL)
    {
      return JNI_FALSE;
    }

  result = gconf_engine_unset (engine, _key, &err);
  if (err != NULL)
    {
      result = JNI_FALSE;
      g_error_free (err);
      err = NULL;
    }
    
  JCL_free_cstring (env, key, _key);

  return result;
}

/*
 * Class:     gnu_java_util_prefs_gconf_GConfNativePeer
 * Method:    gconf_get_string
 * Signature: (Ljava/lang/String;)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_gnu_java_util_prefs_gconf_GConfNativePeer_gconf_1get_1string
  (JNIEnv *env, jclass clazz __attribute__ ((unused)), jstring key)
{
  const char *_key = NULL;
  const char *_value = NULL;
  GError *err = NULL;
  jstring result = NULL;

  _key = JCL_jstring_to_cstring (env, key);
  if (_key == NULL)
    {
      return NULL;
    }

  _value = gconf_engine_get_string (engine, _key, &err);
  JCL_free_cstring (env, key, _key);
  if (err != NULL)
    {
      /* just in case */
      if (_value != NULL) g_free ((gpointer) _value);
      g_error_free (err);
      err = NULL;
      
      return NULL;
    }

  /* Even if Gconf reported no error it is possible that NULL was returned */
  /* and it should be prevented to create a Java string from that value. */
  if (_value != NULL)
    {
      result = (*env)->NewStringUTF (env, _value);
      g_free ((gpointer) _value);
    }
  
  gconf_engine_suggest_sync (engine, NULL);
  
  return result;
}

/*
 * Class:     gnu_java_util_prefs_gconf_GConfNativePeer
 * Method:    gconf_set_string
 * Signature: (Ljava/lang/String;Ljava/lang/String;)Z
 */
JNIEXPORT jboolean JNICALL
Java_gnu_java_util_prefs_gconf_GConfNativePeer_gconf_1set_1string
  (JNIEnv *env, jclass clazz __attribute__ ((unused)),
   jstring key, jstring value)
{
  const char *_key = NULL;
  const char *_value = NULL;
  GError *err = NULL;

  gboolean result = JNI_FALSE;

  /* load an UTF string from the virtual machine. */
  _key = JCL_jstring_to_cstring (env, key);
  _value = JCL_jstring_to_cstring (env, value);
  if (_key == NULL || _value == NULL)
    {
      return JNI_FALSE;
    }

  result = gconf_engine_set_string (engine, _key, _value, &err);
  if (err != NULL)
  	{
      g_error_free (err);
      err = NULL;
      result = JNI_FALSE;
  	}
	
  JCL_free_cstring (env, key, _key);
  JCL_free_cstring (env, value, _value);

  return (jboolean) result;
}

/*
 * Class:     gnu_java_util_prefs_gconf_GConfNativePeer
 * Method:    gconf_dir_exists
 * Signature: (Ljava/lang/String;)Z
 */
JNIEXPORT jboolean JNICALL
Java_gnu_java_util_prefs_gconf_GConfNativePeer_gconf_1dir_1exists
  (JNIEnv *env, jclass clazz __attribute__ ((unused)), jstring node)
{
  const char *dir = NULL;
  GError *err = NULL;
  jboolean value = JNI_FALSE;

  dir = JCL_jstring_to_cstring (env, node);
  if (dir == NULL)
    return value;

  /* on error return false */
  value = gconf_engine_dir_exists (engine, dir, &err);
  if (err != NULL)
    value = JNI_FALSE;

  JCL_free_cstring (env, node, dir);

  return value;
}

/*
 * Class:     gnu_java_util_prefs_gconf_GConfNativePeer
 * Method:    finalize_class
 * Signature: ()V
 */
JNIEXPORT void JNICALL
Java_gnu_java_util_prefs_gconf_GConfNativePeer_finalize_1class
  (JNIEnv *env, jclass clazz __attribute__ ((unused)))
{
  if (reference_count == 0)
    {
      /* last reference, free all resources and return */
      g_object_unref (G_OBJECT (engine));
      
      (*env)->DeleteGlobalRef (env, jlist_class);

      jlist_class = NULL;
      jlist_init_id = NULL;
      jlist_add_id = NULL;

      return;
    }

  reference_count--;
}

/*
 * Class:     gnu_java_util_prefs_gconf_GConfNativePeer
 * Method:    Java_gnu_java_util_prefs_gconf_GConfNativePeer_gconf_1escape_1key
 * Signature: (Ljava/lang/String;)Z
 */
JNIEXPORT jstring JNICALL
Java_gnu_java_util_prefs_gconf_GConfNativePeer_gconf_1escape_1key
  (JNIEnv *env, jclass clazz __attribute__ ((unused)), jstring plain)
{
  const char *escaped = NULL;
  const char *_plain = NULL;
  jstring result = NULL;
  
  _plain = JCL_jstring_to_cstring (env, plain);
  if (_plain == NULL)
    {
      return NULL;
    }

  escaped = gconf_escape_key (_plain, strlen (_plain));
  
  JCL_free_cstring (env, plain, _plain);
  /* check for NULL, if so prevent string creation */
  if (escaped != NULL)
    {
      result = (*env)->NewStringUTF (env, escaped);
      g_free ((gpointer) escaped);
    }
  
  return result;
}

/*
 * Class:     gnu_java_util_prefs_gconf_GConfNativePeer
 * Method:    Java_gnu_java_util_prefs_gconf_GConfNativePeer_gconf_1unescape_1key
 * Signature: (Ljava/lang/String;)Z
 */
JNIEXPORT jstring JNICALL
Java_gnu_java_util_prefs_gconf_GConfNativePeer_gconf_1unescape_1key
  (JNIEnv *env, jclass clazz __attribute__ ((unused)), jstring escaped)
{
  const char *plain = NULL;
  const char *_escaped = NULL;
  jstring result = NULL;
  
  _escaped = JCL_jstring_to_cstring (env, escaped);
  if (_escaped == NULL)
    {
      return NULL;
    }

  plain = gconf_unescape_key (_escaped, strlen (_escaped));
  
  JCL_free_cstring (env, escaped, _escaped);
  /* check for NULL, if so prevent string creation */
  if (plain != NULL)
    {
      result = (*env)->NewStringUTF (env, plain);
      g_free ((gpointer) plain);
    }
  
  return result;
}

/* ***** END: NATIVE FUNCTIONS ***** */

/* ***** PRIVATE FUNCTIONS IMPLEMENTATION ***** */

static void throw_exception (JNIEnv *env, const char *msg)
{
  throw_exception_by_name (env, "java/lang/RuntimeException", msg);
}

static void
throw_exception_by_name (JNIEnv *env, const char *name, const char *msg)
{
  JCL_ThrowException (env, name, msg);
}

static void init_gconf (void)
{
  engine = gconf_engine_get_default ();
}

static gboolean set_jlist_class (JNIEnv *env)
{
  jclass local_jlist_class = NULL;

  /* gets a reference to the ArrayList class */
  local_jlist_class = JCL_FindClass (env, "java/util/ArrayList");
  if (local_jlist_class == NULL)
    {
      return FALSE;
    }

  jlist_class = (*env)->NewGlobalRef (env, local_jlist_class);
  (*env)->DeleteLocalRef (env, local_jlist_class);
  if (jlist_class == NULL)
    {
      return FALSE;
    }

  /* and initialize it */
  jlist_init_id = (*env)->GetMethodID (env, jlist_class, "<init>", "()V");
  if (jlist_init_id == NULL)
    {
      return FALSE;
    }

  jlist_add_id = (*env)->GetMethodID (env, jlist_class, "add",
				      				  "(Ljava/lang/Object;)Z");
  if (jlist_add_id == NULL)
    {
      return FALSE;
    }

  return TRUE;
}

static jobject get_jlist_reference (JNIEnv *env, jclass jlist_class)
{
  return (*env)->NewObject (env, jlist_class, jlist_init_id);
}

/* ***** END: PRIVATE FUNCTIONS IMPLEMENTATION ***** */
