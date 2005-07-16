/* gthread-jni.c -- JNI threading routines for GLIB
   Copyright (C) 1998, 2004 Free Software Foundation, Inc.

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

/************************************************************************/
/* Header				     				*/
/************************************************************************/

/*
 * @author Julian Dolby (dolby@us.ibm.com)
 * @date February 7, 2003  implemented for GLIB v.1
 * 
 *
 * @author Steven Augart 
 * <steve+classpath at augart dot com>, <augart at watson dot ibm dot com>
 * @date April 30, 2004 -- May 10 2004: Support new functions for Glib v.2,
 * fix cond_wait to free and re-acquire the mutex,
 * replaced trylock stub implementation with a full one.
 *
 *  This code implements the GThreadFunctions interface for GLIB using 
 * Java threading primitives.  All of the locking and conditional variable
 * functionality required by GThreadFunctions is implemented using the
 * monitor and wait/notify functionality of Java objects.  The thread-
 * local functionality uses the java.lang.ThreadLocal class. 
 *
 *  Classpath's AWT support uses GTK+ peers.  GTK+ uses GLIB.  GLIB by default
 * uses the platform's native threading model -- pthreads in most cases.  If
 * the Java runtime doesn't use the native threading model, then it needs this
 * code in order to use Classpath's (GTK+-based) AWT routines.
 *
 *  This code should be portable; I believe it makes no assumptions
 * about the underlying VM beyond that it implements the JNI functionality
 * that this code uses.
 *
 *  Currently, use of this code is governed by the configuration option
 * --enable-portable-native-sync.  We will soon add a VM hook so the VM can
 * select which threading model it wants to use at run time; at that point,
 * the configuration option will go away.
 *
 * The code in this file uses only JNI 1.1, except for one JNI 1.2 function:
 * GetEnv, in the JNI Invocation API.  (There seems to be no way around using
 * GetEnv).
 *
 * ACKNOWLEDGEMENT:
 * 
 *  I would like to thank Mark Wielaard for his kindness in spending at least
 * six hours of his own time in reviewing this code and correcting my GNU
 * coding and commenting style.  --Steve Augart
 *
 *
 * NOTES:
 *
 *  This code has been tested with Jikes RVM and with Kaffe.
 *
 *  This code should have proper automated unit tests.  I manually tested it
 *  by running an application that uses AWT. --Steven Augart
 *
 * MINOR NIT:
 *
 *  - Using a jboolean in the arglist to "throw()" and "rethrow()"
 *    triggers many warnings from GCC's -Wconversion operation, because that
 *    is not the same as the conversion (upcast to an int) that would occur in
 *    the absence of a prototype.
 *    
 *    It would be very slightly more efficient to just pass the jboolean, but
 *    is not worth the clutter of messages.  The right solution would be to
 *    turn off the -Wconversion warning for just this file, *except* that
 *    -Wconversion also warns you against constructs such as:
 *        unsigned u = -1;
 *    and that is a useful warning.  So I went from a "jboolean" to a
 *    "gboolean"  (-Wconversion is not enabled by default for GNU Classpath,
 *    but it is in my own CFLAGS, which, for gcc 3.3.3, read: -pipe -ggdb3 -W
 *    -Wall -Wbad-function-cast -Wcast-align -Wpointer-arith -Wcast-qual
 *    -Wshadow -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations
 *    -fkeep-static-consts -fkeep-inline-functions -Wundef -Wwrite-strings
 *    -Wno-aggregate-return -Wmissing-noreturn -Wnested-externs -Wtrigraphs
 *    -Wconversion -Wsign-compare -Wno-float-equal -Wmissing-format-attribute
 *    -Wno-unreachable-code -Wdisabled-optimization )
 */

#include <config.h>

/************************************************************************/
/* Configuration							*/
/************************************************************************/

/** Tracing and Reporting  **/
#define TRACE_API_CALLS	    0	/* announce entry and exit into each method,
				   by printing to stderr. */

#define TRACE_MONITORS      0	/* Every enterMonitor() and exitMonitor() goes
				   to stderr. */

/** Trouble handling.  There is a discussion below of this.  **/ 
#define EXPLAIN_TROUBLE	    1	/* Describe any unexpected trouble that
				   happens.  This is a superset
				   of EXPLAIN_BROKEN, and if set trumps an
				   unset EXPLAIN_BROKEN.  It is not a strict
				   superset, since at the moment there is no
				   TROUBLE that is not also BROKEN.   

				   Use criticalMsg() to describe the problem.
				 */

#define EXPLAIN_BROKEN	    1	/* Describe trouble that is serious enough to
				   be BROKEN.  (Right now all trouble is at
				   least BROKEN.) */

/* There is no EXPLAIN_BADLY_BROKEN definition.  We always explain
   BADLY_BROKEN trouble, since there is no other way to report it.  */


/** Error Handling  **/
#define DIE_IF_BROKEN	    1	/* Dies if serious trouble happens.  There is
				   really no non-serious trouble, except
				   possibly problems that arise during
				   pthread_create, which are reported by a
				   GError.

				   If you do not set DIE_IF_BROKEN, then
				   trouble will raise a Java RuntimeException.
				   We probably do want to die right away,
				   since anything that's BROKEN really
				   indicates a programming error or a
				   system-wide error, and that's what the glib
				   documentation says you should do in case of
				   that kind of error in a glib-style
				   function.  But it does work to turn this
				   off.  */

#if  DIE_IF_BROKEN
#define DIE_IF_BADLY_BROKEN 1	/* DIE_IF_BROKEN implies DIE_IF_BADLY_BROKEN */
#else
#define DIE_IF_BADLY_BROKEN 1	/* Die if the system is badly broken --
				   that is, if we have further trouble while
				   attempting to throw an exception
				   upwards, or if we are unable to generate
				   one of the classes we'll need in order to
				   throw wrapped exceptions upward.

				   If unset, we will print a warning message,
				   and limp along anyway.  Not that the system
				   is likely to work.  */
#endif

/** Performance tuning parameters **/

#define ENABLE_EXPENSIVE_ASSERTIONS 0	/* Enable expensive assertions? */

#define DELETE_LOCAL_REFS   1	/* Whether to delete local references.   

				   JNI only guarantees that there wil be 16
				   available.  (Jikes RVM provides an number
				   only limited by VM memory.)

				   Jikes RVM will probably perform faster if
				   this is turned off, but other VMs may need
				   this to be turned on in order to perform at
				   all, or might need it if things change.

				   Remember, we don't know how many of those
				   local refs might have already been used up
				   by higher layers of JNI code that end up
				   calling g_thread_self(),
				   g_thread_set_private(), and so on.

				   We set this to 1 for GNU Classpath, since
				   one of our principles is "always go for the
				   most robust implementation" */

#define  HAVE_JNI_VERSION_1_2   0 /* Assume we don't.  We could
				     dynamically check for this.  We will
				     assume JNI 1.2 in later versions of
				     Classpath.  

                                     As it stands, the code in this file
                                     already needs one JNI 1.2 function:
                                     GetEnv, in the JNI Invocation API.

				     TODO This code hasn't been tested yet.
				     And really hasn't been implemented yet.
				     */ 

/************************************************************************/
/* Global data				     				*/
/************************************************************************/

#if defined HAVE_STDINT_H
#include <stdint.h>		/* provides intptr_t */
#elif defined HAVE_INTTYPES_H
#include <inttypes.h>
#endif
#include <stdarg.h>		/* va_list */
#include <glib.h>
#include "gthread-jni.h"
#include <assert.h>		/* assert() */

/* For Java thread priority constants. */
#include <gnu_java_awt_peer_gtk_GThreadNativeMethodRunner.h>

/* Since not all JNI header generators actually define constants we
 define them here explicitly. */
#ifndef gnu_java_awt_peer_gtk_GThreadNativeMethodRunner_MIN_PRIORITY
#define gnu_java_awt_peer_gtk_GThreadNativeMethodRunner_MIN_PRIORITY 1
#endif
#ifndef gnu_java_awt_peer_gtk_GThreadNativeMethodRunner_NORM_PRIORITY
#define gnu_java_awt_peer_gtk_GThreadNativeMethodRunner_NORM_PRIORITY 5
#endif
#ifndef gnu_java_awt_peer_gtk_GThreadNativeMethodRunner_MAX_PRIORITY
#define gnu_java_awt_peer_gtk_GThreadNativeMethodRunner_MAX_PRIORITY 10
#endif

/*  The VM handle.  This is set in
    Java_gnu_java_awt_peer_gtk_GtkMainThread_gtkInit */
JavaVM *cp_gtk_the_vm;

/* Unions used for type punning. */
union env_union
{
  void **void_env;
  JNIEnv **jni_env;
};

union func_union
{
  void *void_func;
  GThreadFunc g_func;
};

/* Forward Declarations for Functions  */
static int threadObj_set_priority (JNIEnv * env, jobject threadObj,
				   GThreadPriority gpriority);
static void fatalMsg (const char fmt[], ...)
     __attribute__ ((format (printf, 1, 2)))
     __attribute__ ((noreturn));

static void criticalMsg (const char fmt[], ...)
     __attribute__ ((format (printf, 1, 2)));

static void tracing (const char fmt[], ...)
     __attribute__ ((format (printf, 1, 2)));

static jint javaPriorityLevel (GThreadPriority priority)
     __attribute__ ((const));

/************************************************************************/
/* Trouble-handling, including utilities to reflect exceptions		*/
/* back to the VM.  Also some status reporting.				*/
/************************************************************************/

/* How are we going to handle problems?

   There are several approaches:

   1)  Report them with the GError mechanism.

       (*thread_create)() is the only one of these functions that takes a
       GError pointer.  And the only G_THREAD error defined maps onto EAGAIN.
       We don't have any errors in our (*thread_create)() implementation that
       can be mapped to EAGAIN.  So this idea is a non-starter.

   2)  Reflect the exception back to the VM, wrapped in a RuntimeException.
       This will fail sometimes, if we're so broken (BADLY_BROKEN) that we
       fail to throw the exception. 

   3)  Abort execution.  This is what the glib functions themselves do for
       errors that they can't report via GError.

       Enable DIE_IF_BROKEN and/or DIE_IF_BADLY_BROKEN to
       make this the default for BROKEN and/or BADLY_BROKEN trouble.

   4) Display messages to stderr.  We always do this for BADLY_BROKEN
      trouble.  The glib functions do that for errors they can't report via
      GError. 

   There are some complications.

   When I attempted to report a problem in g_thread_self() using g_critical (a
   macro around g_log(), I found that g_log in turn looks for thread-private
   data and calls g_thread_self() again.

   We got a segfault, probably due to stack overflow.  So, this code doesn't
   use the g_critical() and g_error() functions any more.  Nor do we use
   g_assert(); we use the C library's assert() instead.
*/


#define WHERE __FILE__ ":" G_STRINGIFY(__LINE__) ": "

/* This is portable to older compilers that lack variable-argument macros.
   This used to be just g_critical(), but then we ran into the error reporting
   problem discussed above.
*/
static void
fatalMsg (const char fmt[], ...)
{
  va_list ap;
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);
  fputs ("\nAborting execution\n", stderr);
  abort ();
}


static void
criticalMsg (const char fmt[], ...)
{
  va_list ap;
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);
  putc ('\n', stderr);
}

/* Unlike the other two, this one does not append a newline.  This is only
   used if one of the TRACE_ macros is defined.  */
static void
tracing (const char fmt[], ...)
{
  va_list ap;
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);
}

#define assert_not_reached()						\
  do									\
    {									\
      fputs(WHERE "You should never get here.  Aborting execution.\n", 	\
	    stderr);							\
      abort();								\
    }									\
  while(0)


#if DIE_IF_BADLY_BROKEN
#define BADLY_BROKEN fatalMsg
#else
#define BADLY_BROKEN criticalMsg
/* So, the user may still attempt to recover, even though we do not advise
   this. */
#endif

/* I find it so depressing to have to use C without varargs macros. */
#define BADLY_BROKEN_MSG WHERE "Something fundamental"		\
	" to GNU Classpath's AWT JNI broke while we were trying to pass up a Java error message"

#define BADLY_BROKEN0()				\
    BADLY_BROKEN(BADLY_BROKEN_MSG);
#define	    BADLY_BROKEN1(msg)			\
    BADLY_BROKEN(BADLY_BROKEN_MSG ": " msg)
#define	    BADLY_BROKEN2(msg, arg)			\
    BADLY_BROKEN(BADLY_BROKEN_MSG ": " msg, arg)
#define	    BADLY_BROKEN3(msg, arg, arg2) 		\
    BADLY_BROKEN(BADLY_BROKEN_MSG ": " msg, arg1, arg2)
#define	    BADLY_BROKEN4(msg, arg, arg2, arg3) 		\
    BADLY_BROKEN(BADLY_BROKEN_MSG ": " msg, arg1, arg2, arg3)

#define DELETE_LOCAL_REF(env, ref) 		\
  do 						\
    {						\
      if ( DELETE_LOCAL_REFS )			\
	{					\
	  (*env)->DeleteLocalRef (env, ref);	\
	  (ref) = NULL;				\
	}					\
    }						\
  while(0)

/* Cached info for Exception-wrapping */

static jclass runtimeException_class;	/* java.lang.RuntimeException */
static jmethodID runtimeException_ctor; /* constructor for it */


/* Throw a new RuntimeException.  It may wrap around an existing exception.
   1 if we did rethrow, -1 if we had trouble while rethrowing.
   isBroken is always true in this case. */
static int
throw (JNIEnv * env, jthrowable cause, const char *message,
       gboolean isBroken, const char *file, int line)
{
  jstring jmessage;
  gboolean describedException = FALSE;	/* Did we already describe the
					   exception to stderr or the
					   equivalent?   */
  jthrowable wrapper;

  /* allocate local message in Java */
  const char fmt[] = "In AWT JNI, %s (at %s:%d)";
  size_t len = strlen (message) + strlen (file) + sizeof fmt + 25;
  char *buf;

  if (EXPLAIN_TROUBLE || (isBroken && EXPLAIN_BROKEN))
    {
      criticalMsg ("%s:%d: AWT JNI failure%s: %s\n", file, line,
		   isBroken ? " (BROKEN)" : "", message);
      if (cause)
	{
	  jthrowable currentException = (*env)->ExceptionOccurred (env);

	  if (cause == currentException)
	    {
	      criticalMsg ("Description follows to System.err:");
	      (*env)->ExceptionDescribe (env);
	      /* ExceptionDescribe has the side-effect of clearing the pending
	         exception; relaunch it.  */
	      describedException = TRUE;

	      if ((*env)->Throw (env, cause))
		{
		  BADLY_BROKEN1
		    ("Relaunching an exception with Throw failed.");
		  return -1;
		}
	    }
	  else
	    {
	      DELETE_LOCAL_REF (env, currentException);
	      criticalMsg (WHERE
			   "currentException != cause; something else happened"
			   " while handling an exception.");
	    }
	}
    }				/* if (EXPLAIN_TROUBLE) */

  if (isBroken && DIE_IF_BROKEN)
    fatalMsg ("%s:%d: Aborting execution; BROKEN: %s\n", file, line, message);

  if ((buf = malloc (len)))
    {
      memset (buf, 0, len);
      g_snprintf (buf, len, fmt, message, file, line);
      jmessage = (*env)->NewStringUTF (env, buf);
      free (buf);
    }
  else
    {
      jmessage = NULL;
    }

  /* Create the RuntimeException wrapper object and throw it.  It is OK for
     CAUSE to be NULL. */
  wrapper = (jthrowable) (*env)->NewObject
    (env, runtimeException_class, runtimeException_ctor, jmessage, cause);
  DELETE_LOCAL_REF (env, jmessage);

  if (!wrapper)
    {
      /* I think this should only happen:
         - if there are bugs in my JNI code, or
         - if the VM is broken, or 
         - if we run out of memory. 
       */
      if (EXPLAIN_TROUBLE)
	{
	  criticalMsg (WHERE "GNU Classpath: JNI NewObject() could not create"
		       " a new java.lang.RuntimeException.");
	  criticalMsg ("We were trying to warn about the following"
		       " previous failure:");
	  criticalMsg ("%s:%d: %s", file, line, message);
	  criticalMsg ("The latest (NewObject()) exception's description"
		       " follows, to System.err:");
	  (*env)->ExceptionDescribe (env);
	}
      BADLY_BROKEN1 ("Failure of JNI NewObject()"
		     " to make a java.lang.RuntimeException");
      return -1;
    }


  /* throw it */
  if ((*env)->Throw (env, wrapper))
    {
      /* Throw() should just never fail, unless we're in such severe trouble
         that we might as well die. */
      BADLY_BROKEN1
	("GNU Classpath: Failure of JNI Throw to report an Exception");
      return -1;
    }

  DELETE_LOCAL_REF (env, wrapper);
  return 1;
}



/* Rethrow an exception we received, wrapping it with a RuntimeException.  1
   if we did rethrow, -1 if we had trouble while rethrowing.
   CAUSE should be identical to the most recent exception that happened, so
   that ExceptionDescribe will work.  (Otherwise nix.) */
static int
rethrow (JNIEnv * env, jthrowable cause, const char *message,
	 gboolean isBroken, const char *file, int line)
{
  assert (cause);
  return throw (env, cause, message, isBroken, file, line);
}


/* This function checks for a pending exception, and rethrows it with
 * a wrapper RuntimeException to deal with possible type problems (in
 * case some calling piece of code does not expect the exception being
 * thrown) and to include the given extra message.
 *
 * Returns 0 if no problems found (so no exception thrown), 1 if we rethrew an
 * exception.   Returns -1 on failure. 
 */
static int
maybe_rethrow (JNIEnv * env, const char *message, gboolean isBroken,
	       const char *file, int line)
{
  jthrowable cause = (*env)->ExceptionOccurred (env);
  int ret = 0;

  /* rethrow if an exception happened */
  if (cause)
    {
      ret = rethrow (env, cause, message, isBroken, file, line);
      DELETE_LOCAL_REF (env, cause);
    }

  return 0;
}

/* MAYBE_TROUBLE() is used to include a source location in the exception
   message. Once we have run maybe_rethrow, if there WAS trouble, 
   return TRUE, else FALSE.   

   MAYBE_TROUBLE() is actually never used; all problems that throw exceptions
   are BROKEN, at least.  Nothing is recoverable :(.  See the discussion of
   possible errors at thread_create_jni_impl().  */
#define MAYBE_TROUBLE(_env, _message)				\
	maybe_rethrow(_env, _message, FALSE, __FILE__, __LINE__)

/* MAYBE_TROUBLE(), but something would be BROKEN if it were true. */
#define MAYBE_BROKEN(_env, _message)				\
	maybe_rethrow(_env, _message, TRUE, __FILE__, __LINE__)

/* Like MAYBE_TROUBLE(), TROUBLE() is never used. */
#define TROUBLE(_env, _message)						\
	rethrow(_env, (*env)->ExceptionOccurred (env), _message, FALSE, \
		__FILE__, __LINE__)

#define BROKEN(_env, _message)						\
	rethrow (_env, (*env)->ExceptionOccurred (env), _message, TRUE, \
		 __FILE__, __LINE__)

/* Like MAYBE_TROUBLE(), NEW_TROUBLE() is never used. */
#define NEW_TROUBLE(_env, _message)					\
	throw (_env, NULL,  _message, FALSE, __FILE__, __LINE__)

#define NEW_BROKEN(_env, _message)				\
	throw (_env, NULL, _message, TRUE, __FILE__, __LINE__)

/* Like MAYBE_TROUBLE(), RETHROW_CAUSE() is never used. */
#define RETHROW_CAUSE(_env, _cause, _message)				\
	rethrow (_env, _cause, _message, FALSE, __FILE__, __LINE__)

#define BROKEN_CAUSE(_env, _cause, _message)				\
	rethrow (_env, _cause, _message, TRUE, __FILE__, __LINE__)

/* Macros to handle the possibility that someone might have called one of the
   GThreadFunctions API functions with a Java exception pending.  It is
   generally discouraged to continue to use JNI after a Java exception has
   been raised.  Sun's JNI book advises that one trap JNI errors immediately
   and not continue with an exception pending.

   These are #if'd out for these reasons:

   1) They do not work in the C '89 subset that Classpath is currently 
      (2004 May 10) sticking to; HIDE_OLD_TROUBLE() includes a declaration
      that should be in scope for the rest of the function, so it needs a
      language version that lets you mix declarations and statements.  (This
      could be worked around if it were important.)

   2) They chew up more time and resources.  

   3) There does not ever seem to be old trouble -- the assertion in
      HIDE_OLD_TROUBLE never goes off. 

   You will want to re-enable them if this code needs to be used in a context
   where old exceptions might be pending when the GThread functions are
   called.

   The implementations in this file are responsible for skipping around calls
   to SHOW_OLD_TROUBLE() if they've raised exceptions during the call.  So, if
   we reach SHOW_OLD_TROUBLE, we are guaranteed that there are no exceptions
   pending. */
#if 1
#define HIDE_OLD_TROUBLE(env)				\
    assert ( NULL == (*env)->ExceptionOccurred (env) )

#define SHOW_OLD_TROUBLE()	\
    assert ( NULL == (*env)->ExceptionOccurred (env) )
#else  /* 0 */
#define HIDE_OLD_TROUBLE(env)					\
   jthrowable savedTrouble = (*env)->ExceptionOccurred (env);	\
   (*env)->ExceptionClear (env);

#define SHOW_OLD_TROUBLE() do 					\
{								\
  assert ( NULL == (*env)->ExceptionOccurred (env) )		\
  if (savedTrouble) 						\
    {								\
      if ((*env)->Throw (env, savedTrouble)) 			\
	  BADLY_BROKEN ("ReThrowing the savedTrouble failed");	\
    }								\
  DELETE_LOCAL_REF (env, savedTrouble);				\
} while(0)

#endif /* 0 */

/* Set up the cache of jclass and jmethodID primitives we need
   in order to throw new exceptions and rethrow exceptions.  We do this
   independently of the other caching.  We need to have this cache set up
   first, so that we can then report errors properly. 

   If any errors while setting up the error cache, the world is BADLY_BROKEN.

   May be called more than once.

   Returns -1 if the cache was not initialized properly, 1 if it was.  
*/
static int
setup_exception_cache (JNIEnv * env)
{
  static int exception_cache_initialized = 0;	/* -1 for trouble, 1 for proper
						   init.  */

  jclass lcl_class;		/* a class used for local refs */

  if (exception_cache_initialized)
    return exception_cache_initialized;
  lcl_class = (*env)->FindClass (env, "java/lang/RuntimeException");
  if ( ! lcl_class )
    {
      BADLY_BROKEN1 ("Broken Class library or VM?"
		     "  Couldn't find java/lang/RuntimeException");
      return exception_cache_initialized = -1;
    }
  /* Pin it down. */
  runtimeException_class = (jclass) (*env)->NewGlobalRef (env, lcl_class);
  DELETE_LOCAL_REF (env, lcl_class);
  if (!runtimeException_class)
    {
      BADLY_BROKEN1 ("Serious trouble: could not turn"
		     " java.lang.RuntimeException into a global reference");
      return exception_cache_initialized = -1;
    }

  runtimeException_ctor = 
    (*env)->GetMethodID (env, runtimeException_class, "<init>",
			   "(Ljava/lang/String;Ljava/lang/Throwable;)V");
  if ( ! runtimeException_ctor )
    {
      BADLY_BROKEN1 ("Serious trouble: classpath couldn't find a"
		     " two-arg constructor for java/lang/RuntimeException");
      return exception_cache_initialized = -1;
    }

  return exception_cache_initialized = 1;
}


/**********************************************************/
/***** The main cache *************************************/
/**********************************************************/

/** This is a cache of all classes, methods, and field IDs that we use during
   the run.  We maintain a permanent global reference to each of the classes
   we cache, since otherwise the (local) jclass that refers to that class
   would go out of scope and possibly be reused in further calls.

   The permanent global reference also achieves the secondary goal of
   protecting the validity of the methods and field IDs in case the classes
   were otherwise unloaded and then later loaded again.  Obviously, this will
   never happen to classes such as java.lang.Thread and java.lang.Object, but
   the primary reason for maintaining permanent global refs is sitll valid.

   The code in jnilink.c has a similar objective.  TODO: Consider using that
   code instead.

   --Steven Augart
*/

/* All of these are cached classes and method IDs: */
/* java.lang.Object */
static jclass obj_class;		/* java.lang.Object */
static jmethodID obj_ctor;		/* no-arg Constructor for java.lang.Object */
static jmethodID obj_notify_mth;	/* java.lang.Object.notify() */
static jmethodID obj_notifyall_mth;	/* java.lang.Object.notifyall() */
static jmethodID obj_wait_mth;		/* java.lang.Object.wait() */
static jmethodID obj_wait_nanotime_mth; /* java.lang.Object.wait(JI) */

/* GThreadMutex and its methods */
static jclass mutex_class;
static jmethodID mutex_ctor;
static jfieldID mutex_lockForPotentialLockers_fld;
static jfieldID mutex_potentialLockers_fld;

/* java.lang.Thread and its methods*/
static jclass thread_class;		/* java.lang.Thread */
static jmethodID thread_current_mth;	/* Thread.currentThread() */
static jmethodID thread_equals_mth;	/* Thread.equals() */
static jmethodID thread_join_mth;	/* Thread.join() */
static jmethodID thread_setPriority_mth; /* Thread.setPriority() */
static jmethodID thread_stop_mth;	/* Thread.stop() */
static jmethodID thread_yield_mth;	/* Thread.yield() */

/* java.lang.ThreadLocal and its methods */
static jclass threadlocal_class;	/* java.lang.ThreadLocal */
static jmethodID threadlocal_ctor;	/* Its constructor */
static jmethodID threadlocal_set_mth;	/* ThreadLocal.set() */
static jmethodID threadlocal_get_mth;	/* ThreadLocal.get() */

/* java.lang.Long and its methods */
static jclass long_class;		/* java.lang.Long */
static jmethodID long_ctor;		/* constructor for it: (J) */
static jmethodID long_longValue_mth;	/* longValue()J */


/* GThreadNativeMethodRunner */
static jclass runner_class;
static jmethodID runner_ctor;
static jmethodID runner_threadToThreadID_mth;
static jmethodID runner_threadIDToThread_mth;
static jmethodID runner_deRegisterJoinable_mth;
static jmethodID runner_start_mth;	/* Inherited Thread.start() */


/* java.lang.InterruptedException */
static jclass interrupted_exception_class;




/* Returns a negative value if there was trouble during initialization.
   Returns a positive value of the cache was initialized correctly.
   Never returns zero. */
static int
setup_cache (JNIEnv * env)
{
  jclass lcl_class;
  static int initialized = 0;	/* 1 means initialized, 0 means uninitialized,
				   -1 means mis-initialized */

  if (initialized)
    return initialized;

  /* make sure we can report on trouble */
  if (setup_exception_cache (env) < 0)
    return initialized = -1;

#ifdef JNI_VERSION_1_2
  if (HAVE_JNI_VERSION_1_2)
    assert ( ! (*env)->ExceptionCheck (env));
  else
#endif
    assert ( ! (*env)->ExceptionOccurred (env));

  /* java.lang.Object and its methods */
  lcl_class = (*env)->FindClass (env, "java/lang/Object");
  if (!lcl_class)
    {
      BROKEN (env, "cannot find java.lang.Object");
      return initialized = -1;
    }

  /* Pin it down. */
  obj_class = (jclass) (*env)->NewGlobalRef (env, lcl_class);
  DELETE_LOCAL_REF (env, lcl_class);
  if (!obj_class)
    {
      BROKEN (env, "Cannot get a global reference to java.lang.Object");
      return initialized = -1;
    }

  obj_ctor = (*env)->GetMethodID (env, obj_class, "<init>", "()V");
  if (!obj_ctor)
    {
      BROKEN (env, "cannot find constructor for java.lang.Object");
      return initialized = -1;
    }

  obj_notify_mth = (*env)->GetMethodID (env, obj_class, "notify", "()V");
  if ( ! obj_notify_mth )
    {
      BROKEN (env, "cannot find java.lang.Object.notify()V");
      return initialized = -1;
    }

  obj_notifyall_mth =
    (*env)->GetMethodID (env, obj_class, "notifyAll", "()V");
  if ( ! obj_notifyall_mth)
    {
      BROKEN (env, "cannot find java.lang.Object.notifyall()V");
      return initialized = -1;
    }

  obj_wait_mth = (*env)->GetMethodID (env, obj_class, "wait", "()V");
  if ( ! obj_wait_mth )
    {
      BROKEN (env, "cannot find Object.<wait()V>");
      return initialized = -1;
    }

  obj_wait_nanotime_mth = 
    (*env)->GetMethodID (env, obj_class, "wait", "(JI)V");
  if ( ! obj_wait_nanotime_mth )
    {
      BROKEN (env, "cannot find Object.<wait(JI)V>");
      return initialized = -1;
    }

  /* GThreadMutex and its methods */
  lcl_class = (*env)->FindClass (env, "gnu/java/awt/peer/gtk/GThreadMutex");
  if ( ! lcl_class)
    {
      BROKEN (env, "cannot find gnu.java.awt.peer.gtk.GThreadMutex");
      return initialized = -1;
    }
  /* Pin it down. */
  mutex_class = (jclass) (*env)->NewGlobalRef (env, lcl_class);
  DELETE_LOCAL_REF (env, lcl_class);
  if ( ! mutex_class)
    {
      BROKEN (env, "Cannot get a global reference to GThreadMutex");
      return initialized = -1;
    }

  mutex_ctor = (*env)->GetMethodID (env, mutex_class, "<init>", "()V");
  if ( ! mutex_ctor)
    {
      BROKEN (env, "cannot find zero-arg constructor for GThreadMutex");
      return initialized = -1;
    }

  mutex_potentialLockers_fld = (*env)->GetFieldID
    (env, mutex_class, "potentialLockers", "I");
  if ( ! mutex_class )
    {
      BROKEN (env, "cannot find GThreadMutex.potentialLockers");
      return initialized = -1;
    }

  if (! (mutex_lockForPotentialLockers_fld = (*env)->GetFieldID
	 (env, mutex_class, "lockForPotentialLockers", "Ljava/lang/Object;")))
    {
      BROKEN (env, "cannot find GThreadMutex.lockForPotentialLockers");
      return initialized = -1;
    }


  /* java.lang.Thread */
  if (! (lcl_class = (*env)->FindClass (env, "java/lang/Thread")))
    {
      BROKEN (env, "cannot find java.lang.Thread");
      return initialized = -1;
    }

  /* Pin it down. */
  thread_class = (jclass) (*env)->NewGlobalRef (env, lcl_class);
  DELETE_LOCAL_REF (env, lcl_class);
  if (!thread_class)
    {
      BROKEN (env, "Cannot get a global reference to java.lang.Thread");
      return initialized = -1;
    }

  thread_current_mth =
    (*env)->GetStaticMethodID (env, thread_class, "currentThread",
			       "()Ljava/lang/Thread;");
  if (!thread_current_mth)
    {
      BROKEN (env, "cannot find Thread.currentThread() method");
      return initialized = -1;
    }

  thread_equals_mth = 
    (*env)->GetMethodID (env, thread_class, "equals", "(Ljava/lang/Object;)Z");
  if (!thread_equals_mth)
    {
      BROKEN (env, "cannot find Thread.equals() method");
      return initialized = -1;
    }

  thread_join_mth = (*env)->GetMethodID (env, thread_class, "join", "()V");
  if (!thread_join_mth)
    {
      BROKEN (env, "cannot find Thread.join() method");
      return initialized = -1;
    }

  thread_stop_mth = (*env)->GetMethodID (env, thread_class, "stop", "()V");
  if ( ! thread_stop_mth )
    {
      BROKEN (env, "cannot find Thread.stop() method");
      return initialized = -1;
    }

  thread_setPriority_mth = 
    (*env)->GetMethodID (env, thread_class, "setPriority", "(I)V");
  if ( ! thread_setPriority_mth )
    {
      BROKEN (env, "cannot find Thread.setPriority() method");
      return initialized = -1;
    }

  thread_yield_mth = 
    (*env)->GetStaticMethodID (env, thread_class, "yield", "()V");
  if ( ! thread_yield_mth )
    {
      BROKEN (env, "cannot find Thread.yield() method");
      return initialized = -1;
    }

  /* java.lang.ThreadLocal */
  lcl_class = (*env)->FindClass (env, "java/lang/ThreadLocal");
  if ( ! lcl_class )
    {
      BROKEN (env, "cannot find class java.lang.ThreadLocal");
      return initialized = -1;
    }

  /* Pin it down. */
  threadlocal_class = (jclass) (*env)->NewGlobalRef (env, lcl_class);
  DELETE_LOCAL_REF (env, lcl_class);
  if ( ! threadlocal_class )
    {
      BROKEN (env, "Cannot get a global reference to java.lang.ThreadLocal");
      return initialized = -1;
    }

  threadlocal_ctor = (*env)->GetMethodID (env, threadlocal_class, 
                                          "<init>", "()V");
  if ( ! threadlocal_ctor )
    {
      BROKEN (env, "cannot find ThreadLocal.<init>()V");
      return initialized = -1;
    }
  
  threadlocal_get_mth = (*env)->GetMethodID (env, threadlocal_class,
                                             "get", "()Ljava/lang/Object;");
  if ( ! threadlocal_get_mth )
    {
      BROKEN (env, "cannot find java.lang.ThreadLocal.get()Object");
      return initialized = -1;
    }

  threadlocal_set_mth = (*env)->GetMethodID (env, threadlocal_class,
                                             "set", "(Ljava/lang/Object;)V");
  if ( ! threadlocal_set_mth )
    {
      BROKEN (env, "cannot find ThreadLocal.set(Object)V");
      return initialized = -1;
    }

  /* java.lang.Long */
  lcl_class = (*env)->FindClass (env, "java/lang/Long");
  if ( ! lcl_class )
    {
      BROKEN (env, "cannot find class java.lang.Long");
      return initialized = -1;
    }

  /* Pin it down. */
  long_class = (jclass) (*env)->NewGlobalRef (env, lcl_class);
  DELETE_LOCAL_REF (env, lcl_class);
  if (!long_class)
    {
      BROKEN (env, "Cannot get a global reference to java.lang.Long");
      return initialized = -1;
    }

  long_ctor = (*env)->GetMethodID (env, long_class, "<init>", "(J)V");
  if (!long_ctor)
    {
      BROKEN (env, "cannot find method java.lang.Long.<init>(J)V");
      return initialized = -1;
    }

  long_longValue_mth =
    (*env)->GetMethodID (env, long_class, "longValue", "()J");
  if (!long_longValue_mth)
    {
      BROKEN (env, "cannot find method java.lang.Long.longValue()J");
      return initialized = -1;
    }


  /* GThreadNativeMethodRunner */
  lcl_class = 
    (*env)->FindClass (env,
                       "gnu/java/awt/peer/gtk/GThreadNativeMethodRunner");
  if ( ! lcl_class )
    {
      BROKEN (env,
	      "cannot find gnu.java.awt.peer.gtk.GThreadNativeMethodRunner");
      return initialized = -1;
    }

  /* Pin it down. */
  runner_class = (jclass) (*env)->NewGlobalRef (env, lcl_class);
  DELETE_LOCAL_REF (env, lcl_class);
  if (!runner_class)
    {
      BROKEN (env,
	      "Cannot get a global reference to the class GThreadNativeMethodRunner");
      return initialized = -1;
    }

  runner_ctor = (*env)->GetMethodID (env, runner_class, "<init>", "(JJZ)V");
  if ( ! runner_ctor )
    {
      BROKEN (env,
	      "cannot find method GThreadNativeMethodRunner.<init>(JJZ)");
      return initialized = -1;
    }
      
  runner_start_mth = (*env)->GetMethodID (env, runner_class, "start", "()V");
  if ( ! runner_start_mth )
    {
      BROKEN (env, "cannot find method GThreadNativeMethodRunner.start()V");
      return initialized = -1;
    }


  runner_threadToThreadID_mth = 
    (*env)->GetStaticMethodID (env, runner_class,
                               "threadToThreadID", "(Ljava/lang/Thread;)I");
  if ( ! runner_threadToThreadID_mth )
    {
      BROKEN (env,
	      "cannot find method GThreadNativeMethodRunner.threadToThreadID(java.lang.Thread)I");
      return initialized = -1;
    }


  runner_threadIDToThread_mth = 
    (*env)->GetStaticMethodID (env, runner_class,
                               "threadIDToThread", "(I)Ljava/lang/Thread;");
  if ( ! runner_threadIDToThread_mth )
    {
      BROKEN (env,
	      "cannot find method GThreadNativeMethodRunner.threadIDToThread(I)java.lang.Thread");
      return initialized = -1;
    }


  runner_deRegisterJoinable_mth =
    (*env)->GetStaticMethodID (env, runner_class, "deRegisterJoinable",
			       "(Ljava/lang/Thread;)V");
  if (!runner_deRegisterJoinable_mth)
    {
      BROKEN (env,
	      "cannot find method GThreadNativeMethodRunner.deRegisterJoinable(java.lang.Thread)V");
      return initialized = -1;
    }


  /* java.lang.InterruptedException */
  lcl_class = (*env)->FindClass (env, "java/lang/InterruptedException");
  if ( ! lcl_class )
    {
      BROKEN (env, "cannot find class java.lang.InterruptedException");
      return initialized = -1;
    }

  /* Pin it down. */
  interrupted_exception_class = (jclass) (*env)->NewGlobalRef (env, lcl_class);
  DELETE_LOCAL_REF (env, lcl_class);
  if (!interrupted_exception_class)
    {
      BROKEN (env, "Cannot make a global reference"
	      " to java.lang.InterruptedException");
      return initialized = -1;
    }

#ifdef JNI_VERSION_1_2
  if (HAVE_JNI_VERSION_1_2)
    assert ( ! (*env)->ExceptionCheck (env));
  else
#endif
    assert ( ! (*env)->ExceptionOccurred (env));


  return initialized = 1;
}





/************************************************************************/
/* Utilities to allocate and free java.lang.Objects			*/
/************************************************************************/

/* The condition variables are java.lang.Object objects,
 * which this method allocates and returns a global ref.  Note that global
 * refs must be explicitly freed (isn't C fun?).
 */
static jobject
allocatePlainObject (JNIEnv * env)
{
  jobject lcl_obj, global_obj;

  lcl_obj = (*env)->NewObject (env, obj_class, obj_ctor);
  if (!lcl_obj)
    {
      BROKEN (env, "cannot allocate object");
      return NULL;
    }

  global_obj = (*env)->NewGlobalRef (env, lcl_obj);
  DELETE_LOCAL_REF (env, lcl_obj);
  if (!global_obj)
    {
      NEW_BROKEN (env, "cannot make global ref for a new plain Java object");
      /* Deliberate fall-through */
    }

  return global_obj;
}

/*  Frees any Java object given a global ref (isn't C fun?) */
static void
freeObject (JNIEnv * env, jobject obj)
{
  if (obj)
    {
      (*env)->DeleteGlobalRef (env, obj);
      /* DeleteGlobalRef can never fail */
    }
}


/************************************************************************/
/* Utilities to allocate and free Java mutexes				*/
/************************************************************************/

/* The mutexes are gnu.java.awt.peer.gtk.GThreadMutex objects,
 * which this method allocates and returns a global ref.  Note that global
 * refs must be explicitly freed (isn't C fun?).
 *
 * Free this with freeObject()
 */
static jobject
allocateMutexObject (JNIEnv * env)
{
  jobject lcl_obj, global_obj;

  lcl_obj = (*env)->NewObject (env, mutex_class, mutex_ctor);
  if (!lcl_obj)
    {
      BROKEN (env, "cannot allocate a GThreadMutex");
      return NULL;
    }

  global_obj = (*env)->NewGlobalRef (env, lcl_obj);
  DELETE_LOCAL_REF (env, lcl_obj);
  if (!global_obj)
    {
      NEW_BROKEN (env, "cannot make global ref");
      /* Deliberate fallthrough */
    }

  return global_obj;
}


/************************************************************************/
/* Locking code				     				*/
/************************************************************************/

/* Lock a Java object */
#define ENTER_MONITOR(env, m)			\
    enterMonitor(env, m, G_STRINGIFY(m))

/* Return -1 on failure, 0 on success. */
static int
enterMonitor (JNIEnv * env, jobject monitorObj, const char monName[])
{
  if (TRACE_MONITORS)
    tracing ("  <MonitorEnter(%s)>", monName);
  assert (monitorObj);
  if ((*env)->MonitorEnter (env, monitorObj) < 0)
    {
      BROKEN (env, "cannot enter monitor");
      return -1;
    }
  return 0;
}


/* Unlock a Java object */
#define EXIT_MONITOR(env, m)			\
    exitMonitor(env, m, G_STRINGIFY(m))

static int
exitMonitor (JNIEnv * env, jobject mutexObj, const char monName[])
{
  if (TRACE_MONITORS)
    tracing (" <MonitorExit(%s)>", monName);
  assert (mutexObj);
  if ((*env)->MonitorExit (env, mutexObj) < 0)
    {
      BROKEN (env, "cannot exit monitor ");
      return -1;
    }
  return 0;
}


/************************************************************************/
/* Miscellaneous utilities		     				*/
/************************************************************************/

/* Get the Java Thread object that corresponds to a particular thread ID. 
   A negative thread Id gives us a null object.

   Returns a local reference. 
*/
static jobject
getThreadFromThreadID (JNIEnv * env, gpointer gThreadID)
{
  jint threadNum = (jint) gThreadID;
  jobject thread;

  if (threadNum < 0)
    {
      NEW_BROKEN (env, "getThreadFromThreadID asked to look up"
		       " a negative thread index");
      return NULL;
    }

  thread = (*env)->CallStaticObjectMethod
    (env, runner_class, runner_threadIDToThread_mth, threadNum);

  if (MAYBE_BROKEN (env, "cannot get Thread for threadID "))
    return NULL;

  return thread;
}

/** Return the unique threadID of THREAD.

   Error handling: Return (gpointer) -1 on all failures, 
   and propagate an exception. 
*/
static gpointer
getThreadIDFromThread (JNIEnv * env, jobject thread)
{
  jint threadNum;

  if (ENABLE_EXPENSIVE_ASSERTIONS)
    assert ((*env)->IsInstanceOf (env, thread, thread_class));

  HIDE_OLD_TROUBLE (env);

  threadNum = (*env)->CallStaticIntMethod
    (env, runner_class, runner_threadToThreadID_mth, thread);

  if (MAYBE_BROKEN (env, "cannot get ThreadID for a Thread "))
    {
      threadNum = -1;
      goto done;
    }


  SHOW_OLD_TROUBLE ();

done:
  return (gpointer) threadNum;
}


/************************************************************************/
/* The Actual JNI functions that we pass to the function vector.	*/
/************************************************************************/


/************************************************************************/
/* Mutex Functions                                                  	*/
/************************************************************************/

/*** Mutex Utilities  ****/
struct mutexObj_cache
{
  jobject lockForPotentialLockersObj;	/* Lock for the potentialLockers
					   field.  Local reference. */
  jobject lockObj;		/* The real lock we use.  This is a GLOBAL
				   reference and must not be freed. */
};

/* Initialize the cache of sub-locks for a particular mutex object.

  -1 on error, 0 on success.  The caller is not responsible for freeing the
   partially-populated cache in case of failure (but in practice does anyway)
   (This actually never fails, though, since GetObjectField allegedly never
   fails.)  

   Guaranteed to leave all fields of the cache initialized, even if only to
   zero. 
*/
static int
populate_mutexObj_cache (JNIEnv * env, jobject mutexObj,
			 struct mutexObj_cache *mcache)
{
  mcache->lockObj = mutexObj;	/* the mutexObj is its own lock.  */
  assert (mcache->lockObj);

  mcache->lockForPotentialLockersObj = (*env)->GetObjectField
    (env, mutexObj, mutex_lockForPotentialLockers_fld);
  /* GetObjectField can never fail. */

  /*  Retrieving a NULL object could only happen if we somehow got a
      a mutex object that was not properly intialized. */ 
  assert (mcache->lockForPotentialLockersObj);

  return 0;
}


/* Clean out the mutexObj_cache, even if it was never populated. */
static void
clean_mutexObj_cache (JNIEnv * env, struct mutexObj_cache *mcache)
{
  /* OK to pass NULL refs to DELETE_LOCAL_REF */
  DELETE_LOCAL_REF (env, mcache->lockForPotentialLockersObj);
  /* mcache->lockObj is a GLOBAL reference. */
  mcache->lockObj = NULL;
}

/* -1 on failure, 0 on success.
   The mutexObj_cache is already populated for this particular object. */
static int
mutexObj_lock (JNIEnv * env, jobject mutexObj, struct mutexObj_cache *mcache)
{
  jint potentialLockers;

  if (ENTER_MONITOR (env, mcache->lockForPotentialLockersObj))
    return -1;

  assert(mutexObj);
  potentialLockers = 
    (*env)->GetIntField (env, mutexObj, mutex_potentialLockers_fld);
  /* GetIntField() never fails. */

  ++potentialLockers;

  (*env)->SetIntField
    (env, mutexObj, mutex_potentialLockers_fld, potentialLockers);

  if (EXIT_MONITOR (env, mcache->lockForPotentialLockersObj))
    return -1;

  if (ENTER_MONITOR (env, mcache->lockObj))
    return -1;

  SHOW_OLD_TROUBLE ();

  return 0;
}

/* Unlock a GMutex, once we're already in JNI and have already gotten the
   mutexObj for it.  This skips the messages that TRACE_API_CALLS would
   print.

   Returns -1 on error, 0 on success. */
static int
mutexObj_unlock (JNIEnv * env, jobject mutexObj,
		 struct mutexObj_cache *mcache)
{
  jint potentialLockers;
  int ret = -1;			/* assume failure until we suceed.  */

  /* Free the lock first, so that someone waiting for the lock can get it
     ASAP. */
  /* This is guaranteed not to block. */
  if (EXIT_MONITOR (env, mcache->lockObj) < 0)
    goto done;

  /* Kick down potentialLockers by one.  We do this AFTER we free the lock, so
     that we hold it no longer than necessary. */
  if (ENTER_MONITOR (env, mcache->lockForPotentialLockersObj) < 0)
    goto done;

  potentialLockers = (*env)->GetIntField
    (env, mutexObj, mutex_potentialLockers_fld);
  /* GetIntField never fails */

  assert (potentialLockers >= 1);
  --potentialLockers;

  (*env)->SetIntField
    (env, mutexObj, mutex_potentialLockers_fld, potentialLockers);
  /* Never fails, so the JNI book says. */

  /* Clean up. */
  if (EXIT_MONITOR (env, mcache->lockForPotentialLockersObj) < 0)
    goto done;
  ret = 0;

done:
  return ret;
}

/*** Mutex Implementations ****/

/* Create a mutex, which is a java.lang.Object for us.
   In case of failure, we'll return NULL.  Which will implicitly 
   cause future calls to fail. */
static GMutex *
mutex_new_jni_impl (void)
{
  jobject mutexObj;
  JNIEnv *env;
  union env_union e;

  if (TRACE_API_CALLS)
    tracing ("mutex_new_jni_impl()");

  e.jni_env = &env;
  (*cp_gtk_the_vm)->GetEnv (cp_gtk_the_vm, e.void_env, JNI_VERSION_1_1);

  if (setup_cache (env) < 0)
    {
      mutexObj = NULL;
      goto done;
    }

  mutexObj = allocateMutexObject (env);

done:
  if (TRACE_API_CALLS)
    tracing (" ==> %p \n", mutexObj);

  return (GMutex *) mutexObj;

}

/* Lock a mutex. */
static void
mutex_lock_jni_impl (GMutex * mutex)
{
  struct mutexObj_cache mcache;
  jobject mutexObj = (jobject) mutex;
  JNIEnv *env;
  union env_union e;

  if (TRACE_API_CALLS)
    tracing ("mutex_lock_jni_impl( mutexObj = %p )", mutexObj);

  assert (mutexObj);
  e.jni_env = &env;
  (*cp_gtk_the_vm)->GetEnv (cp_gtk_the_vm, e.void_env, JNI_VERSION_1_1);

  if (setup_cache (env) < 0)
    goto done;

  HIDE_OLD_TROUBLE (env);

  if (populate_mutexObj_cache (env, mutexObj, &mcache) < 0)
    goto done;

  mutexObj_lock (env, mutexObj, &mcache);
  /* No need to error check; we've already reported it in any case. */

done:
  clean_mutexObj_cache (env, &mcache);
  if (TRACE_API_CALLS)
    tracing (" ==> VOID \n");
}


/*  Try to lock a mutex.  Return TRUE if we succeed, FALSE if we fail.  
    FALSE on error. */
static gboolean
mutex_trylock_jni_impl (GMutex * gmutex)
{
  jobject mutexObj = (jobject) gmutex;
  jint potentialLockers;
  gboolean ret = FALSE;
  JNIEnv *env;
  union env_union e;
  struct mutexObj_cache mcache;

  if (TRACE_API_CALLS)
    tracing ("mutex_trylock_jni_impl(mutexObj=%p)", mutexObj);

  assert (mutexObj);

  e.jni_env = &env;
  (*cp_gtk_the_vm)->GetEnv (cp_gtk_the_vm, e.void_env, JNI_VERSION_1_1);
  if (setup_cache (env) < 0)
    goto done;
  HIDE_OLD_TROUBLE (env);

  if (populate_mutexObj_cache (env, mutexObj, &mcache) < 0)
    goto done;

  if (ENTER_MONITOR (env, mcache.lockForPotentialLockersObj))
    goto done;

  potentialLockers = (*env)->GetIntField
    (env, mutexObj, mutex_potentialLockers_fld);

  assert (potentialLockers >= 0);

  if (potentialLockers)
    {
      /* Already locked.  Clean up and leave. */
      EXIT_MONITOR (env, mcache.lockForPotentialLockersObj);	
      /* Ignore any error code from EXIT_MONITOR; there's nothing we could do
	 at this level, in any case. */
      goto done;
    }

  /* Guaranteed not to block. */
  if (ENTER_MONITOR (env, mcache.lockObj))
    {
      /* Clean up the existing lock. */
      EXIT_MONITOR (env, mcache.lockForPotentialLockersObj);	
      /* Ignore any error code from EXIT_MONITOR; there's nothing we could do
	 at this level, in any case. */
      goto done;
    }
  

  /* We have the monitor.  Record that fact. */
  potentialLockers = 1;
  (*env)->SetIntField
    (env, mutexObj, mutex_potentialLockers_fld, potentialLockers);
  /* Set*Field() never fails */

  ret = TRUE;			/* We have the lock. */

  /* Clean up. */
  if (EXIT_MONITOR (env, mcache.lockForPotentialLockersObj))
      goto done;		/* If we fail at this point, still keep the
				   main lock.  */

  SHOW_OLD_TROUBLE ();
done:
  clean_mutexObj_cache (env, &mcache);
  if (TRACE_API_CALLS)
    tracing (" ==> %s\n", ret ? "TRUE" : "FALSE");
  return ret;
}


/* Unlock a mutex. */
static void
mutex_unlock_jni_impl (GMutex * gmutex)
{
  jobject mutexObj = (jobject) gmutex;
  struct mutexObj_cache mcache;
  JNIEnv *env;
  union env_union e;

  if (TRACE_API_CALLS)
    tracing ("mutex_unlock_jni_impl(mutexObj=%p)", mutexObj);

  e.jni_env = &env;
  (*cp_gtk_the_vm)->GetEnv (cp_gtk_the_vm, e.void_env, JNI_VERSION_1_1);
  if (setup_cache (env) < 0)
    goto done;
  HIDE_OLD_TROUBLE (env);

  assert (mutexObj);

  if ( populate_mutexObj_cache (env, mutexObj, &mcache) < 0)
    goto done;

  (void) mutexObj_unlock (env, mutexObj, &mcache);

  SHOW_OLD_TROUBLE ();

done:
  clean_mutexObj_cache (env, &mcache);
  if (TRACE_API_CALLS)
    tracing (" ==> VOID\n");
}



/* Free a mutex (isn't C fun?).  OK this time for it to be NULL.  
   No failure conditions, for a change.  */
static void
mutex_free_jni_impl (GMutex * mutex)
{
  jobject mutexObj = (jobject) mutex;
  JNIEnv *env;
  union env_union e;

  e.jni_env = &env;
  (*cp_gtk_the_vm)->GetEnv (cp_gtk_the_vm, e.void_env, JNI_VERSION_1_1);

  if (TRACE_API_CALLS)
    tracing ("mutex_free_jni_impl(%p)", mutexObj);

  freeObject (env, mutexObj);

  if (TRACE_API_CALLS)
    tracing (" ==> VOID\n");
}




/************************************************************************/
/* Condition variable code		     				*/
/************************************************************************/

/* Create a new condition variable.  This is a java.lang.Object for us. */
static GCond *
cond_new_jni_impl (void)
{
  jobject condObj;
  JNIEnv *env;
  union env_union e;

  if (TRACE_API_CALLS)
    tracing ("mutex_free_jni_impl()");

  e.jni_env = &env;
  (*cp_gtk_the_vm)->GetEnv (cp_gtk_the_vm, e.void_env, JNI_VERSION_1_1);

  condObj = allocatePlainObject (env);

  if (TRACE_API_CALLS)
    tracing (" ==> %p\n", condObj);

  return (GCond *) condObj;
}

/*  Signal on a condition variable.  This is simply calling Object.notify
 * for us.
 */
static void
cond_signal_jni_impl (GCond * gcond)
{
  JNIEnv *env;
  union env_union e;
  jobject condObj = (jobject) gcond;

  if (TRACE_API_CALLS)
    tracing ("cond_signal_jni_impl(condObj = %p)", condObj);

  e.jni_env = &env;
  (*cp_gtk_the_vm)->GetEnv (cp_gtk_the_vm, e.void_env, JNI_VERSION_1_1);
  if (setup_cache (env) < 0)
    goto done;
  HIDE_OLD_TROUBLE (env);

  assert (condObj);

  /* Must have locked an object to call notify */
  if (ENTER_MONITOR (env, condObj))
    goto done;

  (*env)->CallVoidMethod (env, condObj, obj_notify_mth);
  if (MAYBE_BROKEN (env, "cannot signal mutex with Object.notify()"))
    {
      if (EXIT_MONITOR (env, condObj))
	BADLY_BROKEN1 ("Failed to unlock a monitor; the VM may deadlock.");
      goto done;
    }

  EXIT_MONITOR (env, condObj);

  SHOW_OLD_TROUBLE ();

done:
  if (TRACE_API_CALLS)
    tracing (" ==> VOID\n");
}

/*  Broadcast to all waiting on a condition variable.  This is simply 
 * calling Object.notifyAll for us.
 */
static void
cond_broadcast_jni_impl (GCond * gcond)
{
  jobject condObj = (jobject) gcond;
  JNIEnv *env;
  union env_union e;

  if (TRACE_API_CALLS)
    tracing ("cond_broadcast_jni_impl(condObj=%p)", condObj);

  e.jni_env = &env;
  (*cp_gtk_the_vm)->GetEnv (cp_gtk_the_vm, e.void_env, JNI_VERSION_1_1);
  if (setup_cache (env) < 0)
    goto done;
  HIDE_OLD_TROUBLE (env);

  assert (condObj);
  /* Must have locked an object to call notifyAll */
  if (ENTER_MONITOR (env, condObj))
    goto done;

  (*env)->CallVoidMethod (env, condObj, obj_notifyall_mth);
  if (MAYBE_BROKEN (env, "cannot broadcast to mutex with Object.notify()"))
    {
      EXIT_MONITOR (env, condObj);
      goto done;
    }

  EXIT_MONITOR (env, condObj);

  SHOW_OLD_TROUBLE ();

done:
  if (TRACE_API_CALLS)
    tracing (" ==> VOID\n");
}


/* Wait on a condition variable.  For us, this simply means calling
 * Object.wait.
 *
 * Throws a Java exception on trouble; may leave the mutexes set arbitrarily.
 * XXX TODO: Further improve error recovery.
 */
static void
cond_wait_jni_impl (GCond * gcond, GMutex * gmutex)
{
  struct mutexObj_cache cache;
  jobject condObj = (jobject) gcond;
  jobject mutexObj = (jobject) gmutex;
  JNIEnv *env;
  union env_union e;

  if (TRACE_API_CALLS)
    tracing ("cond_wait_jni_impl(condObj=%p, mutexObj=%p)",
	     condObj, mutexObj);

  e.jni_env = &env;
  (*cp_gtk_the_vm)->GetEnv (cp_gtk_the_vm, e.void_env, JNI_VERSION_1_1);
  if (setup_cache (env) < 0)
    goto done;
  HIDE_OLD_TROUBLE (env);

  assert (condObj);
  assert (mutexObj);
  /* Must have locked a Java object to call wait on it */
  if (ENTER_MONITOR (env, condObj) < 0)
    goto done;

  /* Our atomicity is now guaranteed; we're protected by the Java monitor on
     condObj.  Unlock the GMutex. */
  if (mutexObj_unlock (env, mutexObj, &cache))
    goto done;

  (*env)->CallVoidMethod (env, condObj, obj_wait_mth);
  if (MAYBE_BROKEN (env, "cannot wait on condObj"))
    {
      EXIT_MONITOR (env, condObj);	/* ignore err checking */
      goto done;
    }

  /* Re-acquire the lock on the GMutex.  Do this while we're protected by the
     Java monitor on condObj. */
  if (mutexObj_lock (env, mutexObj, &cache))
    goto done;

  EXIT_MONITOR (env, condObj);

  SHOW_OLD_TROUBLE ();

done:
  if (TRACE_API_CALLS)
    tracing (" ==> VOID\n");
}


/** Wait on a condition variable until a timeout.  This is a little tricky
 * for us.  We first call Object.wait(J) giving it the appropriate timeout
 * value.  On return, we check whether an InterruptedException happened.  If
 * so, that is Java-speak for wait timing out.  
 * 
 * We return FALSE if we timed out.  Return TRUE if the condition was
 * signalled first, before we timed out.
 *
 * In case of trouble we throw a Java exception.  Whether we return FALSE or
 * TRUE depends upon whether the condition was raised before the trouble
 * happened. 
 *
 * I believe that this function goes to the proper lengths to try to unlock
 * all of the locked mutexes and monitors, as appropriate, and that it further
 * tries to make sure that the thrown exception is the current one, not any
 * future cascaded one from something like a failure to unlock the monitors.
 */
static gboolean
cond_timed_wait_jni_impl (GCond * gcond, GMutex * gmutex, GTimeVal * end_time)
{
  JNIEnv *env;
  union env_union e;
  jlong time_millisec;
  jint time_nanosec;
  jthrowable cause;
  jobject condObj = (jobject) gcond;
  jobject mutexObj = (jobject) gmutex;
  gboolean condRaised = FALSE;	/*  Condition has not been raised yet. */
  struct mutexObj_cache cache;
  gboolean interrupted;

  if (TRACE_API_CALLS)
    {
      tracing ("cond_timed_wait_jni_impl(cond=%p, mutex=%p,"
	       " end_time=< sec=%lu, usec=%lu >)", condObj, mutexObj,
	       (unsigned long) end_time->tv_sec,
	       (unsigned long) end_time->tv_usec);
    }


  e.jni_env = &env;
  (*cp_gtk_the_vm)->GetEnv (cp_gtk_the_vm, e.void_env, JNI_VERSION_1_1);
  if (setup_cache (env) < 0)
    goto done;
  HIDE_OLD_TROUBLE (env);

  time_millisec = end_time->tv_sec * 1000 + end_time->tv_usec / 1000;
  time_nanosec = 1000 * (end_time->tv_usec % 1000);

  /* Must have locked an object to call wait */
  if (ENTER_MONITOR (env, condObj) < 0)
    goto done;

  if (mutexObj_unlock (env, mutexObj, &cache) < 0)
    {
      if (EXIT_MONITOR (env, condObj) < 0)
	criticalMsg
	  ("Unable to unlock an existing lock on a condition; your proram may deadlock");
      goto done;
    }


  (*env)->CallVoidMethod (env, condObj, obj_wait_nanotime_mth,
			  time_millisec, time_nanosec);

  /* If there was trouble, save that fact, and the reason for the trouble.  We
     want to respond to this condition as fast as possible. */
  cause = (*env)->ExceptionOccurred (env);

  if ( ! cause )
    {
      condRaised = TRUE;	/* condition was signalled */
    }
  else if ((*env)->IsInstanceOf (env, cause, interrupted_exception_class))
    {
      condRaised = FALSE;	/* Condition was not raised before timeout.
				   (This is redundant with the initialization
				   of condRaised above) */
      (*env)->ExceptionClear (env);	/* Clear the InterruptedException. */
      cause = NULL;		/* no pending cause now.  */
    }
  else
    {
      interrupted = FALSE;	/* Trouble, but not because of
				   InterruptedException.  Assume the condition
				   was not raised. */
      /* Leave condRaised set to FALSE */
    }

  /* Irrespective of whether there is a pending problem to report, go ahead
     and try to clean up.  This may end up throwing an exception that is
     different from the one that was thrown by the call to Object.wait().
     So we will override it with the first exception (don't want to have
     cascading problems). */
  if (mutexObj_lock (env, mutexObj, &cache) && !cause)
    {
      cause = (*env)->ExceptionOccurred (env);
      assert (cause);
    }

  if (EXIT_MONITOR (env, condObj) && !cause)
    {
      cause = (*env)->ExceptionOccurred (env);
      assert (cause);
    }

  if (cause)			/* Raise the first cause. */
    {
      BROKEN_CAUSE (env, cause, "error in timed wait or during its cleanup");
      goto done;
    }

  SHOW_OLD_TROUBLE ();

done:
  if (TRACE_API_CALLS)
    tracing (" ==> condRaised = %s\n", condRaised ? "TRUE" : "FALSE");
  return condRaised;
}


/* Free a condition variable.  (isn't C fun?).  Can not fail. */
static void
cond_free_jni_impl (GCond * cond)
{
  jobject condObj = (jobject) cond;
  JNIEnv *env;
  union env_union e;

  if (TRACE_API_CALLS)
    tracing ("cond_free_jni_impl(condObj = %p)", condObj);
  e.jni_env = &env;
  (*cp_gtk_the_vm)->GetEnv (cp_gtk_the_vm, e.void_env, JNI_VERSION_1_1);

  freeObject (env, condObj);

  if (TRACE_API_CALLS)
    tracing (" ==> VOID\n");
}


/************************************************************************/
/* Thread-local data code		     				*/
/************************************************************************/

/* Create a new thread-local key.  We use java.lang.ThreadLocal objects
 * for this.  This returns the pointer representation of a Java global
 * reference. 
 * 
 * We will throw a Java exception and return NULL in case of failure.
 */
static GPrivate *
private_new_jni_impl (GDestroyNotify notify __attribute__ ((unused)))
{
  JNIEnv *env;
  union env_union e;
  jobject lcl_key;
  jobject global_key;
  GPrivate *gkey = NULL;	/* Error return code */

  if (TRACE_API_CALLS)
    tracing ("private_new_jni_impl()");

  e.jni_env = &env;
  (*cp_gtk_the_vm)->GetEnv (cp_gtk_the_vm, e.void_env, JNI_VERSION_1_1);
  if (setup_cache (env) < 0)
    goto done;
  HIDE_OLD_TROUBLE (env);

  lcl_key = (*env)->NewObject (env, threadlocal_class, threadlocal_ctor);
  if ( ! lcl_key )
    {
      BROKEN (env, "cannot allocate a ThreadLocal");
      goto done;
    }

  global_key = ((*env)->NewGlobalRef (env, lcl_key));
  DELETE_LOCAL_REF (env, lcl_key);
  if ( ! global_key)
    {
      NEW_BROKEN (env, "cannot create a GlobalRef to a new ThreadLocal");
      goto done;
    }

  gkey = (GPrivate *) global_key;
  SHOW_OLD_TROUBLE ();

done:
  if (TRACE_API_CALLS)
    tracing (" ==> %p\n", (void *) gkey);

  return gkey;
}

/*  Get this thread's value for a thread-local key.  This is simply
 * ThreadLocal.get for us.  Return NULL if no value.  (I can't think of
 * anything else to do.)
 */
static gpointer
private_get_jni_impl (GPrivate * gkey)
{
  JNIEnv *env;
  union env_union e;
  jobject val_wrapper;
  jobject keyObj = (jobject) gkey;
  gpointer thread_specific_data = NULL;	/* Init to the error-return value */

  jlong val;

  if (TRACE_API_CALLS)
    tracing ("private_get_jni_impl(keyObj=%p)", keyObj);

  e.jni_env = &env;
  (*cp_gtk_the_vm)->GetEnv (cp_gtk_the_vm, e.void_env, JNI_VERSION_1_1);
  if (setup_cache (env) < 0)
    goto done;
  HIDE_OLD_TROUBLE (env);

  val_wrapper = (*env)->CallObjectMethod (env, keyObj, threadlocal_get_mth);
  if (MAYBE_BROKEN (env, "cannot find thread-local object"))
    goto done;

  if (! val_wrapper ) 
    {
      /* It's Java's "null" object.  No ref found.  This is OK; we must never
         have set a value in this thread.  Note that this next statement is
         not necessary, strictly speaking, since we're already initialized to
         NULL.  A good optimizing C compiler will detect that and optimize out
         this statement. */
      thread_specific_data = NULL;
      goto done;
    }

  val = (*env)->CallLongMethod (env, val_wrapper, long_longValue_mth);

  if (MAYBE_BROKEN (env, "cannot get thread local value"))
    goto done;

  thread_specific_data = (gpointer) (intptr_t) val;

  /* Only re-raise the old pending exception if a new one hasn't come along to
     supersede it.  */
  SHOW_OLD_TROUBLE ();

done:

  if (TRACE_API_CALLS)
    tracing (" ==> %p\n", thread_specific_data);

  return thread_specific_data;
}

/* Set this thread's value for a thread-local key.  This is simply
 * ThreadLocal.set() for us.
 */
static void
private_set_jni_impl (GPrivate * gkey, gpointer thread_specific_data)
{
  JNIEnv *env;
  union env_union e;
  jobject val_wrapper;
  jobject keyObj = (jobject) gkey;


  if (TRACE_API_CALLS)
    tracing ("private_set_jni_impl(keyObj=%p, thread_specific_data=%p)",
	     keyObj, thread_specific_data);

  e.jni_env = &env;
  (*cp_gtk_the_vm)->GetEnv (cp_gtk_the_vm, e.void_env, JNI_VERSION_1_1);
  if (setup_cache (env) < 0)
    goto done;
  HIDE_OLD_TROUBLE (env);

  /* We are just going to always use a Java long to represent a C pointer.
     Otherwise all of the code would end up being conditionalized for various
     pointer sizes, and that seems like too much of a hassle, in order to save
     a paltry few bytes, especially given the horrendous overhead of JNI in
     any case. 
   */

  val_wrapper = (*env)->NewObject (env, long_class, long_ctor,
				   (jlong) (intptr_t) thread_specific_data);
  if ( ! val_wrapper )
    {
      BROKEN (env, "cannot create a java.lang.Long");
      goto done;
    }

  /* At this point, we now have set lcl_obj as a numeric class that wraps
     around the thread-specific data we were given. */
  (*env)->CallVoidMethod (env, keyObj, threadlocal_set_mth, val_wrapper);
  if (MAYBE_BROKEN (env, "cannot set thread local value"))
    goto done;

  SHOW_OLD_TROUBLE ();
done:
  if (TRACE_API_CALLS)
    tracing (" ==> VOID\n");
}


/** Create an object of type gnu.java.awt.peer.gtk.GThreadNativeMethodRunner.
    Run it.

    We need to create joinable threads.  We handle the notion of a joinable
    thread by determining whether or not we are going to maintain a permanent
    hard reference to it until it croaks.

    Posix does not appear to have a Java-like concept of daemon threads, where
    the JVM will exit when there are only daemon threads running.

    Error handling: 

    To quote from the glib guide:
       "GError should only be used to report recoverable runtime errors, never
        to report programming errors."   

    So how do we consider the failure to create a thread?  Well, each of the
    failure cases in this function are discussed, and none of them are really
    recoverable.

    The glib library is really designed so that you should fail
    catastrophically in case of "programming errors".  The only error defined
    for the GThread functions is G_THREAD_ERROR_AGAIN, and that for
    thread_create.

    Most of these GThread functions could fail if we run out of memory, for
    example, but the only one capable of reporting that fact is
    thread_create. */
static void
thread_create_jni_impl (GThreadFunc	    func,
			gpointer            data,
			gulong              stack_size __attribute__((unused)),
			gboolean            joinable,
			gboolean            bound __attribute__((unused)),
			GThreadPriority     gpriority,
			/* This prototype is horrible.  threadIDp is actually
			   a gpointer to the thread's thread-ID.  Which is, 
			   of course, itself a gpointer-typed value.  Ouch. */ 
			gpointer            threadIDp, 
			/* Do not touch the GError stuff unless you have
			   RECOVERABLE trouble.   There is no recoverable
			   trouble in this implementation.  */ 
			GError	      **errorp __attribute__((unused)))
{
  JNIEnv *env;
  union env_union e;
  union func_union f;
  jboolean jjoinable = joinable;
  jobject newThreadObj;
  gpointer threadID;		/* to be filled in */

  if (TRACE_API_CALLS)
    {
      f.g_func = func;
      tracing ("thread_create_jni_impl(func=%p, data=%p, joinable=%s,"
               " threadIDp=%p, *(int *) threadIDp = %d)",
               f.void_func, data, joinable ? "TRUE" : "FALSE",
               threadIDp, *(int *) threadIDp);
    }

  e.jni_env = &env;
  (*cp_gtk_the_vm)->GetEnv (cp_gtk_the_vm, e.void_env, JNI_VERSION_1_1);
  if (setup_cache (env) < 0)
    {
      /*  The failed call to setup the cache is certainly not recoverable;
	  not appropriate for G_THREAD_ERROR_AGAIN.  */
      *(gpointer *) threadIDp = NULL;
      goto done;
    }
  HIDE_OLD_TROUBLE (env);

  /* If a thread is joinable, then notify its constructor.  The constructor
     will enter a hard reference for it, and the hard ref. won't go away until
     the thread has been joined. */
  newThreadObj = 
    (*env)->NewObject (env, runner_class, runner_ctor, 
                       (jlong) (intptr_t) func, (jlong) (intptr_t) data, 
                       jjoinable);
  if ( ! newThreadObj )
    {
      BROKEN (env, "creating a new thread failed in the constructor");
      *(gpointer *) threadIDp = NULL;
      /*  The failed call to the constructor does not throw any errors such
	  that G_THREAD_ERROR_AGAIN is appropriate.  No other recoverable
	  errors defined.  Once again, we go back to the VM. */
      goto done;
    }

  if (threadObj_set_priority (env, newThreadObj, gpriority) < 0)
    {
      *(gpointer *) threadIDp = NULL;
      /* None of these possible exceptions from Thread.setPriority() are
	 recoverable, so they are not appropriate for EAGAIN.  So we should
	 fail. */  
      goto done;
    }

  (*env)->CallVoidMethod (env, runner_class, runner_start_mth);

  if (MAYBE_BROKEN (env, "starting a new thread failed"))
    {
      *(gpointer *) threadIDp = NULL;
      /* The only exception Thread.start() throws is
	 IllegalStateException.  And that would indicate a programming error. 

	 So there are no situations such that G_THREAD_ERROR_AGAIN would be
	 OK. 

	 So, we don't use g_set_error() here to perform any error reporting.
	 */
      goto done;
    }

  threadID = getThreadIDFromThread (env, newThreadObj);

  *(gpointer *) threadIDp = threadID;
  SHOW_OLD_TROUBLE ();

done:
  if (TRACE_API_CALLS)
    tracing (" ==> (threadID = %p) \n", threadID);
}


/* Wraps a call to g_thread_yield. */
static void
thread_yield_jni_impl (void)
{
  JNIEnv *env;
  union env_union e;

  if (TRACE_API_CALLS)
    tracing ("thread_yield_jni_impl()");

  e.jni_env = &env;
  (*cp_gtk_the_vm)->GetEnv (cp_gtk_the_vm, e.void_env, JNI_VERSION_1_1);
  if (setup_cache (env) < 0)
    goto done;
  HIDE_OLD_TROUBLE (env);

  (*env)->CallStaticVoidMethod (env, thread_class, thread_yield_mth);
  if (MAYBE_BROKEN (env, "Thread.yield() failed"))
    goto done;

  SHOW_OLD_TROUBLE ();

done:
  if (TRACE_API_CALLS)
    tracing (" ==> VOID\n");
}


static void
thread_join_jni_impl (gpointer threadID)
{
  JNIEnv *env;
  union env_union e;
  jobject threadObj = NULL;

  if ( TRACE_API_CALLS )
    tracing ("thread_join_jni_impl(threadID=%p) ", threadID);

  e.jni_env = &env;
  (*cp_gtk_the_vm)->GetEnv (cp_gtk_the_vm, e.void_env, JNI_VERSION_1_1);
  if (setup_cache (env) < 0)
    goto done;
  HIDE_OLD_TROUBLE (env);

  threadObj = getThreadFromThreadID (env, threadID);
  if ( ! threadObj )		/* Already reported with BROKEN  */
    goto done;

  (*env)->CallVoidMethod (env, threadObj, thread_join_mth);
  if (MAYBE_BROKEN (env, "Thread.join() failed"))
    goto done;


  (*env)->CallStaticVoidMethod
    (env, runner_class, runner_deRegisterJoinable_mth, threadObj);
  if (MAYBE_BROKEN (env, "Thread.deRegisterJoinableThread() failed"))
    goto done;

  SHOW_OLD_TROUBLE ();

done:
  DELETE_LOCAL_REF (env, threadObj);
  if (TRACE_API_CALLS)
    tracing (" ==> VOID \n");
}

/* Terminate the current thread.  Unlike pthread_exit(), here we do not need
   to bother with a return value or exit value for the thread which is about
   to croak.  (The gthreads abstraction doesn't use it.)  However, we *do*
   need to bail immediately.  We handle this with Thread.stop(), which is
   a deprecated method.

   It's deprecated since we might leave objects protected by monitors in
   half-constructed states on the way out -- Thread.stop() throws a
   ThreadDeath exception, which is usually unchecked.  There is no good
   solution that I can see. */ 
static void
thread_exit_jni_impl (void)
{
  JNIEnv *env;
  union env_union e;
  jobject this_thread;

  if (TRACE_API_CALLS)
    tracing ("thread_exit_jni_impl() ");

  e.jni_env = &env;
  (*cp_gtk_the_vm)->GetEnv (cp_gtk_the_vm, e.void_env, JNI_VERSION_1_1);
  if (setup_cache (env) < 0)
    goto done;

  HIDE_OLD_TROUBLE (env);

  this_thread = (*env)->
    CallStaticObjectMethod (env, thread_class, thread_current_mth);

  if ( ! this_thread )
    {
      BROKEN (env, "cannot get current thread");
      goto done;
    }

  (*env)->CallVoidMethod (env, this_thread, thread_stop_mth);
  if (MAYBE_BROKEN (env, "cannot call Thread.stop() on current thread"))
    goto done;

  SHOW_OLD_TROUBLE ();

done:
  if (TRACE_API_CALLS)
    tracing (" ==> VOID \n");
}


/* Translate a GThreadPriority to a Java priority level. */
static jint
javaPriorityLevel (GThreadPriority priority)
{
  /* We have these fields in java.lang.Thread to play with:

     static int MIN_PRIORITY     The minimum priority that a thread can have.
     static int NORM_PRIORITY    The default priority that is assigned to a 
     thread.
     static int MAX_PRIORITY     The maximum priority that a thread can have.

     We get these from the header file generated by javah, even though they're
     documented as being 1, 5, and 10.
   */
  static const jint minJPri	= 
    gnu_java_awt_peer_gtk_GThreadNativeMethodRunner_MIN_PRIORITY;
  static const jint normJPri	= 
    gnu_java_awt_peer_gtk_GThreadNativeMethodRunner_NORM_PRIORITY;
  static const jint maxJPri	= 
    gnu_java_awt_peer_gtk_GThreadNativeMethodRunner_MAX_PRIORITY;

  switch (priority)
    {
    case G_THREAD_PRIORITY_LOW:
      return minJPri;
      break;

    default:
      assert_not_reached ();
      /* Deliberate fall-through if assertions are turned off; also shuts up
         GCC warnings if they're turned on.   */
    case G_THREAD_PRIORITY_NORMAL:
      return normJPri;
      break;

    case G_THREAD_PRIORITY_HIGH:
      return (normJPri + maxJPri) / 2;
      break;

    case G_THREAD_PRIORITY_URGENT:
      return maxJPri;
      break;
    }
}


/** It would be safe not to implement this, according to the JNI docs, since
    not all platforms do thread priorities.  However, we might as well
    provide the hint for those who want it. 
*/
static void
thread_set_priority_jni_impl (gpointer gThreadID, GThreadPriority gpriority)
{
  jobject threadObj = NULL;
  JNIEnv *env;
  union env_union e;

  if (TRACE_API_CALLS)
    tracing ("thread_set_priority_jni_impl(gThreadID=%p, gpriority = %u) ",
	     gThreadID, gpriority);

  e.jni_env = &env;
  (*cp_gtk_the_vm)->GetEnv (cp_gtk_the_vm, e.void_env, JNI_VERSION_1_1);

  if (setup_cache (env) < 0)
    goto done;

  HIDE_OLD_TROUBLE (env);


  threadObj = getThreadFromThreadID (env, gThreadID);
  if ( ! threadObj)		/* Reported with BROKEN already.  */
    goto done;

  if (threadObj_set_priority (env, threadObj, gpriority))
    goto done;

  SHOW_OLD_TROUBLE ();

done:
  DELETE_LOCAL_REF (env, threadObj);

  if (TRACE_API_CALLS)
    tracing (" ==> VOID\n");
}


/** It would be safe not to implement this, according to the JNI docs, since
    not all platforms do thread priorities.  However, we might as well
    provide the hint for those who want it.

    -1 on failure, 0 on success. */
static int
threadObj_set_priority (JNIEnv * env, jobject threadObj,
			GThreadPriority gpriority)
{
  jint javaPriority = javaPriorityLevel (gpriority);
  (*env)->CallVoidMethod (env, threadObj, thread_setPriority_mth,
			  javaPriority);
  return MAYBE_BROKEN (env, "Thread.setPriority() failed");
}


/** Return the result of Thread.currentThread(), a static method. */
static void
thread_self_jni_impl (/* Another confusing glib prototype.  This is
			 actually  a gpointer to the thread's thread-ID.
			 Which is, of course, a gpointer. */
		      gpointer my_thread_IDp)
{
  JNIEnv *env;
  union env_union e;
  jobject this_thread;
  gpointer my_threadID;

  if (TRACE_API_CALLS)
    tracing ("thread_self_jni_impl(my_thread_IDp=%p)", my_thread_IDp);

  e.jni_env = &env;
  (*cp_gtk_the_vm)->GetEnv (cp_gtk_the_vm, e.void_env, JNI_VERSION_1_1);

  if (setup_cache (env) < 0)
    return;

  HIDE_OLD_TROUBLE (env);

  this_thread = (*env)->
    CallStaticObjectMethod (env, thread_class, thread_current_mth);
  if (! this_thread )
    {
      BROKEN (env, "cannot get current thread");
      my_threadID = NULL;
      goto done;
    }

  my_threadID = getThreadIDFromThread (env, this_thread);
  SHOW_OLD_TROUBLE ();

done:
  if (TRACE_API_CALLS)
    tracing (" ==> (my_threadID = %p) \n", my_threadID);

  *(gpointer *) my_thread_IDp = my_threadID;
}


static gboolean
thread_equal_jni_impl (gpointer thread1, gpointer thread2)
{
  JNIEnv *env;
  union env_union e;

  gpointer threadID1 = *(gpointer *) thread1;
  gpointer threadID2 = *(gpointer *) thread2;

  jobject thread1_obj = NULL;
  jobject thread2_obj = NULL;
  gboolean ret;

  if (TRACE_API_CALLS)
    tracing ("thread_equal_jni_impl(threadID1=%p, threadID2=%p)",
	     threadID1, threadID2);

  e.jni_env = &env;
  (*cp_gtk_the_vm)->GetEnv (cp_gtk_the_vm, e.void_env, JNI_VERSION_1_1);
  if (setup_cache (env) < 0)
    {
      ret = FALSE;		/* what is safer?  We really don't ever want
				   to return from here.  */
      goto done;
    }

  HIDE_OLD_TROUBLE (env);
  thread1_obj = getThreadFromThreadID (env, threadID1);
  thread2_obj = getThreadFromThreadID (env, threadID2);

  ret = (*env)->CallBooleanMethod (env, thread1_obj,
				   thread_equals_mth, thread2_obj);

  if (MAYBE_BROKEN (env, "Thread.equals() failed"))
    {
      ret = FALSE;
      goto done;
    }

  SHOW_OLD_TROUBLE ();


done:
  DELETE_LOCAL_REF (env, thread1_obj);
  DELETE_LOCAL_REF (env, thread2_obj);

  if (TRACE_API_CALLS)
    tracing (" ==> %s\n", ret ? "TRUE" : "FALSE");

  return ret;
}




/************************************************************************/
/* GLIB interface			     				*/
/************************************************************************/

/* set of function pointers to give to glib. */
GThreadFunctions cp_gtk_portable_native_sync_jni_functions = {
  mutex_new_jni_impl,		/* mutex_new */
  mutex_lock_jni_impl,		/* mutex_lock */
  mutex_trylock_jni_impl,	/* mutex_trylock */
  mutex_unlock_jni_impl,	/* mutex_unlock */
  mutex_free_jni_impl,		/* mutex_free */
  cond_new_jni_impl,		/* cond_new */
  cond_signal_jni_impl,		/* cond_signal */
  cond_broadcast_jni_impl,	/* cond_broadcast */
  cond_wait_jni_impl,		/* cond_wait */
  cond_timed_wait_jni_impl,	/* cond_timed_wait */
  cond_free_jni_impl,		/* cond_free */
  private_new_jni_impl,		/* private_new */
  private_get_jni_impl,		/* private_get */
  private_set_jni_impl,		/* private_set */
  thread_create_jni_impl,	/* thread_create */
  thread_yield_jni_impl,	/* thread_yield */
  thread_join_jni_impl,		/* thread_join */
  thread_exit_jni_impl,		/* thread_exit */
  thread_set_priority_jni_impl,	/* thread_set_priority */
  thread_self_jni_impl,		/* thread_self */
  thread_equal_jni_impl,	/* thread_equal */
};


/* Keep c-font-lock-extra-types in alphabetical order. */
/* Local Variables: */
/* c-file-style: "gnu" */
/* c-font-lock-extra-types: ("\\sw+_t" "gboolean" "GError" "gpointer"
   "GPrivate" "GThreadFunc" "GThreadFunctions" "GThreadPriority" 
   "gulong" 
   "JNIEnv" 
   "jboolean" "jclass" "jfieldID" "jint" "jlong" "jmethodID" "jobject" "jstring" "jthrowable" ) */
/* End: */
