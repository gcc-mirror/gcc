/* xmlj_error.c -
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

#include "xmlj_error.h"
#include "xmlj_io.h"
#include "xmlj_util.h"

void
xmljXsltErrorFunc (void *ctx, const char *msg, ...)
{
  if (NULL != ctx)
    {
      SAXParseContext *sax = ((SAXParseContext *) ctx);
      
      if (NULL != sax)
        {
          JNIEnv *env = sax->env;
          
          if (!(*env)->ExceptionOccurred (env))
            {
              jobject target = sax->obj;
              xmlChar *x_msg;
              jstring j_msg;
              va_list args;
              
              if (sax->error == NULL)
                {
                  sax->error =
                    xmljGetMethodID (env,
                                     target,
                                     "error",
                                 "(Ljava/lang/String;IILjava/lang/String;Ljava/lang/String;)V");
                  if (sax->error == NULL)
                    {
                      return;
                    }
                }
              
              va_start (args, msg);
              x_msg = (msg == NULL) ? NULL : xmlCharStrdup (msg);
              va_end (args);
              j_msg = xmljNewString (env, x_msg);
              
              (*env)->CallVoidMethod (env,
                                      target,
                                      sax->error,
                                      j_msg,
                                      -1,
                                      -1,
                                      NULL,
                                      NULL);
            }
        }
    }
  else
    {
      va_list va;
      va_start (va, msg);
      fprintf (stderr, "libxslt error: ");
      vfprintf (stderr, msg, va);
      fflush (stderr);
      va_end (va);
    }
}

void
xmljThrowException (JNIEnv *env,
                    const char *classname,
                    const char *message)
{
  jclass cls;
  jmethodID method;
  jthrowable ex;
  jstring jmsg;

  /*fprintf(stderr, "Throwing exception %s %s\n", classname, message);*/
  cls = (*env)->FindClass (env, classname);
  if (cls == NULL)
    {
      fprintf (stderr, "Can't find class %s\n", classname);
      fflush (stderr);
      return;
    }
  method = (*env)->GetMethodID (env, cls, "<init>", "(Ljava/lang/String;)V");
  if (method == NULL)
    {
      fprintf (stderr, "Can't find method %s.<init>\n", classname);
      fflush (stderr);
      return;
    }
  jmsg = (message == NULL) ? NULL : (*env)->NewStringUTF (env, message);
  ex = (jthrowable) (*env)->NewObject (env, cls, method, jmsg);
  if (ex == NULL)
    {
      fprintf (stderr, "Can't instantiate new %s\n", classname);
      fflush (stderr);
      return;
    }
  (*env)->Throw (env, ex);
}

void
xmljThrowDOMException (JNIEnv *env,
                       int code,
                       const char *message)
{
  jclass cls;
  jmethodID method;
  jthrowable ex;
  jstring jmsg;

  if ((*env)->ExceptionOccurred (env))
    {
      return;
    }

  cls = (*env)->FindClass (env, "gnu/xml/libxmlj/dom/GnomeDOMException");
  if (cls == NULL)
    {
      fprintf (stderr, "Can't find DOMException class!\n");
      fflush (stderr);
      return;
    }
  method = (*env)->GetMethodID (env, cls, "<init>", "(SLjava/lang/String;)V");
  if (method == NULL)
    {
      fprintf (stderr, "Can't find DOMException constructor!\n");
      fflush (stderr);
      return;
    }
  jmsg = (message == NULL) ? NULL : (*env)->NewStringUTF (env, message);
  ex = (jthrowable) (*env)->NewObject (env, cls, method, code, jmsg);
  (*env)->Throw (env, ex);
}

