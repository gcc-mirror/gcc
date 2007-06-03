/* VMNetworkInterface.c - Native methods for NetworkInterface class
   Copyright (C) 2003, 2005, 2006  Free Software Foundation, Inc.

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <sys/types.h>
#include <sys/socket.h>
#ifdef HAVE_IFADDRS_H
#include <ifaddrs.h>
#endif
#include <netinet/in.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <jni.h>
#include <jcl.h>

#include "java_net_VMNetworkInterface.h"


static jmethodID java_net_VMNetworkInterface_init;
static jmethodID java_net_VMNetworkInterface_addAddress;

/*
 * Initialize our static method ID's.
 *
 * Class:     java_net_VMNetworkInterface
 * Method:    initIds
 * Signature: ()V
 */
JNIEXPORT void JNICALL
Java_java_net_VMNetworkInterface_initIds (JNIEnv *env, jclass clazz)
{
  java_net_VMNetworkInterface_init =
    (*env)->GetMethodID (env, clazz, "<init>", "(Ljava/lang/String;)V");
  if (java_net_VMNetworkInterface_init == NULL)
    {
      if (!(*env)->ExceptionCheck (env))
        JCL_ThrowException (env, "java/lang/NoSuchMethodError",
                            "VMNetworkinterface.addAddress");
      return;
    }
  java_net_VMNetworkInterface_addAddress =
    (*env)->GetMethodID (env, clazz, "addAddress", "(Ljava/nio/ByteBuffer;)V");
  if (java_net_VMNetworkInterface_addAddress == NULL)
    {
      if (!(*env)->ExceptionCheck (env))
        JCL_ThrowException (env, "java/lang/NoSuchMethodError",
                            "VMNetworkinterface.addAddress");
    }
}

struct netif_entry
{
  char *name;
  jobject netif;
  int numaddrs;
  struct netif_entry *next;
};

#if defined (HAVE_IFADDRS_H) && defined (HAVE_GETIFADDRS)
static void
free_netif_list (JNIEnv *env, struct netif_entry *list)
{
  while (list != NULL)
    {
      struct netif_entry *e = list->next;
      JCL_free (env, list);
      list = e;
    }
}
#endif

/*
 * Returns all local network interfaces as an array.
 */
JNIEXPORT jobjectArray JNICALL
Java_java_net_VMNetworkInterface_getVMInterfaces (JNIEnv * env,
                                                  jclass clazz UNUSED)
{
#if defined (HAVE_IFADDRS_H) && defined (HAVE_GETIFADDRS)
  struct ifaddrs *ifaddrs, *i;
  struct netif_entry *iflist = NULL, *e;
  jobjectArray netifs;
  int numifs = 0;
  int k;

  if (getifaddrs (&ifaddrs) == -1)
    {
      JCL_ThrowException (env, "java/net/SocketException", strerror (errno));
      return NULL;
    }

  for (i = ifaddrs; i != NULL; i = i->ifa_next)
    {
      if (iflist == NULL)
        {
          iflist = JCL_malloc (env, sizeof (struct netif_entry));
          if (iflist == NULL)
            {
              freeifaddrs (ifaddrs);
              return NULL;
            }
          iflist->name = i->ifa_name;
          iflist->numaddrs = 0;
          iflist->next = NULL;
          iflist->netif = (*env)->NewObject (env, clazz, java_net_VMNetworkInterface_init,
                                             (*env)->NewStringUTF (env, i->ifa_name));
          if (iflist->netif == NULL)
            {
              freeifaddrs (ifaddrs);
              JCL_free (env, iflist);
              return NULL;
            }
          e = iflist;
        }
      else
        {
          struct netif_entry *p = NULL;
          for (e = iflist; e != NULL; e = e->next)
            {
              if (strcmp (e->name, i->ifa_name) == 0)
                break;
              p = e;
            }

          if (e == NULL)
            {
              p->next = (struct netif_entry *) JCL_malloc (env, sizeof (struct netif_entry));
              if (p->next == NULL)
                {
                  free_netif_list (env, iflist);
                  freeifaddrs (ifaddrs);
                  return NULL;
                }
              e = p->next;
              e->name = i->ifa_name;
              e->numaddrs = 0;
              e->next = NULL;
              e->netif = (*env)->NewObject (env, clazz, java_net_VMNetworkInterface_init,
                                            (*env)->NewStringUTF (env, i->ifa_name));
              if (e->netif == NULL)
                {
                  free_netif_list (env, iflist);
                  freeifaddrs (ifaddrs);
                  return NULL;
                }
            }
        }

      if (i->ifa_addr == NULL)
        continue;

      if (i->ifa_addr->sa_family == AF_INET)
        {
          struct sockaddr_in *sin = (struct sockaddr_in *) i->ifa_addr;
          jobject buffer = (*env)->NewDirectByteBuffer (env, &(sin->sin_addr.s_addr), 4);
          (*env)->CallVoidMethod (env, e->netif, java_net_VMNetworkInterface_addAddress,
                                  buffer);
          if ((*env)->ExceptionCheck (env))
            {
              free_netif_list (env, iflist);
              freeifaddrs (ifaddrs);
              return NULL;
            }
          (*env)->DeleteLocalRef (env, buffer);
          e->numaddrs++;
        }
#ifdef HAVE_INET6
      else if (i->ifa_addr->sa_family == AF_INET6)
        {
          struct sockaddr_in6 *sin = (struct sockaddr_in6 *) i->ifa_addr;
          jobject buffer = (*env)->NewDirectByteBuffer (env, &(sin->sin6_addr.s6_addr), 16);
          (*env)->CallVoidMethod (env, e->netif, java_net_VMNetworkInterface_addAddress,
                                  buffer);
          if ((*env)->ExceptionCheck (env))
            {
              free_netif_list (env, iflist);
              freeifaddrs (ifaddrs);
              return NULL;
            }
          (*env)->DeleteLocalRef (env, buffer);
          e->numaddrs++;
        }
#endif /* HAVE_INET6 */
    }

  /* Count how many interfaces we have that have addresses. */
  for (e = iflist; e != NULL; e = e->next)
    {
      if (e->numaddrs != 0)
        numifs++;
    }

  netifs = (*env)->NewObjectArray (env, numifs, clazz, NULL);
  k = 0;
  for (e = iflist; e != NULL && k < numifs; e = e->next)
    {
      if (e->numaddrs != 0)
        {
          (*env)->SetObjectArrayElement (env, netifs, k, e->netif);
          (*env)->DeleteLocalRef (env, e->netif);
          k++;
        }
    }

  free_netif_list (env, iflist);
  freeifaddrs (ifaddrs);
  return netifs;
#else
  JCL_ThrowException (env, "java/net/SocketException", "getifaddrs not supported");
  return NULL;
#endif /* HAVE_IFADDRS_H && HAVE_GETIFADDRS */
}

/* end of file */
