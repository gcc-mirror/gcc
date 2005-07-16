/* VMInetAddress.c - Native methods for InetAddress class
   Copyright (C) 1998, 2002, 2005  Free Software Foundation, Inc.

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

/* do not move; needed here because of some macro definitions */
#include <config.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <jni.h>
#include <jcl.h>

#include "javanet.h"

#include "target_native.h"
#ifndef WITHOUT_NETWORK
#include "target_native_network.h"
#endif /* WITHOUT_NETWORK */

#include "java_net_VMInetAddress.h"

/*************************************************************************/

/*
 * Function to return the local hostname
 */
JNIEXPORT jstring JNICALL
Java_java_net_VMInetAddress_getLocalHostname (JNIEnv * env,
					      jclass class
					      __attribute__ ((__unused__)))
{
  char hostname[256];
  int result;
  jstring retval;

  assert (env != NULL);
  assert ((*env) != NULL);

#ifndef WITHOUT_NETWORK
  TARGET_NATIVE_NETWORK_GET_HOSTNAME (hostname, sizeof (hostname), result);
  if (result != TARGET_NATIVE_OK)
    {
      strcpy (hostname, "localhost");
    }
#else /* not WITHOUT_NETWORK */
  strcpy (hostname, "localhost");
#endif /* not WITHOUT_NETWORK */

  retval = (*env)->NewStringUTF (env, hostname);

  return (retval);
}

/*************************************************************************/

/*
 * Returns the value of the special IP address INADDR_ANY 
 */
JNIEXPORT jarray JNICALL
Java_java_net_VMInetAddress_lookupInaddrAny (JNIEnv * env,
					     jclass class
					     __attribute__ ((__unused__)))
{
  jarray IParray;
  jbyte *octets;

  assert (env != NULL);
  assert ((*env) != NULL);

  /* Allocate an array for the IP address */
  IParray = (*env)->NewByteArray (env, 4);
  if (IParray == NULL)
    {
      JCL_ThrowException (env, UNKNOWN_HOST_EXCEPTION, "Internal Error");
      return (jarray) NULL;
    }

  /* Copy in the values */
  octets = (*env)->GetByteArrayElements (env, IParray, 0);

#ifndef WITHOUT_NETWORK
  TARGET_NATIVE_NETWORK_INT_TO_IPADDRESS_BYTES (INADDR_ANY,
						octets[0],
						octets[1],
						octets[2], octets[3]);
  (*env)->ReleaseByteArrayElements (env, IParray, octets, 0);
#else /* not WITHOUT_NETWORK */
  octets[0] = 0;
  octets[1] = 0;
  octets[2] = 0;
  octets[3] = 0;
#endif /* not WITHOUT_NETWORK */

  return (IParray);
}

/*************************************************************************/

/*
 * Function to return the canonical hostname for a given IP address passed
 * in as a byte array
 */
JNIEXPORT jstring JNICALL
Java_java_net_VMInetAddress_getHostByAddr (JNIEnv * env,
					   jclass class
					   __attribute__ ((__unused__)),
					   jarray arr)
{
#ifndef WITHOUT_NETWORK
  jbyte *octets;
  jsize len;
  int addr;
  char hostname[255];
  int result;
  jstring retval;

  assert (env != NULL);
  assert ((*env) != NULL);

  /* Grab the byte[] array with the IP out of the input data */
  len = (*env)->GetArrayLength (env, arr);
  if (len != 4)
    {
      JCL_ThrowException (env, UNKNOWN_HOST_EXCEPTION, "Bad IP Address");
      return (jstring) NULL;
    }

  octets = (*env)->GetByteArrayElements (env, arr, 0);
  if (!octets)
    {
      JCL_ThrowException (env, UNKNOWN_HOST_EXCEPTION, "Bad IP Address");
      return (jstring) NULL;
    }

  /* Convert it to a 32 bit address */
  TARGET_NATIVE_NETWORK_IPADDRESS_BYTES_TO_INT (octets[0],
						octets[1],
						octets[2], octets[3], addr);

  /* Release some memory */
  (*env)->ReleaseByteArrayElements (env, arr, octets, 0);

  /* Resolve the address and return the name */
  TARGET_NATIVE_NETWORK_GET_HOSTNAME_BY_ADDRESS (addr, hostname,
						 sizeof (hostname), result);
  if (result != TARGET_NATIVE_OK)
    {
      JCL_ThrowException (env, UNKNOWN_HOST_EXCEPTION, "Bad IP address");
      return (jstring) NULL;
    }

  retval = (*env)->NewStringUTF (env, hostname);

  return (retval);
#else /* not WITHOUT_NETWORK */
  return (jstring) NULL;
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

JNIEXPORT jobjectArray JNICALL
Java_java_net_VMInetAddress_getHostByName (JNIEnv * env,
					   jclass class
					   __attribute__ ((__unused__)),
					   jstring host)
{
#ifndef WITHOUT_NETWORK
  const char *hostname;
/* FIXME: limitation of max. 64 addresses - how to make it more flexibale? */
  int addresses[64];
  jsize addresses_count;
  int result;
  jclass arr_class;
  jobjectArray addrs;
  int i;
  jbyte *octets;
  jarray ret_octets;
  int max_addresses;

  assert (env != NULL);
  assert ((*env) != NULL);

  /* Grab the hostname string */
  hostname = (*env)->GetStringUTFChars (env, host, 0);
  if (!hostname)
    {
      JCL_ThrowException (env, UNKNOWN_HOST_EXCEPTION, "Null hostname");
      return (jobjectArray) NULL;
    }

  max_addresses = sizeof (addresses) / sizeof (addresses[0]);
  TARGET_NATIVE_NETWORK_GET_HOSTNAME_BY_NAME (hostname,
					      addresses,
					      max_addresses,
					      addresses_count, result);
  if (result != TARGET_NATIVE_OK)
    {
      JCL_ThrowException (env, UNKNOWN_HOST_EXCEPTION, (char *) hostname);
      return (jobjectArray) NULL;
    }
  (*env)->ReleaseStringUTFChars (env, host, hostname);

  arr_class = (*env)->FindClass (env, "[B");
  if (!arr_class)
    {
      JCL_ThrowException (env, UNKNOWN_HOST_EXCEPTION, "Internal Error");
      return (jobjectArray) NULL;
    }

  addrs = (*env)->NewObjectArray (env, addresses_count, arr_class, 0);
  if (!addrs)
    {
      JCL_ThrowException (env, UNKNOWN_HOST_EXCEPTION, "Internal Error");
      return (jobjectArray) NULL;
    }

  /* Now loop and copy in each address */
  for (i = 0; i < addresses_count; i++)
    {
      ret_octets = (*env)->NewByteArray (env, 4);
      if (!ret_octets)
	{
	  JCL_ThrowException (env, UNKNOWN_HOST_EXCEPTION, "Internal Error");
	  return (jobjectArray) NULL;
	}

      octets = (*env)->GetByteArrayElements (env, ret_octets, 0);

      TARGET_NATIVE_NETWORK_INT_TO_IPADDRESS_BYTES (addresses[i],
						    octets[0],
						    octets[1],
						    octets[2], octets[3]);

      (*env)->ReleaseByteArrayElements (env, ret_octets, octets, 0);

      (*env)->SetObjectArrayElement (env, addrs, i, ret_octets);
    }

  return (addrs);
#else /* not WITHOUT_NETWORK */
  return (jobjectArray) NULL;
#endif /* not WITHOUT_NETWORK */
}

/* end of file */
