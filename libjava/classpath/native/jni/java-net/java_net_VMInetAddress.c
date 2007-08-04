/* VMInetAddress.c - Native methods for InetAddress class
   Copyright (C) 1998, 2002, 2005, 2006  Free Software Foundation, Inc.

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

#include <jni.h>
#include <jcl.h>

#include "cpnative.h"
#include "cpnet.h"
#include "javanet.h"

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

#ifndef WITHOUT_NETWORK
  result = cpnet_getHostname (env, hostname, sizeof (hostname));
  if (result != CPNATIVE_OK)
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
  cpnet_address *addr;
  jbyte *octets;

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
  addr = cpnet_newIPV4Address (env);
  cpnet_setIPV4Any (addr);
  cpnet_IPV4AddressToBytes (addr, octets);
  cpnet_freeAddress (env, addr);
#else /* not WITHOUT_NETWORK */
  octets[0] = 0;
  octets[1] = 0;
  octets[2] = 0;
  octets[3] = 0;
#endif /* not WITHOUT_NETWORK */

  (*env)->ReleaseByteArrayElements (env, IParray, octets, 0);

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
  cpnet_address *addr;
  char hostname[255];
  int result;
  jstring retval;

  /* Grab the byte[] array with the IP out of the input data */
  len = (*env)->GetArrayLength (env, arr);
  if (len != 4 && len != 16)
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

  switch (len)
    {
    case 4:
      addr = cpnet_newIPV4Address(env);
      cpnet_bytesToIPV4Address (addr, octets);      
      break;
#ifdef HAVE_INET6
    case 16:
      addr = cpnet_newIPV6Address(env);
      cpnet_bytesToIPV6Address (addr, octets);
      break;
#endif
    default:
      JCL_ThrowException (env, UNKNOWN_HOST_EXCEPTION, "Bad IP Address");
      return (jstring) NULL;

   }

  /* Release some memory */
  (*env)->ReleaseByteArrayElements (env, arr, octets, 0);

  /* Resolve the address and return the name */
  result = cpnet_getHostByAddr (env, addr, hostname, sizeof (hostname));
  if (result != CPNATIVE_OK)
    {
      JCL_ThrowException (env, UNKNOWN_HOST_EXCEPTION,
			  cpnative_getErrorString (result));
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
  cpnet_address **addresses;
  jsize addresses_count;
  int result;
  jclass arr_class;
  jobjectArray addrs;
  jint i;
  jbyte *octets;
  jarray ret_octets;

  /* Grab the hostname string */
  hostname = (*env)->GetStringUTFChars (env, host, 0);
  if (!hostname)
    {
      JCL_ThrowException (env, UNKNOWN_HOST_EXCEPTION, "Null hostname");
      return (jobjectArray) NULL;
    }

  result = cpnet_getHostByName (env, hostname, &addresses, &addresses_count);
  if (result != CPNATIVE_OK || addresses_count == 0)
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
      if (cpnet_isIPV4Address (addresses[i]))
	{
	  ret_octets = (*env)->NewByteArray (env, 4);

	  if (!ret_octets)
	    {
	      JCL_ThrowException (env, UNKNOWN_HOST_EXCEPTION, "Internal Error");
	      cpnet_freeAddresses (env, addresses, addresses_count);
	      return (jobjectArray) NULL;
	    }
	  
	  octets = (*env)->GetByteArrayElements (env, ret_octets, 0);

	  cpnet_IPV4AddressToBytes (addresses[i], octets);

	  (*env)->ReleaseByteArrayElements (env, ret_octets, octets, 0);

	  (*env)->SetObjectArrayElement (env, addrs, i, ret_octets);
	}
#ifdef HAVE_INET6
      else if (cpnet_isIPV6Address (addresses[i]))
	{
	  ret_octets = (*env)->NewByteArray (env, 16);

	  if (!ret_octets)
	    {
	      JCL_ThrowException (env, UNKNOWN_HOST_EXCEPTION, "Internal Error");
	      cpnet_freeAddresses (env, addresses, addresses_count);
	      return (jobjectArray) NULL;
	    }
	  
	  octets = (*env)->GetByteArrayElements (env, ret_octets, 0);

	  cpnet_IPV6AddressToBytes (addresses[i], octets);

	  (*env)->ReleaseByteArrayElements (env, ret_octets, octets, 0);

	  (*env)->SetObjectArrayElement (env, addrs, i, ret_octets);
	}
#endif
      else
	{
	  JCL_ThrowException (env, UNKNOWN_HOST_EXCEPTION, "Internal Error");
	  cpnet_freeAddresses (env, addresses, addresses_count);
	  return (jobjectArray) NULL;
	}
    }

  cpnet_freeAddresses (env, addresses, addresses_count);

  return (addrs);
#else /* not WITHOUT_NETWORK */
  return (jobjectArray) NULL;
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Return the IP address represented by a literal address.
 * Will return null if the literal address is not valid.
 */
JNIEXPORT jbyteArray JNICALL
Java_java_net_VMInetAddress_aton (JNIEnv *env,
				  jclass class
				  __attribute__ ((__unused__)),
				  jstring host)
{
#ifndef WITHOUT_NETWORK
  const char *hostname;
  cpnet_address *address;
  int result;
  jbyte *octets;
  jbyteArray ret_octets;

  hostname = (*env)->GetStringUTFChars (env, host, 0);
  if (!hostname)
    {
      JCL_ThrowException (env, UNKNOWN_HOST_EXCEPTION, "Null hostname");
      return (jbyteArray) NULL;
    }

  result = cpnet_aton (env, hostname, &address);
  if (result != CPNATIVE_OK)
    {
      JCL_ThrowException (env, UNKNOWN_HOST_EXCEPTION, "Internal Error");
      if (address)
	cpnet_freeAddress (env, address);
      return (jbyteArray) NULL;
    }
  if (!address)
    return (jbyteArray) NULL;

  if (cpnet_isIPV4Address (address))
    {
      ret_octets = (jbyteArray) (*env)->NewByteArray (env, 4);

      if (!ret_octets)
	{
	  JCL_ThrowException (env, UNKNOWN_HOST_EXCEPTION, "Internal Error");
	  cpnet_freeAddress (env, address);
	  return (jbyteArray) NULL;
	}
	  
      octets = (*env)->GetByteArrayElements (env, ret_octets, 0);

      cpnet_IPV4AddressToBytes (address, octets);

      (*env)->ReleaseByteArrayElements (env, ret_octets, octets, 0);
    }
#ifdef HAVE_INET6
  else if (cpnet_isIPV6Address (address))
    {
      ret_octets = (jbyteArray) (*env)->NewByteArray (env, 16);

      if (!ret_octets)
	{
	  JCL_ThrowException (env, UNKNOWN_HOST_EXCEPTION, "Internal Error");
	  cpnet_freeAddress (env, address);
	  return (jbyteArray) NULL;
	}
	  
      octets = (*env)->GetByteArrayElements (env, ret_octets, 0);

      cpnet_IPV6AddressToBytes (address, octets);

      (*env)->ReleaseByteArrayElements (env, ret_octets, octets, 0);
    }
#endif
  else
    {
      JCL_ThrowException (env, UNKNOWN_HOST_EXCEPTION, "Internal Error");
      cpnet_freeAddress (env, address);
      return (jbyteArray) NULL;
    }

  cpnet_freeAddress (env, address);

  return (ret_octets);
  
#else /* not WITHOUT_NETWORK */
  return (jbyteArray) NULL;
#endif /* not WITHOUT_NETWORK */
}

/* end of file */
