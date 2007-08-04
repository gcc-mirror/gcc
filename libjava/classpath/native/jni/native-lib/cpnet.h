/* cpnet.h -
   Copyright (C) 2003, 2004, 2005, 2006  Free Software Foundation, Inc.

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

#ifndef _CLASSPATH_NET_H_INCLUDED
#define _CLASSPATH_NET_H_INCLUDED

#include <jni.h>
#include <jcl.h>
#include <string.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in_systm.h>
#include <netinet/in.h>
#include <netinet/ip.h>

typedef struct {
  jint len;
  char data[1];
} cpnet_address;

#define CPNET_SHUTDOWN_READ 1
#define CPNET_SHUTDOWN_WRITE 2

JNIEXPORT jint cpnet_openSocketStream(JNIEnv *env, jint *fd, jint family);
JNIEXPORT jint cpnet_openSocketDatagram(JNIEnv *env, jint *fd, jint family);
JNIEXPORT jint cpnet_shutdown (JNIEnv *env, jint fd, jbyte flag);
JNIEXPORT jint cpnet_close(JNIEnv *env, jint fd);
JNIEXPORT jint cpnet_listen(JNIEnv *env, jint fd, jint queuelen);
JNIEXPORT jint cpnet_accept(JNIEnv *env, jint fd, jint *newfd);
JNIEXPORT jint cpnet_bind(JNIEnv *env, jint fd, cpnet_address *addr);
JNIEXPORT jint cpnet_connect(JNIEnv *env, jint fd, cpnet_address *addr);
JNIEXPORT jint cpnet_getLocalAddr(JNIEnv *env, jint fd, cpnet_address **addr);
JNIEXPORT jint cpnet_getRemoteAddr(JNIEnv *env, jint fd, cpnet_address **addr);
JNIEXPORT jint cpnet_setBroadcast(JNIEnv *env, jint fd, jint flag);
JNIEXPORT jint cpnet_send (JNIEnv *env, jint fd, jbyte *data, jint len, jint *bytes_sent);
JNIEXPORT jint cpnet_sendTo (JNIEnv *env, jint fd, jbyte *data, jint len, cpnet_address *addr, jint *bytes_sent);
JNIEXPORT jint cpnet_recv (JNIEnv *env, jint fd, jbyte *data, jint len, jint *bytes_recv);
JNIEXPORT jint cpnet_recvFrom (JNIEnv *env, jint fd, jbyte *data, jint len, cpnet_address **addr, jint *bytes_recv);
JNIEXPORT jint cpnet_setSocketTCPNoDelay (JNIEnv *env, jint fd, jint nodelay);
JNIEXPORT jint cpnet_getSocketTCPNoDelay (JNIEnv *env, jint fd, jint *nodelay);
JNIEXPORT jint cpnet_setLinger (JNIEnv *env, jint fd, jint flag, jint value);
JNIEXPORT jint cpnet_getLinger (JNIEnv *env, jint fd, jint *flag, jint *value);
JNIEXPORT jint cpnet_setSocketTimeout (JNIEnv *env, jint fd, jint value);
JNIEXPORT jint cpnet_getSocketTimeout (JNIEnv *env, jint fd, jint *value);
JNIEXPORT jint cpnet_setSendBuf (JNIEnv *env, jint fd, jint value);
JNIEXPORT jint cpnet_getSendBuf (JNIEnv *env, jint fd, jint *value);
JNIEXPORT jint cpnet_setRecvBuf (JNIEnv *env, jint fd, jint value);
JNIEXPORT jint cpnet_getRecvBuf (JNIEnv *env, jint fd, jint *value);
JNIEXPORT jint cpnet_setTTL (JNIEnv *env, jint fd, jint value);
JNIEXPORT jint cpnet_getTTL (JNIEnv *env, jint fd, jint *value);
JNIEXPORT jint cpnet_setMulticastIF (JNIEnv *env, jint fd, cpnet_address *addr);
JNIEXPORT jint cpnet_getMulticastIF (JNIEnv *env, jint fd, cpnet_address **addr);
JNIEXPORT jint cpnet_setReuseAddress (JNIEnv *env, jint fd, jint reuse);
JNIEXPORT jint cpnet_getReuseAddress (JNIEnv *env, jint fd, jint *reuse);
JNIEXPORT jint cpnet_setKeepAlive (JNIEnv *env, jint fd, jint keep);
JNIEXPORT jint cpnet_getKeepAlive (JNIEnv *env, jint fd, jint *keep);
JNIEXPORT jint cpnet_getBindAddress (JNIEnv *env, jint fd, cpnet_address **addr);
JNIEXPORT jint cpnet_addMembership (JNIEnv *env, jint fd, cpnet_address *addr);
JNIEXPORT jint cpnet_dropMembership (JNIEnv *env, jint fd, cpnet_address *addr);
JNIEXPORT jint cpnet_getAvailableBytes (JNIEnv *env, jint fd, jint *availableBytes);
JNIEXPORT jint cpnet_getHostname (JNIEnv *env, char *hostname, jint hostname_len);
JNIEXPORT jint cpnet_getHostByName (JNIEnv *env, const char *hostname, cpnet_address ***adresses, jint *addresses_count);
JNIEXPORT jint cpnet_getHostByAddr (JNIEnv *env, cpnet_address *addr, char *hostname, jint hostname_len);
JNIEXPORT jint cpnet_aton (JNIEnv *env, const char *hostname, cpnet_address **addr);
JNIEXPORT void cpnet_freeAddresses(JNIEnv * env, cpnet_address **addr, jint addresses_count);

static inline cpnet_address *cpnet_newIPV4Address(JNIEnv * env)
{
  cpnet_address *addr = (cpnet_address *)JCL_malloc(env, sizeof(cpnet_address) + sizeof(struct sockaddr_in));
  struct sockaddr_in *netaddr = (struct sockaddr_in *)&(addr->data[0]);

  addr->len = sizeof(struct sockaddr_in);
  memset(netaddr, 0, addr->len);
  netaddr->sin_family = AF_INET;
  return addr;
}

static inline void cpnet_setIPV4Any(cpnet_address *addr)
{
  struct sockaddr_in *netaddr = (struct sockaddr_in *)&(addr->data[0]);
  
  netaddr->sin_addr.s_addr = INADDR_ANY;
}

#ifdef HAVE_INET6
static inline cpnet_address *cpnet_newIPV6Address(JNIEnv * env)
{
  cpnet_address * addr = (cpnet_address *)JCL_malloc(env, sizeof(cpnet_address) + sizeof(struct sockaddr_in6));
  struct sockaddr_in6 *netaddr = (struct sockaddr_in6 *)&(addr->data[0]);

  addr->len = sizeof(struct sockaddr_in6);
  memset(netaddr, 0, addr->len);
  netaddr->sin6_family = AF_INET6;

  return addr;
}
#endif

static inline void cpnet_freeAddress(JNIEnv * env, cpnet_address *addr)
{
  JCL_free(env, addr);
}

static inline void cpnet_addressSetPort(cpnet_address *addr, jint port)
{
  struct sockaddr_in *ipaddr = (struct sockaddr_in *)&(addr->data[0]);

  ipaddr->sin_port = htons(port);
}

static inline jint cpnet_addressGetPort(cpnet_address *addr)
{
  struct sockaddr_in *ipaddr = (struct sockaddr_in *)&(addr->data[0]);

  return ntohs(ipaddr->sin_port);
}

static inline jboolean cpnet_isAddressEqual(cpnet_address *addr1, cpnet_address *addr2)
{
  if (addr1->len != addr2->len)
    return JNI_FALSE;

  return memcmp(addr1->data, addr2->data, addr1->len) == 0;
}

#ifdef HAVE_INET6
static inline jboolean cpnet_isIPV6Address(cpnet_address *addr)
{
  struct sockaddr_in *ipaddr = (struct sockaddr_in *)&(addr->data[0]);

  return ipaddr->sin_family == AF_INET6;
}
#endif

static inline jboolean cpnet_isIPV4Address(cpnet_address *addr)
{
  struct sockaddr_in *ipaddr = (struct sockaddr_in *)&(addr->data[0]);

  return ipaddr->sin_family == AF_INET;
}

static inline void cpnet_IPV4AddressToBytes(cpnet_address *netaddr, jbyte *octets)
{
  struct sockaddr_in *ipaddr = (struct sockaddr_in *)&(netaddr->data[0]);
  unsigned long sysaddr = ntohl(ipaddr->sin_addr.s_addr);

  octets[0] = ((sysaddr >> 24) & 0xff);
  octets[1] = ((sysaddr >> 16) & 0xff);
  octets[2] = ((sysaddr >> 8) & 0xff);
  octets[3] = (sysaddr & 0xff);
}

static inline void cpnet_bytesToIPV4Address(cpnet_address *netaddr, jbyte *octets)
{
  jint sysaddr;
  struct sockaddr_in *ipaddr = (struct sockaddr_in *)&(netaddr->data[0]);

  sysaddr = ((jint)(unsigned char)octets[0]) << 24;
  sysaddr |= ((jint)(unsigned char)octets[1]) << 16;
  sysaddr |= ((jint)(unsigned char)octets[2]) << 8;
  sysaddr |= ((jint)(unsigned char)octets[3]);

  ipaddr->sin_addr.s_addr = htonl(sysaddr);
}

#ifdef HAVE_INET6
static inline void cpnet_IPV6AddressToBytes(cpnet_address *netaddr, jbyte *octets)
{
  struct sockaddr_in6 *ipaddr = (struct sockaddr_in6 *)&(netaddr->data[0]);

  memcpy(octets, &ipaddr->sin6_addr, 16);
}

static inline void cpnet_bytesToIPV6Address(cpnet_address *netaddr, jbyte *octets)
{
  struct sockaddr_in6 *ipaddr = (struct sockaddr_in6 *)&(netaddr->data[0]);

  memcpy(&ipaddr->sin6_addr, octets, 16);
}
#endif

#endif
