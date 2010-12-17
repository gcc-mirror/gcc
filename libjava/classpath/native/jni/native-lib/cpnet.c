/* cpnet.c -
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

#include "config.h"
#include <jni.h>
#include <assert.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <sys/time.h>
#include <unistd.h>
#include <arpa/inet.h>

#if defined(HAVE_SYS_IOCTL_H)
#define BSD_COMP /* Get FIONREAD on Solaris2 */
#include <sys/ioctl.h>
#endif
#if defined(HAVE_SYS_FILIO_H) /* Get FIONREAD on Solaris 2.5 */
#include <sys/filio.h>
#endif

#include "cpnet.h"

#define SOCKET_DEFAULT_TIMEOUT -1 /* milliseconds */

static int socketTimeouts[FD_SETSIZE];

static jint waitForWritable(jint fd)
{
  struct timeval tv;
  fd_set writeset;
  int ret;
  
 
  FD_ZERO(&writeset);
  FD_SET(fd, &writeset);
  if (socketTimeouts[fd] > 0)
    {
      tv.tv_sec = socketTimeouts[fd] / 1000;
      tv.tv_usec = (socketTimeouts[fd] % 1000) * 1000;
      ret = select(fd+1, NULL, &writeset, NULL, &tv);
    }
  else
    ret = select(fd+1, NULL, &writeset, NULL, NULL);

  return (ret <= 0) ? -1 : 0;
}

static jint waitForReadable(jint fd)
{
  struct timeval tv;
  fd_set readset;
  int ret;


  FD_ZERO(&readset);
  FD_SET(fd, &readset);
  if (socketTimeouts[fd] > 0)
    {
      tv.tv_sec = socketTimeouts[fd] / 1000;
      tv.tv_usec = (socketTimeouts[fd] % 1000) * 1000;
      ret = select(fd+1, &readset, NULL, NULL, &tv);
    }
  else
    ret = select(fd+1, &readset, NULL, NULL, NULL);

  return (ret <= 0) ? -1 : 0;
}

jint cpnet_openSocketStream(JNIEnv *env UNUSED, jint *fd, jint family)
{
  *fd = socket(family, SOCK_STREAM, 0);
  if (*fd == -1)
    return errno;

  fcntl(*fd, F_SETFD, FD_CLOEXEC);
  assert(*fd < FD_SETSIZE);
  socketTimeouts[*fd] = SOCKET_DEFAULT_TIMEOUT;
  return 0;
}

jint cpnet_openSocketDatagram(JNIEnv *env UNUSED, jint *fd, jint family)
{
  *fd = socket(family, SOCK_DGRAM, 0);
  if (*fd == -1)
    return errno;

  fcntl(*fd, F_SETFD, FD_CLOEXEC);
  assert(*fd < FD_SETSIZE);
  socketTimeouts[*fd] = SOCKET_DEFAULT_TIMEOUT;
  return 0;
}

jint cpnet_shutdown (JNIEnv *env UNUSED, jint fd, jbyte flag)
{
  int ret;
  int shut_flag = 0;

  if (flag == CPNET_SHUTDOWN_READ)
    shut_flag = SHUT_RD;
  else if (flag == CPNET_SHUTDOWN_WRITE)
    shut_flag = SHUT_WR;

  ret = shutdown (fd, shut_flag);
  if (ret != 0)
    return errno;
  return 0;
}

jint cpnet_close(JNIEnv *env UNUSED, jint fd)
{
  if (close (fd) != 0)
    return errno;
  return 0;
}

jint cpnet_listen(JNIEnv *env UNUSED, jint fd, jint queuelen)
{
  if (listen (fd, queuelen) != 0)
    return errno;
  return 0;
}

jint cpnet_accept(JNIEnv *env UNUSED, jint fd, jint *newfd)
{
  if (waitForReadable (fd) < 0)
    return ETIMEDOUT;

  *newfd = accept(fd, NULL, 0);
  if (*newfd != 0)
    return errno;

  return 0;
}

jint cpnet_bind(JNIEnv *env UNUSED, jint fd, cpnet_address *addr)
{
  int ret;

  ret = bind(fd, (struct sockaddr *)addr->data, addr->len);
  if (ret != 0)
    return errno;

  return 0;
}

jint cpnet_connect(JNIEnv *env UNUSED, jint fd, cpnet_address *addr)
{
  int ret;

  /* TODO: implement socket time out */
  ret = connect(fd, (struct sockaddr *)addr->data, addr->len);
  if (ret != 0)
    return errno;

  return 0;
}

jint cpnet_getLocalAddr(JNIEnv *env, jint fd, cpnet_address **addr)
{
  socklen_t slen = 1024;
  int ret;

  *addr = JCL_malloc(env, slen);

  slen -= sizeof(jint);
  ret = getsockname(fd, (struct sockaddr *)(*addr)->data, &slen );
  if (ret != 0)
    {
      int err = errno;
      JCL_free(env, *addr);
      return err;
    }

  (*addr)->len = slen;
  
  return 0;
}

jint cpnet_getRemoteAddr(JNIEnv *env, jint fd, cpnet_address **addr)
{
  socklen_t slen = 1024;
  int ret;

  *addr = JCL_malloc(env, slen);

  slen -= sizeof(jint);
  ret = getpeername(fd, (struct sockaddr *)(*addr)->data, &slen );
  if (ret != 0)
    {
      int err = errno;
      JCL_free(env, *addr);
      return err;
    }

  (*addr)->len = slen;
  
  return 0;
}

jint cpnet_setBroadcast(JNIEnv *env UNUSED, jint fd, jint flag)
{
  int ret;

  ret = setsockopt(fd, SOL_SOCKET, SO_BROADCAST, &flag, sizeof(flag));
  if (ret != 0)
    return errno;

  return 0;
}

#if defined (HAVE_MSG_NOSIGNAL)
#elif defined (HAVE_SO_NOSIGPIPE)
static int setsockopt_NOSIGPIPE (int fd)
{
  int setToTrue = 1;
  return setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &setToTrue, sizeof(setToTrue));
}
#endif

jint cpnet_send (JNIEnv *env UNUSED, jint fd, jbyte *data, jint len, jint *bytes_sent)
{
  ssize_t ret;

  if (waitForWritable(fd) < 0)
    return ETIMEDOUT;

#if defined (HAVE_MSG_NOSIGNAL)
  ret = send(fd, data, len, MSG_NOSIGNAL);
#elif defined (HAVE_SO_NOSIGPIPE)
  ret = setsockopt_NOSIGPIPE(fd);
  if (ret == 0) ret = send(fd, data, len, 0);
#else
  /* We want SIGPIPE to be omitted. But this configuration does not have an
   * option for that.
   */
  ret = send(fd, data, len, 0);
#endif
  if (ret < 0)
    return errno;

  *bytes_sent = ret;

  return 0;
}

jint cpnet_sendTo (JNIEnv *env UNUSED, jint fd, jbyte *data, jint len, cpnet_address *addr, jint *bytes_sent)
{
  ssize_t ret;

  if (waitForWritable(fd) < 0)
    return ETIMEDOUT;

#if defined (HAVE_MSG_NOSIGNAL)
  ret = sendto(fd, data, len, MSG_NOSIGNAL, (struct sockaddr *)addr->data,
	       addr->len);
#elif defined (HAVE_SO_NOSIGPIPE)
  ret = setsockopt_NOSIGPIPE(fd);
  if (ret == 0)
  {
    ret = sendto(fd, data, len, 0, (struct sockaddr *)addr->data,
	       addr->len);
  }
#else
  /* We want SIGPIPE to be omitted. But this configuration does not have an
   * option for that.
   */
  ret = sendto(fd, data, len, 0, (struct sockaddr *)addr->data,
	       addr->len);
#endif

  if (ret < 0)
    return errno;

  *bytes_sent = ret;
  return 0;
}

jint cpnet_recv (JNIEnv *env UNUSED, jint fd, jbyte *data, jint len, jint *bytes_recv)
{
  ssize_t ret;

  if (waitForReadable(fd) < 0)
    return ETIMEDOUT;

  ret = recv(fd, data, len, 0);
  if (ret < 0)
    return errno;

  *bytes_recv = ret;

  return 0;
}

jint cpnet_recvFrom (JNIEnv *env, jint fd, jbyte *data, jint len, cpnet_address **addr, jint *bytes_recv)
{
  socklen_t slen = 1024;
  ssize_t ret;

  if (waitForReadable(fd) < 0)
    return ETIMEDOUT;

  *addr = JCL_malloc(env, slen);

  slen -= sizeof(jint);
  ret = recvfrom(fd, data, len, 0, (struct sockaddr *) (*addr)->data, &slen);
  if (ret < 0)
    {
      int err = errno;
      JCL_free(env, *addr);
      return err;
    }

  (*addr)->len = slen;
  *bytes_recv = ret;

  return 0;
}

jint cpnet_setSocketTCPNoDelay (JNIEnv *env UNUSED, jint fd, jint nodelay)
{
  socklen_t len = sizeof(jint);
  int ret;

  ret = setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &nodelay, len);
  if (ret < 0)
    return errno;

  return 0; 
}

jint cpnet_getSocketTCPNoDelay (JNIEnv *env UNUSED, jint fd, jint *nodelay)
{
  socklen_t len = sizeof(jint);
  int ret;

  ret = getsockopt(fd, IPPROTO_TCP, TCP_NODELAY, nodelay, &len);
  if (ret < 0)
    return errno;

  return 0;
}

jint cpnet_setLinger (JNIEnv *env UNUSED, jint fd, jint flag, jint value)
{
  socklen_t len = sizeof(struct linger);
  int ret;
  struct linger __linger;

  if (flag)
    {
      __linger.l_onoff = 0;
    }
  else
    {
      __linger.l_linger = value;
      __linger.l_onoff = 1;
    }

  ret = setsockopt(fd, SOL_SOCKET, SO_LINGER, &__linger, len);
  if (ret < 0)
    return errno;

  return 0; 
}

jint cpnet_getLinger (JNIEnv *env UNUSED, jint fd, jint *flag, jint *value)
{
  socklen_t slen = sizeof(struct linger);
  struct linger __linger;
  int ret;

  ret = getsockopt(fd, SOL_SOCKET, SO_LINGER, &__linger, &slen);
  if (ret != 0)
    return errno;

  *flag = __linger.l_onoff;
  *value = __linger.l_linger;

  return ret;
}

jint cpnet_setSocketTimeout (JNIEnv *env UNUSED, jint fd, jint value)
{
  socketTimeouts[fd] = value;
  return 0;
}

jint cpnet_getSocketTimeout (JNIEnv *env UNUSED, jint fd, jint *value)
{
  *value = socketTimeouts[fd];
  return 0;
}

jint cpnet_setSendBuf (JNIEnv *env UNUSED, jint fd, jint value)
{
  int ret;

  ret = setsockopt(fd, SOL_SOCKET, SO_SNDBUF, &value, sizeof(value));
  if (ret != 0)
    return errno;

  return 0;
}

jint cpnet_getSendBuf (JNIEnv *env UNUSED, jint fd, jint *value)
{
  int ret;
  socklen_t slen = sizeof(*value);

  ret = getsockopt(fd, SOL_SOCKET, SO_SNDBUF, value, &slen);
  if (ret != 0)
    return errno;

  return 0;
}

jint cpnet_setRecvBuf (JNIEnv *env UNUSED, jint fd, jint value)
{
  int ret;

  ret = setsockopt(fd, SOL_SOCKET, SO_RCVBUF, &value, sizeof(value));
  if (ret != 0)
    return errno;

  return 0;
}

jint cpnet_getRecvBuf (JNIEnv *env UNUSED, jint fd, jint *value)
{
  int ret;
  socklen_t slen = sizeof(*value);

  ret = getsockopt(fd, SOL_SOCKET, SO_RCVBUF, value, &slen);
  if (ret != 0)
    return errno;
  
  return 0;
}

jint cpnet_setTTL (JNIEnv *env UNUSED, jint fd, jint value)
{ 
  int ret;

  ret = setsockopt(fd, IPPROTO_IP, IP_TTL, &value, sizeof(value));
  if (ret != 0)
    return errno;

  return 0;
}

jint cpnet_getTTL (JNIEnv *env UNUSED, jint fd, jint *value)
{
  int ret;
  socklen_t slen = sizeof(*value);

  ret = getsockopt(fd, IPPROTO_IP, IP_TTL, value, &slen);
  if (ret != 0)
    return errno;
  
  return 0;
}

jint cpnet_setMulticastIF (JNIEnv *env UNUSED, jint fd, cpnet_address *addr)
{
  int ret;

  ret = setsockopt(fd, IPPROTO_IP, IP_MULTICAST_IF, (struct sockaddr *)addr->data, addr->len);
  if (ret != 0)
    return errno;

  return 0;
}

jint cpnet_getMulticastIF (JNIEnv *env, jint fd, cpnet_address **addr)
{
  socklen_t slen = 1024;
  int ret;

  *addr = JCL_malloc(env, slen);

  slen -= sizeof(jint);
  ret = getsockopt(fd, IPPROTO_IP, IP_MULTICAST_IF, (struct sockaddr *)(*addr)->data, &slen);
  (*addr)->len = slen;

  if (ret != 0)
    return errno;
  
  return 0;
}

jint cpnet_setReuseAddress (JNIEnv *env UNUSED, jint fd, jint reuse)
{
  int ret;

  ret = setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(reuse));
  if (ret != 0)
    return errno;

  return 0;
}

jint cpnet_getReuseAddress (JNIEnv *env UNUSED, jint fd, jint *reuse)
{
  int ret;
  socklen_t slen = sizeof(*reuse);

  ret = getsockopt(fd, SOL_SOCKET, SO_REUSEADDR, reuse, &slen);
  if (ret != 0)
    return errno;

  return 0;
}

jint cpnet_setKeepAlive (JNIEnv *env UNUSED, jint fd, jint keep)
{
  int ret;

  ret = setsockopt(fd, SOL_SOCKET, SO_KEEPALIVE, &keep, sizeof(keep));
  if (ret != 0)
    return errno;

  return 0;
}

jint cpnet_getKeepAlive (JNIEnv *env UNUSED, jint fd, jint *keep)
{
  int ret;
  socklen_t slen = sizeof(*keep);

  ret = getsockopt(fd, SOL_SOCKET, SO_KEEPALIVE, keep, &slen);
  if (ret != 0)
    return errno;

  return 0;
}

jint cpnet_addMembership (JNIEnv *env UNUSED, jint fd, cpnet_address *addr)
{
  struct ip_mreq req;
  int ret;
  struct sockaddr_in *sockaddr = (struct sockaddr_in *)addr->data;
  
  memset(&req, 0, sizeof(req));
  req.imr_multiaddr = sockaddr->sin_addr;
  req.imr_interface.s_addr = INADDR_ANY;
  ret = setsockopt(fd, IPPROTO_IP, IP_ADD_MEMBERSHIP, &req, sizeof(req));
  if (ret != 0)
    return errno;
  
  return 0;
}

jint cpnet_dropMembership (JNIEnv *env UNUSED, jint fd, cpnet_address *addr)
{
  struct ip_mreq req;
  int ret;
  struct sockaddr_in *sockaddr = (struct sockaddr_in *)addr->data;
  
  memset(&req, 0, sizeof(req));
  req.imr_multiaddr = sockaddr->sin_addr;
  req.imr_interface.s_addr = INADDR_ANY;
  ret = setsockopt(fd, IPPROTO_IP, IP_DROP_MEMBERSHIP, &req, sizeof(req));
  if (ret != 0)
    return errno;
  
  return 0;
}

jint cpnet_getAvailableBytes (JNIEnv *env UNUSED, jint fd, jint *availableBytes)
{
  int ret;

  ret = ioctl(fd, FIONREAD, availableBytes);
  if (ret != 0)
    return errno;

  return 0;
}

jint cpnet_getHostname (JNIEnv *env UNUSED, char *hostname, jint hostname_len)
{
  int ret;

  ret = gethostname(hostname, hostname_len);
  if (ret != 0)
    return errno;

  hostname[hostname_len-1] = 0;
  return 0;
}

jint cpnet_getHostByName (JNIEnv *env, const char *hostname, cpnet_address ***addresses, jint *addresses_count)
{
  struct hostent hret;
  struct hostent *result;
  jint buflen = 1024;
  int herr = 0;
  int ret = 0;
  int counter = 0;
  cpnet_address **addr_arr;
  int i;
  char *buf;

  do
    {
      buf = (char *)JCL_malloc(env, buflen);

#ifdef HAVE_GETHOSTBYNAME_R
# if defined(HAVE_FUNC_GETHOSTBYNAME_R_6)
      ret = gethostbyname_r (hostname, &hret, buf, buflen, &result, &herr);
# elif defined(HAVE_FUNC_GETHOSTBYNAME_R_5)
      result = gethostbyname_r(hostname, &hret, buf, buflen, &herr);
# elif defined(HAVE_FUNC_GETHOSTBYNAME_R_3)
#  error IMPLEMENT ME!
# else
#  error unknown number of arguments for gethostbyname_r
# endif
#else
      hret.h_addr_list = NULL;
      hret.h_addrtype = 0;

      result = gethostbyname (hostname);
      if (result == NULL)
        return -errno;
      memcpy (&hret, result, sizeof (struct hostent));
#endif
      if (ret != 0 || result == NULL)
	{
	  if (herr == ERANGE)
	    {
	      buflen *= 2;
	      JCL_free(env, buf);
	      continue;
	    }
	  JCL_free(env, buf);	  

	  return -herr;
	}

      break;
    }
  while (1);
  
  while (hret.h_addr_list[counter] != NULL)
    counter++;

  *addresses_count = counter;
  addr_arr = *addresses = JCL_malloc(env, sizeof(cpnet_address *) * counter);
  switch (hret.h_addrtype)
    {
    case AF_INET:
      for (i = 0; i < counter; i++)
	{
	  addr_arr[i] = cpnet_newIPV4Address(env);
	  cpnet_bytesToIPV4Address(addr_arr[i], (jbyte *)hret.h_addr_list[i]);
	}
      break;
#ifdef HAVE_INET6
    case AF_INET6:
      for (i = 0; i < counter; i++)
	{
	  addr_arr[i] = cpnet_newIPV6Address(env);
	  cpnet_bytesToIPV6Address(addr_arr[i], (jbyte *)hret.h_addr_list[i]);
	}
      break;
#endif
    default:
      *addresses_count = 0;
      JCL_free(env, addr_arr);
      break;
    }

  JCL_free(env, buf);

  return 0;
}

jint cpnet_getHostByAddr (JNIEnv *env UNUSED, cpnet_address *addr, char *hostname, jint hostname_len)
{
  union 
  {
    struct sockaddr_in *addr_v4;
    struct sockaddr_in6 *addr_v6;
    char *data;
  } haddr;
  void *raw_addr;
  int addr_type;
  struct hostent *ret;
  int addr_len;

  haddr.data = addr->data;
  
  if (haddr.addr_v4->sin_family == AF_INET)
    {
      raw_addr = &haddr.addr_v4->sin_addr;
      addr_len = sizeof(haddr.addr_v4->sin_addr);
      addr_type = AF_INET;
    }
#ifdef HAVE_INET6
  else if (haddr.addr_v6->sin6_family == AF_INET6)
    {
      raw_addr = &haddr.addr_v6->sin6_addr;
      addr_type = AF_INET6;
      addr_len = sizeof(haddr.addr_v6->sin6_addr);
    }
#endif
  else
    return EINVAL;

  /* Here we do not have any thread safe call. VM implementors will have to
   * do a big lock. Or it should be put on the Classpath VM interface.
   */
  ret = gethostbyaddr(raw_addr, addr_len, addr_type);
  if (ret == NULL)
    {
      /* The trouble here is how to distinguish the two cases ? */
      if (h_errno != 0)
	return h_errno;
      else
	return errno;

    }
  strncpy(hostname, ret->h_name, hostname_len);

  return 0;
}

jint cpnet_aton (JNIEnv *env, const char *hostname, cpnet_address **addr)
{
  jbyte *bytes = NULL;

#if defined(HAVE_INET_PTON) && defined(HAVE_INET6)
  jbyte inet6_addr[16];
#endif

#ifdef HAVE_INET_ATON
  struct in_addr laddr;
  if (inet_aton (hostname, &laddr))
    {
      bytes = (jbyte *) &laddr;
    }
#elif defined(HAVE_INET_ADDR)
#if ! HAVE_IN_ADDR_T
  typedef jint in_addr_t;
#endif
  in_addr_t laddr = inet_addr (hostname);
  if (laddr != (in_addr_t)(-1))
    {
      bytes = (jbyte *) &laddr;
    }
#endif
  if (bytes)
    {
      *addr = cpnet_newIPV4Address(env);
      cpnet_bytesToIPV4Address(*addr, bytes);
      return 0;
    }

#if defined(HAVE_INET_PTON) && defined(HAVE_INET6)
  if (inet_pton (AF_INET6, hostname, inet6_addr) > 0)
    {
      *addr = cpnet_newIPV6Address(env);
      cpnet_bytesToIPV6Address(*addr, inet6_addr);
      return 0;
    }
#endif

  *addr = NULL;
  return 0;
}

void cpnet_freeAddresses(JNIEnv * env, cpnet_address **addr, jint addresses_count)
{
  jint i;

  for (i = 0; i < addresses_count; i++)
    cpnet_freeAddress(env, addr[i]);
}
