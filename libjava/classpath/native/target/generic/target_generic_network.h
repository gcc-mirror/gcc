/* target_generic_network.h - Native methods for network operations.
   Copyright (C) 1998, 2004, 2005 Free Software Foundation, Inc.

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

/*
Description: generic target defintions of network functions
Systems    : all
*/

#ifndef __TARGET_GENERIC_NETWORK__
#define __TARGET_GENERIC_NETWORK__

/* check if target_native_network.h included */
#ifndef __TARGET_NATIVE_NETWORK__
  #error Do NOT INCLUDE generic target files! Include the corresponding native target files instead!
#endif

/****************************** Includes *******************************/
/* do not move; needed here because of some macro definitions */
#include "config.h"

#include <stdlib.h>

#include "target_native.h"

/****************** Conditional compilation switches *******************/

/***************************** Constants *******************************/

/***************************** Datatypes *******************************/

/***************************** Variables *******************************/

/****************************** Macros *********************************/

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_IPADDRESS_BYTES_TO_INT
* Purpose    : convert IP adddress (4 parts) into integer (host-format
*              32bit)
* Input      : n0,n1,n2,n3 - IP address parts
* Output     : i - integer with IP address in host-format
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_IPADDRESS_BYTES_TO_INT
  #define TARGET_NATIVE_NETWORK_IPADDRESS_BYTES_TO_INT(n0,n1,n2,n3,i) \
    do { \
      i=(((unsigned char)n0) << 24) | \
        (((unsigned char)n1) << 16) | \
        (((unsigned char)n2) <<  8) | \
        (((unsigned char)n3) <<  0); \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_INT_TO_IPADDRESS_BYTES
* Purpose    : convert IP adddress (4 parts) into integer (host-format
*              32bit)
* Input      : n0,n1,n2,n3 - IP address parts
* Output     : i - integer with IP address in host-format
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_INT_TO_IPADDRESS_BYTES
  #define TARGET_NATIVE_NETWORK_INT_TO_IPADDRESS_BYTES(i,n0,n1,n2,n3) \
    do { \
      n0=(i & 0xFF000000) >> 24; \
      n1=(i & 0x00FF0000) >> 16; \
      n2=(i & 0x0000FF00) >>  8; \
      n3=(i & 0x000000FF) >>  0; \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_GET_HOSTNAME
* Purpose    : get hostname
* Input      : maxNameLen - max. length of name
* Output     : name   - name (NUL terminated)
*              result - TARGET_NATIVE_OK if no error occurred,
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_GET_HOSTNAME
  #include <unistd.h>
  #define TARGET_NATIVE_NETWORK_GET_HOSTNAME(name,maxNameLen,result) \
    do { \
      result=(gethostname(name,maxNameLen-1)==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
      name[maxNameLen-1]='\0'; \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_GET_HOSTNAME_BY_ADDRESS
* Purpose    : get hostname by address
* Input      : address    - IP address (32bit, NOT network byte order!)
*              maxNameLen - max. length of name
* Output     : name   - name (NUL terminated)
*              result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

/* XXX NYI??? reentrant? */
#ifndef TARGET_NATIVE_NETWORK_GET_HOSTNAME_BY_ADDRESS
  #include <netdb.h>
  #define TARGET_NATIVE_NETWORK_GET_HOSTNAME_BY_ADDRESS(address,name,maxNameLen,result) \
    do { \
      int            __networkAddress; \
      struct hostent *__hostEntry; \
      \
      __networkAddress=htonl(address); \
      __hostEntry = gethostbyaddr((char*)&__networkAddress,sizeof(__networkAddress),AF_INET); \
      if (__hostEntry!=NULL) \
      { \
        strncpy(name,__hostEntry->h_name,maxNameLen-1); \
        name[maxNameLen]='\0'; \
        result=TARGET_NATIVE_OK; \
      } \
      else \
      { \
        result=TARGET_NATIVE_ERROR; \
      } \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_GET_HOSTNAME_BY_NAME
* Purpose    : get hostname by name
* Input      : name           - hostname
*              maxAddressSize - max. size of address array
* Output     : addresses     - host adddresses (array, NOT in network
*                              byte order!)
*              addressCount  - number of entries in address array
*              result        - TARGET_NATIVE_OK if no error occurred, 
*                              TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

/* XXX NYI??? reentrant? */
#ifndef TARGET_NATIVE_NETWORK_GET_HOSTNAME_BY_NAME
  #include <netdb.h>
  #define TARGET_NATIVE_NETWORK_GET_HOSTNAME_BY_NAME(name,addresses,maxAddressSize,addressCount,result) \
    do { \
      struct hostent *__hostEntry; \
      \
      addressCount=0; \
      \
      __hostEntry = gethostbyname(name); \
      if (__hostEntry!=NULL) \
      { \
        while ((addressCount<maxAddressSize) && (__hostEntry->h_addr_list[addressCount]!=NULL)) \
        { \
          addresses[addressCount]=ntohl(*(int*)(__hostEntry->h_addr_list[addressCount])); \
          addressCount++; \
        } \
        result=TARGET_NATIVE_OK; \
      } \
      else \
      { \
        result=TARGET_NATIVE_ERROR; \
      } \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_OPEN_STREAM
* Purpose    : open stream socket
* Input      : -
* Output     : socketDescriptor - socket descriptor
*              result           - TARGET_NATIVE_OK if no error occurred, 
*                                 TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_OPEN_STREAM
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <fcntl.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_OPEN_STREAM(socketDescriptor,result) \
    do { \
      socketDescriptor=socket(AF_INET,SOCK_STREAM,0); \
      fcntl(socketDescriptor,F_SETFD,FD_CLOEXEC); \
      result=(socketDescriptor!=-1)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_OPEN_DATAGRAM
* Purpose    : open datagram socket
* Input      : -
* Output     : socketDescriptor - socket descriptor
*              result           - TARGET_NATIVE_OK if no error occurred, 
*                                 TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_OPEN_DATAGRAM
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <fcntl.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_OPEN_DATAGRAM(socketDescriptor,result) \
    do { \
      socketDescriptor=socket(AF_INET,SOCK_DGRAM,0); \
      fcntl(socketDescriptor,F_SETFD,FD_CLOEXEC); \
      result=(socketDescriptor!=-1)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_CLOSE
* Purpose    : close socket
* Input      : socketDescriptor - socket descriptor
* Output     : result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_CLOSE
  #include <unistd.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_CLOSE(socketDescriptor,result) \
    do { \
      result=(close(socketDescriptor)==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_CONNECT
* Purpose    : connect socket
* Input      : socketDescriptor - socket descriptor
*              address          - address (network format???)
*              port             - port number (NOT in network byte order!)
* Output     : result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_CONNECT
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_CONNECT(socketDescriptor,address,port,result) \
    do { \
      struct sockaddr_in __socketAddress; \
      \
      memset(&__socketAddress,0,sizeof(__socketAddress)); \
      __socketAddress.sin_family      = AF_INET; \
      __socketAddress.sin_addr.s_addr = htonl(address); \
      __socketAddress.sin_port        = htons(((short)port)); \
      \
      result=(connect(socketDescriptor,(struct sockaddr*)&__socketAddress,sizeof(__socketAddress))==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_BIND
* Purpose    : bind socket
* Input      : socketDescriptor - socket descriptor
*              address          - address (NOT ??? in network byte order!)
*              port             - port (NOT in network byte order!)
* Output     : result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

/* XXX ??? address in network byte order? */
#ifndef TARGET_NATIVE_NETWORK_SOCKET_BIND
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_BIND(socketDescriptor,address,port,result) \
    do { \
      struct sockaddr_in __socketAddress; \
      \
      memset(&__socketAddress,0,sizeof(__socketAddress)); \
      __socketAddress.sin_family      = AF_INET; \
      __socketAddress.sin_addr.s_addr = htonl(address); \
      __socketAddress.sin_port        = htons(((short)port)); \
      \
      result=(bind(socketDescriptor,(struct sockaddr*)&__socketAddress,sizeof(__socketAddress))==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_LISTEN
* Purpose    : listen socket
* Input      : socketDescriptor - socket descriptor
*              maxQueueLength   - max. number of pending connections
* Output     : result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

/* XXX ??? address in network byte order? */
#ifndef TARGET_NATIVE_NETWORK_SOCKET_LISTEN
  #include <sys/socket.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_LISTEN(socketDescriptor,maxQueueLength,result) \
    do { \
      result=(listen(socketDescriptor,maxQueueLength)==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_ACCEPT
* Purpose    : accept socket
* Input      : socketDescriptor - socket descriptor
* Output     : result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

/* XXX ??? address in network byte order? */
#ifndef TARGET_NATIVE_NETWORK_SOCKET_ACCEPT
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_ACCEPT(socketDescriptor,newSocketDescriptor,result) \
    do { \
      struct sockaddr_in __socketAddress; \
      socklen_t          __socketAddressLength; \
      \
      memset(&__socketAddress,0,sizeof(__socketAddress)); \
      __socketAddressLength=sizeof(__socketAddress); \
      newSocketDescriptor=accept(socketDescriptor,(struct sockaddr*)&__socketAddress,&__socketAddressLength); \
      result=(newSocketDescriptor!=-1)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_GET_LOCAL_INFO
* Purpose    : get local socket data info
* Input      : socketDescriptor - socket descriptor
* Output     : localAddress     - local address (NOT in network byte order!)
*              localPort        - local port number (NOT in network byte order!)
*              result           - TARGET_NATIVE_OK if no error occurred, 
*                                 TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_GET_LOCAL_INFO
  #include <sys/socket.h>
  #include <netinet/in.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_GET_LOCAL_INFO(socketDescriptor,localAddress,localPort,result) \
    do { \
      struct sockaddr_in __socketAddress; \
      socklen_t          __socketAddressLength; \
      \
      localAddress=0; \
      localPort   =0; \
      \
      __socketAddressLength=sizeof(__socketAddress); \
      result=(getsockname(socketDescriptor,(struct sockaddr*)&__socketAddress,&__socketAddressLength)==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
      if (result==TARGET_NATIVE_OK) \
      { \
        localAddress=ntohl(__socketAddress.sin_addr.s_addr); \
        localPort   =ntohs(__socketAddress.sin_port); \
      } \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_GET_REMOTE_INFO
* Purpose    : get remote socket data info
* Input      : socketDescriptor - socket descriptor
* Output     : remoteAddress    - remote address (NOT in network byte order!)
*              remotePort       - remote port number (NOT in network byte order!)
*            : result           - TARGET_NATIVE_OK if no error occurred, 
*                                 TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_GET_REMOTE_INFO
  #include <sys/socket.h>
  #include <netinet/in.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_GET_REMOTE_INFO(socketDescriptor,remoteAddress,remotePort,result) \
    do { \
      struct sockaddr_in __socketAddress; \
      socklen_t          __socketAddressLength; \
      \
      remoteAddress=0; \
      remotePort   =0; \
      \
      __socketAddressLength=sizeof(__socketAddress); \
      result=(getpeername(socketDescriptor,(struct sockaddr*)&__socketAddress,&__socketAddressLength)==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
      if (result==TARGET_NATIVE_OK) \
      { \
        remoteAddress=ntohl(__socketAddress.sin_addr.s_addr); \
        remotePort   =ntohs(__socketAddress.sin_port); \
      } \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_RECEIVE_AVAILABLE
* Purpose    : get number of available bytes for receive
* Input      : socketDescriptor - socket descriptor
* Output     : bytesAvailable - available bytes for receive
*            : result         - TARGET_NATIVE_OK if no error occurred, 
*                               TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_RECEIVE_AVAILABLE
  #include <sys/ioctl.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_RECEIVE_AVAILABLE(socketDescriptor,bytesAvailable,result) \
    do { \
      int __value; \
      \
      bytesAvailable=0; \
      \
      result=(ioctl(socketDescriptor,FIONREAD,&__value)==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
      if (result==TARGET_NATIVE_OK) \
      { \
        bytesAvailable=__value; \
      } \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_RECEIVE
* Purpose    : receive data from socket
* Input      : socketDescriptor - socket descriptor
*              maxLength - max. size of bfufer
* Output     : buffer       - received data
*              bytesReceive - length of received data
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_RECEIVE
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_RECEIVE(socketDescriptor,buffer,maxLength,bytesReceived) \
    do { \
      struct sockaddr_in __socketAddress; \
      socklen_t          __socketAddressLength; \
      \
      memset(&__socketAddress,0,sizeof(__socketAddress)); \
      __socketAddressLength=sizeof(__socketAddress); \
      bytesReceived=recv(socketDescriptor,buffer,maxLength,0); \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_RECEIVE_WITH_ADDRESS_PORT
* Purpose    : receive data from socket
* Input      : socketDescriptor - socket descriptor
*              maxLength - max. size of bfufer
* Output     : buffer       - received data
*              address      - from address (NOT in network byte order!)
*              port         - from port (NOT in network byte order!)
*              bytesReceive - length of received data
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_RECEIVE_WITH_ADDRESS_PORT
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_RECEIVE_WITH_ADDRESS_PORT(socketDescriptor,buffer,maxLength,address,port,bytesReceived) \
    do { \
      struct sockaddr_in __socketAddress; \
      socklen_t          __socketAddressLength; \
      \
      port=0; \
      \
      memset(&__socketAddress,0,sizeof(__socketAddress)); \
      __socketAddressLength=sizeof(__socketAddress); \
      bytesReceived=recvfrom(socketDescriptor,buffer,maxLength,0,(struct sockaddr*)&__socketAddress,&__socketAddressLength); \
      if (__socketAddressLength==sizeof(__socketAddress)) \
      { \
        address=ntohl(__socketAddress.sin_addr.s_addr); \
        port   =ntohs(__socketAddress.sin_port); \
      } \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_SEND
* Purpose    : send data to socket
* Input      : socketDescriptor - socket descriptor
*            : buffer  - data to send
*              length  - length of data to send
* Output     : bytesSent - number of bytes sent, -1 otherwise
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_SEND
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_SEND(socketDescriptor,buffer,length,bytesSent) \
    do { \
      bytesSent=send(socketDescriptor,buffer,length,0); \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_SEND_WITH_ADDRESS_PORT
* Purpose    : send data to socket
* Input      : socketDescriptor - socket descriptor
*            : buffer  - data to send
*              length  - length of data to send
*              Address - to address (NOT in network byte order!)
*              Port    - to port (NOT in network byte order!)
* Output     : bytesSent - number of bytes sent, -1 otherwise
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_SEND_WITH_ADDRESS_PORT
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_SEND_WITH_ADDRESS_PORT(socketDescriptor,buffer,length,address,port,bytesSent) \
    do { \
      struct sockaddr_in __socketAddress; \
      \
      memset(&__socketAddress,0,sizeof(__socketAddress)); \
      __socketAddress.sin_family      = AF_INET; \
      __socketAddress.sin_addr.s_addr = htonl(address); \
      __socketAddress.sin_port        = htons((short)port); \
      bytesSent=sendto(socketDescriptor,buffer,length,0,(struct sockaddr*)&__socketAddress,sizeof(__socketAddress)); \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_TCP_NODELAY
* Purpose    : set socket option TCP_NODELAY
* Input      : socketDescriptor - socket descriptor
*              flag             - 1 or 0
* Output     : result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_TCP_NODELAY
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/tcp.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_TCP_NODELAY(socketDescriptor,flag,result) \
    do { \
      int __value; \
      \
      __value=flag; \
      result=(setsockopt(socketDescriptor,IPPROTO_TCP,TCP_NODELAY,&__value,sizeof(__value))==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_SO_LINGER
* Purpose    : set socket option SO_LINGER
* Input      : socketDescriptor - socket descriptor
*              flag             - 1 or 0
*              value            - linger value
* Output     : result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_SO_LINGER
  #include <sys/types.h>
  #include <sys/socket.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_SO_LINGER(socketDescriptor,flag,value,result) \
    do { \
      struct linger __linger; \
      \
      memset(&__linger,0,sizeof(__linger)); \
      if (flag) \
      { \
        __linger.l_onoff=0; \
      } \
      else \
      { \
        __linger.l_linger=value; \
        __linger.l_onoff =1; \
      } \
      result=(setsockopt(socketDescriptor,SOL_SOCKET,SO_LINGER,&__linger,sizeof(__linger))==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_SO_TIMEOUT
* Purpose    : set socket option SO_TIMEOUT
* Input      : socketDescriptor - socket descriptor
*              flag             - 1 or 0
* Output     : result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_SO_TIMEOUT
  #include <sys/types.h>
  #include <sys/socket.h>
#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif
  #define TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_SO_TIMEOUT(socketDescriptor,flag,result) \
    do { \
      struct timeval __value; \
      \
      __value.tv_sec = flag / 1000; \
      __value.tv_usec = (flag % 1000) * 1000; \
      result=(setsockopt(socketDescriptor,SOL_SOCKET,SO_TIMEOUT,&__value,sizeof(__value))==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_SO_SNDBUF
* Purpose    : set socket option SO_SNDBUF
* Input      : socketDescriptor - socket descriptor
*              size             - size of send buffer
* Output     : result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_SO_SNDBUF
  #include <sys/types.h>
  #include <sys/socket.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_SO_SNDBUF(socketDescriptor,size,result) \
    do { \
      int __value; \
      \
      __value=size; \
      result=(setsockopt(socketDescriptor,SOL_SOCKET,SO_SNDBUF,&__value,sizeof(__value))==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_SO_RCDBUF
* Purpose    : set socket option SO_RCDBUF
* Input      : socketDescriptor - socket descriptor
*              size             - size of receive buffer
* Output     : result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_SO_RCDBUF
  #include <sys/types.h>
  #include <sys/socket.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_SO_RCDBUF(socketDescriptor,size,result) \
    do { \
      int __value; \
      \
      __value=size; \
      result=(setsockopt(socketDescriptor,SOL_SOCKET,SO_RCVBUF,&__value,sizeof(__value))==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_IP_TTL
* Purpose    : set socket option IP_TTL
* Input      : socketDescriptor - socket descriptor
*              value            - value
* Output     : result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_IP_TTL
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_IP_TTL(socketDescriptor,value,result) \
    do { \
      int __value; \
      \
      __value=value; \
      result=(setsockopt(socketDescriptor,IPPROTO_IP,IP_TTL,&__value,sizeof(__value))==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_IP_MULTICAST_IF
* Purpose    : set socket option IP_MULTICAST_IF
* Input      : socketDescriptor - socket descriptor
*              address          - integer with IP address in host-format
* Output     : result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_IP_MULTICAST_IF
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_IP_MULTICAST_IF(socketDescriptor,address,result) \
    do { \
      struct sockaddr_in __socketAddress; \
      \
      memset(&__socketAddress,0,sizeof(__socketAddress)); \
      __socketAddress.sin_family      = AF_INET; \
      __socketAddress.sin_addr.s_addr = htonl(address); \
      result=(setsockopt(socketDescriptor,IPPROTO_IP,IP_MULTICAST_IF,&__socketAddress,sizeof(__socketAddress))==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_REUSE_ADDRESS
* Purpose    : set socket option REUSE_ADDRESS
* Input      : socketDescriptor - socket descriptor
*              flag             - 1 or 0
* Output     : result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_REUSE_ADDRESS
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_REUSE_ADDRESS(socketDescriptor,flag,result) \
    do { \
      int __value; \
      \
      __value=flag; \
      result=(setsockopt(socketDescriptor,SOL_SOCKET,SO_REUSEADDR,&__value,sizeof(__value))==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_ADD_MEMBERSHIP
* Purpose    : set socket option IP_ADD_MEMBERSHIP
* Input      : socketDescriptor - socket descriptor
*              address          - network address (host-format)
* Output     : result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_ADD_MEMBERSHIP
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_ADD_MEMBERSHIP(socketDescriptor,address,result) \
    do { \
      struct ip_mreq __request; \
      \
      memset(&__request,0,sizeof(__request)); \
      __request.imr_multiaddr.s_addr=htonl(address); \
      __request.imr_interface.s_addr=INADDR_ANY; \
      result=(setsockopt(socketDescriptor,IPPROTO_IP,IP_ADD_MEMBERSHIP,&__request,sizeof(__request))==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_DROP_MEMBERSHIP
* Purpose    : set socket option IP_DROP_MEMBERSHIP
* Input      : socketDescriptor - socket descriptor
*              address          - network address (host-format)
* Output     : result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_DROP_MEMBERSHIP
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_DROP_MEMBERSHIP(socketDescriptor,address,result) \
    do { \
      struct ip_mreq __request; \
      \
      memset(&__request,0,sizeof(__request)); \
      __request.imr_multiaddr.s_addr=htonl(address); \
      __request.imr_interface.s_addr=INADDR_ANY; \
      result=(setsockopt(socketDescriptor,IPPROTO_IP,IP_DROP_MEMBERSHIP,&__request,sizeof(__request))==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_KEEP_ALIVE
* Purpose    : set socket option KEEP_ALIVE
* Input      : socketDescriptor - socket descriptor
*              flag             - 1 or 0
* Output     : result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_KEEP_ALIVE
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_KEEP_ALIVE(socketDescriptor,flag,result) \
    do { \
      int __value; \
      \
      __value=flag; \
      result=(setsockopt(socketDescriptor,SOL_SOCKET,SO_KEEPALIVE,&__value,sizeof(__value))==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_BROADCAST
* Purpose    : set socket option SO_BROADCAST
* Input      : socketDescriptor - socket descriptor
*              flag             - 1 or 0
* Output     : result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_BROADCAST 
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/tcp.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_BROADCAST(socketDescriptor,flag,result) \
    do { \
      int __value; \
      \
      __value=flag; \
      result=(setsockopt(socketDescriptor,SOL_SOCKET,SO_BROADCAST,&__value,sizeof(__value))==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
    } while (0)
#endif

/*---------------------------------------------------------------------*/

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_TCP_NODELAY
* Purpose    : get socket option TCP_NODELAY
* Input      : socketDescriptor - socket descriptor
* Output     : flag   - 1 or 0
*              result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_TCP_NODELAY
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/tcp.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_TCP_NODELAY(socketDescriptor,flag,result) \
    do { \
      int       __value; \
      socklen_t __len; \
      \
      flag=0; \
      \
      __len=sizeof(__value); \
      result=(getsockopt(socketDescriptor,IPPROTO_TCP,TCP_NODELAY,&__value,&__len)==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
      if (result==TARGET_NATIVE_OK) \
      { \
        flag=__value; \
      } \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_SO_LINGER
* Purpose    : get socket option SO_LINGER
* Input      : socketDescriptor - socket descriptor
* Output     : flag   - 1 or 0
*              value  - linger value
*              result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_SO_LINGER
  #include <sys/types.h>
  #include <sys/socket.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_SO_LINGER(socketDescriptor,flag,value,result) \
    do { \
      struct linger __linger; \
      socklen_t     __len; \
      \
      flag =0; \
      value=0; \
      \
      __len=sizeof(__linger); \
      result=(getsockopt(socketDescriptor,SOL_SOCKET,SO_LINGER,&__linger,&__len)==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
      if (result==TARGET_NATIVE_OK) \
      { \
        flag =__linger.l_onoff; \
        value=__linger.l_linger; \
      } \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_SO_TIMEOUT
* Purpose    : get socket option SO_TIMEOUT
* Input      : socketDescriptor - socket descriptor
* Output     : flag   - 1 or 0
*              result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_SO_TIMEOUT
  #include <sys/types.h>
  #include <sys/socket.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_SO_TIMEOUT(socketDescriptor,flag,result) \
    do { \
      struct timeval   __value; \
      socklen_t __len; \
      \
      flag=0; \
      \
      __len=sizeof(__value); \
      result=(getsockopt(socketDescriptor,SOL_SOCKET,SO_TIMEOUT,&__value,&__len)==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
      if (result==TARGET_NATIVE_OK) \
      { \
        flag = (__value.tv_sec * 1000LL) + (__value.tv_usec / 1000LL); \
      } \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_SO_SNDBUF
* Purpose    : get socket option SO_SNDBUF
* Input      : socketDescriptor - socket descriptor
* Output     : size   - size of send buffer
*              result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_SO_SNDBUF
  #include <sys/types.h>
  #include <sys/socket.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_SO_SNDBUF(socketDescriptor,size,result) \
    do { \
      int       __value; \
      socklen_t __len; \
      \
      size=0; \
      \
      __len=sizeof(__value); \
      result=(getsockopt(socketDescriptor,SOL_SOCKET,SO_SNDBUF,&__value,&__len)==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
      if (result==TARGET_NATIVE_OK) \
      { \
        size=__value; \
      } \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_SO_RCDBUF
* Purpose    : get socket option SO_RCDBUF
* Input      : socketDescriptor - socket descriptor
* Output     : size   - size of receive buffer
*              result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_SO_RCDBUF
  #include <sys/types.h>
  #include <sys/socket.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_SO_RCDBUF(socketDescriptor,size,result) \
    do { \
      int       __value; \
      socklen_t __len; \
      \
      size=0; \
      \
      __len=sizeof(__value); \
      result=(getsockopt(socketDescriptor,SOL_SOCKET,SO_RCVBUF,&__value,&__len)==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
      if (result==TARGET_NATIVE_OK) \
      { \
        size=__value; \
      } \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_IP_TTL
* Purpose    : get socket option IP_TTL
* Input      : socketDescriptor - socket descriptor
* Output     : flag   - 1 or 0
*              result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_IP_TTL
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_IP_TTL(socketDescriptor,flag,result) \
    do { \
      int       __value; \
      socklen_t __len; \
      \
      flag=0; \
      \
      __len=sizeof(__value); \
      result=(getsockopt(socketDescriptor,IPPROTO_IP,IP_TTL,&__value,&__len)==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
      if (result==TARGET_NATIVE_OK) \
      { \
        flag=__value; \
      } \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_IP_MULTICAST_IF
* Purpose    : get socket option IP_MULTICAST_IF
* Input      : socketDescriptor - socket descriptor
* Output     : address - integer with IP address in host-format
*              result  - TARGET_NATIVE_OK if no error occurred, 
*                        TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_IP_MULTICAST_IF
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_IP_MULTICAST_IF(socketDescriptor,address,result) \
    do { \
      struct sockaddr_in __socketAddress; \
      socklen_t          __socketAddressLength; \
      \
      address=0;\
      \
      memset(&__socketAddress,0,sizeof(__socketAddress)); \
      __socketAddress.sin_family      = AF_INET; \
      __socketAddress.sin_addr.s_addr = htonl(address); \
      __socketAddressLength=sizeof(__socketAddress); \
      result=(getsockopt(socketDescriptor,IPPROTO_IP,IP_MULTICAST_IF,&__socketAddress,&__socketAddressLength)==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
      if (result==TARGET_NATIVE_OK) \
      { \
        address=ntohl(__socketAddress.sin_addr.s_addr); \
      } \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_BIND_ADDRESS
* Purpose    : get socket option SOCKOPT_SO_BINDADDR
* Input      : socketDescriptor - socket descriptor
* Output     : address - integer with IP address in host-format
*              result  - TARGET_NATIVE_OK if no error occurred, 
*                        TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_BIND_ADDRESS
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_BIND_ADDRESS(socketDescriptor,address,result) \
    do { \
      struct sockaddr_in __socketAddress; \
      socklen_t          __socketAddressLength; \
      \
      address=0;\
      \
      memset(&__socketAddress,0,sizeof(__socketAddress)); \
      __socketAddressLength=sizeof(__socketAddress); \
      result=(getsockname(socketDescriptor,(struct sockaddr*)&__socketAddress,&__socketAddressLength)==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
      if (result==TARGET_NATIVE_OK) \
      { \
        address=ntohl(__socketAddress.sin_addr.s_addr); \
      } \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_REUSE_ADDRESS
* Purpose    : get socket option REUSE_ADDRESS
* Input      : socketDescriptor - socket descriptor
* Output     : flag   - 1 or 0
*              result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_REUSE_ADDRESS
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_REUSE_ADDRESS(socketDescriptor,flag,result) \
    do { \
      int       __value; \
      socklen_t __len; \
      \
      flag=0; \
      \
      __len=sizeof(__value); \
      result=(getsockopt(socketDescriptor,SOL_SOCKET,SO_REUSEADDR,&__value,&__len)==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
      if (result==TARGET_NATIVE_OK) \
      { \
        flag=__value; \
      } \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_KEEP_ALIVE
* Purpose    : get socket option KEEP_ALIVE
* Input      : socketDescriptor - socket descriptor
* Output     : flag   - 1 or 0
*              result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_KEEP_ALIVE
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_KEEP_ALIVE(socketDescriptor,flag,result) \
    do { \
      int       __value; \
      socklen_t __len; \
      \
      flag=0; \
      \
      __len=sizeof(__value); \
      result=(getsockopt(socketDescriptor,SOL_SOCKET,SO_KEEPALIVE,&__value,&__len)==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
      if (result==TARGET_NATIVE_OK) \
      { \
        flag=__value; \
      } \
    } while (0)
#endif

/***********************************************************************\
* Name       : TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_BROADCAST
* Purpose    : get socket option SO_BROADCAST
* Input      : socketDescriptor - socket descriptor
* Output     : flag   - 1 or 0
*              result - TARGET_NATIVE_OK if no error occurred, 
*                       TARGET_NATIVE_ERROR otherwise
* Return     : -
* Side-effect: unknown
* Notes      : -
\***********************************************************************/

#ifndef TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_BROADCAST 
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/tcp.h>
  #define TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_BROADCAST(socketDescriptor,flag,result) \
    do { \
      int __value; \
      socklen_t __len; \
      \
      flag=0; \
      \
      __len=sizeof(__value); \
      result=(getsockopt(socketDescriptor,SOL_SOCKET,SO_BROADCAST,&__value,&__len)==0)?TARGET_NATIVE_OK:TARGET_NATIVE_ERROR; \
      if (result==TARGET_NATIVE_OK) \
      { \
        flag=__value; \
      } \
    } while (0)
#endif

/***************************** Functions *******************************/

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __cplusplus
}
#endif

#endif /* __TARGET_GENERIC_NETWORK__ */

/* end of file */

