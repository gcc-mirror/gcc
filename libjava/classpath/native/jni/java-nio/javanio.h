/* javanio.h -- reference implementation of native functions.
   Copyright (C) 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


#ifndef __JAVANIO_H__
#define __JAVANIO_H__

#include <sys/time.h>

/**
 * This header defines functions that are called by our JNI reference
 * implementation of java.nio.*. In our reference implementation, these
 * functions map exactly to their counterparts in POSIX; in implementations
 * that can't use these functions directly (such as systems that use user-land
 * threads, and thus can't call blocking system calls directly) can provide
 * their own implementations suitable for their system.
 */

/**
 * This macro is used in all function prototypes below; if any additional
 * keywords need to be added to a prototype, declare them in this macro.
 */
#define CPNIO_EXPORT static inline

/**
 * Read bytes from the given file descriptor into the given memory address, which
 * has sufficient space for NBYTES bytes.
 *
 * \param fd     The file descriptor to read from.
 * \param buf    The memory address to read bytes into.
 * \param nbytes The number of bytes available to store in BUF.
 * \return The number of bytes read, possibly zero, on success; return -1 on failure,
 *  and set ERRNO to an appropriate value.
 * \see read(2)
 *
 * Allowed errno values:
 *   [EBADF]   If FD is not a valid file descriptor, or is not open for reading.
 *   [EFAULT]  If BUF points outside the process's address space.
 *   [EIO]     An I/O error occurrs.
 *   [EINTR]   If the read is interrupted by a signal.
 *   [EINVAL]  If FD is negative.
 *   [EAGAIN]  If FD was marked for non-blocking I/O, and no data were ready to
 *             be read.
 */
CPNIO_EXPORT ssize_t cpnio_read (int fd, void *buf, size_t nbytes);

/*
 * Read bytes from a file descriptor into a sequence of IO buffers.
 *
 * The iovec structure is defined as:
 *
 *   struct iovec {
 *     char   *iov_base;
 *     size_t  iov_len;
 *   };
 *
 * The call to _cp_readv should do a scattering read, where for each struct iovec
 * in the supplied list, up to IOV_LEN bytes are read into IOV_BASE. The function
 * returns the total number of bytes read into all supplied buffers.
 *
 * \param fd     The file descriptor.
 * \param iov    A pointer to the head of a list of iovec structures.
 * \param iovcnt The number of iovec structures pointed to by IOV.
 * \return The total number of bytes read accross all buffers, possibly zero. On
 *  error, -1 is returned and ERRNO is set.
 * \see readv(2)
 *
 * Allowed ERRNO values include all of those listed for _cp_read, as well as the
 * following:
 *   [EINVAL] If IOVCNT overflows the maximum number of iovec structures
 *            this platform supports (usually 16), if any IOV_LEN value
 *            is negative, or if the sum of all IOV_LEN values is too
 *            large to be stored in a ssize_t (usually a 32-bit integer).
 *   [EFAULT] If part of IOV points outside the process's address space.
 */
CPNIO_EXPORT ssize_t cpnio_readv (int fd, const struct iovec *iov, int iovcnt);

/*
 * Write NBYTES bytes from BUF to the file descriptor FD, returning the number
 * of bytes successfully written.
 *
 * \param fd     The file descriptor.
 * \param buf    A pointer to the bytes to write.
 * \param nbytes The maximum number of bytes to write.
 * \return The number of bytes written to the file descriptor, possibly zero. -1
 *  is returned if an error is encountered, and ERRNO will be set.
 * \see write(2)
 *
 * Allowed ERRNO values:
 *   [EBADF]   If FD is not a valid file descriptor or is not open for writing.
 *   [EPIPE]   If FD is a pipe, when the other side is disconnected; if FD is a
 *             socket, when the peer is not connected.
 *   [EFBIG]   When FD is a file, and writing to it overflows the process's
 *             or the system's maximim file size.
 *   [EFAULT]  If the buffer to write points outside the process's address
 *             space.
 *   [EINVAL]  If the descriptor FD is negative.
 *   [ENOSPC]  If FD is a file, and there is insufficient space on the
 *             filesystem.
 *   [EDQUOT]  If FD is a file, and the user's disk quota has been exceeded.
 *   [EIO]     If an I/O error occurs.
 *   [EINTR]   If the call is interrupted by a signal.
 *   [EAGAIN]  If FD is in non-blocking mode, and no bytes could be immediately
 *             written.
 */
CPNIO_EXPORT ssize_t cpnio_write (int fd, const void *buf, size_t nbytes);

/*
 * Write data from a sequence of IOVCNT buffers IOV to a file descriptor FD.
 *
 * \param fd     The file descriptor.
 * \param iov    The list of buffers to write.
 * \param iovcnt The number of iovec structures pointed to by IOV.
 * \return The total number of bytes written from the given buffers, possibly
 *   zero. -1 if an error occurs, and ERRNO will be set.
 * \see writev(2)
 *
 * Allowed ERRNO values include those mentioned in _cp_write, as well as:
 *  [EDESTADDRREQ]  If the descriptor is a datagram socket, and the peer is
 *                  no longer available.
 *  [EINVAL]        If IOVCNT is out of range, if any IOV_LEN value is
 *                  negative, or if the sum of all IOVCNT IOV_LEN values
 *                  will overflow a ssize_t.
 *  [ENOBUFS]       If the mbuf pool is exhausted (???).
 */
CPNIO_EXPORT ssize_t cpnio_writev (int fd, const struct iovec *iov, size_t iovcnt);

/**
 * Open a new, unbound and unconnected socket.
 *
 * \param domain The socket domain. Implementations need only handle AF_INET.
 * \param type   The socket type; implementations need only handle types
 *  SOCK_STREAM (for streaming sockets) and SOCK_DGRAM (for datagram sockets).
 * \param protocol This should always be 0. It can be ignored.
 * \return A new file descriptor pointing to a newly created socket, or -1 on
 *  error, and ERRNO set.
 *
 * Allowed ERRNO values:
 *  [EPROTONOSUPPORT]  If TYPE is unrecognized.
 *  [EMFILE]           If a new file descriptor cannot be allocated, because
 *                     the process's descriptor table is full.
 *  [ENFILE]           Likewise, but when the system table is full.
 *  [EACCES]           If this operation is not allowed.
 *  [ENOBUFS]          If there is not enough buffer space available for the
 *                     new socket.
 */
CPNIO_EXPORT int cpnio_socket (int domain, int type, int protocol);

/**
 * Connect a socket to a remote address.
 *
 * \param fd      The file descriptor of the socket to connect.
 * \param addr    The address to connect to. In practice, this should be
 *                either a `struct sockaddr_in' or a `struct sockaddr_in6'.
 * \param addrlen The size of the address structure passed by ADDR.
 * \return Zero if the connect succeeds. -1 on error, and ERRNO should be set.
 *
 * Allowed ERRNO values:
 *  [EBADF]         If FD is not a valid file descriptor.
 *  [ENOTSOCK]      If FD is not a socket descriptor.
 *  [EADDRNOTAVAIL] If ADDR is not available for use to this process.
 *  [EAFNOSUPPORT]  If the address family of ADDR is not supported.
 *  [EISCONN]       If the socket is already connected.
 *  [ETIMEDOUT]     If the connection could not be made in a reasonable
 *                  amount of time.
 *  [ECONNREFUSED]  If the connection attempt was rejected.
 *  [ENETUNREACH]   If the network ADDR is on is unreachable.
 *  [EADDRINUSE]    If the address is already in use.
 *  [EFAULT]        If ADDR points outside the addressable space.
 *  [EINPROGRESS]   If FD is in non-blocking mode, and the connection could
 *                  not be completed immediately.
 *  [EALREADY]      If FD is in non-blocking mode, and a connection attempt
 *                  is still pending.
 *  [EACCESS]       If ADDR is the broadcast address, and the socket option
 *                  SO_BROADCAST is not set.
 */
CPNIO_EXPORT int cpnio_connect (int fd, const struct sockaddr *addr, socklen_t addrlen);

/**
 * Accept an incoming connection on a socket, returning a new socket for
 * the connection, and storing the peer address in ADDR.
 *
 * \param fd      The socket file descriptor.
 * \param addr    The structure to store the peer address in.
 * \param addrlen The size of the data available in ADDR; upon return, the
 *                number of bytes stored in ADDR will be placed here.
 * \return The new socket file descriptor, or -1 on error, and ERRNO set.
 *
 * Allowed ERRNO values:
 *  [EBADF]         If FD is not a valid file descriptor.
 *  [ENOTSOCK]      If FD in not a socket descriptor.
 *  [EOPNOTSUPP]    If the socket is not a SOCK_STREAM socket.
 *  [EFAULT]        If ADDR points outside the process's addressable space.
 *  [EWOULDBLOCK]   If the socket is in non-blocking mode, and no connection
 *                  attempt is currently ready.
 *  [EMFILE]        If the process's descriptor table is full.
 *  [ENFILE]        If the system's descriptor table is full.
 */
CPNIO_EXPORT int cpnio_accept (int fd, struct sockaddr *addr, socklen_t *addrlen);

/**
 * Send a datagram to the given address.
 *
 * \param fd    The socket file descriptor.
 * \param msg   A pointer to the message to send.
 * \param len   The size of the message to send.
 * \param flags Flags for sending.
 * \param to    The remote address to send the message to.
 * \param tolen The size of the TO address structure.
 * \return The number of bytes written, possibly zero, on success. Returns
 *  -1 on failure, and sets ERRNO.
 * \see sendto(2)
 *
 * Allowed ERRNO values:
 *  [EBADF]
 *  [ENOTSOCK]
 *  [EFAULT]
 *  [EMSGSIZE]
 *  [EAGAIN]
 *  [ENOBUFS]
 *  [EACCES]
 *  [EHOSTUNREACH]
 */
CPNIO_EXPORT ssize_t cpnio_sendto (int fd, const void *msg, size_t len, int flags,
                                   const struct sockaddr *to, socklen_t tolen);

/**
 * Receive a message on a socket, storing the remote host's address in
 * FROM.
 *
 * \param fd      The socket file descriptor.
 * \param buf     The buffer to store received bytes in.
 * \param flags   Flags to control the receive.
 * \param from    Where to store the remote address.
 * \param fromlen Pointer to the size of FROM; on return, it will contain the
 *  size of the structure placed in FROM.
 * \return The number of bytes received on success. -1 on error, and ERRNO will
 *  be set.
 * \see recvfrom(2)
 *
 * Allewed ERRNO values:
 *  [EBADF]    FD is not a valid file descriptor.
 *  [ENOTCONN] If the socket is stream-oriented, and no prior call to
 *             connect(2) was made.
 *  [ENOTSOCK] FD is not a socket.
 *  [EAGAIN]   FD is in non-blocking mode, and no message was
 *             immediately available.
 *  [EINTR]    The system call was interrupted by a signal.
 *  [EFAULT]   BUF, FROM, or FROMLEN lie outside the process's address
 *             space.
 */
CPNIO_EXPORT ssize_t cpnio_recvfrom (int fd, void *buf, size_t len, int flags,
                                     struct sockaddr *from, socklen_t *fromlen);


/**
 * Control file descriptor properties.
 *
 * \param fd  The file descriptor to control.
 * \param cmd The command to execute.
 * \param arg The command argument.
 * \return A value other than -1, specific to CMD. On error, -1 is
 *  returned, and ERRNO is set.
 *
 * Allowed ERRNO values:
 *  FIXME
 */
CPNIO_EXPORT int cpnio_fcntl (int fd, int cmd, long arg);


/**
 * Select from one of the given file descriptor sets a descriptor that
 * is ready for the given operation (read, write, etc.).
 *
 * \param nfds      A value one larger than the largest file
 *                  descriptor.
 * \param readfds   A set of file descriptors to select for
 *                  readability.
 * \param writefds  A set of file descriptors to select for
 *                  writability.
 * \param exceptfds A set of file descriptors to select for
 *                  exceptional conditions.
 * \param tm        The selection timeout.
 * \return The number of file descriptors selected, possibly zero, or
 *                  -1 on error (and with ERRNO set).
 */
CPNIO_EXPORT int cpnio_select (int nfds, fd_set *readfds, fd_set *writefds,
                               fd_set *exceptfds, struct timeval *tm);

/*
 * We include the implementation file here, because our reference
 * implementation is trivial, and the functions are declared extern
 * inline.
 *
 * Implementations that need different implementations of these functions
 * SHOULD remove this line, and compile javanio.c as a separate unit.
 */
#include "javanio.c"

#endif /* __JAVANIO_H__ */
