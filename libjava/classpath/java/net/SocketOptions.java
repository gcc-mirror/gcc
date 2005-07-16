/* SocketOptions.java -- Implements options for sockets (duh!)
   Copyright (C) 1998, 1999, 2000, 2001,
                 2002, 2003 Free Software Foundation, Inc.

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

package java.net;


/**
 * Written using on-line Java Platform 1.2 API Specification.
 * Status:  Believed complete and correct.
 */
/**
 * This interface is used by <code>SocketImpl</code> and
 * <code>DatagramSocketImpl</code> to implement options
 * on sockets.
 *
 * @since 1.2
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Warren Levy (warrenl@cygnus.com)
 * @status should be completely JDK 1.4 compatible
 */
public interface SocketOptions
{
  /**
   * Option id for the SO_KEEPALIVE value
   * @since 1.3
   */
  int SO_KEEPALIVE = 0x8;

  /**
   * Option id for the SO_LINGER value
   */
  int SO_LINGER = 0x80; // 128

  /**
   * Option id for the SO_TIMEOUT value
   */
  int SO_TIMEOUT = 0x1006; // 4102

  /**
   * Retrieve the local address to which the socket is bound.
   */
  int SO_BINDADDR = 0x0F; // 15

  /**
   * Option id for the send buffer size
   * @since 1.2
   */
  int SO_SNDBUF = 0x1001; // 4097

  /**
   * Option id for the receive buffer size
   * @since 1.2
   */
  int SO_RCVBUF = 0x1002; // 4098

  /**
   * Sets the SO_REUSEADDR parameter on a socket
   */
  int SO_REUSEADDR = 0x04; // 4

  /**
   * Sets SO_BROADCAST for a socket
   * @since 1.4
   */
  int SO_BROADCAST = 0x20; // 32

  /**
   * Sets SO_OOBINLINE for a socket
   * @since 1.4
   */
  int SO_OOBINLINE = 0x1003; // 4099

  /**
   * Option id for the TCP_NODELAY value
   */
  int TCP_NODELAY = 0x01; // 1

  /**
   * Options id for the IP_MULTICAST_IF value
   */
  int IP_MULTICAST_IF = 0x10; // 16

  /**
   * same as above
   * @since 1.4
   */
  int IP_MULTICAST_IF2 = 0x1F; // 31

  /**
   * This option enables or disables local loopback of multicast datagrams.
   * @since 1.4
   */
  int IP_MULTICAST_LOOP = 0x12; // 18

  /**
   * This option sets the type-of-service or traffic class field in the
   * IP header for a TCP or UDP socket.
   * @since 1.4
   */
  int IP_TOS = 0x03; // 3

  /**
   * Sets the specified option on a socket to the passed in object.  For
   * options that take an integer argument, the passed in object is an
   * <code>Integer</code>.  For options that are set to on or off, the
   * value passed will be a <code>Boolean</code>.   The <code>optionId</code>
   * parameter is one of the defined constants in this interface.
   *
   * @param optionId The identifier of the option
   * @param val The value to set the option to
   *
   * @exception SocketException If an error occurs
   */
  void setOption(int optionId, Object val) throws SocketException;

  /**
   * Returns the current setting of the specified option.  The
   * <code>Object</code> returned will be an <code>Integer</code> for options
   * that have integer values.  For options that are set to on or off, a
   * <code>Boolean</code> will be returned.   The <code>optionId</code>
   * parameter is one of the defined constants in this interface.
   *
   * @param optionId The option identifier
   *
   * @return The current value of the option
   *
   * @exception SocketException If an error occurs
   */
  Object getOption(int optionId) throws SocketException;
} // interface SocketOptions
