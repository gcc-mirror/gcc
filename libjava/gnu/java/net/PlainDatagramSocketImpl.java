/* PlainDatagramSocketImpl.java -- Default DatagramSocket implementation
   Copyright (C) 1998, 1999, 2001, 2003, 2004  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package gnu.java.net;

import gnu.classpath.Configuration;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocketImpl;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.NetworkInterface;
import java.net.SocketAddress;
import java.net.SocketException;
import java.net.SocketOptions;

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

/**
 * This is the default socket implementation for datagram sockets.
 * It makes native calls to C routines that implement BSD style
 * SOCK_DGRAM sockets in the AF_INET family.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Warren Levy (warrenl@cygnus.com)
 */
public final class PlainDatagramSocketImpl extends DatagramSocketImpl
{
  // Static initializer to load native library
  static
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
        System.loadLibrary("javanet");
      }
  }
  
  // These fields are mirrored for use in native code to avoid cpp conflicts
  // when the #defines in system header files are the same as the public fields.
  static final int _Jv_TCP_NODELAY_ = SocketOptions.TCP_NODELAY,
                   _Jv_SO_BINDADDR_ = SocketOptions.SO_BINDADDR,
                   _Jv_SO_REUSEADDR_ = SocketOptions.SO_REUSEADDR,
                   _Jv_SO_BROADCAST_ = SocketOptions.SO_BROADCAST,
                   _Jv_SO_OOBINLINE_ = SocketOptions.SO_OOBINLINE,
                   _Jv_IP_MULTICAST_IF_ = SocketOptions.IP_MULTICAST_IF,
                   _Jv_IP_MULTICAST_IF2_ = SocketOptions.IP_MULTICAST_IF2,
                   _Jv_IP_MULTICAST_LOOP_ = SocketOptions.IP_MULTICAST_LOOP,
                   _Jv_IP_TOS_ = SocketOptions.IP_TOS,
                   _Jv_SO_LINGER_ = SocketOptions.SO_LINGER,
                   _Jv_SO_TIMEOUT_ = SocketOptions.SO_TIMEOUT,
                   _Jv_SO_SNDBUF_ = SocketOptions.SO_SNDBUF,
                   _Jv_SO_RCVBUF_ = SocketOptions.SO_RCVBUF,
                   _Jv_SO_KEEPALIVE_ = SocketOptions.SO_KEEPALIVE;

  /**
   * This is the actual underlying file descriptor
   */
  int native_fd = -1;
  
  /**
   * Lock object to serialize threads wanting to receive 
   */
  private final Object RECEIVE_LOCK = new Object();
  
  /**
   * Lock object to serialize threads wanting to send 
   */
  private final Object SEND_LOCK = new Object();

  // FIXME: Is this necessary?  Could it help w/ DatagramSocket.getLocalAddress?
  // InetAddress address;
  
  // localAddress cache  
  InetAddress localAddress;

  // 'timeout' is set/read by setOption/getOption.
  int timeout = 0;

  /**
   * Default do nothing constructor
   */
  public PlainDatagramSocketImpl()
  {
  }

  protected void finalize() throws Throwable
  {
    synchronized (this)
      {
	if (native_fd != -1)
	  close();
      }
    super.finalize();
  }

  public int getNativeFD()
  {
    return native_fd;
  }

  /**
   * Binds this socket to a particular port and interface
   *
   * @param port The port to bind to
   * @param addr The address to bind to
   *
   * @exception SocketException If an error occurs
   */
  protected native void bind(int port, InetAddress addr)
    throws SocketException;

  protected native void connect(InetAddress addr, int port)
    throws SocketException;
  
  protected native void disconnect();
  
  /**
   * Creates a new datagram socket
   *
   * @exception SocketException If an error occurs
   */
  protected native void create() throws SocketException;
  
  protected native int peek(InetAddress addr) throws IOException;
  
  protected native int peekData(DatagramPacket packet) throws IOException;

  /**
   * Sets the Time to Live value for the socket
   *
   * @param ttl The new TTL value
   *
   * @exception IOException If an error occurs
   */
  protected native void setTimeToLive(int ttl) throws IOException;

  /**
   * Gets the Time to Live value for the socket
   *
   * @return The TTL value
   *
   * @exception IOException If an error occurs
   */
  protected native int getTimeToLive() throws IOException;

  /**
   * Sends a packet of data to a remote host
   *
   * @param packet The packet to send
   *
   * @exception IOException If an error occurs
   */
  protected native void send(DatagramPacket packet) throws IOException;

  /**
   * Receives a UDP packet from the network
   *
   * @param packet The packet to fill in with the data received
   *
   * @exception IOException IOException If an error occurs
   */
  protected native void receive(DatagramPacket packet) throws IOException;

  /**
   * Sets the value of an option on the socket
   *
   * @param option_id The identifier of the option to set
   * @param val The value of the option to set
   *
   * @exception SocketException If an error occurs
   */
  public native void setOption(int option_id, Object val)
    throws SocketException;

  /**
   * Retrieves the value of an option on the socket
   *
   * @param option_id The identifier of the option to retrieve
   *
   * @return The value of the option
   *
   * @exception SocketException If an error occurs
   */
  public native Object getOption(int option_id)
    throws SocketException;

  /**
   * Joins or leaves a broadcasting group on a given network interface.
   * If the network interface is <code>null</code> the group is join/left on
   * all locale network interfaces.
   * 
   * @param inetAddr The broadcast address.
   * @param netIf The network interface to join the group on.
   * @param join True to join a broadcasting group, fals to leave it.
   *
   * @exception IOException If an error occurs.
   */
  private native void mcastGrp(InetAddress inetAddr, NetworkInterface netIf,
		               boolean join)
    throws IOException;

  /**
   * Closes the socket
   */
  protected native void close();

  /**
   * Gets the Time to Live value for the socket
   *
   * @return The TTL value
   *
   * @exception IOException If an error occurs
   *
   * @deprecated 1.2
   */
  protected byte getTTL() throws IOException
  {
    return (byte) getTimeToLive();
  }

  /**
   * Sets the Time to Live value for the socket
   *
   * @param ttl The new TTL value
   *
   * @exception IOException If an error occurs
   *
   * @deprecated 1.2
   */
  protected void setTTL(byte ttl) throws IOException
  {
    setTimeToLive(((int) ttl) & 0xFF);
  }

  /**
   * Joins a multicast group
   *
   * @param addr The group to join
   *
   * @exception IOException If an error occurs
   */
  protected void join(InetAddress addr) throws IOException
  {
    mcastGrp(addr, null, true);
  }

  /**
   * Leaves a multicast group
   *
   * @param addr The group to leave
   *
   * @exception IOException If an error occurs
   */
  protected void leave(InetAddress addr) throws IOException
  {
    mcastGrp(addr, null, false);
  }

  protected void joinGroup(SocketAddress mcastaddr, NetworkInterface netIf)
    throws IOException
  {
    mcastGrp(((InetSocketAddress) mcastaddr).getAddress(), netIf, true);
  }

  protected void leaveGroup(SocketAddress mcastaddr, NetworkInterface netIf)
    throws IOException
  {
    mcastGrp(((InetSocketAddress) mcastaddr).getAddress(), netIf, false);
  }
}
