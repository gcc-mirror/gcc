/* PlainDatagramSocketImpl.java -- Default DatagramSocket implementation
   Copyright (C) 1998, 1999, 2001, 2003, 2004, 2005  Free Software Foundation, Inc.

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


package gnu.java.net;

import gnu.classpath.Configuration;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocketImpl;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketAddress;
import java.net.SocketException;

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
  
  /**
   * Option id for the IP_TTL (time to live) value.
   */
  private static final int IP_TTL = 0x1E61; // 7777

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
  protected synchronized native void bind(int port, InetAddress addr)
    throws SocketException;

  /**
   * Creates a new datagram socket
   *
   * @exception SocketException If an error occurs
   */
  protected synchronized native void create() throws SocketException;

  /**
   * Sets the Time to Live value for the socket
   *
   * @param ttl The new TTL value
   *
   * @exception IOException If an error occurs
   */
  protected synchronized void setTimeToLive(int ttl) throws IOException
  {
    setOption(IP_TTL, new Integer(ttl));
  }

  /**
   * Gets the Time to Live value for the socket
   *
   * @return The TTL value
   *
   * @exception IOException If an error occurs
   */
  protected synchronized int getTimeToLive() throws IOException
  {
    Object obj = getOption(IP_TTL);

    if (! (obj instanceof Integer))
      throw new IOException("Internal Error");

    return ((Integer) obj).intValue();
  }

  /**
   * Sends a packet of data to a remote host
   *
   * @param addr The address to send to
   * @param port The port to send to 
   * @param buf The buffer to send
   * @param offset The offset of the data in the buffer to send
   * @param len The length of the data to send
   *
   * @exception IOException If an error occurs
   */
  private native void sendto (InetAddress addr, int port,
                              byte[] buf, int offset, int len)
    throws IOException;

  /**
   * Sends a packet of data to a remote host
   *
   * @param packet The packet to send
   *
   * @exception IOException If an error occurs
   */
  protected void send(DatagramPacket packet) throws IOException
  {
    synchronized(SEND_LOCK)
      {
      sendto(packet.getAddress(), packet.getPort(), packet.getData(), 
             packet.getOffset(), packet.getLength());
      }
    
  }

  /**
   * Receives a UDP packet from the network
   *
   * @param packet The packet to fill in with the data received
   *
   * @exception IOException IOException If an error occurs
   */
  protected void receive(DatagramPacket packet)
    throws IOException
  {
      synchronized(RECEIVE_LOCK)
        {
        receive0(packet);		
        }
  }

  /**
   * Native call to receive a UDP packet from the network
   * 
   * @param packet The packet to fill in with the data received
   *
   * @exception IOException IOException If an error occurs
   */
  private native void receive0(DatagramPacket packet) throws IOException;

  /**
   * Sets the value of an option on the socket
   *
   * @param option_id The identifier of the option to set
   * @param val The value of the option to set
   *
   * @exception SocketException If an error occurs
   */
  public synchronized native void setOption(int option_id, Object val)
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
  public synchronized native Object getOption(int option_id)
    throws SocketException;

  /**
   * Closes the socket
   */
  protected synchronized native void close();

  /**
   * Gets the Time to Live value for the socket
   *
   * @return The TTL value
   *
   * @exception IOException If an error occurs
   *
   * @deprecated 1.2
   */
  protected synchronized byte getTTL() throws IOException
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
  protected synchronized void setTTL(byte ttl) throws IOException
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
  protected synchronized native void join(InetAddress addr) throws IOException;

  /**
   * Leaves a multicast group
   *
   * @param addr The group to leave
   *
   * @exception IOException If an error occurs
   */
  protected synchronized native void leave(InetAddress addr) throws IOException;

  /**
   * What does this method really do?
   */
  protected synchronized int peek(InetAddress addr) throws IOException
  {
    throw new IOException("Not Implemented Yet");
  }

  public int peekData(DatagramPacket packet)
  {
    throw new InternalError
      ("PlainDatagramSocketImpl::peekData is not implemented");
  }

  public void joinGroup(SocketAddress address, NetworkInterface netIf)
  {
    throw new InternalError
      ("PlainDatagramSocketImpl::joinGroup is not implemented");
  }

  public void leaveGroup(SocketAddress address, NetworkInterface netIf)
  {
    throw new InternalError
      ("PlainDatagramSocketImpl::leaveGroup is not implemented");
  }
}
