/* DatagramSocketImpl.java -- Abstract class for UDP socket implementations
   Copyright (C) 1998, 1999 2000, 2001 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.net;

import java.io.IOException;
import java.io.FileDescriptor;

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

/**
 * This abstract class models a datagram socket implementation.  An
 * actual implementation class would implement these methods, probably
 * via redirecting them to native code.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Warren Levy <warrenl@cygnus.com>
 */
public abstract class DatagramSocketImpl implements SocketOptions
{

  /**
   * The local port to which this socket is bound
   */
  protected int localPort;

  /**
   * The FileDescriptor object for this object. 
   */
  protected FileDescriptor fd;

  /**
   * Default, no-argument constructor for subclasses to call.
   */
  public DatagramSocketImpl()
  {
  }

  /**
   * This method binds the socket to the specified local port and address.
   *
   * @param lport The port number to bind to
   * @param laddr The address to bind to
   *
   * @exception SocketException If an error occurs
   */
  protected abstract void bind(int lport, InetAddress laddr)
  	throws SocketException;

  /**
   * This methods closes the socket
   */
  protected abstract void close();

  /**
   * Creates a new datagram socket.
   *
   * @exception SocketException If an error occurs
   */
  protected abstract void create() throws SocketException;

  /**
   * Takes a peek at the next packet received in order to retrieve the
   * address of the sender
   *
   * @param i The InetAddress to fill in with the information about the
   *          sender if the next packet
   *
   * @return The port number of the sender of the packet
   *
   * @exception IOException If an error occurs
   */
  protected abstract int peek(InetAddress i) throws IOException;

  /**
   * Transmits the specified packet of data to the network.  The destination
   * host and port should be encoded in the packet.
   *
   * @param p The packet to send
   *
   * @exception IOException If an error occurs
   */
  protected abstract void send(DatagramPacket p) throws IOException;

  /**
   * Receives a packet of data from the network  Will block until a packet
   * arrives.  The packet info in populated into the passed in 
   * DatagramPacket object.
   *
   * @param p A place to store the incoming packet.
   *
   * @exception IOException If an error occurs
   */
  protected abstract void receive(DatagramPacket p) throws IOException;

  /**
   * Sets the Time to Live (TTL) setting on this socket to the specified
   * value. <b>Use <code>setTimeToLive(int)</code></b> instead.
   *
   * @param ttl The new Time to Live value
   *
   * @exception IOException If an error occurs
   * @deprecated
   */
  protected abstract void setTTL(byte ttl) throws IOException;

  /**
   * This method returns the current Time to Live (TTL) setting on this
   * socket.  <b>Use <code>getTimeToLive()</code></b> instead.
   *
   * @exception IOException If an error occurs
   * @deprecated
   */
  protected abstract byte getTTL() throws IOException;

 /**
   * Sets the Time to Live (TTL) setting on this socket to the specified
   * value.
   *
   * @param ttl The new Time to Live value
   *
   * @exception IOException If an error occurs
   */
  protected abstract void setTimeToLive(int ttl) throws IOException;

  /**
   * This method returns the current Time to Live (TTL) setting on this
   * socket.
   *
   * @exception IOException If an error occurs
   */
  protected abstract int getTimeToLive() throws IOException;

  /**
   * Causes this socket to join the specified multicast group
   *
   * @param inetaddr The multicast address to join with
   *
   * @exception IOException If an error occurs
   */
  protected abstract void join(InetAddress inetaddr) throws IOException;

 /**
   * Causes the socket to leave the specified multicast group.
   *
   * @param inetaddr The multicast address to leave
   *
   * @exception IOException If an error occurs
   */
  protected abstract void leave(InetAddress inetaddr) throws IOException;

  /**
   * Returns the FileDescriptor for this socket
   */
  protected FileDescriptor getFileDescriptor()
  {
    return fd;
  }

  /**
   * Returns the local port this socket is bound to
   */
  protected int getLocalPort()
  {
    return localPort;
  }
}
