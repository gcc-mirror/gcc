/* DatagramSocketImpl.java -- Abstract class for UDP socket implementations
   Copyright (C) 1998, 1999 2000, 2001, 
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


package java.net;

import java.io.IOException;
import java.io.FileDescriptor;

/**
 * This abstract class models a datagram socket implementation.  An
 * actual implementation class would implement these methods, probably
 * via redirecting them to native code.
 * <p>
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * <p>
 * Status:  Believed complete and correct.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Warren Levy <warrenl@cygnus.com>
 * @since 1.1
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
   * @param i The <code>InetAddress</code> to fill in with the information 
   *          about the sender if the next packet
   *
   * @return The port number of the sender of the packet
   *
   * @exception IOException If an error occurs
   * @exception PortUnreachableException May be thrown if the socket is
   * connected to a currently unreachable destination. Note, there is no
   * guarantee that the exception will be thrown.
   */
  protected abstract int peek(InetAddress i) throws IOException;

  /**
   * Takes a peek at the next packet received.  This packet is not consumed.
   * With the next peekData/receive operation this packet will be read again.
   * 
   * @param p The <code>DatagramPacket</code> to fill in with the data sent.
   *
   * @return The port number of the sender of the packet.
   * 
   * @exception IOException If an error occurs
   * @exception PortUnreachableException May be thrown if the socket is
   * connected to a currently unreachable destination. Note, there is no
   * guarantee that the exception will be thrown.
   * 
   * @since 1.4
   */
  protected abstract int peekData (DatagramPacket p) throws IOException;

  /**
   * Transmits the specified packet of data to the network.  The destination
   * host and port should be encoded in the packet.
   *
   * @param p The packet to send
   *
   * @exception IOException If an error occurs
   * @exception PortUnreachableException May be thrown if the socket is
   * connected to a currently unreachable destination. Note, there is no
   * guarantee that the exception will be thrown.
   */
  protected abstract void send(DatagramPacket p) throws IOException;

  /**
   * Receives a packet of data from the network  Will block until a packet
   * arrives.  The packet info in populated into the passed in
   * <code>DatagramPacket</code> object.
   *
   * @param p A place to store the incoming packet.
   *
   * @exception IOException If an error occurs
   * @exception PortUnreachableException May be thrown if the socket is
   * connected to a currently unreachable destination. Note, there is no
   * guarantee that the exception will be thrown.
   */
  protected abstract void receive(DatagramPacket p) throws IOException;

  /**
   * Connects the socket to a host specified by address and port.
   *
   * @param address The <code>InetAddress</code> of the host to connect to
   * @param port The port number of the host to connect to
   *
   * @exception SocketException If an error occurs
   *
   * @since 1.4
   */
  protected void connect (InetAddress address, int port) throws SocketException
  {
    // This method has to be overwritten by real implementations
  }

  /**
   * Disconnects the socket.
   * 
   * @since 1.4
   */
  protected void disconnect ()
  {
    // This method has to be overwritten by real implementations
  }

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
   * Causes this socket to join the specified multicast group on a specified
   * device 
   * 
   * @param mcastaddr The address to leave
   * @param netIf The specified network interface to join the group at
   *
   * @exception IOException If an error occurs
   * 
   * @since 1.4
   */
  protected abstract void joinGroup (SocketAddress mcastaddr,
		                     NetworkInterface netIf)
    throws IOException;

  /**
   * Leaves a multicast group
   * 
   * @param mcastaddr The address to join
   * @param netIf The specified network interface to leave the group at
   *
   * @exception IOException If an error occurs
   * 
   * @since 1.4
   */
  protected abstract void leaveGroup (SocketAddress mcastaddr,
		                      NetworkInterface netIf)
    throws IOException;
  
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
