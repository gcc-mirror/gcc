/* MulticastSocket.java -- Class for using multicast sockets
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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

/*
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

/**
  * This class models a multicast UDP socket.  A multicast address is a
  * class D internet address (one whose most significant bits are 1110).  
  * A multicast group consists of a multicast address and a well known
  * port number.  All members of the group listening on that address and
  * port will receive all the broadcasts to the group.
  * <p>
  * Please note that applets are not allowed to use multicast sockets 
  * 
  * Written using on-line Java Platform 1.2 API Specification, as well
  * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
  * Status:  Believed complete and correct.
  *
  * @author Warren Levy <warrenl@cygnus.com>
  * @author Aaron M. Renn (arenn@urbanophile.com) (Documentation comments)
  * @date May 18, 1999.
  */
public class MulticastSocket extends DatagramSocket
{
  // FIXME: the local addr bound to the multicast socket can be reused;
  // unlike unicast sockets.  It binds to any available network interface.
  // See p.1159 JCL book.

/**
  * Create a MulticastSocket that this not bound to any address
  *
  * @exception IOException If an error occurs
  */
  public MulticastSocket() throws IOException
  {
    super(0, ServerSocket.ANY_IF);
  }

/**
  * Create a multicast socket bound to the specified port
  *
  * @param The port to bind to
  *
  * @exception IOException If an error occurs
  */
  public MulticastSocket(int port) throws IOException
  {
    super(port, ServerSocket.ANY_IF);
  }

/**
  * Returns the interface being used for multicast packets
  * 
  * @return The multicast interface
  *
  * @exception SocketException If an error occurs
  */
  public InetAddress getInterface() throws SocketException
  {
    // FIXME: Is it possible that an InetAddress wasn't returned from getOption?
    return (InetAddress) impl.getOption(SocketOptions.IP_MULTICAST_IF);
  }

/**
  * Returns the current value of the "Time to Live" option.  This is the
  * number of hops a packet can make before it "expires".   This method id
  * deprecated.  Use <code>getTimeToLive</code> instead.
  * 
  * @return The TTL value
  *
  * @exception IOException If an error occurs
  *
  * @deprecated Replaced by getTimeToLive() in Java 1.2
  */
  public byte getTTL() throws IOException
  {
    // Use getTTL here rather than getTimeToLive in case we're using an impl
    // other than the default PlainDatagramSocketImpl and it doesn't have
    // getTimeToLive yet.
    return impl.getTTL();
  }

/**
  * Returns the current value of the "Time to Live" option.  This is the
  * number of hops a packet can make before it "expires". 
  * 
  * @return The TTL value
  *
  * @exception IOException If an error occurs
  *
  * @since Java 1.2
  */
  public int getTimeToLive() throws IOException
  {
    return impl.getTimeToLive();
  }

/**
  * Sets the interface to use for multicast packets.
  *
  * @param addr The new interface to use
  *
  * @exception SocketException If an error occurs
  */
  public void setInterface(InetAddress inf) throws SocketException
  {
    impl.setOption(SocketOptions.IP_MULTICAST_IF, inf);
  }

/**
  * Sets the "Time to Live" value for a socket.  The value must be between
  * 1 and 255.
  *
  * @param ttl The new TTL value
  *
  * @exception IOException If an error occurs
  *
  * @deprecated Replaced by <code>setTimeToLive</code> in Java 1.2
  */
  public void setTTL(byte ttl) throws IOException
  {
    // Use setTTL here rather than setTimeToLive in case we're using an impl
    // other than the default PlainDatagramSocketImpl and it doesn't have
    // setTimeToLive yet.
    impl.setTTL(ttl);
  }

/**
  * Sets the "Time to Live" value for a socket.  The value must be between
  * 1 and 255.  
  *
  * @param ttl The new TTL value
  *
  * @exception IOException If an error occurs
  * 
  * @since Java 1.2
  */
  public void setTimeToLive(int ttl) throws IOException
  {
    if (ttl <= 0 || ttl > 255)
      throw new IllegalArgumentException("Invalid ttl: " + ttl);

    impl.setTimeToLive(ttl);
  }

/**
  * Joins the specified mulitcast group.
  *
  * @param addr The address of the group to join
  * 
  * @exception IOException If an error occurs
  */
  public void joinGroup(InetAddress mcastaddr) throws IOException
  {
    if (! mcastaddr.isMulticastAddress())
      throw new IOException("Not a Multicast address");

    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkMulticast(mcastaddr);

    impl.join(mcastaddr);
  }

/**
  * Leaves the specified multicast group
  *
  * @param addr The address of the group to leave
  *
  * @exception IOException If an error occurs
  */
  public void leaveGroup(InetAddress mcastaddr) throws IOException
  {
    if (! mcastaddr.isMulticastAddress())
      throw new IOException("Not a Multicast address");

    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkMulticast(mcastaddr);

    impl.leave(mcastaddr);
  }

/**
  * Sends a packet of data to a multicast address with a TTL that is
  * different from the default TTL on this socket.  The default TTL for
  * the socket is not changed.
  *
  * @param packet The packet of data to send
  * @param ttl The TTL for this packet
  *
  * @exception IOException If an error occurs
  */
  public synchronized void send(DatagramPacket p, byte ttl) throws IOException
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      {
	InetAddress addr = p.getAddress();
	if (addr.isMulticastAddress())
	  s.checkMulticast(addr, ttl);
	else
	  s.checkConnect(addr.getHostAddress(), p.getPort());
      }

    int oldttl = impl.getTimeToLive();
    impl.setTimeToLive(((int) ttl) & 0xFF);
    impl.send(p);
    impl.setTimeToLive(oldttl);
  }
} // class MulticastSocket
