/* MulticastSocket.java -- Class for using multicast sockets
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2007
   Free Software Foundation, Inc.

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

import java.io.IOException;
import java.util.Enumeration;


/**
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
 * @author Warren Levy (warrenl@cygnus.com)
 * @author Aaron M. Renn (arenn@urbanophile.com) (Documentation comments)
 * @since 1.1
 * @date May 18, 1999.
 */
public class MulticastSocket extends DatagramSocket
{
  /**
   * Create a MulticastSocket that this not bound to any address
   *
   * @exception IOException If an error occurs
   * @exception SecurityException If a security manager exists and its
   * checkListen method doesn't allow the operation
   */
  public MulticastSocket() throws IOException
  {
    this(new InetSocketAddress(0));
  }

  /**
   * Create a multicast socket bound to the specified port
   *
   * @param port The port to bind to
   *
   * @exception IOException If an error occurs
   * @exception SecurityException If a security manager exists and its
   * checkListen method doesn't allow the operation
   */
  public MulticastSocket(int port) throws IOException
  {
    this(new InetSocketAddress(port));
  }

  /**
   * Create a multicast socket bound to the specified SocketAddress.
   *
   * @param address The SocketAddress the multicast socket will be bound to
   *
   * @exception IOException If an error occurs
   * @exception SecurityException If a security manager exists and its
   * checkListen method doesn't allow the operation
   *
   * @since 1.4
   */
  public MulticastSocket(SocketAddress address) throws IOException
  {
    super((SocketAddress) null);
    setReuseAddress(true);
    if (address != null)
      bind(address);
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
    if (isClosed())
      throw new SocketException("socket is closed");

    return (InetAddress) getImpl().getOption(SocketOptions.IP_MULTICAST_IF);
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
   * @deprecated 1.2 Replaced by getTimeToLive()
   *
   * @see MulticastSocket#getTimeToLive()
   */
  public byte getTTL() throws IOException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    // Use getTTL here rather than getTimeToLive in case we're using an impl
    // other than the default PlainDatagramSocketImpl and it doesn't have
    // getTimeToLive yet.
    return getImpl().getTTL();
  }

  /**
   * Returns the current value of the "Time to Live" option.  This is the
   * number of hops a packet can make before it "expires".
   *
   * @return The TTL value
   *
   * @exception IOException If an error occurs
   *
   * @since 1.2
   */
  public int getTimeToLive() throws IOException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    return getImpl().getTimeToLive();
  }

  /**
   * Sets the interface to use for sending multicast packets.
   *
   * @param addr The new interface to use.
   *
   * @exception SocketException If an error occurs.
   *
   * @since 1.4
   */
  public void setInterface(InetAddress addr) throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    getImpl().setOption(SocketOptions.IP_MULTICAST_IF, addr);
  }

  /**
   * Sets the local network interface used to send multicast messages
   *
   * @param netIf The local network interface used to send multicast messages
   *
   * @exception SocketException If an error occurs
   *
   * @see MulticastSocket#getNetworkInterface()
   *
   * @since 1.4
   */
  public void setNetworkInterface(NetworkInterface netIf)
    throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");
    
    InetAddress address;
    if (netIf != null)
      out:
      {
        Enumeration e = netIf.getInetAddresses();
        if (getLocalAddress() instanceof Inet4Address)
          {
            // Search for a IPv4 address.
            while (e.hasMoreElements())
              {
                address = (InetAddress) e.nextElement();
                if (address instanceof Inet4Address)
                  break out;
              }
            throw new SocketException("interface " + netIf.getName() + " has no IPv6 address");
          }
        else if (getLocalAddress() instanceof Inet6Address)
          {
            // Search for a IPv6 address.
            while (e.hasMoreElements())
              {
                address = (InetAddress) e.nextElement();
                if (address instanceof Inet6Address)
                  break out;
              }
            throw new SocketException("interface " + netIf.getName() + " has no IPv6 address");
          }
        else
          throw new SocketException("interface " + netIf.getName() + " has no suitable IP address");
      }
    else
      address = InetAddress.ANY_IF;
    
    
    getImpl().setOption(SocketOptions.IP_MULTICAST_IF, address);
  }

  /**
   * Gets the local network interface which is used to send multicast messages
   *
   * @return The local network interface to send multicast messages
   *
   * @exception SocketException If an error occurs
   *
   * @see MulticastSocket#setNetworkInterface(NetworkInterface netIf)
   *
   * @since 1.4
   */
  public NetworkInterface getNetworkInterface() throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    InetAddress address =
      (InetAddress) getImpl().getOption(SocketOptions.IP_MULTICAST_IF);
    
    // FIXME: libgcj doesn't have createAnyInterface.
//     if (address.isAnyLocalAddress())
//       return NetworkInterface.createAnyInterface();
    
    NetworkInterface netIf = NetworkInterface.getByInetAddress(address);

    return netIf;
  }

  /**
   * Disable/Enable local loopback of multicast packets.  The option is used by
   * the platform's networking code as a hint for setting whether multicast
   * data will be looped back to the local socket.
   *
   * Because this option is a hint, applications that want to verify what
   * loopback mode is set to should call #getLoopbackMode
   *
   * @param disable True to disable loopback mode
   *
   * @exception SocketException If an error occurs
   *
   * @since 1.4
   */
  public void setLoopbackMode(boolean disable) throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    getImpl().setOption(SocketOptions.IP_MULTICAST_LOOP,
                        Boolean.valueOf(disable));
  }

  /**
   * Checks if local loopback mode is enabled
   *
   * @return true if loopback mode is enabled, false otherwise
   * 
   * @exception SocketException If an error occurs
   *
   * @since 1.4
   */
  public boolean getLoopbackMode() throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    Object buf = getImpl().getOption(SocketOptions.IP_MULTICAST_LOOP);

    if (buf instanceof Boolean)
      return ((Boolean) buf).booleanValue();

    throw new SocketException("unexpected type");
  }

  /**
   * Sets the "Time to Live" value for a socket.  The value must be between
   * 1 and 255.
   *
   * @param ttl The new TTL value
   *
   * @exception IOException If an error occurs
   *
   * @deprecated 1.2 Replaced by <code>setTimeToLive</code>
   *
   * @see MulticastSocket#setTimeToLive(int ttl)
   */
  public void setTTL(byte ttl) throws IOException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    // Use setTTL here rather than setTimeToLive in case we're using an impl
    // other than the default PlainDatagramSocketImpl and it doesn't have
    // setTimeToLive yet.
    getImpl().setTTL(ttl);
  }

  /**
   * Sets the "Time to Live" value for a socket.  The value must be between
   * 0 and 255, inclusive.
   *
   * @param ttl The new TTL value
   *
   * @exception IOException If an error occurs
   *
   * @since 1.2
   */
  public void setTimeToLive(int ttl) throws IOException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    if (ttl < 0 || ttl > 255)
      throw new IllegalArgumentException("Invalid ttl: " + ttl);

    getImpl().setTimeToLive(ttl);
  }

  /**
   * Joins the specified multicast group.
   *
   * @param mcastaddr The address of the group to join
   *
   * @exception IOException If an error occurs
   * @exception SecurityException If a security manager exists and its
   * checkMulticast method doesn't allow the operation
   */
  public void joinGroup(InetAddress mcastaddr) throws IOException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    if (! mcastaddr.isMulticastAddress())
      throw new IOException("Not a Multicast address");

    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkMulticast(mcastaddr);

    getImpl().join(mcastaddr);
  }

  /**
   * Leaves the specified multicast group
   *
   * @param mcastaddr The address of the group to leave
   *
   * @exception IOException If an error occurs
   * @exception SecurityException If a security manager exists and its
   * checkMulticast method doesn't allow the operation
   */
  public void leaveGroup(InetAddress mcastaddr) throws IOException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    if (! mcastaddr.isMulticastAddress())
      throw new IOException("Not a Multicast address");

    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkMulticast(mcastaddr);

    getImpl().leave(mcastaddr);
  }

  /**
   * Joins the specified mulitcast group on a specified interface.
   *
   * @param mcastaddr The multicast address to join
   * @param netIf The local network interface to receive the multicast
   * messages on or null to defer the interface set by #setInterface or
   * #setNetworkInterface
   *
   * @exception IOException If an error occurs
   * @exception IllegalArgumentException If address type is not supported
   * @exception SecurityException If a security manager exists and its
   * checkMulticast method doesn't allow the operation
   *
   * @see MulticastSocket#setInterface(InetAddress addr)
   * @see MulticastSocket#setNetworkInterface(NetworkInterface netIf)
   *
   * @since 1.4
   */
  public void joinGroup(SocketAddress mcastaddr, NetworkInterface netIf)
    throws IOException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    if (! (mcastaddr instanceof InetSocketAddress))
      throw new IllegalArgumentException("SocketAddress type not supported");

    InetSocketAddress tmp = (InetSocketAddress) mcastaddr;

    if (! tmp.getAddress().isMulticastAddress())
      throw new IOException("Not a Multicast address");

    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkMulticast(tmp.getAddress());

    getImpl().joinGroup(mcastaddr, netIf);
  }

  /**
   * Leaves the specified mulitcast group on a specified interface.
   *
   * @param mcastaddr The multicast address to leave
   * @param netIf The local networki interface or null to defer to the
   * interface set by setInterface or setNetworkInterface
   *
   * @exception IOException If an error occurs
   * @exception IllegalArgumentException If address type is not supported
   * @exception SecurityException If a security manager exists and its
   * checkMulticast method doesn't allow the operation
   *
   * @see MulticastSocket#setInterface(InetAddress addr)
   * @see MulticastSocket#setNetworkInterface(NetworkInterface netIf)
   *
   * @since 1.4
   */
  public void leaveGroup(SocketAddress mcastaddr, NetworkInterface netIf)
    throws IOException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    InetSocketAddress tmp = (InetSocketAddress) mcastaddr;

    if (! tmp.getAddress().isMulticastAddress())
      throw new IOException("Not a Multicast address");

    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkMulticast(tmp.getAddress());

    getImpl().leaveGroup(mcastaddr, netIf);
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
   * @exception SecurityException If a security manager exists and its
   * checkConnect or checkMulticast method doesn't allow the operation
   *
   * @deprecated
   */
  public synchronized void send(DatagramPacket packet, byte ttl)
    throws IOException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    SecurityManager s = System.getSecurityManager();
    if (s != null)
      {
	InetAddress addr = packet.getAddress();
	if (addr.isMulticastAddress())
	  s.checkPermission(new SocketPermission(addr.getHostName()
	                                         + packet.getPort(),
	                                         "accept,connect"));
	else
	  s.checkConnect(addr.getHostAddress(), packet.getPort());
      }

    int oldttl = getImpl().getTimeToLive();
    getImpl().setTimeToLive(((int) ttl) & 0xFF);
    getImpl().send(packet);
    getImpl().setTimeToLive(oldttl);
  }
}
