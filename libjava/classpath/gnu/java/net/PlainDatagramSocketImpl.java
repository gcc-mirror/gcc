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
    // Nothing to do here.
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
  protected  synchronized void bind(int port, InetAddress addr)
    throws SocketException
    {
      VMPlainDatagramSocketImpl.bind(this, port, addr);
    }

  /**
   * Creates a new datagram socket
   *
   * @exception SocketException If an error occurs
   */
  protected  synchronized void create() throws SocketException
  {
    VMPlainDatagramSocketImpl.create(this);
  }

  /**
   * Connects to the remote address and port specified as arguments.
   *
   * @param addr The remote address to connect to
   * @param port The remote port to connect to
   *
   * @exception SocketException If an error occurs
   */
  protected void connect(InetAddress addr, int port) throws SocketException
  {
    VMPlainDatagramSocketImpl.connect(this, addr, port);
  }

  /**
   * Disconnects the socket.
   *
   * @since 1.4
   */
  protected void disconnect()
  {
    synchronized (this)
      {
	if (native_fd != -1)
	  close();
      }
  }

  /**
   * Sets the Time to Live value for the socket
   *
   * @param ttl The new TTL value
   *
   * @exception IOException If an error occurs
   */
  protected synchronized void setTimeToLive(int ttl) throws IOException
  {
    setOption(VMPlainDatagramSocketImpl.IP_TTL, new Integer(ttl));
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
    Object obj = getOption(VMPlainDatagramSocketImpl.IP_TTL);

    if (! (obj instanceof Integer))
      throw new IOException("Internal Error");

    return ((Integer) obj).intValue();
  }


  /**
   * Sends a packet of data to a remote host
   *
   * @param packet The packet to send
   *
   * @exception IOException If an error occurs
   */
  protected void send(DatagramPacket packet) throws IOException
  {
    if (native_fd != -1)
      {
        synchronized(SEND_LOCK)
          {
            VMPlainDatagramSocketImpl.send(this, packet);
          }
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
          VMPlainDatagramSocketImpl.receive(this, packet);	
        }
  }


  /**
   * Sets the value of an option on the socket
   *
   * @param option_id The identifier of the option to set
   * @param val The value of the option to set
   *
   * @exception SocketException If an error occurs
   */
  public synchronized void setOption(int option_id, Object val)
    throws SocketException
    {
      VMPlainDatagramSocketImpl.setOption(this, option_id, val);
    }

  /**
   * Retrieves the value of an option on the socket
   *
   * @param option_id The identifier of the option to retrieve
   *
   * @return The value of the option
   *
   * @exception SocketException If an error occurs
   */
  public synchronized Object getOption(int option_id)
    throws SocketException
    {
      return VMPlainDatagramSocketImpl.getOption(this, option_id);
    }

  /**
   * Closes the socket
   */
  protected synchronized void close()
  {
    VMPlainDatagramSocketImpl.close(this);
  }

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
  protected synchronized void join(InetAddress addr) throws IOException
  {
    VMPlainDatagramSocketImpl.join(this,addr);
  }

  /**
   * Leaves a multicast group
   *
   * @param addr The group to leave
   *
   * @exception IOException If an error occurs
   */
  protected synchronized void leave(InetAddress addr) throws IOException
  {
    VMPlainDatagramSocketImpl.leave(this, addr);
  }

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
    throws IOException
  {
    VMPlainDatagramSocketImpl.joinGroup(this, address, netIf);
  }

  public void leaveGroup(SocketAddress address, NetworkInterface netIf)
    throws IOException
  {
    VMPlainDatagramSocketImpl.leaveGroup(this, address, netIf);
  }
}
