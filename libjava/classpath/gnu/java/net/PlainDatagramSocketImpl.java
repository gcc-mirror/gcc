/* PlainDatagramSocketImpl.java -- Default DatagramSocket implementation
   Copyright (C) 1998, 1999, 2001, 2003, 2004, 2005, 2006  Free Software Foundation, Inc.

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

import gnu.java.nio.VMChannel;

import java.io.IOException;
import java.io.InterruptedIOException;
import java.net.DatagramPacket;
import java.net.DatagramSocketImpl;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.NetworkInterface;
import java.net.SocketAddress;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.nio.ByteBuffer;

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
  private final VMChannel channel;
  
  /**
   * The platform-specific socket implementation.
   */
  private final VMPlainSocketImpl impl;
  
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
  public PlainDatagramSocketImpl() throws IOException
  {
    channel = new VMChannel();
    impl = new VMPlainSocketImpl(channel);
  }

  /*protected void finalize() throws Throwable
  {
    synchronized (this)
      {
        if (channel.getState().isValid())
	  close();
      }
    super.finalize();
  }*/

  /*public int getNativeFD()
  {
    return native_fd;
  }*/

  /**
   * Binds this socket to a particular port and interface
   *
   * @param port The port to bind to
   * @param addr The address to bind to
   *
   * @exception SocketException If an error occurs
   */
  protected synchronized void bind(int port, InetAddress addr)
    throws SocketException
  {
    try
      {
        impl.bind(new InetSocketAddress(addr, port));
      }
    catch (SocketException se)
      {
        throw se;
      }
    catch (IOException ioe)
      {
        SocketException se = new SocketException();
        se.initCause(ioe);
        throw se;
      }
  }

  /**
   * Creates a new datagram socket
   *
   * @exception SocketException If an error occurs
   */
  protected synchronized void create() throws SocketException
  {
    try
      {
        channel.initSocket(false);
      }
    catch (SocketException se)
      {
        throw se;
      }
    catch (IOException ioe)
      {
        SocketException se = new SocketException();
        se.initCause(ioe);
        throw se;
      }
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
    channel.connect(new InetSocketAddress(addr, port), 0);
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
        try
          {
            if (channel.getState().isValid())
              channel.disconnect();
          }
        catch (IOException ioe)
          {
          }
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
    impl.setTimeToLive(ttl);
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
    return impl.getTimeToLive();
  }

  protected int getLocalPort()
  {
    if (channel == null)
      return -1;

    try
      {
        InetSocketAddress local = channel.getLocalAddress();
        if (local == null)
          return -1;
        return local.getPort();
      }
    catch (IOException ioe)
      {
        return -1;
      }
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
    synchronized (SEND_LOCK)
      {
        ByteBuffer buf = ByteBuffer.wrap(packet.getData(),
                                         packet.getOffset(),
                                         packet.getLength());
        InetAddress remote = packet.getAddress();
        int port = packet.getPort();
        if (remote == null)
          throw new NullPointerException();
        if (port <= 0)
          throw new SocketException("invalid port " + port);
        while (true)
          {
            try
              {
                channel.send(buf, new InetSocketAddress(remote, port));
                break;
              }
            catch (InterruptedIOException ioe)
              {
                // Ignore; interrupted system call.
              }
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
        ByteBuffer buf = ByteBuffer.wrap(packet.getData(),
                                         packet.getOffset(),
                                         packet.getLength());
        SocketAddress addr = null;
        while (true)
          {
            try
              {
                addr = channel.receive(buf);
                break;
              }
            catch (SocketTimeoutException ste)
              {
                throw ste;
              }
            catch (InterruptedIOException iioe)
              {
                // Ignore. Loop.
              }
          }
        if (addr != null)
          packet.setSocketAddress(addr);
        packet.setLength(buf.position() - packet.getOffset());
      }
  }


  /**
   * Sets the value of an option on the socket
   *
   * @param optionId The identifier of the option to set
   * @param value The value of the option to set
   *
   * @exception SocketException If an error occurs
   */
  public synchronized void setOption(int optionId, Object value)
    throws SocketException
  {
    switch (optionId)
      {
        case IP_MULTICAST_IF:
        case IP_MULTICAST_IF2:
          impl.setMulticastInterface(optionId, (InetAddress) value);
          break;

        case IP_MULTICAST_LOOP:
        case SO_BROADCAST:
        case SO_KEEPALIVE:
        case SO_OOBINLINE:
        case TCP_NODELAY:
        case IP_TOS:
        case SO_LINGER:
        case SO_RCVBUF:
        case SO_SNDBUF:
        case SO_TIMEOUT:
        case SO_REUSEADDR:
          impl.setOption(optionId, value);
          return;

      default:
        throw new SocketException("cannot set option " + optionId);
      }
  }

  /**
   * Retrieves the value of an option on the socket
   *
   * @param optionId The identifier of the option to retrieve
   *
   * @return The value of the option
   *
   * @exception SocketException If an error occurs
   */
  public synchronized Object getOption(int optionId)
    throws SocketException
  {
    if (optionId == SO_BINDADDR)
      {
        try
          {
            InetSocketAddress local = channel.getLocalAddress();
            if (local == null)
              return null;
            return local.getAddress();
          }
        catch (SocketException se)
          {
            throw se;
          }
        catch (IOException ioe)
          {
            SocketException se = new SocketException();
            se.initCause(ioe);
            throw se;
          }
      }
    if (optionId == IP_MULTICAST_IF || optionId == IP_MULTICAST_IF2)
      return impl.getMulticastInterface(optionId);

    return impl.getOption(optionId);
  }

  /**
   * Closes the socket
   */
  protected synchronized void close()
  {
    try
      {
        if (channel.getState().isValid())
          channel.close();
      }
    catch (IOException ioe)
      {
      }
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
    impl.join(addr);
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
    impl.leave(addr);
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
    if (address == null)
      throw new NullPointerException();
    if (!(address instanceof InetSocketAddress))
      throw new SocketException("unknown address type");
    impl.joinGroup((InetSocketAddress) address, netIf);
  }

  public void leaveGroup(SocketAddress address, NetworkInterface netIf)
    throws IOException
  {
    if (address == null)
      throw new NullPointerException();
    if (!(address instanceof InetSocketAddress))
      throw new SocketException("unknown address type");
    impl.leaveGroup((InetSocketAddress) address, netIf);
  }
}
