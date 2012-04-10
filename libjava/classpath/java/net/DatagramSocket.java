/* DatagramSocket.java -- A class to model UDP sockets
   Copyright (C) 1998, 1999, 2000, 2002, 2003, 2004, 2005, 2006
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

import gnu.classpath.SystemProperties;

import gnu.java.net.PlainDatagramSocketImpl;
import gnu.java.nio.DatagramChannelImpl;

import java.io.IOException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.IllegalBlockingModeException;


/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */
/**
 * This class models a connectionless datagram socket that sends
 * individual packets of data across the network.  In the TCP/IP world,
 * this means UDP.  Datagram packets do not have guaranteed delivery,
 * or any guarantee about the order the data will be received on the
 * remote host.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Warren Levy (warrenl@cygnus.com)
 * @date May 3, 1999.
 */
public class DatagramSocket
{
  /**
   * This is the user DatagramSocketImplFactory for this class.  If this
   * variable is null, a default factory is used.
   */
  private static DatagramSocketImplFactory factory;

  /**
   * This is the implementation object used by this socket.
   */
  private DatagramSocketImpl impl;

  /**
   * True if socket implementation was created.
   */
  private boolean implCreated;

  /**
   * This is the address we are "connected" to
   */
  private InetAddress remoteAddress;

  /**
   * This is the port we are "connected" to
   */
  private int remotePort = -1;

  /**
   * True if socket is bound.
   */
  private boolean bound;

  /**
   * Creates a <code>DatagramSocket</code> from a specified
   * <code>DatagramSocketImpl</code> instance
   *
   * @param impl The <code>DatagramSocketImpl</code> the socket will be
   * created from
   *
   * @since 1.4
   */
  protected DatagramSocket(DatagramSocketImpl impl)
  {
    if (impl == null)
      throw new NullPointerException("impl may not be null");

    this.impl = impl;
    this.remoteAddress = null;
    this.remotePort = -1;
  }

  /**
   * Initializes a new instance of <code>DatagramSocket</code> that binds to
   * a random port and every address on the local machine.
   *
   * @exception SocketException If an error occurs.
   * @exception SecurityException If a security manager exists and
   * its <code>checkListen</code> method doesn't allow the operation.
   */
  public DatagramSocket() throws SocketException
  {
    this(new InetSocketAddress(0));
  }

  /**
   * Initializes a new instance of <code>DatagramSocket</code> that binds to
   * the specified port and every address on the local machine.
   *
   * @param port The local port number to bind to.
   *
   * @exception SecurityException If a security manager exists and its
   * <code>checkListen</code> method doesn't allow the operation.
   * @exception SocketException If an error occurs.
   */
  public DatagramSocket(int port) throws SocketException
  {
    this(new InetSocketAddress(port));
  }

  /**
   * Initializes a new instance of <code>DatagramSocket</code> that binds to
   * the specified local port and address.
   *
   * @param port The local port number to bind to.
   * @param addr The local address to bind to.
   *
   * @exception SecurityException If a security manager exists and its
   * checkListen method doesn't allow the operation.
   * @exception SocketException If an error occurs.
   */
  public DatagramSocket(int port, InetAddress addr) throws SocketException
  {
    this(new InetSocketAddress(addr, port));
  }

  /**
   * Initializes a new instance of <code>DatagramSocket</code> that binds to
   * the specified local port and address.
   *
   * @param address The local address and port number to bind to.
   *
   * @exception SecurityException If a security manager exists and its
   * <code>checkListen</code> method doesn't allow the operation.
   * @exception SocketException If an error occurs.
   *
   * @since 1.4
   */
  public DatagramSocket(SocketAddress address) throws SocketException
  {
    String propVal = SystemProperties.getProperty("impl.prefix");
    if (propVal == null || propVal.equals(""))
      {
        if (factory != null)
          impl = factory.createDatagramSocketImpl();
        else
          {
            try
              {
                impl = new PlainDatagramSocketImpl();
              }
            catch (IOException ioe)
              {
                SocketException se = new SocketException();
                se.initCause(ioe);
                throw se;
              }
          }
      }
    else
      try
        {
          impl =
            (DatagramSocketImpl) Class.forName("java.net." + propVal
                                               + "DatagramSocketImpl")
                                      .newInstance();
        }
      catch (Exception e)
        {
          System.err.println("Could not instantiate class: java.net."
                             + propVal + "DatagramSocketImpl");
          try
            {
              impl = new PlainDatagramSocketImpl();
            }
          catch (IOException ioe)
            {
              SocketException se = new SocketException();
              se.initCause(ioe);
              throw se;
            }
        }

    if (address != null)
      bind(address);
  }

  // This needs to be accessible from java.net.MulticastSocket
  DatagramSocketImpl getImpl() throws SocketException
  {
    try
      {
        if (! implCreated)
          {
            impl.create();
            implCreated = true;
          }

        return impl;
      }
    catch (IOException e)
      {
        SocketException se = new SocketException();
        se.initCause(e);
        throw se;
      }
  }

  /**
   * Closes this datagram socket.
   */
  public void close()
  {
    if (isClosed())
      return;

    try
      {
        getImpl().close();
      }
    catch (SocketException e)
      {
        // Ignore this case, just close the socket in finally clause.
      }
    finally
      {
        remoteAddress = null;
        remotePort = -1;
        impl = null;
      }

    try
      {
        if (getChannel() != null)
          getChannel().close();
      }
    catch (IOException e)
      {
        // Do nothing.
      }
  }

  /**
   * This method returns the remote address to which this socket is
   * connected.  If this socket is not connected, then this method will
   * return <code>null</code>.
   *
   * @return The remote address.
   *
   * @since 1.2
   */
  public InetAddress getInetAddress()
  {
    return remoteAddress;
  }

  /**
   * This method returns the remote port to which this socket is
   * connected.  If this socket is not connected, then this method will
   * return -1.
   *
   * @return The remote port.
   *
   * @since 1.2
   */
  public int getPort()
  {
    return remotePort;
  }

  /**
   * Returns the local address this datagram socket is bound to.
   *
   * @return The local address is the socket is bound or null
   *
   * @since 1.1
   */
  public InetAddress getLocalAddress()
  {
    if (! isBound())
      return null;

    InetAddress localAddr;

    try
      {
        localAddr =
          (InetAddress) getImpl().getOption(SocketOptions.SO_BINDADDR);

        SecurityManager s = System.getSecurityManager();
        if (s != null)
          s.checkConnect(localAddr.getHostAddress(), -1);
      }
    catch (SecurityException e)
      {
        localAddr = InetAddress.ANY_IF;
      }
    catch (SocketException e)
      {
        // This cannot happen as we are bound.
        return null;
      }

    return localAddr;
  }

  /**
   * Returns the local port this socket is bound to.
   *
   * @return The local port number.
   */
  public int getLocalPort()
  {
    if (isClosed())
      return -1;

    try
      {
        return getImpl().getLocalPort();
      }
    catch (SocketException e)
      {
        // This cannot happen as we are bound.
        return 0;
      }
  }

  /**
   * Returns the value of the socket's SO_TIMEOUT setting.  If this method
   * returns 0 then SO_TIMEOUT is disabled.
   *
   * @return The current timeout in milliseconds.
   *
   * @exception SocketException If an error occurs.
   *
   * @since 1.1
   */
  public synchronized int getSoTimeout() throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    Object buf = getImpl().getOption(SocketOptions.SO_TIMEOUT);

    if (buf instanceof Integer)
      return ((Integer) buf).intValue();

    throw new SocketException("unexpected type");
  }

  /**
   * Sets the value of the socket's SO_TIMEOUT value.  A value of 0 will
   * disable SO_TIMEOUT.  Any other value is the number of milliseconds
   * a socket read/write will block before timing out.
   *
   * @param timeout The new SO_TIMEOUT value in milliseconds.
   *
   * @exception SocketException If an error occurs.
   *
   * @since 1.1
   */
  public synchronized void setSoTimeout(int timeout) throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    if (timeout < 0)
      throw new IllegalArgumentException("Invalid timeout: " + timeout);

    getImpl().setOption(SocketOptions.SO_TIMEOUT, Integer.valueOf(timeout));
  }

  /**
   * This method returns the value of the system level socket option
   * SO_SNDBUF, which is used by the operating system to tune buffer
   * sizes for data transfers.
   *
   * @return The send buffer size.
   *
   * @exception SocketException If an error occurs.
   *
   * @since 1.2
   */
  public int getSendBufferSize() throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    Object buf = getImpl().getOption(SocketOptions.SO_SNDBUF);

    if (buf instanceof Integer)
      return ((Integer) buf).intValue();

    throw new SocketException("unexpected type");
  }

  /**
   * This method sets the value for the system level socket option
   * SO_SNDBUF to the specified value.  Note that valid values for this
   * option are specific to a given operating system.
   *
   * @param size The new send buffer size.
   *
   * @exception SocketException If an error occurs.
   * @exception IllegalArgumentException If size is 0 or negative.
   *
   * @since 1.2
   */
  public void setSendBufferSize(int size) throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    if (size < 0)
      throw new IllegalArgumentException("Buffer size is less than 0");

    getImpl().setOption(SocketOptions.SO_SNDBUF, Integer.valueOf(size));
  }

  /**
   * This method returns the value of the system level socket option
   * SO_RCVBUF, which is used by the operating system to tune buffer
   * sizes for data transfers.
   *
   * @return The receive buffer size.
   *
   * @exception SocketException If an error occurs.
   *
   * @since 1.2
   */
  public int getReceiveBufferSize() throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    Object buf = getImpl().getOption(SocketOptions.SO_RCVBUF);

    if (buf instanceof Integer)
      return ((Integer) buf).intValue();

    throw new SocketException("unexpected type");
  }

  /**
   * This method sets the value for the system level socket option
   * SO_RCVBUF to the specified value.  Note that valid values for this
   * option are specific to a given operating system.
   *
   * @param size The new receive buffer size.
   *
   * @exception SocketException If an error occurs.
   * @exception IllegalArgumentException If size is 0 or negative.
   *
   * @since 1.2
   */
  public void setReceiveBufferSize(int size) throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    if (size < 0)
      throw new IllegalArgumentException("Buffer size is less than 0");

    getImpl().setOption(SocketOptions.SO_RCVBUF, Integer.valueOf(size));
  }

  /**
   * This method connects this socket to the specified address and port.
   * When a datagram socket is connected, it will only send or receive
   * packets to and from the host to which it is connected. A multicast
   * socket that is connected may only send and not receive packets.
   *
   * @param address The address to connect this socket to.
   * @param port The port to connect this socket to.
   *
   * @exception IllegalArgumentException If address or port are invalid.
   * @exception SecurityException If the caller is not allowed to send
   * datagrams to or receive from this address and port.
   *
   * @since 1.2
   */
  public void connect(InetAddress address, int port)
  {
    if (address == null)
      throw new IllegalArgumentException("Connect address may not be null");

    if ((port < 1) || (port > 65535))
      throw new IllegalArgumentException("Port number is illegal: " + port);

    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      {
        if (address.isMulticastAddress())
          sm.checkMulticast(address);
        else
          {
            sm.checkConnect(address.getHostAddress(), port);
            sm.checkAccept(address.getHostAddress(), port);
          }
      }

    if (!isBound())
      {
        try
          {
            bind(new InetSocketAddress(0));
          }
        catch (SocketException e)
          {
            throw new Error("Binding socket failed.", e);
          }
      }

    try
      {
        getImpl().connect(address, port);
        remoteAddress = address;
        remotePort = port;
      }
    catch (SocketException e)
      {
        // This means simply not connected or connect not implemented.
      }
  }

  /**
   * This method disconnects this socket from the address/port it was
   * connected to.  If the socket was not connected in the first place,
   * this method does nothing.
   *
   * @since 1.2
   */
  public void disconnect()
  {
    if (! isConnected())
      return;

    try
      {
        getImpl().disconnect();
      }
    catch (SocketException e)
      {
        // This cannot happen as we are connected.
      }
    finally
      {
        remoteAddress = null;
        remotePort = -1;
      }
  }

  /**
   * Reads a datagram packet from the socket.  Note that this method
   * will block until a packet is received from the network.  On return,
   * the passed in <code>DatagramPacket</code> is populated with the data
   * received and all the other information about the packet.
   *
   * @param p A <code>DatagramPacket</code> for storing the data
   *
   * @exception IOException If an error occurs.
   * @exception SocketTimeoutException If setSoTimeout was previously called
   * and the timeout has expired.
   * @exception PortUnreachableException If the socket is connected to a
   * currently unreachable destination. Note, there is no guarantee that the
   * exception will be thrown.
   * @exception IllegalBlockingModeException If this socket has an associated
   * channel, and the channel is in non-blocking mode.
   * @exception SecurityException If a security manager exists and its
   * checkAccept method doesn't allow the receive.
   */
  public synchronized void receive(DatagramPacket p) throws IOException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    if (remoteAddress != null && remoteAddress.isMulticastAddress())
      throw new IOException
        ("Socket connected to a multicast address my not receive");

    if (getChannel() != null && ! getChannel().isBlocking()
        && ! ((DatagramChannelImpl) getChannel()).isInChannelOperation())
      throw new IllegalBlockingModeException();

    DatagramPacket p2 = new DatagramPacket(p.getData(), p.getOffset(), p.maxlen);
    getImpl().receive(p2);
    p.length = p2.length;
    if (p2.getAddress() != null)
      p.setAddress(p2.getAddress());
    if (p2.getPort() != -1)
      p.setPort(p2.getPort());

    SecurityManager s = System.getSecurityManager();
    if (s != null && isConnected())
      s.checkAccept(p.getAddress().getHostAddress(), p.getPort());
  }

  /**
   * Sends the specified packet.  The host and port to which the packet
   * are to be sent should be set inside the packet.
   *
   * @param p The datagram packet to send.
   *
   * @exception IOException If an error occurs.
   * @exception SecurityException If a security manager exists and its
   * checkMulticast or checkConnect method doesn't allow the send.
   * @exception PortUnreachableException If the socket is connected to a
   * currently unreachable destination. Note, there is no guarantee that the
   * exception will be thrown.
   * @exception IllegalBlockingModeException If this socket has an associated
   * channel, and the channel is in non-blocking mode.
   */
  public void send(DatagramPacket p) throws IOException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    // JDK1.2: Don't do security checks if socket is connected; see jdk1.2 api.
    SecurityManager s = System.getSecurityManager();
    if (s != null && ! isConnected())
      {
        InetAddress addr = p.getAddress();
        if (addr.isMulticastAddress())
          s.checkMulticast(addr);
        else
          s.checkConnect(addr.getHostAddress(), p.getPort());
      }

    if (isConnected())
      {
        if (p.getAddress() != null
            && (remoteAddress != p.getAddress() || remotePort != p.getPort()))
          throw new IllegalArgumentException
            ("DatagramPacket address does not match remote address");
      }

    // FIXME: if this is a subclass of MulticastSocket,
    // use getTimeToLive for TTL val.
    if (getChannel() != null && ! getChannel().isBlocking()
        && ! ((DatagramChannelImpl) getChannel()).isInChannelOperation())
      throw new IllegalBlockingModeException();

    getImpl().send(p);
  }

  /**
   * Binds the socket to the given socket address.
   *
   * @param address The socket address to bind to.
   *
   * @exception SocketException If an error occurs.
   * @exception SecurityException If a security manager exists and
   * its checkListen method doesn't allow the operation.
   * @exception IllegalArgumentException If address type is not supported.
   *
   * @since 1.4
   */
  public void bind(SocketAddress address) throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    if (address == null)
      address = new InetSocketAddress(InetAddress.ANY_IF, 0);

    if (! (address instanceof InetSocketAddress))
      throw new IllegalArgumentException("unsupported address type");

    InetAddress addr = ((InetSocketAddress) address).getAddress();
    int port = ((InetSocketAddress) address).getPort();

    if (port < 0 || port > 65535)
      throw new IllegalArgumentException("Invalid port: " + port);

    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkListen(port);

    if (addr == null)
      addr = InetAddress.ANY_IF;

    try
      {
        getImpl().bind(port, addr);
        bound = true;
      }
    catch (SocketException exception)
      {
        getImpl().close();
        throw exception;
      }
    catch (RuntimeException exception)
      {
        getImpl().close();
        throw exception;
      }
    catch (Error error)
      {
        getImpl().close();
        throw error;
      }
  }

  /**
   * Checks if the datagram socket is closed.
   *
   * @return True if socket is closed, false otherwise.
   *
   * @since 1.4
   */
  public boolean isClosed()
  {
    return impl == null;
  }

  /**
   * Returns the datagram channel assoziated with this datagram socket.
   *
   * @return The associated <code>DatagramChannel</code> object or null
   *
   * @since 1.4
   */
  public DatagramChannel getChannel()
  {
    return null;
  }

  /**
   * Connects the datagram socket to a specified socket address.
   *
   * @param address The socket address to connect to.
   *
   * @exception SocketException If an error occurs.
   * @exception IllegalArgumentException If address type is not supported.
   *
   * @since 1.4
   */
  public void connect(SocketAddress address) throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    if (! (address instanceof InetSocketAddress))
      throw new IllegalArgumentException("unsupported address type");

    InetSocketAddress tmp = (InetSocketAddress) address;
    connect(tmp.getAddress(), tmp.getPort());
  }

  /**
   * Returns the binding state of the socket.
   *
   * @return True if socket bound, false otherwise.
   *
   * @since 1.4
   */
  public boolean isBound()
  {
    return bound;
  }

  /**
   * Returns the connection state of the socket.
   *
   * @return True if socket is connected, false otherwise.
   *
   * @since 1.4
   */
  public boolean isConnected()
  {
    return remoteAddress != null;
  }

  /**
   * Returns the SocketAddress of the host this socket is conneted to
   * or null if this socket is not connected.
   *
   * @return The socket address of the remote host if connected or null
   *
   * @since 1.4
   */
  public SocketAddress getRemoteSocketAddress()
  {
    if (! isConnected())
      return null;

    return new InetSocketAddress(remoteAddress, remotePort);
  }

  /**
   * Returns the local SocketAddress this socket is bound to.
   *
   * @return The local SocketAddress or null if the socket is not bound.
   *
   * @since 1.4
   */
  public SocketAddress getLocalSocketAddress()
  {
    if (! isBound())
      return null;

    return new InetSocketAddress(getLocalAddress(), getLocalPort());
  }

  /**
   * Enables/Disables SO_REUSEADDR.
   *
   * @param on Whether or not to have SO_REUSEADDR turned on.
   *
   * @exception SocketException If an error occurs.
   *
   * @since 1.4
   */
  public void setReuseAddress(boolean on) throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    getImpl().setOption(SocketOptions.SO_REUSEADDR, Boolean.valueOf(on));
  }

  /**
   * Checks if SO_REUSEADDR is enabled.
   *
   * @return True if SO_REUSEADDR is set on the socket, false otherwise.
   *
   * @exception SocketException If an error occurs.
   *
   * @since 1.4
   */
  public boolean getReuseAddress() throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    Object buf = getImpl().getOption(SocketOptions.SO_REUSEADDR);

    if (buf instanceof Boolean)
      return ((Boolean) buf).booleanValue();

    throw new SocketException("unexpected type");
  }

  /**
   * Enables/Disables SO_BROADCAST
   *
   * @param enable True if SO_BROADCAST should be enabled, false otherwise.
   *
   * @exception SocketException If an error occurs
   *
   * @since 1.4
   */
  public void setBroadcast(boolean enable) throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    getImpl().setOption(SocketOptions.SO_BROADCAST, Boolean.valueOf(enable));
  }

  /**
   * Checks if SO_BROADCAST is enabled
   *
   * @return Whether SO_BROADCAST is set
   *
   * @exception SocketException If an error occurs
   *
   * @since 1.4
   */
  public boolean getBroadcast() throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    Object buf = getImpl().getOption(SocketOptions.SO_BROADCAST);

    if (buf instanceof Boolean)
      return ((Boolean) buf).booleanValue();

    throw new SocketException("unexpected type");
  }

  /**
   * Sets the traffic class value
   *
   * @param tc The traffic class
   *
   * @exception SocketException If an error occurs
   * @exception IllegalArgumentException If tc value is illegal
   *
   * @see DatagramSocket#getTrafficClass()
   *
   * @since 1.4
   */
  public void setTrafficClass(int tc) throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    if (tc < 0 || tc > 255)
      throw new IllegalArgumentException();

    getImpl().setOption(SocketOptions.IP_TOS, Integer.valueOf(tc));
  }

  /**
   * Returns the current traffic class
   *
   * @return The current traffic class.
   *
   * @see DatagramSocket#setTrafficClass(int tc)
   *
   * @exception SocketException If an error occurs
   *
   * @since 1.4
   */
  public int getTrafficClass() throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    Object buf = getImpl().getOption(SocketOptions.IP_TOS);

    if (buf instanceof Integer)
      return ((Integer) buf).intValue();

    throw new SocketException("unexpected type");
  }

  /**
   * Sets the datagram socket implementation factory for the application
   *
   * @param fac The factory to set
   *
   * @exception IOException If an error occurs
   * @exception SocketException If the factory is already defined
   * @exception SecurityException If a security manager exists and its
   * checkSetFactory method doesn't allow the operation
   */
  public static void setDatagramSocketImplFactory(DatagramSocketImplFactory fac)
    throws IOException
  {
    if (factory != null)
      throw new SocketException("DatagramSocketImplFactory already defined");

    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSetFactory();

    factory = fac;
  }
}
