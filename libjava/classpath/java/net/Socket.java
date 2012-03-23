/* Socket.java -- Client socket implementation
   Copyright (C) 1998, 1999, 2000, 2002, 2003, 2004, 2006, 2007
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

import gnu.java.net.PlainSocketImpl;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.channels.IllegalBlockingModeException;
import java.nio.channels.SocketChannel;


/* Written using on-line Java Platform 1.2 API Specification.
 * Status:  I believe all methods are implemented.
 */

/**
 * This class models a client site socket.  A socket is a TCP/IP endpoint
 * for network communications conceptually similar to a file handle.
 * <p>
 * This class does not actually do any work.  Instead, it redirects all of
 * its calls to a socket implementation object which implements the
 * <code>SocketImpl</code> interface.  The implementation class is
 * instantiated by factory class that implements the
 * <code>SocketImplFactory interface</code>.  A default
 * factory is provided, however the factory may be set by a call to
 * the <code>setSocketImplFactory</code> method.  Note that this may only be
 * done once per virtual machine.  If a subsequent attempt is made to set the
 * factory, a <code>SocketException</code> will be thrown.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Per Bothner (bothner@cygnus.com)
 */
public class Socket
{
  /**
   * This is the user SocketImplFactory for this class.  If this variable is
   * null, a default factory is used.
   */
  static SocketImplFactory factory;

  /**
   * The implementation object to which calls are redirected
   */
  // package-private because ServerSocket.implAccept() needs to access it.
  SocketImpl impl;

  /**
   * True if impl.create() has been called.
   */
  // package-private because ServerSocket.implAccept() needs to access it.
  boolean implCreated;

  /**
   * True if the socket is bound.
   * Package private so it can be set from ServerSocket when accept is called.
   */
  boolean bound;

  /**
   * True if input is shutdown.
   */
  private boolean inputShutdown;

  /**
   * True if output is shutdown.
   */
  private boolean outputShutdown;

  /**
   * Initializes a new instance of <code>Socket</code> object without
   * connecting to a remote host.  This useful for subclasses of socket that
   * might want this behavior.
   *
   * @specnote This constructor is public since JDK 1.4
   * @since 1.1
   */
  public Socket()
  {
    if (factory != null)
      impl = factory.createSocketImpl();
    else
      impl = new PlainSocketImpl();
  }

  /**
   * Initializes a new instance of <code>Socket</code> object without
   * connecting to a remote host.  This is useful for subclasses of socket
   * that might want this behavior.
   * <p>
   * Additionally, this socket will be created using the supplied
   * implementation class instead the default class or one returned by a
   * factory.  If this value is <code>null</code>, the default Socket
   * implementation is used.
   *
   * @param impl The <code>SocketImpl</code> to use for this
   *             <code>Socket</code>
   *
   * @exception SocketException If an error occurs
   *
   * @since 1.1
   */
  protected Socket(SocketImpl impl) throws SocketException
  {
    if (impl == null)
      this.impl = new PlainSocketImpl();
    else
      this.impl = impl;
  }

  /**
   * Initializes a new instance of <code>Socket</code> and connects to the
   * hostname and port specified as arguments.
   *
   * @param host The name of the host to connect to
   * @param port The port number to connect to
   *
   * @exception UnknownHostException If the hostname cannot be resolved to a
   * network address.
   * @exception IOException If an error occurs
   * @exception SecurityException If a security manager exists and its
   * checkConnect method doesn't allow the operation
   */
  public Socket(String host, int port)
    throws UnknownHostException, IOException
  {
    this(InetAddress.getByName(host), port, null, 0, true);
  }

  /**
   * Initializes a new instance of <code>Socket</code> and connects to the
   * address and port number specified as arguments.
   *
   * @param address The address to connect to
   * @param port The port number to connect to
   *
   * @exception IOException If an error occurs
   * @exception SecurityException If a security manager exists and its
   * checkConnect method doesn't allow the operation
   */
  public Socket(InetAddress address, int port) throws IOException
  {
    this(address, port, null, 0, true);
  }

  /**
   * Initializes a new instance of <code>Socket</code> that connects to the
   * named host on the specified port and binds to the specified local address
   * and port.
   *
   * @param host The name of the remote host to connect to.
   * @param port The remote port to connect to.
   * @param localAddr The local address to bind to.
   * @param localPort The local port to bind to.
   *
   * @exception SecurityException If the <code>SecurityManager</code>
   * exists and does not allow a connection to the specified host/port or
   * binding to the specified local host/port.
   * @exception IOException If a connection error occurs.
   *
   * @since 1.1
   */
  public Socket(String host, int port, InetAddress localAddr, int localPort)
    throws IOException
  {
    this(InetAddress.getByName(host), port, localAddr, localPort, true);
  }

  /**
   * Initializes a new instance of <code>Socket</code> and connects to the
   * address and port number specified as arguments, plus binds to the
   * specified local address and port.
   *
   * @param address The remote address to connect to
   * @param port The remote port to connect to
   * @param localAddr The local address to connect to
   * @param localPort The local port to connect to
   *
   * @exception IOException If an error occurs
   * @exception SecurityException If a security manager exists and its
   * checkConnect method doesn't allow the operation
   *
   * @since 1.1
   */
  public Socket(InetAddress address, int port, InetAddress localAddr,
                int localPort) throws IOException
  {
    this(address, port, localAddr, localPort, true);
  }

  /**
   * Initializes a new instance of <code>Socket</code> and connects to the
   * hostname and port specified as arguments.  If the stream argument is set
   * to <code>true</code>, then a stream socket is created.  If it is
   * <code>false</code>, a datagram socket is created.
   *
   * @param host The name of the host to connect to
   * @param port The port to connect to
   * @param stream <code>true</code> for a stream socket, <code>false</code>
   * for a datagram socket
   *
   * @exception IOException If an error occurs
   * @exception SecurityException If a security manager exists and its
   * checkConnect method doesn't allow the operation
   *
   * @deprecated Use the <code>DatagramSocket</code> class to create
   * datagram oriented sockets.
   */
  public Socket(String host, int port, boolean stream)
    throws IOException
  {
    this(InetAddress.getByName(host), port, null, 0, stream);
  }

  /**
   * Initializes a new instance of <code>Socket</code> and connects to the
   * address and port number specified as arguments.  If the stream param is
   * <code>true</code>, a stream socket will be created, otherwise a datagram
   * socket is created.
   *
   * @param host The address to connect to
   * @param port The port number to connect to
   * @param stream <code>true</code> to create a stream socket,
   * <code>false</code> to create a datagram socket.
   *
   * @exception IOException If an error occurs
   * @exception SecurityException If a security manager exists and its
   * checkConnect method doesn't allow the operation
   *
   * @deprecated Use the <code>DatagramSocket</code> class to create
   * datagram oriented sockets.
   */
  public Socket(InetAddress host, int port, boolean stream)
    throws IOException
  {
    this(host, port, null, 0, stream);
  }

  /**
   * This constructor is where the real work takes place.  Connect to the
   * specified address and port.  Use default local values if not specified,
   * otherwise use the local host and port passed in.  Create as stream or
   * datagram based on "stream" argument.
   * <p>
   *
   * @param raddr The remote address to connect to
   * @param rport The remote port to connect to
   * @param laddr The local address to connect to
   * @param lport The local port to connect to
   * @param stream true for a stream socket, false for a datagram socket
   *
   * @exception IOException If an error occurs
   * @exception SecurityException If a security manager exists and its
   * checkConnect method doesn't allow the operation
   */
  private Socket(InetAddress raddr, int rport, InetAddress laddr, int lport,
                 boolean stream) throws IOException
  {
    this();

    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkConnect(raddr.getHostAddress(), rport);

    // bind socket
    SocketAddress bindaddr =
      laddr == null ? null : new InetSocketAddress(laddr, lport);
    bind(bindaddr);

    // Connect socket in case of Exceptions we must close the socket
    // because an exception in the constructor means that the caller will
    // not have a reference to this instance.
    // Note: You may have the idea that the exception treatment
    // should be moved into connect() but there is a Mauve test which
    // shows that a failed connect should not close the socket.
    try
      {
        connect(new InetSocketAddress(raddr, rport));
      }
    catch (IOException ioe)
      {
        impl.close();
        throw ioe;
      }
    catch (RuntimeException re)
      {
        impl.close();
        throw re;
      }

    // FIXME: JCL p. 1586 says if localPort is unspecified, bind to any port,
    // i.e. '0' and if localAddr is unspecified, use getLocalAddress() as
    // that default.  JDK 1.2 doc infers not to do a bind.
  }

  private SocketImpl getImpl() throws SocketException
  {
    if (! implCreated)
      {
        try
          {
            impl.create(true);
          }
        catch (IOException x)
          {
            throw (SocketException) new SocketException().initCause(x);
          }
        implCreated = true;
      }
    return impl;
  }

  /**
   * Binds the socket to the given local address/port
   *
   * @param bindpoint The address/port to bind to
   *
   * @exception IOException If an error occurs
   * @exception SecurityException If a security manager exists and its
   * checkConnect method doesn't allow the operation
   * @exception IllegalArgumentException If the address type is not supported
   *
   * @since 1.4
   */
  public void bind(SocketAddress bindpoint) throws IOException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    // XXX: JDK 1.4.1 API documentation says that if bindpoint is null the
    // socket will be bound to an ephemeral port and a valid local address.
    if (bindpoint == null)
      bindpoint = new InetSocketAddress(InetAddress.ANY_IF, 0);

    if (! (bindpoint instanceof InetSocketAddress))
      throw new IllegalArgumentException();

    InetSocketAddress tmp = (InetSocketAddress) bindpoint;

    // bind to address/port
    try
      {
        getImpl().bind(tmp.getAddress(), tmp.getPort());
        bound = true;
      }
    catch (IOException exception)
      {
        close();
        throw exception;
      }
    catch (RuntimeException exception)
      {
        close();
        throw exception;
      }
    catch (Error error)
      {
        close();
        throw error;
      }
  }

  /**
   * Connects the socket with a remote address.
   *
   * @param endpoint The address to connect to
   *
   * @exception IOException If an error occurs
   * @exception IllegalArgumentException If the addess type is not supported
   * @exception IllegalBlockingModeException If this socket has an associated
   * channel, and the channel is in non-blocking mode
   *
   * @since 1.4
   */
  public void connect(SocketAddress endpoint) throws IOException
  {
    connect(endpoint, 0);
  }

  /**
   * Connects the socket with a remote address. A timeout of zero is
   * interpreted as an infinite timeout. The connection will then block
   * until established or an error occurs.
   *
   * @param endpoint The address to connect to
   * @param timeout The length of the timeout in milliseconds, or
   * 0 to indicate no timeout.
   *
   * @exception IOException If an error occurs
   * @exception IllegalArgumentException If the address type is not supported
   * @exception IllegalBlockingModeException If this socket has an associated
   * channel, and the channel is in non-blocking mode
   * @exception SocketTimeoutException If the timeout is reached
   * @throws SecurityException if the SocketAddress is an {@link InetSocketAddress}
   *                           and a security manager is present which does not
   *                           allow connections on the given host and port.
   * @since 1.4
   */
  public void connect(SocketAddress endpoint, int timeout)
    throws IOException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    if (! (endpoint instanceof InetSocketAddress))
      throw new IllegalArgumentException("unsupported address type");

    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      {
        InetSocketAddress inetAddr = (InetSocketAddress) endpoint;
        sm.checkConnect(inetAddr.getHostName(), inetAddr.getPort());
      }

    // The Sun spec says that if we have an associated channel and
    // it is in non-blocking mode, we throw an IllegalBlockingModeException.
    // However, in our implementation if the channel itself initiated this
    // operation, then we must honor it regardless of its blocking mode.
    if (getChannel() != null && ! getChannel().isBlocking()
        && ! ((PlainSocketImpl) getImpl()).isInChannelOperation())
      throw new IllegalBlockingModeException();

    if (! isBound())
      bind(null);

    getImpl().connect(endpoint, timeout);
  }

  /**
   * Returns the address of the remote end of the socket.  If this socket
   * is not connected, then <code>null</code> is returned.
   *
   * @return The remote address this socket is connected to
   */
  public InetAddress getInetAddress()
  {
    if (! isConnected())
      return null;

    try
      {
        return getImpl().getInetAddress();
      }
    catch (SocketException e)
      {
        // This cannot happen as we are connected.
      }

    return null;
  }

  /**
   * Returns the local address to which this socket is bound.  If this socket
   * is not connected, then a wildcard address, for which
   * @see InetAddress#isAnyLocalAddress() is <code>true</code>, is returned.
   *
   * @return The local address
   *
   * @since 1.1
   */
  public InetAddress getLocalAddress()
  {
    if (! isBound())
      return InetAddress.ANY_IF;

    InetAddress addr = null;

    if (impl instanceof PlainSocketImpl)
      addr = ((PlainSocketImpl) impl).getLocalAddress().getAddress();

    if (addr == null)
      {
        try
          {
            addr = (InetAddress) getImpl().getOption(SocketOptions.SO_BINDADDR);
          }
        catch (SocketException e)
          {
            // (hopefully) shouldn't happen
            // throw new java.lang.InternalError
            //      ("Error in PlainSocketImpl.getOption");
            return null;
          }
      }

    // FIXME: According to libgcj, checkConnect() is supposed to be called
    // before performing this operation.  Problems: 1) We don't have the
    // addr until after we do it, so we do a post check.  2). The docs I
    // see don't require this in the Socket case, only DatagramSocket, but
    // we'll assume they mean both.
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkConnect(addr.getHostName(), getLocalPort());

    return addr;
  }

  /**
   * Returns the port number of the remote end of the socket connection.  If
   * this socket is not connected, then 0 is returned.
   *
   * @return The remote port this socket is connected to
   */
  public int getPort()
  {
    if (! isConnected())
      return 0;

    try
      {
        return getImpl().getPort();
      }
    catch (SocketException e)
      {
        // This cannot happen as we are connected.
      }

    return 0;
  }

  /**
   * Returns the local port number to which this socket is bound.  If this
   * socket is not connected, then -1 is returned.
   *
   * @return The local port
   */
  public int getLocalPort()
  {
    if (! isBound())
      return -1;

    try
      {
        if (getImpl() != null)
          return getImpl().getLocalPort();
      }
    catch (SocketException e)
      {
        // This cannot happen as we are bound.
      }

    return -1;
  }

  /**
   * Returns local socket address.
   *
   * @return the local socket address, null if not bound
   *
   * @since 1.4
   */
  public SocketAddress getLocalSocketAddress()
  {
    if (! isBound())
      return null;

    InetAddress addr = getLocalAddress();

    try
      {
        return new InetSocketAddress(addr, getImpl().getLocalPort());
      }
    catch (SocketException e)
      {
        // This cannot happen as we are bound.
        return null;
      }
  }

  /**
   * Returns the remote socket address.
   *
   * @return the remote socket address, null of not connected
   *
   * @since 1.4
   */
  public SocketAddress getRemoteSocketAddress()
  {
    if (! isConnected())
      return null;

    try
      {
        return new InetSocketAddress(getImpl().getInetAddress(),
                                     getImpl().getPort());
      }
    catch (SocketException e)
      {
        // This cannot happen as we are connected.
        return null;
      }
  }

  /**
   * Returns an InputStream for reading from this socket.
   *
   * @return The InputStream object
   *
   * @exception IOException If an error occurs or Socket is not connected
   */
  public InputStream getInputStream() throws IOException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    if (! isConnected())
      throw new IOException("not connected");

    return getImpl().getInputStream();
  }

  /**
   * Returns an OutputStream for writing to this socket.
   *
   * @return The OutputStream object
   *
   * @exception IOException If an error occurs or Socket is not connected
   */
  public OutputStream getOutputStream() throws IOException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    if (! isConnected())
      throw new IOException("not connected");

    return getImpl().getOutputStream();
  }

  /**
   * Sets the TCP_NODELAY option on the socket.
   *
   * @param on true to enable, false to disable
   *
   * @exception SocketException If an error occurs or Socket is not connected
   *
   * @since 1.1
   */
  public void setTcpNoDelay(boolean on) throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    getImpl().setOption(SocketOptions.TCP_NODELAY, Boolean.valueOf(on));
  }

  /**
   * Tests whether or not the TCP_NODELAY option is set on the socket.
   * Returns true if enabled, false if disabled. When on it disables the
   * Nagle algorithm which means that packets are always send immediatly and
   * never merged together to reduce network trafic.
   *
   * @return Whether or not TCP_NODELAY is set
   *
   * @exception SocketException If an error occurs or Socket not connected
   *
   * @since 1.1
   */
  public boolean getTcpNoDelay() throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    Object on = getImpl().getOption(SocketOptions.TCP_NODELAY);

    if (on instanceof Boolean)
      return (((Boolean) on).booleanValue());
    else
      throw new SocketException("Internal Error");
  }

  /**
   * Sets the value of the SO_LINGER option on the socket.  If the
   * SO_LINGER option is set on a socket and there is still data waiting to
   * be sent when the socket is closed, then the close operation will block
   * until either that data is delivered or until the timeout period
   * expires.  The linger interval is specified in hundreths of a second
   * (platform specific?)
   *
   * @param on true to enable SO_LINGER, false to disable
   * @param linger The SO_LINGER timeout in hundreths of a second or -1 if
   * SO_LINGER not set.
   *
   * @exception SocketException If an error occurs or Socket not connected
   * @exception IllegalArgumentException If linger is negative
   *
   * @since 1.1
   */
  public void setSoLinger(boolean on, int linger) throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    if (on)
      {
        if (linger < 0)
          throw new IllegalArgumentException("SO_LINGER must be >= 0");

        if (linger > 65535)
          linger = 65535;

        getImpl().setOption(SocketOptions.SO_LINGER, Integer.valueOf(linger));
      }
    else
      getImpl().setOption(SocketOptions.SO_LINGER, Integer.valueOf(-1));
  }

  /**
   * Returns the value of the SO_LINGER option on the socket.  If the
   * SO_LINGER option is set on a socket and there is still data waiting to
   * be sent when the socket is closed, then the close operation will block
   * until either that data is delivered or until the timeout period
   * expires.  This method either returns the timeouts (in hundredths of
   * of a second (platform specific?)) if SO_LINGER is set, or -1 if
   * SO_LINGER is not set.
   *
   * @return The SO_LINGER timeout in hundreths of a second or -1
   * if SO_LINGER not set
   *
   * @exception SocketException If an error occurs or Socket is not connected
   *
   * @since 1.1
   */
  public int getSoLinger() throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    Object linger = getImpl().getOption(SocketOptions.SO_LINGER);

    if (linger instanceof Integer)
      return (((Integer) linger).intValue());
    else
      return -1;
  }

  /**
   * Sends urgent data through the socket
   *
   * @param data The data to send.
   * Only the lowest eight bits of data are sent
   *
   * @exception IOException If an error occurs
   *
   * @since 1.4
   */
  public void sendUrgentData(int data) throws IOException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    getImpl().sendUrgentData(data);
  }

  /**
   * Enables/disables the SO_OOBINLINE option
   *
   * @param on True if SO_OOBLINE should be enabled
   *
   * @exception SocketException If an error occurs
   *
   * @since 1.4
   */
  public void setOOBInline(boolean on) throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    getImpl().setOption(SocketOptions.SO_OOBINLINE, Boolean.valueOf(on));
  }

  /**
   * Returns the current setting of the SO_OOBINLINE option for this socket
   *
   * @return True if SO_OOBINLINE is set, false otherwise.
   *
   * @exception SocketException If an error occurs
   *
   * @since 1.4
   */
  public boolean getOOBInline() throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    Object buf = getImpl().getOption(SocketOptions.SO_OOBINLINE);

    if (buf instanceof Boolean)
      return (((Boolean) buf).booleanValue());
    else
      throw new SocketException("Internal Error: Unexpected type");
  }

  /**
   * Sets the value of the SO_TIMEOUT option on the socket.  If this value
   * is set, and an read/write is performed that does not complete within
   * the timeout period, a short count is returned (or an EWOULDBLOCK signal
   * would be sent in Unix if no data had been read).  A value of 0 for
   * this option implies that there is no timeout (ie, operations will
   * block forever).  On systems that have separate read and write timeout
   * values, this method returns the read timeout.  This
   * value is in milliseconds.
   *
   * @param timeout The length of the timeout in milliseconds, or
   * 0 to indicate no timeout.
   *
   * @exception SocketException If an error occurs or Socket not connected
   *
   * @since 1.1
   */
  public synchronized void setSoTimeout(int timeout) throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    if (timeout < 0)
      throw new IllegalArgumentException("SO_TIMEOUT value must be >= 0");

    getImpl().setOption(SocketOptions.SO_TIMEOUT, Integer.valueOf(timeout));
  }

  /**
   * Returns the value of the SO_TIMEOUT option on the socket.  If this value
   * is set, and an read/write is performed that does not complete within
   * the timeout period, a short count is returned (or an EWOULDBLOCK signal
   * would be sent in Unix if no data had been read).  A value of 0 for
   * this option implies that there is no timeout (ie, operations will
   * block forever).  On systems that have separate read and write timeout
   * values, this method returns the read timeout.  This
   * value is in thousandths of a second (implementation specific?).
   *
   * @return The length of the timeout in thousandth's of a second or 0
   * if not set
   *
   * @exception SocketException If an error occurs or Socket not connected
   *
   * @since 1.1
   */
  public synchronized int getSoTimeout() throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    Object timeout = getImpl().getOption(SocketOptions.SO_TIMEOUT);
    if (timeout instanceof Integer)
      return (((Integer) timeout).intValue());
    else
      return 0;
  }

  /**
   * This method sets the value for the system level socket option
   * SO_SNDBUF to the specified value.  Note that valid values for this
   * option are specific to a given operating system.
   *
   * @param size The new send buffer size.
   *
   * @exception SocketException If an error occurs or Socket not connected
   * @exception IllegalArgumentException If size is 0 or negative
   *
   * @since 1.2
   */
  public void setSendBufferSize(int size) throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    if (size <= 0)
      throw new IllegalArgumentException("SO_SNDBUF value must be > 0");

    getImpl().setOption(SocketOptions.SO_SNDBUF, Integer.valueOf(size));
  }

  /**
   * This method returns the value of the system level socket option
   * SO_SNDBUF, which is used by the operating system to tune buffer
   * sizes for data transfers.
   *
   * @return The send buffer size.
   *
   * @exception SocketException If an error occurs or socket not connected
   *
   * @since 1.2
   */
  public int getSendBufferSize() throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    Object buf = getImpl().getOption(SocketOptions.SO_SNDBUF);

    if (buf instanceof Integer)
      return (((Integer) buf).intValue());
    else
      throw new SocketException("Internal Error: Unexpected type");
  }

  /**
   * This method sets the value for the system level socket option
   * SO_RCVBUF to the specified value.  Note that valid values for this
   * option are specific to a given operating system.
   *
   * @param size The new receive buffer size.
   *
   * @exception SocketException If an error occurs or Socket is not connected
   * @exception IllegalArgumentException If size is 0 or negative
   *
   * @since 1.2
   */
  public void setReceiveBufferSize(int size) throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    if (size <= 0)
      throw new IllegalArgumentException("SO_RCVBUF value must be > 0");

    getImpl().setOption(SocketOptions.SO_RCVBUF, Integer.valueOf(size));
  }

  /**
   * This method returns the value of the system level socket option
   * SO_RCVBUF, which is used by the operating system to tune buffer
   * sizes for data transfers.
   *
   * @return The receive buffer size.
   *
   * @exception SocketException If an error occurs or Socket is not connected
   *
   * @since 1.2
   */
  public int getReceiveBufferSize() throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    Object buf = getImpl().getOption(SocketOptions.SO_RCVBUF);

    if (buf instanceof Integer)
      return (((Integer) buf).intValue());
    else
      throw new SocketException("Internal Error: Unexpected type");
  }

  /**
   * This method sets the value for the socket level socket option
   * SO_KEEPALIVE.
   *
   * @param on True if SO_KEEPALIVE should be enabled
   *
   * @exception SocketException If an error occurs or Socket is not connected
   *
   * @since 1.3
   */
  public void setKeepAlive(boolean on) throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    getImpl().setOption(SocketOptions.SO_KEEPALIVE, Boolean.valueOf(on));
  }

  /**
   * This method returns the value of the socket level socket option
   * SO_KEEPALIVE.
   *
   * @return The setting
   *
   * @exception SocketException If an error occurs or Socket is not connected
   *
   * @since 1.3
   */
  public boolean getKeepAlive() throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    Object buf = getImpl().getOption(SocketOptions.SO_KEEPALIVE);

    if (buf instanceof Boolean)
      return (((Boolean) buf).booleanValue());
    else
      throw new SocketException("Internal Error: Unexpected type");
  }

  /**
   * Closes the socket.
   *
   * @exception IOException If an error occurs
   */
  public synchronized void close() throws IOException
  {
    if (isClosed())
      return;

    impl.close();
    impl = null;
  }

  /**
   * Converts this <code>Socket</code> to a <code>String</code>.
   *
   * @return The <code>String</code> representation of this <code>Socket</code>
   */
  public String toString()
  {
    try
      {
        if (isConnected())
          return (super.toString()
                  + " [addr=" + getImpl().getInetAddress() + ",port="
                  + getImpl().getPort() + ",localport="
                  + getImpl().getLocalPort() + "]");
      }
    catch (SocketException e)
      {
        // This cannot happen as we are connected.
      }

    return super.toString() + " [unconnected]";
  }

  /**
   * Sets the <code>SocketImplFactory</code>.  This may be done only once per
   * virtual machine.  Subsequent attempts will generate a
   * <code>SocketException</code>.  Note that a <code>SecurityManager</code>
   * check is made prior to setting the factory.  If
   * insufficient privileges exist to set the factory, then an
   * <code>IOException</code> will be thrown.
   *
   * @param fac the factory to set
   *
   * @exception SecurityException If the <code>SecurityManager</code> does
   * not allow this operation.
   * @exception SocketException If the SocketImplFactory is already defined
   * @exception IOException If any other error occurs
   */
  public static synchronized void setSocketImplFactory(SocketImplFactory fac)
    throws IOException
  {
    // See if already set
    if (factory != null)
      throw new SocketException("SocketImplFactory already defined");

    // Check permissions
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSetFactory();

    if (fac == null)
      throw new SocketException("SocketImplFactory cannot be null");

    factory = fac;
  }

  /**
   * Closes the input side of the socket stream.
   *
   * @exception IOException If an error occurs.
   *
   * @since 1.3
   */
  public void shutdownInput() throws IOException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    getImpl().shutdownInput();
    inputShutdown = true;
  }

  /**
   * Closes the output side of the socket stream.
   *
   * @exception IOException If an error occurs.
   *
   * @since 1.3
   */
  public void shutdownOutput() throws IOException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    getImpl().shutdownOutput();
    outputShutdown = true;
  }

  /**
   * Returns the socket channel associated with this socket.
   *
   * @return the associated socket channel,
   * null if no associated channel exists
   *
   * @since 1.4
   */
  public SocketChannel getChannel()
  {
    return null;
  }

  /**
   * Checks if the SO_REUSEADDR option is enabled
   *
   * @return True if SO_REUSEADDR is set, false otherwise.
   *
   * @exception SocketException If an error occurs
   *
   * @since 1.4
   */
  public boolean getReuseAddress() throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    Object reuseaddr = getImpl().getOption(SocketOptions.SO_REUSEADDR);

    if (! (reuseaddr instanceof Boolean))
      throw new SocketException("Internal Error");

    return ((Boolean) reuseaddr).booleanValue();
  }

  /**
   * Enables/Disables the SO_REUSEADDR option
   *
   * @param reuseAddress true if SO_REUSEADDR should be enabled,
   * false otherwise
   *
   * @exception SocketException If an error occurs
   *
   * @since 1.4
   */
  public void setReuseAddress(boolean reuseAddress) throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    getImpl().setOption(SocketOptions.SO_REUSEADDR,
                        Boolean.valueOf(reuseAddress));
  }

  /**
   * Returns the current traffic class
   *
   * @return The current traffic class.
   *
   * @exception SocketException If an error occurs
   *
   * @see Socket#setTrafficClass(int tc)
   *
   * @since 1.4
   */
  public int getTrafficClass() throws SocketException
  {
    if (isClosed())
      throw new SocketException("socket is closed");

    Object obj = getImpl().getOption(SocketOptions.IP_TOS);

    if (obj instanceof Integer)
      return ((Integer) obj).intValue();
    else
      throw new SocketException("Unexpected type");
  }

  /**
   * Sets the traffic class value
   *
   * @param tc The traffic class
   *
   * @exception SocketException If an error occurs
   * @exception IllegalArgumentException If tc value is illegal
   *
   * @see Socket#getTrafficClass()
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
   * Checks if the socket is connected
   *
   * @return True if socket is connected, false otherwise.
   *
   * @since 1.4
   */
  public boolean isConnected()
  {
    if (impl == null)
      return false;

    return impl.getInetAddress() != null;
  }

  /**
   * Checks if the socket is already bound.
   *
   * @return True if socket is bound, false otherwise.
   *
   * @since 1.4
   */
  public boolean isBound()
  {
    if (isClosed())
      return false;
    if (impl instanceof PlainSocketImpl)
      {
        InetSocketAddress addr = ((PlainSocketImpl) impl).getLocalAddress();
        return addr != null && addr.getAddress() != null;
      }
    return bound;
  }

  /**
   * Checks if the socket is closed.
   *
   * @return True if socket is closed, false otherwise.
   *
   * @since 1.4
   */
  public boolean isClosed()
  {
    SocketChannel channel = getChannel();

    return impl == null || (channel != null && ! channel.isOpen());
  }

  /**
   * Checks if the socket's input stream is shutdown
   *
   * @return True if input is shut down.
   *
   * @since 1.4
   */
  public boolean isInputShutdown()
  {
    return inputShutdown;
  }

  /**
   * Checks if the socket's output stream is shutdown
   *
   * @return True if output is shut down.
   *
   * @since 1.4
   */
  public boolean isOutputShutdown()
  {
    return outputShutdown;
  }
}
