/* Socket.java -- Client socket implementation
   Copyright (C) 1998, 1999, 2000, 2002 Free Software Foundation, Inc.

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

import java.io.*;
import java.nio.channels.SocketChannel;
import java.nio.channels.IllegalBlockingModeException;

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

  // Class Variables

  /**
   * This is the user SocketImplFactory for this class.  If this variable is
   * null, a default factory is used.
   */
  static SocketImplFactory factory;

  // Instance Variables

  /**
   * The implementation object to which calls are redirected
   */
  SocketImpl impl;

  private boolean inputShutdown;
  private boolean outputShutdown;

  SocketChannel ch; // this field must have been set if created by SocketChannel

  private boolean closed = false;

  // Constructors

  /**
   * Initializes a new instance of <code>Socket</code> object without 
   * connecting to a remote host.  This useful for subclasses of socket that 
   * might want this behavior.
   *
   * @specnote This constructor is public since JDK 1.4
   * @since 1.1
   */
  public Socket ()
  {
    if (factory != null)
      impl = factory.createSocketImpl();
    else
      impl = new PlainSocketImpl();

    inputShutdown = false;
    outputShutdown = false;
  }

  /**
   * Initializes a new instance of <code>Socket</code> object without
   * connecting to a remote host.  This is useful for subclasses of socket
   * that might want this behavior.  
   * <p>
   * Additionally, this socket will be created using the supplied
   * implementation class instead the default class or one returned by a
   * factory.  This value can be <code>null</code>, but if it is, all instance
   * methods in <code>Socket</code> should be overridden because most of them
   * rely on this value being populated.
   *
   * @param impl The <code>SocketImpl</code> to use for this
   *             <code>Socket</code>
   *
   * @exception SocketException If an error occurs
   *
   * @since 1.1
   */
  protected Socket (SocketImpl impl) throws SocketException
  {
    this.impl = impl;
    this.inputShutdown = false;
    this.outputShutdown = false;
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
  public Socket (String host, int port)
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
  public Socket (InetAddress address, int port)
    throws IOException 
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
   * @param loadAddr The local address to bind to.
   * @param localPort The local port to bind to.
   *
   * @exception SecurityException If the <code>SecurityManager</code>
   * exists and does not allow a connection to the specified host/port or
   * binding to the specified local host/port.
   * @exception IOException If a connection error occurs.
   *
   * @since 1.1
   */
  public Socket (String host, int port,
		 InetAddress localAddr, int localPort) throws IOException
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
  public Socket (InetAddress address, int port,
		 InetAddress localAddr, int localPort) throws IOException
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
  public Socket (String host, int port, boolean stream) throws IOException
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
  public Socket (InetAddress host, int port, boolean stream) throws IOException
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
    this.inputShutdown = false;
    this.outputShutdown = false;

    if (impl == null)
      throw new IOException("Cannot initialize Socket implementation");

    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkConnect(raddr.getHostName(), rport);

    impl.create(stream);

    // FIXME: JCL p. 1586 says if localPort is unspecified, bind to any port,
    // i.e. '0' and if localAddr is unspecified, use getLocalAddress() as
    // that default.  JDK 1.2 doc infers not to do a bind.
    if (laddr != null)
      impl.bind(laddr, lport);

    if (raddr != null)
      impl.connect(raddr, rport);
  }

  /**
   * Binds the socket to the givent local address/port
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
  public void bind (SocketAddress bindpoint) throws IOException
  {
    if (closed)
      throw new SocketException ("Socket is closed");
    
    if ( !(bindpoint instanceof InetSocketAddress))
      throw new IllegalArgumentException ();

    InetSocketAddress tmp = (InetSocketAddress) bindpoint;
    impl.bind (tmp.getAddress(), tmp.getPort());
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
  public void connect (SocketAddress endpoint)
    throws IOException
  {
    if (closed)
      throw new SocketException ("Socket is closed");
    
    if (! (endpoint instanceof InetSocketAddress))
      throw new IllegalArgumentException ("Address type not supported");

    if (ch != null && !ch.isBlocking ())
      throw new IllegalBlockingModeException ();
    
    impl.connect (endpoint, 0);
  }

  /**
   * Connects the socket with a remote address. A timeout of zero is
   * interpreted as an infinite timeout. The connection will then block
   * until established or an error occurs.
   *
   * @param endpoint The address to connect to
   *
   * @exception IOException If an error occurs
   * @exception IllegalArgumentException If the address type is not supported
   * @exception IllegalBlockingModeException If this socket has an associated
   * channel, and the channel is in non-blocking mode
   * @exception SocketTimeoutException If the timeout is reached
   * 
   * @since 1.4
   */
  public void connect (SocketAddress endpoint, int timeout)
    throws IOException
  {
    if (closed)
      throw new SocketException ("Socket is closed");
    
    if (! (endpoint instanceof InetSocketAddress))
      throw new IllegalArgumentException ("Address type not supported");

    if (ch != null && !ch.isBlocking ())
      throw new IllegalBlockingModeException ();
    
    impl.connect (endpoint, timeout);
  }

  /**
   * Returns the address of the remote end of the socket.  If this socket
   * is not connected, then <code>null</code> is returned.
   *
   * @return The remote address this socket is connected to
   */
  public InetAddress getInetAddress ()
  {
    if (impl != null)
      return impl.getInetAddress();

    return null;
  }

  /**
   * Returns the local address to which this socket is bound.  If this socket
   * is not connected, then <code>null</code> is returned.
   *
   * @return The local address
   *
   * @since 1.1
   */
  public InetAddress getLocalAddress ()
  {
    if (impl == null)
      return null;

    InetAddress addr = null;
    try
      {
        addr = (InetAddress)impl.getOption(SocketOptions.SO_BINDADDR);
      }
    catch(SocketException e)
      {
        // (hopefully) shouldn't happen
        // throw new java.lang.InternalError
        //      ("Error in PlainSocketImpl.getOption");
        return null;
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
   * this socket is not connected, then -1 is returned.
   *
   * @return The remote port this socket is connected to
   */
  public int getPort ()
  {
    if (impl != null)
      return impl.getPort();

    return -1;
  }

  /**
   * Returns the local port number to which this socket is bound.  If this
   * socket is not connected, then -1 is returned.
   *
   * @return The local port
   */
  public int getLocalPort ()
  {
    if (impl != null)
      return impl.getLocalPort();

    return -1;
  }

  /**
   * If the socket is already bound this returns the local SocketAddress,
   * otherwise null
   *
   * @since 1.4
   */
  public SocketAddress getLocalSocketAddress()
  {
    InetAddress addr = getLocalAddress ();

    if (addr == null)
      return null;
    
    return new InetSocketAddress (addr, impl.getLocalPort());
  }

  /**
   * If the socket is already connected this returns the remote SocketAddress,
   * otherwise null
   *
   * @since 1.4
   */
  public SocketAddress getRemoteSocketAddress()
  {
    if (!isConnected ())
      return null;

    return new InetSocketAddress (impl.getInetAddress (), impl.getPort ());
  }

  /**
   * Returns an InputStream for reading from this socket.
   *
   * @return The InputStream object
   *
   * @exception IOException If an error occurs or Socket is not connected
   */
  public InputStream getInputStream () throws IOException
  {
    if (impl != null)
      return(impl.getInputStream());

    throw new IOException("Not connected");
  }

  /**
   * Returns an OutputStream for writing to this socket.
   *
   * @return The OutputStream object
   *
   * @exception IOException If an error occurs or Socket is not connected
   */
  public OutputStream getOutputStream () throws IOException
  {
    if (impl != null)
      return impl.getOutputStream();

    throw new IOException("Not connected");
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
  public void setTcpNoDelay (boolean on)  throws SocketException
  {
    if (impl == null)
      throw new SocketException("Not connected");

    impl.setOption(SocketOptions.TCP_NODELAY, new Boolean(on));
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
    if (impl == null)
      throw new SocketException("Not connected");

    Object on = impl.getOption(SocketOptions.TCP_NODELAY);
  
    if (on instanceof Boolean)
      return(((Boolean)on).booleanValue());
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
    if (impl == null)
      throw new SocketException("No socket created");

    if (on == true)
      {
        if (linger < 0)
          throw new IllegalArgumentException("SO_LINGER must be >= 0");

        if (linger > 65535)
          linger = 65535;

        impl.setOption(SocketOptions.SO_LINGER, new Integer(linger));
      }
    else
      {
        impl.setOption(SocketOptions.SO_LINGER, new Boolean(false));
      }
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
    if (impl == null)
      throw new SocketException("Not connected");

    Object linger = impl.getOption(SocketOptions.SO_LINGER);
    if (linger instanceof Integer)
      return(((Integer)linger).intValue());
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
  public void sendUrgentData (int data) throws IOException
  {
    impl.sendUrgentData (data);
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
  public void setOOBInline (boolean on) throws SocketException
  {
    if (impl == null)
      throw new SocketException("Not connected");

    impl.setOption(SocketOptions.SO_OOBINLINE, new Boolean(on));
  }

  /**
   * Returns the current setting of the SO_OOBINLINE option for this socket
   * 
   * @exception SocketException If an error occurs
   * 
   * @since 1.4
   */
  public boolean getOOBInline () throws SocketException
  {
    if (impl == null)
      throw new SocketException("Not connected");

    Object buf = impl.getOption(SocketOptions.SO_OOBINLINE);

    if (buf instanceof Boolean)
      return(((Boolean)buf).booleanValue());
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
   * value is in thousandths of a second (****????*****)
   *
   * @param timeout The length of the timeout in thousandth's of a second or 
   * 0 if not set
   *
   * @exception SocketException If an error occurs or Socket not connected
   *
   * @since 1.1
   */
  public synchronized void setSoTimeout (int timeout) throws SocketException
  {
    if (impl == null)
      throw new SocketException("Not connected");
    
    if (timeout < 0)
      throw new IllegalArgumentException("SO_TIMEOUT value must be >= 0");
      
    impl.setOption(SocketOptions.SO_TIMEOUT, new Integer(timeout));
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
  public synchronized int getSoTimeout () throws SocketException
  {
    if (impl == null) 
      throw new SocketException("Not connected");

    Object timeout = impl.getOption(SocketOptions.SO_TIMEOUT);
    if (timeout instanceof Integer)
      return(((Integer)timeout).intValue());
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
  public void setSendBufferSize (int size) throws SocketException
  {
    if (impl == null)
      throw new SocketException("Not connected");
    
    if (size <= 0)
      throw new IllegalArgumentException("SO_SNDBUF value must be > 0");
    
    impl.setOption(SocketOptions.SO_SNDBUF, new Integer(size));
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
  public int getSendBufferSize () throws SocketException
  {
    if (impl == null)
      throw new SocketException("Not connected");

    Object buf = impl.getOption(SocketOptions.SO_SNDBUF);

    if (buf instanceof Integer)
      return(((Integer)buf).intValue());
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
  public void setReceiveBufferSize (int size) throws SocketException
  {
    if (impl == null)
      throw new SocketException("Not connected");

    if (size <= 0)
      throw new IllegalArgumentException("SO_RCVBUF value must be > 0");
      
    impl.setOption(SocketOptions.SO_RCVBUF, new Integer(size));
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
  public int getReceiveBufferSize () throws SocketException
  {
    if (impl == null)
      throw new SocketException("Not connected");

    Object buf = impl.getOption(SocketOptions.SO_RCVBUF);

    if (buf instanceof Integer)
      return(((Integer)buf).intValue());
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
  public void setKeepAlive (boolean on) throws SocketException
  {
    if (impl == null)
      throw new SocketException("Not connected");

    impl.setOption(SocketOptions.SO_KEEPALIVE, new Boolean(on));
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
  public boolean getKeepAlive () throws SocketException
  {
    if (impl == null)
      throw new SocketException("Not connected");

    Object buf = impl.getOption(SocketOptions.SO_KEEPALIVE);

    if (buf instanceof Boolean)
      return(((Boolean)buf).booleanValue());
    else
      throw new SocketException("Internal Error: Unexpected type");
  }

  /**
   * Closes the socket.
   *
   * @exception IOException If an error occurs
   */
  public synchronized void close ()  throws IOException
  {
    if (impl != null)
      impl.close();

    if (ch != null)
      ch.close();
    
    closed = true;
  }

  /**
   * Converts this <code>Socket</code> to a <code>String</code>.
   *
   * @return The <code>String</code> representation of this <code>Socket</code>
   */
  public String toString ()
  {
    return("Socket " + impl);
  }

  // Class Methods

  /**
   * Sets the <code>SocketImplFactory</code>.  This may be done only once per 
   * virtual machine.  Subsequent attempts will generate a 
   * <code>SocketException</code>.  Note that a <code>SecurityManager</code>
   * check is made prior to setting the factory.  If 
   * insufficient privileges exist to set the factory, then an 
   * <code>IOException</code> will be thrown.
   *
   * @exception SecurityException If the <code>SecurityManager</code> does
   * not allow this operation.
   * @exception SocketException If the SocketImplFactory is already defined
   * @exception IOException If any other error occurs
   */
  public static synchronized void setSocketImplFactory (SocketImplFactory fac)
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
    if (impl != null)
      impl.shutdownInput();

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
    if (impl != null)
      impl.shutdownOutput();
    
    outputShutdown = true;
  }

  /**
   * Returns the socket channel associated with this socket.
   *
   * It returns null if no associated socket exists.
   *
   * @since 1.4
   */
  public SocketChannel getChannel()
  {
    return ch;
  }

  /**
   * Checks if the SO_REUSEADDR option is enabled
   *
   * @exception SocketException If an error occurs
   *
   * @since 1.4
   */
  public boolean getReuseAddress () throws SocketException
  {
    if (impl == null)
      throw new SocketException ("Cannot initialize Socket implementation");

    Object reuseaddr = impl.getOption (SocketOptions.SO_REUSEADDR);

    if (!(reuseaddr instanceof Boolean))
      throw new SocketException ("Internal Error");

    return ((Boolean) reuseaddr).booleanValue ();
  }

  /**
   * Enables/Disables the SO_REUSEADDR option
   *
   * @exception SocketException If an error occurs
   *
   * @since 1.4
   */
  public void setReuseAddress (boolean on) throws SocketException
  {
    if (impl == null)
      throw new SocketException ("Cannot initialize Socket implementation");

    impl.setOption (SocketOptions.SO_REUSEADDR, new Boolean (on));
  }

  /**
   * Returns the current traffic class
   *
   * @exception SocketException If an error occurs
   *
   * @see Socket:setTrafficClass
   *
   * @since 1.4
   */
  public int getTrafficClass () throws SocketException
  {
    if (impl == null)
      throw new SocketException ("Cannot initialize Socket implementation");

    Object obj = impl.getOption(SocketOptions.IP_TOS);

    if (obj instanceof Integer)
      return ((Integer) obj).intValue ();
    else
      throw new SocketException ("Unexpected type");
  }

  /**
   * Sets the traffic class value
   *
   * @param tc The traffic class
   *
   * @exception SocketException If an error occurs
   * @exception IllegalArgumentException If tc value is illegal
   *
   * @see Socket:getTrafficClass
   *
   * @since 1.4
   */
  public void setTrafficClass (int tc) throws SocketException
  {
    if (impl == null)
      throw new SocketException ("Cannot initialize Socket implementation");

    if (tc < 0 || tc > 255)
      throw new IllegalArgumentException();

    impl.setOption (SocketOptions.IP_TOS, new Integer (tc));
  }

  /**
   * Checks if the socket is connected
   *
   * @since 1.4
   */
  public boolean isConnected ()
  {
    return impl.getInetAddress () != null;
  }

  /**
   * Checks if the socket is already bound.
   *
   * @since 1.4
   */
  public boolean isBound ()
  {
    return getLocalAddress () != null;
  }

  /**
   * Checks if the socket is closed.
   * 
   * @since 1.4
   */
  public boolean isClosed ()
  {
    return closed;
  }

  /**
   * Checks if the socket's input stream is shutdown
   *
   * @since 1.4
   */
  public boolean isInputShutdown ()
  {
    return inputShutdown;
  }

  /**
   * Checks if the socket's output stream is shutdown
   *
   * @since 1.4
   */
  public boolean isOutputShutdown ()
  {
    return outputShutdown;
  }
}
