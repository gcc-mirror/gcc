/* ServerSocket.java -- Class for implementing server side sockets
   Copyright (C) 1998, 1999, 2000, 2002, 2003, 2004
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

import gnu.java.net.PlainSocketImpl;

import java.io.IOException;
import java.nio.channels.IllegalBlockingModeException;
import java.nio.channels.ServerSocketChannel;


/* Written using on-line Java Platform 1.2 API Specification.
 * Status:  I believe all methods are implemented.
 */

/**
 * This class models server side sockets.  The basic model is that the
 * server socket is created and bound to some well known port.  It then
 * listens for and accepts connections.  At that point the client and
 * server sockets are ready to communicate with one another utilizing
 * whatever application layer protocol they desire.
 *
 * As with the <code>Socket</code> class, most instance methods of this class
 * simply redirect their calls to an implementation class.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Per Bothner (bothner@cygnus.com)
 */
public class ServerSocket
{
  /**
   * This is the user defined SocketImplFactory, if one is supplied
   */
  private static SocketImplFactory factory;

  /**
   * This is the SocketImp object to which most instance methods in this
   * class are redirected
   */
  private SocketImpl impl;

  /**
   * True if socket is bound.
   */
  private boolean bound;

  /*
   * This constructor is only used by java.nio.
   */

  // FIXME: Workaround a bug in gcj.
  //ServerSocket (PlainSocketImpl impl) throws IOException
  ServerSocket(SocketImpl impl) throws IOException
  {
    if (impl == null)
      throw new NullPointerException("impl may not be null");

    this.impl = impl;
    this.impl.create(true);
  }

  /*
   * This method is only used by java.nio.
   */

  // FIXME: Workaround a bug in gcj.
  //PlainSocketImpl getImpl()
  SocketImpl getImpl()
  {
    return impl;
  }

  /**
   * Constructor that simply sets the implementation.
   *
   * @exception IOException If an error occurs
   *
   * @specnote This constructor is public since JDK 1.4
   */
  public ServerSocket() throws IOException
  {
    if (factory != null)
      impl = factory.createSocketImpl();
    else
      impl = new PlainSocketImpl();

    impl.create(true);
  }

  /**
   * Creates a server socket and binds it to the specified port.  If the
   * port number is 0, a random free port will be chosen.  The pending
   * connection queue on this socket will be set to 50.
   *
   * @param port The port number to bind to
   *
   * @exception IOException If an error occurs
   * @exception SecurityException If a security manager exists and its
   * checkListen method doesn't allow the operation
   */
  public ServerSocket(int port) throws IOException
  {
    this(port, 50);
  }

  /**
   * Creates a server socket and binds it to the specified port.  If the
   * port number is 0, a random free port will be chosen.  The pending
   * connection queue on this socket will be set to the value passed as
   * arg2.
   *
   * @param port The port number to bind to
   * @param backlog The length of the pending connection queue
   *
   * @exception IOException If an error occurs
   * @exception SecurityException If a security manager exists and its
   * checkListen method doesn't allow the operation
   */
  public ServerSocket(int port, int backlog) throws IOException
  {
    this(port, backlog, null);
  }

  /**
   * Creates a server socket and binds it to the specified port.  If the
   * port number is 0, a random free port will be chosen.  The pending
   * connection queue on this socket will be set to the value passed as
   * backlog.  The third argument specifies a particular local address to
   * bind t or null to bind to all local address.
   *
   * @param port The port number to bind to
   * @param backlog The length of the pending connection queue
   * @param bindAddr The address to bind to, or null to bind to all addresses
   *
   * @exception IOException If an error occurs
   * @exception SecurityException If a security manager exists and its
   * checkListen method doesn't allow the operation
   *
   * @since 1.1
   */
  public ServerSocket(int port, int backlog, InetAddress bindAddr)
    throws IOException
  {
    this();

    // bind/listen socket
    bind(new InetSocketAddress(bindAddr, port), backlog);
  }

  /**
   * Binds the server socket to a specified socket address
   *
   * @param endpoint The socket address to bind to
   *
   * @exception IOException If an error occurs
   * @exception IllegalArgumentException If address type is not supported
   * @exception SecurityException If a security manager exists and its
   * checkListen method doesn't allow the operation
   *
   * @since 1.4
   */
  public void bind(SocketAddress endpoint) throws IOException
  {
    bind(endpoint, 50);
  }

  /**
   * Binds the server socket to a specified socket address
   *
   * @param endpoint The socket address to bind to
   * @param backlog The length of the pending connection queue
   *
   * @exception IOException If an error occurs
   * @exception IllegalArgumentException If address type is not supported
   * @exception SecurityException If a security manager exists and its
   * checkListen method doesn't allow the operation
   *
   * @since 1.4
   */
  public void bind(SocketAddress endpoint, int backlog)
    throws IOException
  {
    if (isClosed())
      throw new SocketException("ServerSocket is closed");

    if (! (endpoint instanceof InetSocketAddress))
      throw new IllegalArgumentException("Address type not supported");

    InetSocketAddress tmp = (InetSocketAddress) endpoint;

    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkListen(tmp.getPort());

    InetAddress addr = tmp.getAddress();

    // Initialize addr with 0.0.0.0.
    if (addr == null)
      addr = InetAddress.ANY_IF;

    try
      {
	impl.bind(addr, tmp.getPort());
	impl.listen(backlog);
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
   * This method returns the local address to which this socket is bound
   *
   * @return The socket's local address
   */
  public InetAddress getInetAddress()
  {
    if (! isBound())
      return null;

    try
      {
	return (InetAddress) impl.getOption(SocketOptions.SO_BINDADDR);
      }
    catch (SocketException e)
      {
	// This never happens as we are bound.
	return null;
      }
  }

  /**
   * This method returns the local port number to which this socket is bound
   *
   * @return The socket's port number
   */
  public int getLocalPort()
  {
    if (! isBound())
      return -1;

    return impl.getLocalPort();
  }

  /**
   * Returns the local socket address
   *
   * @return the local socket address, null if not bound
   * 
   * @since 1.4
   */
  public SocketAddress getLocalSocketAddress()
  {
    if (! isBound())
      return null;

    return new InetSocketAddress(getInetAddress(), getLocalPort());
  }

  /**
   * Accepts a new connection and returns a connected <code>Socket</code>
   * instance representing that connection.  This method will block until a
   * connection is available.
   *
   * @return socket object for the just accepted connection
   *
   * @exception IOException If an error occurs
   * @exception SecurityException If a security manager exists and its
   * checkListen method doesn't allow the operation
   * @exception IllegalBlockingModeException If this socket has an associated
   * channel, and the channel is in non-blocking mode
   * @exception SocketTimeoutException If a timeout was previously set with
   * setSoTimeout and the timeout has been reached
   */
  public Socket accept() throws IOException
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkListen(impl.getLocalPort());

    Socket socket = new Socket();

    try
      {
	implAccept(socket);
      }
    catch (IOException e)
      {
	try
	  {
	    socket.close();
	  }
	catch (IOException e2)
	  {
	    // Ignore.
	  }

	throw e;
      }

    return socket;
  }

  /**
   * This protected method is used to help subclasses override
   * <code>ServerSocket.accept()</code>.  The passed in socket will be
   * connected when this method returns.
   *
   * @param socket The socket that is used for the accepted connection
   *
   * @exception IOException If an error occurs
   * @exception IllegalBlockingModeException If this socket has an associated
   * channel, and the channel is in non-blocking mode
   *
   * @since 1.1
   */
  protected final void implAccept(Socket socket) throws IOException
  {
    if (isClosed())
      throw new SocketException("ServerSocket is closed");

    // The Sun spec says that if we have an associated channel and
    // it is in non-blocking mode, we throw an IllegalBlockingModeException.
    // However, in our implementation if the channel itself initiated this
    // operation, then we must honor it regardless of its blocking mode.
    if (getChannel() != null && ! getChannel().isBlocking()
        && ! ((PlainSocketImpl) getImpl()).isInChannelOperation())
      throw new IllegalBlockingModeException();

    impl.accept(socket.impl);
    socket.implCreated = true;
  }

  /**
   * Closes this socket and stops listening for connections
   *
   * @exception IOException If an error occurs
   */
  public void close() throws IOException
  {
    if (isClosed())
      return;

    impl.close();
    impl = null;
    bound = false;

    if (getChannel() != null)
      getChannel().close();
  }

  /**
   * Returns the unique ServerSocketChannel object
   * associated with this socket, if any.
   *
   * The socket only has a ServerSocketChannel if its created
   * by ServerSocketChannel.open.
   *
   * @return the associated socket channel, null if none exists
   * 
   * @since 1.4
   */
  public ServerSocketChannel getChannel()
  {
    return null;
  }

  /**
   * Returns true when the socket is bound, otherwise false
   *
   * @return true if socket is bound, false otherwise
   * 
   * @since 1.4
   */
  public boolean isBound()
  {
    return bound;
  }

  /**
   * Returns true if the socket is closed, otherwise false
   *
   * @return true if socket is closed, false otherwise
   * 
   * @since 1.4
   */
  public boolean isClosed()
  {
    return impl == null;
  }

  /**
   * Sets the value of SO_TIMEOUT.  A value of 0 implies that SO_TIMEOUT is
   * disabled (ie, operations never time out).  This is the number of
   * milliseconds a socket operation can block before an
   * InterruptedIOException is thrown.
   *
   * @param timeout The new SO_TIMEOUT value
   *
   * @exception SocketException If an error occurs
   *
   * @since 1.1
   */
  public void setSoTimeout(int timeout) throws SocketException
  {
    if (isClosed())
      throw new SocketException("ServerSocket is closed");

    if (timeout < 0)
      throw new IllegalArgumentException("SO_TIMEOUT value must be >= 0");

    impl.setOption(SocketOptions.SO_TIMEOUT, new Integer(timeout));
  }

  /**
   * Retrieves the current value of the SO_TIMEOUT setting.  A value of 0
   * implies that SO_TIMEOUT is disabled (ie, operations never time out).
   * This is the number of milliseconds a socket operation can block before
   * an InterruptedIOException is thrown.
   *
   * @return The value of SO_TIMEOUT
   *
   * @exception IOException If an error occurs
   *
   * @since 1.1
   */
  public int getSoTimeout() throws IOException
  {
    if (isClosed())
      throw new SocketException("ServerSocket is closed");

    Object timeout = impl.getOption(SocketOptions.SO_TIMEOUT);

    if (! (timeout instanceof Integer))
      throw new IOException("Internal Error");

    return ((Integer) timeout).intValue();
  }

  /**
   * Enables/Disables the SO_REUSEADDR option
   *
   * @param on true if SO_REUSEADDR should be enabled, false otherwise
   * 
   * @exception SocketException If an error occurs
   *
   * @since 1.4
   */
  public void setReuseAddress(boolean on) throws SocketException
  {
    if (isClosed())
      throw new SocketException("ServerSocket is closed");

    impl.setOption(SocketOptions.SO_REUSEADDR, Boolean.valueOf(on));
  }

  /**
   * Checks if the SO_REUSEADDR option is enabled
   *
   * @return true if SO_REUSEADDR is set, false otherwise
   *
   * @exception SocketException If an error occurs
   *
   * @since 1.4
   */
  public boolean getReuseAddress() throws SocketException
  {
    if (isClosed())
      throw new SocketException("ServerSocket is closed");

    Object reuseaddr = impl.getOption(SocketOptions.SO_REUSEADDR);

    if (! (reuseaddr instanceof Boolean))
      throw new SocketException("Internal Error");

    return ((Boolean) reuseaddr).booleanValue();
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
   * @since 1.4
   */
  public void setReceiveBufferSize(int size) throws SocketException
  {
    if (isClosed())
      throw new SocketException("ServerSocket is closed");

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
   * @since 1.4
   */
  public int getReceiveBufferSize() throws SocketException
  {
    if (isClosed())
      throw new SocketException("ServerSocket is closed");

    Object buf = impl.getOption(SocketOptions.SO_RCVBUF);

    if (! (buf instanceof Integer))
      throw new SocketException("Internal Error: Unexpected type");

    return ((Integer) buf).intValue();
  }

  /**
   * Returns the value of this socket as a <code>String</code>.
   *
   * @return This socket represented as a <code>String</code>.
   */
  public String toString()
  {
    if (! isBound())
      return "ServerSocket[unbound]";

    return ("ServerSocket[addr=" + getInetAddress() + ",port="
           + impl.getPort() + ",localport=" + impl.getLocalPort() + "]");
  }

  /**
   * Sets the <code>SocketImplFactory</code> for all
   * <code>ServerSocket</code>'s.  This may only be done
   * once per virtual machine.  Subsequent attempts will generate an
   * exception.  Note that a <code>SecurityManager</code> check is made prior
   * to setting the factory.  If insufficient privileges exist to set the
   * factory, an exception will be thrown
   *
   * @param fac the factory to set
   *
   * @exception SecurityException If this operation is not allowed by the
   * <code>SecurityManager</code>.
   * @exception SocketException If the factory object is already defined
   * @exception IOException If any other error occurs
   */
  public static synchronized void setSocketFactory(SocketImplFactory fac)
    throws IOException
  {
    factory = fac;
  }
}
