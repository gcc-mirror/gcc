/* ServerSocket.java -- Class for implementing server side sockets
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
   * ServerSocketChannel of this ServerSocket. This channel only exists
   * when the socket is created by ServerSocketChannel.open().
   */
  private ServerSocketChannel ch;

  private boolean closed = false;
  
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
  public ServerSocket (int port)
    throws IOException
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
  public ServerSocket (int port, int backlog)
    throws IOException
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
  public ServerSocket (int port, int backlog, InetAddress bindAddr)
    throws IOException
  {
    this();

    if (impl == null)
      throw new IOException("Cannot initialize Socket implementation");

    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkListen(port);

    if (bindAddr == null)
      bindAddr = InetAddress.ANY_IF;

    impl.create(true);
    impl.bind(bindAddr, port);
    impl.listen(backlog);
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
  public void bind (SocketAddress endpoint)
    throws IOException
  {
    bind (endpoint, 50);
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
  public void bind (SocketAddress endpoint, int backlog) throws IOException
  {
    if (closed)
      throw new SocketException ("ServerSocket is closed");
    
    if (impl == null)
      throw new IOException ("Cannot initialize Socket implementation");

    if (! (endpoint instanceof InetSocketAddress))
      throw new IllegalArgumentException ("Address type not supported");

    InetSocketAddress tmp = (InetSocketAddress) endpoint;
    
    SecurityManager s = System.getSecurityManager ();
    if (s != null)
      s.checkListen (tmp.getPort ());

    impl.bind (tmp.getAddress (), tmp.getPort ());
    impl.listen(backlog);
  }
  
  /**
   * This method returns the local address to which this socket is bound
   *
   * @return The socket's local address
   */
  public InetAddress getInetAddress()
  {
    try
      {
        return (InetAddress) impl.getOption (SocketOptions.SO_BINDADDR);
      }
    catch (SocketException e)
      {
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
    return impl.getLocalPort();
  }

  /**
   * Returns the local socket address
   *
   * @since 1.4
   */
  public SocketAddress getLocalSocketAddress()
  {
    InetAddress addr = getInetAddress();

    if (addr != null)
      return new InetSocketAddress (getInetAddress(), getLocalPort());

    return null;
  }

  /**
   * Accepts a new connection and returns a connected <code>Socket</code> 
   * instance representing that connection.  This method will block until a 
   * connection is available.
   *
   * @exception IOException If an error occurs
   * @exception SecurityException If a security manager exists and its
   * checkListen method doesn't allow the operation
   * @exception IllegalBlockingModeException If this socket has an associated
   * channel, and the channel is in non-blocking mode
   * @exception SocketTimeoutException If a timeout was previously set with
   * setSoTimeout and the timeout has been reached
   */
  public Socket accept () throws IOException
  {
    if (impl == null)
      throw new IOException ("Cannot initialize Socket implementation");

    SecurityManager sm = System.getSecurityManager ();
    if (sm != null)
      sm.checkListen (impl.getLocalPort ());

    Socket s = new Socket();
    implAccept (s);

    return s;
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
  protected final void implAccept (Socket s)
    throws IOException
  {
    if (ch != null && !ch.isBlocking())
      throw new IllegalBlockingModeException();
	    
    impl.accept(s.impl);
  }

  /**
   * Closes this socket and stops listening for connections
   *
   * @exception IOException If an error occurs
   */
  public void close () throws IOException
  {
    if (impl != null)
      impl.close ();

    if (ch != null)
      ch.close ();
    
    closed = true;
  }

  /**
   * Returns the unique ServerSocketChannel object
   * associated with this socket, if any.
   *
   * The socket only has a ServerSocketChannel if its created
   * by ServerSocketChannel.open.
   * 
   * @since 1.4
   */
  public ServerSocketChannel getChannel()
  {
    return ch;
  }

  /**
   * Returns true then the socket is bound, otherwise false
   * 
   * @since 1.4
   */
  public boolean isBound()
  {
    try
      {
        Object bindaddr = impl.getOption (SocketOptions.SO_BINDADDR);
      }
    catch (SocketException e)
      {
        return false;
      }
    
    return true;
  }

  /**
   * Returns true if the socket is closed, otherwise false
   * 
   * @since 1.4
   */
  public boolean isClosed()
  {
    return closed;
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
  public void setSoTimeout (int timeout) throws SocketException
  {
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
  public int getSoTimeout () throws IOException
  {
    Object timeout = impl.getOption(SocketOptions.SO_TIMEOUT);

    if (!(timeout instanceof Integer))
      throw new IOException("Internal Error");

    return ((Integer)timeout).intValue();
  }

  /**
   * Enables/Disables the SO_REUSEADDR option
   * 
   * @exception SocketException If an error occurs
   * 
   * @since 1.4
   */
  public void setReuseAddress (boolean on)
    throws SocketException
  {
    if (impl == null)
      throw new SocketException ("Cannot initialize Socket implementation");

    impl.setOption (SocketOptions.SO_REUSEADDR, new Boolean (on));
  }

  /**
   * Checks if the SO_REUSEADDR option is enabled
   * 
   * @exception SocketException If an error occurs
   * 
   * @since 1.4
   */
  public boolean getReuseAddress()
    throws SocketException
  {
    if (impl == null)
      throw new SocketException ("Cannot initialize Socket implementation");

    Object reuseaddr = impl.getOption (SocketOptions.SO_REUSEADDR);

    if (!(reuseaddr instanceof Boolean))
      throw new SocketException ("Internal Error");
    
    return ((Boolean) reuseaddr).booleanValue ();
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
  public void setReceiveBufferSize (int size)
    throws SocketException
  {
    if (impl == null)
      throw new SocketException ("Not connected");

    if (size <= 0)
      throw new IllegalArgumentException ("SO_RCVBUF value must be > 0");

    impl.setOption (SocketOptions.SO_RCVBUF, new Integer (size));
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
  public int getReceiveBufferSize ()
    throws SocketException
  {
    if (impl == null)
      throw new SocketException ("Not connected");

    Object buf = impl.getOption (SocketOptions.SO_RCVBUF);

    if (!(buf instanceof Integer))
      throw new SocketException ("Internal Error: Unexpected type");
    
    return ((Integer) buf).intValue ();
  }

  /**
   * Returns the value of this socket as a <code>String</code>. 
   *
   * @return This socket represented as a <code>String</code>.
   */
  public String toString ()
  {
    return "ServerSocket" + impl.toString();
  }

  // Class methods

  /**
   * Sets the <code>SocketImplFactory</code> for all 
   * <code>ServerSocket</code>'s.  This may only be done
   * once per virtual machine.  Subsequent attempts will generate an
   * exception.  Note that a <code>SecurityManager</code> check is made prior
   * to setting the factory.  If insufficient privileges exist to set the
   * factory, an exception will be thrown
   *
   * @exception SecurityException If this operation is not allowed by the
   * <code>SecurityManager</code>.
   * @exception SocketException If the factory object is already defined
   * @exception IOException If any other error occurs
   */
  public static synchronized void setSocketFactory (SocketImplFactory fac)
    throws IOException
  {
    factory = fac;
  }
}
