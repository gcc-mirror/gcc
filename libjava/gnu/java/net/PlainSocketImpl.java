/* PlainSocketImpl.java -- Default socket implementation
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
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


package gnu.java.net;

import gnu.classpath.Configuration;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.SocketException;
import java.net.SocketImpl;
import java.net.SocketOptions;

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

/**
 * Unless the application installs its own SocketImplFactory, this is the
 * default socket implemetation that will be used.  It simply uses a
 * combination of Java and native routines to implement standard BSD
 * style sockets of family AF_INET and types SOCK_STREAM and SOCK_DGRAM
 *
 * @author Per Bothner (bothner@cygnus.com)
 * @author Nic Ferrier (nferrier@tapsellferrier.co.uk)
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public final class PlainSocketImpl extends SocketImpl
{
  // Static initializer to load native library.
  static
    {
      if (Configuration.INIT_LOAD_LIBRARY)
        {
          System.loadLibrary("javanet");
        }
    }
  
  // These fields are mirrored for use in native code to avoid cpp conflicts
  // when the #defines in system header files are the same as the public fields.
  static final int _Jv_TCP_NODELAY_ = SocketOptions.TCP_NODELAY,
                   _Jv_SO_BINDADDR_ = SocketOptions.SO_BINDADDR,
                   _Jv_SO_REUSEADDR_ = SocketOptions.SO_REUSEADDR,
                   _Jv_SO_BROADCAST_ = SocketOptions.SO_BROADCAST,
                   _Jv_SO_OOBINLINE_ = SocketOptions.SO_OOBINLINE,
                   _Jv_IP_MULTICAST_IF_ = SocketOptions.IP_MULTICAST_IF,
                   _Jv_IP_MULTICAST_IF2_ = SocketOptions.IP_MULTICAST_IF2,
                   _Jv_IP_MULTICAST_LOOP_ = SocketOptions.IP_MULTICAST_LOOP,
                   _Jv_IP_TOS_ = SocketOptions.IP_TOS,
                   _Jv_SO_LINGER_ = SocketOptions.SO_LINGER,
                   _Jv_SO_TIMEOUT_ = SocketOptions.SO_TIMEOUT,
                   _Jv_SO_SNDBUF_ = SocketOptions.SO_SNDBUF,
                   _Jv_SO_RCVBUF_ = SocketOptions.SO_RCVBUF,
                   _Jv_SO_KEEPALIVE_ = SocketOptions.SO_KEEPALIVE;

  /**
   * The OS file handle representing the socket.
   * This is used for reads and writes to/from the socket and
   * to close it.
   *
   * When the socket is closed this is reset to -1.
   */
  int native_fd = -1;

  // This value is set/read by setOption/getOption.
  int timeout = 0;
  
  // localAddress cache
  InetAddress localAddress;

  /**
   * A cached copy of the in stream for reading from the socket.
   */
  private InputStream in;

  /**
   * A cached copy of the out stream for writing to the socket.
   */
  private OutputStream out;

  /**
   * Indicates whether a channel initiated whatever operation
   * is being invoked on this socket.
   */
  private boolean inChannelOperation;

  /**
   * Indicates whether we should ignore whether any associated
   * channel is set to non-blocking mode. Certain operations
   * throw an <code>IllegalBlockingModeException</code> if the
   * associated channel is in non-blocking mode, <i>except</i>
   * if the operation is invoked by the channel itself.
   */
  public final boolean isInChannelOperation()
  {
    return inChannelOperation;
  }
  
  /**
   * Sets our indicator of whether an I/O operation is being
   * initiated by a channel.
   */
  public final void setInChannelOperation(boolean b)
  {
    inChannelOperation = b;
  }
 
  /**
   * Default do nothing constructor
   */
  public PlainSocketImpl()
  {
  }
  
  protected void finalize() throws Throwable
  {
    synchronized (this)
      {
	if (native_fd != -1)
	  try
	    {
	      close();
	    }
	  catch (IOException ex)
	    {
	    }
      }
    super.finalize();
  }

  public int getNativeFD()
  {
    return native_fd;
  }

  /**
   * Sets the specified option on a socket to the passed in object.  For
   * options that take an integer argument, the passed in object is an
   * Integer.  The option_id parameter is one of the defined constants in
   * this interface.
   *
   * @param option_id The identifier of the option
   * @param val The value to set the option to
   *
   * @exception SocketException If an error occurs
   */
  public native void setOption(int optID, Object value) throws SocketException;

  /**
   * Returns the current setting of the specified option.  The Object returned
   * will be an Integer for options that have integer values.  The option_id
   * is one of the defined constants in this interface.
   *
   * @param option_id The option identifier
   *
   * @return The current value of the option
   *
   * @exception SocketException If an error occurs
   */
  public native Object getOption(int optID) throws SocketException;

  /**
   * Flushes the input stream and closes it. If you read from the input stream
   * after calling this method a <code>IOException</code> will be thrown.
   * 
   * @throws IOException if an error occurs
   */
  public native void shutdownInput() throws IOException;

  /**
   * Flushes the output stream and closes it. If you write to the output stream
   * after calling this method a <code>IOException</code> will be thrown.
   * 
   * @throws IOException if an error occurs
   */
  public native void shutdownOutput() throws IOException;

  /**
   * Creates a new socket that is not bound to any local address/port and
   * is not connected to any remote address/port.  This will be created as
   * a stream socket if the stream parameter is true, or a datagram socket
   * if the stream parameter is false.
   *
   * @param stream true for a stream socket, false for a datagram socket
   */
  protected native void create(boolean stream) throws IOException;

  /**
   * Connects to the remote hostname and port specified as arguments.
   *
   * @param hostname The remote hostname to connect to
   * @param port The remote port to connect to
   *
   * @exception IOException If an error occurs
   */
  protected void connect(String host, int port) throws IOException
  {
    connect(InetAddress.getByName(host), port);
  }

  /**
   * Connects to the remote address and port specified as arguments.
   *
   * @param addr The remote address to connect to
   * @param port The remote port to connect to
   *
   * @exception IOException If an error occurs
   */
  protected void connect(InetAddress host, int port) throws IOException
  {
    connect (new InetSocketAddress (host, port), 0);
  }

  /**
   * Connects to the remote socket address with a specified timeout.
   *
   * @param timeout The timeout to use for this connect, 0 means infinite.
   *
   * @exception IOException If an error occurs
   */
  protected native void connect(SocketAddress addr, int timeout) throws IOException;

  /**
   * Binds to the specified port on the specified addr.  Note that this addr
   * must represent a local IP address.  **** How bind to INADDR_ANY? ****
   *
   * @param addr The address to bind to
   * @param port The port number to bind to
   *
   * @exception IOException If an error occurs
   */
  protected native void bind(InetAddress host, int port)
    throws IOException;

  /**
   * Starts listening for connections on a socket. The queuelen parameter
   * is how many pending connections will queue up waiting to be serviced
   * before being accept'ed.  If the queue of pending requests exceeds this
   * number, additional connections will be refused.
   *
   * @param queuelen The length of the pending connection queue
   * 
   * @exception IOException If an error occurs
   */
  protected native void listen(int queuelen)
    throws IOException;

  /**
   * Accepts a new connection on this socket and returns in in the 
   * passed in SocketImpl.
   *
   * @param impl The SocketImpl object to accept this connection.
   */
  protected void accept(SocketImpl impl)
    throws IOException
  {
    accept((PlainSocketImpl) impl);
  }

  private native void accept(PlainSocketImpl impl)
    throws IOException;

  /**
   * Returns the number of bytes that the caller can read from this socket
   * without blocking. 
   *
   * @return The number of readable bytes before blocking
   *
   * @exception IOException If an error occurs
   */
  protected native int available() throws IOException;

  /**
   * Closes the socket.  This will cause any InputStream or OutputStream
   * objects for this Socket to be closed as well.
   * <p>
   * Note that if the SO_LINGER option is set on this socket, then the
   * operation could block.
   *
   * @exception IOException If an error occurs
   */
  protected native void close() throws IOException;

  protected native void sendUrgentData(int data) throws IOException;

  /**
   * Returns an InputStream object for reading from this socket.  This will
   * be an instance of SocketInputStream.
   *
   * @return An input stream attached to the socket.
   *
   * @exception IOException If an error occurs
   */
  protected synchronized InputStream getInputStream() throws IOException
  {
    if (in == null)
      in = new SocketInputStream();
    
    return in;
  }

  /**
   * Returns an OutputStream object for writing to this socket.  This will
   * be an instance of SocketOutputStream.
   *
   * @return An output stream attached to the socket.
   *
   * @exception IOException If an error occurs
   */
  protected synchronized OutputStream getOutputStream() throws IOException
  {
    if (out == null)
      out = new SocketOutputStream();
    
    return out;
  }

  /**
   * This class contains an implementation of <code>InputStream</code> for 
   * sockets.  It in an internal only class used by <code>PlainSocketImpl</code>.
   *
   * @author Nic Ferrier <nferrier@tapsellferrier.co.uk>
   */
  final class SocketInputStream
    extends InputStream
  {
    /**
     * Returns the number of bytes available to be read before blocking
     */
    public int available() throws IOException
    {
      return PlainSocketImpl.this.available();
    }

    /**
     * This method not only closes the stream, it closes the underlying socket
     * (and thus any connection) and invalidates any other Input/Output streams
     * for the underlying impl object
     */
    public void close() throws IOException
    {
      PlainSocketImpl.this.close();
    }

    /**
     * Reads the next byte of data and returns it as an int.  
     *
     * @return The byte read (as an int) or -1 if end of stream);
     *
     * @exception IOException If an error occurs.
     */
    public native int read() throws IOException;

    /**
     * Reads up to len bytes of data into the caller supplied buffer starting
     * at offset bytes from the start of the buffer
     *
     * @param buf The buffer
     * @param offset Offset into the buffer to start reading from
     * @param len The number of bytes to read
     *
     * @return The number of bytes actually read or -1 if end of stream
     *
     * @exception IOException If an error occurs.
     */
    public native int read(byte[] buf, int offset, int len) throws IOException;
  }

  /**
   * This class is used internally by <code>PlainSocketImpl</code> to be the 
   * <code>OutputStream</code> subclass returned by its 
   * <code>getOutputStream method</code>.  It expects only to  be used in that
   * context.
   *
   * @author Nic Ferrier  <nferrier@tapsellferrier.co.uk>
   */
  final class SocketOutputStream
    extends OutputStream
  {
    /**
     * This method closes the stream and the underlying socket connection. This
     * action also effectively closes any other InputStream or OutputStream
     * object associated with the connection.
     *
     * @exception IOException If an error occurs
     */
    public void close() throws IOException
    {
      PlainSocketImpl.this.close();
    }

    /**
     * Writes a byte (passed in as an int) to the given output stream
     * 
     * @param b The byte to write
     *
     * @exception IOException If an error occurs
     */
    public native void write(int b) throws IOException;

    /**
     * Writes len number of bytes from the array buf to the stream starting
     * at offset bytes into the buffer.
     *
     * @param buf The buffer
     * @param offset Offset into the buffer to start writing from
     * @param len The number of bytes to write
     *
     * @exception IOException If an error occurs.
     */
    public native void write(byte[] buf, int offset, int len) throws IOException;
  }
}
