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

  /**
   * The OS file handle representing the socket.
   * This is used for reads and writes to/from the socket and
   * to close it.
   *
   * When the socket is closed this is reset to -1.
   */
  int native_fd = -1;

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
  protected synchronized native void create(boolean stream) throws IOException;

  /**
   * Connects to the remote hostname and port specified as arguments.
   *
   * @param hostname The remote hostname to connect to
   * @param port The remote port to connect to
   *
   * @exception IOException If an error occurs
   */
  protected synchronized void connect(String host, int port) throws IOException
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
  protected native void connect(InetAddress addr, int port) throws IOException;

  /**
   * Connects to the remote socket address with a specified timeout.
   *
   * @param timeout The timeout to use for this connect, 0 means infinite.
   *
   * @exception IOException If an error occurs
   */
  protected synchronized void connect(SocketAddress address, int timeout) throws IOException
  {
    InetSocketAddress sockAddr = (InetSocketAddress) address;
    InetAddress addr = sockAddr.getAddress();

    if (addr == null)
      throw new IllegalArgumentException("address is unresolved: " + sockAddr);

    int port = sockAddr.getPort();
    
    if (timeout < 0)
      throw new IllegalArgumentException("negative timeout");

    Object oldTimeoutObj = null;
    
    try
      {
 	oldTimeoutObj = this.getOption (SocketOptions.SO_TIMEOUT);
 	this.setOption (SocketOptions.SO_TIMEOUT, new Integer (timeout));
 	connect (addr, port);
      }
    finally
      {
	if (oldTimeoutObj != null)
	  this.setOption (SocketOptions.SO_TIMEOUT, oldTimeoutObj);
      }
  }

  /**
   * Binds to the specified port on the specified addr.  Note that this addr
   * must represent a local IP address.  **** How bind to INADDR_ANY? ****
   *
   * @param addr The address to bind to
   * @param port The port number to bind to
   *
   * @exception IOException If an error occurs
   */
  protected synchronized native void bind(InetAddress addr, int port)
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
  protected synchronized native void listen(int queuelen)
    throws IOException;

  /**
   * Accepts a new connection on this socket and returns in in the 
   * passed in SocketImpl.
   *
   * @param impl The SocketImpl object to accept this connection.
   */
  protected synchronized native void accept(SocketImpl impl)
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

  public void sendUrgentData(int data)
  {
    throw new InternalError ("PlainSocketImpl::sendUrgentData not implemented");
  }

  /**
   * Internal method used by SocketInputStream for reading data from
   * the connection.  Reads up to len bytes of data into the buffer
   * buf starting at offset bytes into the buffer.
   *
   * @return The actual number of bytes read or -1 if end of stream.
   *
   * @exception IOException If an error occurs
   */
  protected native int read(byte[] buf, int offset, int len)
    throws IOException;

  /**
   * Internal method used by SocketOuputStream for writing data to
   * the connection.  Writes up to len bytes of data from the buffer
   * buf starting at offset bytes into the buffer.
   *
   * @exception IOException If an error occurs
   */
  protected native void write(byte[] buf, int offset, int len)
    throws IOException;

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
   * @author Nic Ferrier (nferrier@tapsellferrier.co.uk)
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
    public int read() throws IOException
    {
      byte buf[] = new byte [1];
      int bytes_read = read(buf, 0, 1);
 
      if (bytes_read == -1)
        return -1;

      return buf[0] & 0xFF;
    }

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
    public int read (byte[] buf, int offset, int len) throws IOException
    {
      int bytes_read = PlainSocketImpl.this.read (buf, offset, len);

      if (bytes_read == 0)
        return -1;

      return bytes_read;
    }
  }

  /**
   * This class is used internally by <code>PlainSocketImpl</code> to be the 
   * <code>OutputStream</code> subclass returned by its 
   * <code>getOutputStream method</code>.  It expects only to  be used in that
   * context.
   *
   * @author Nic Ferrier (nferrier@tapsellferrier.co.uk)
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
    public void write(int b) throws IOException
    {
      byte buf[] = { (byte) b };
      write(buf, 0, 1);
    }

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
    public void write (byte[] buf, int offset, int len) throws IOException
    {
      PlainSocketImpl.this.write (buf, offset, len);
    }
  }
}
