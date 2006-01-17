/* VMPlainSocketImpl.java -- VM interface for default socket implementation
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.SocketException;
import java.net.SocketImpl;
import java.net.SocketOptions;
import java.net.UnknownHostException;

import gnu.classpath.Configuration;

/**
 * The VM interface for {@link gnu.java.net.PlainSocketImpl}.
 *
 * @author Ingo Proetel (proetel@aicas.com)
 * @author Roman Kennke (kennke@aicas.com)
 */
public final class VMPlainSocketImpl
{
  /**
   * Static initializer to load native library.
   */
  static
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
        System.loadLibrary("javanet");
      }
  }

  /**
   * Sets the specified option on a socket to the passed in object.
   * The optionId parameter is one of the defined constants in
   * the SocketImpl interface.
   *
   * @param socket the socket object
   * @param optionId the identifier of the option
   * @param value the value to set the option to
   *
   * @throws SocketException if an error occurs
   */
  static native void setOption(PlainSocketImpl socket, int optionId, Object value)
    throws SocketException;

  /**
   * Returns the current setting of the specified option. The optionId
   * is one of the defined constants in this interface.
   *
   * @param socket the socket object
   * @param optionId the option identifier
   *
   * @return the current value of the option
   *
   * @throws SocketException ff an error occurs
   */
  static native Object getOption(PlainSocketImpl socket, int optionId)
    throws SocketException;

  /**
   * Creates a new socket that is not bound to any local address/port and
   * is not connected to any remote address/port.
   *
   * @param socket the socket object
   *
   * @throws IOException if something goes wrong while creating the socket
   */
  static native void create(PlainSocketImpl socket) throws IOException;

  /**
   * Connects to the remote address and port specified as arguments.
   *
   * @param socket the socket object
   * @param addr the remote address to connect to
   * @param port the remote port to connect to
   *
   * @throws IOException if an error occurs
   */
  static native void connect(PlainSocketImpl socket, InetAddress addr,
                             int port) throws IOException;

  /**
   * Binds to the specified port on the specified addr.  Note that this addr
   * must represent a local IP address.  **** How bind to INADDR_ANY? ****
   *
   * @param socket the socket object
   * @param addr the address to bind to
   * @param port the port number to bind to
   *
   * @exception IOException If an error occurs
   */
  static native void bind(PlainSocketImpl socket, InetAddress addr, int port)
    throws IOException;

  /**
   * Starts listening for connections on a socket. The queueLen parameter
   * is how many pending connections will queue up waiting to be serviced
   * before being accepted.  If the queue of pending requests exceeds this
   * number, additional connections will be refused.
   *
   * @param socket the socket object
   * @param queueLen the length of the pending connection queue
   * 
   * @exception IOException if an error occurs
   */
  static native void listen(PlainSocketImpl socket, int queueLen)
    throws IOException;

  /**
   * Accepts a new connection on this socket.
   *
   * @param socket the socket object
   * @param impl the socket object to accept this connection.
   */
  static native void accept(PlainSocketImpl socket, SocketImpl impl)
    throws IOException;

  /**
   * Returns the number of bytes that the caller can read from this socket
   * without blocking. 
   *
   * @param socket the socket object
   *
   * @return the number of readable bytes before blocking
   *
   * @throws IOException If an error occurs
   */
  static native int available(PlainSocketImpl socket) throws IOException;

  /**
   * Closes the socket.  This will cause any InputStream or OutputStream
   * objects for this Socket to be closed as well.
   *
   * <p>
   * Note that if the SO_LINGER option is set on this socket, then the
   * operation could block.
   * </p>
   *
   * @param socket the socket object
   *
   * @throws IOException if an error occurs
   */
  static native void close(PlainSocketImpl socket) throws IOException;

  /**
   * Internal method used by SocketInputStream for reading data from
   * the connection.  Reads up to len bytes of data into the buffer
   * buf starting at offset bytes into the buffer.
   *
   * @param socket the socket object
   *
   * @return the actual number of bytes read or -1 if end of stream.
   *
   * @throws IOException if an error occurs
   */
  static native int read(PlainSocketImpl socket, byte[] buf, int offset,
                         int len) throws IOException;

  /**
   * Internal method used by SocketInputStream for reading data from
   * the connection.  Reads and returns one byte of data.
   *
   * @param socket the socket object
   *
   * @return read byte or -1 if end of stream.
   *
   * @throws IOException if an error occurs
   */
  static int read(PlainSocketImpl socket) throws IOException
  {
    byte[] buf = new byte[1];
    if (read(socket, buf, 0, 1) > 0)
      return buf[0] & 0xFF;
    else
      return -1;
  }

  /**
   * Internal method used by SocketOuputStream for writing data to
   * the connection. Writes up to len bytes of data from the buffer
   * <code>buf</code> starting at <cod>offset</code> bytes into the buffer.
   *
   * @param socket the socket object
   * @param buf the buffer to write to the stream
   * @param offset the start offset in the buffer
   * @param len the number of bytes to write
   *
   * @throws IOException if an error occurs
   */
  static native void write(PlainSocketImpl socket, byte[] buf, int offset,
                           int len) throws IOException;

  /**
   * Internal method used by SocketOuputStream for writing data to
   * the connection. Writes exactly one byte to the socket.
   *
   * @param socket the socket object
   * @param data the byte to write to the socket
   *
   * @throws IOException if an error occurs
   */
  static void write(PlainSocketImpl socket, int data)
    throws IOException
  {
    write(socket, new byte[]{ (byte) data }, 0, 1);
  }

  /**
   * Sets the input stream for this socket to the end of the stream. Any
   * attempts to read more bytes from the stream will return an EOF.
   *
   * @param socket the socket object
   *
   * @throws IOException if I/O errors occur
   */
  static native void shutdownInput(PlainSocketImpl socket) throws IOException;

  /**
   * Disables the output stream for this socket. Any attempt to write more
   * data to the socket will throw an IOException.
   *
   * @param socket the socket object
   *
   * @throws IOException if I/O errors occur
   */
  static native void shutdownOutput(PlainSocketImpl socket) throws IOException;

  /**
   * Connects to the remote socket address with a specified timeout.
   *
   * @param socket the socket object
   * @param address the remote address to connect to
   * @param timeout the timeout to use for this connect, 0 means infinite.
   *
   * @throws IOException if an error occurs
   */
  static synchronized void connect(PlainSocketImpl socket,
                                      SocketAddress address, int timeout)
    throws IOException
  {
    InetSocketAddress sockAddr = (InetSocketAddress) address;
    InetAddress addr = sockAddr.getAddress();

    if (addr == null)
      throw new UnknownHostException(sockAddr.getHostName());

    int port = sockAddr.getPort();

    if (timeout < 0)
      throw new IllegalArgumentException("negative timeout");

    Object oldTimeoutObj = null;
    try
      {
        oldTimeoutObj = getOption(socket, SocketOptions.SO_TIMEOUT);
        setOption(socket, SocketOptions.SO_TIMEOUT, new Integer(timeout));
        connect(socket, addr, port);
      }
    finally
      {
        if (oldTimeoutObj != null)
          setOption(socket, SocketOptions.SO_TIMEOUT, oldTimeoutObj);
      }
  }

  /**
   * Send one byte of urgent data over the socket.
   *
   * @param socket the socket object
   * @param data the byte to send
   */
  static void sendUrgendData(PlainSocketImpl socket, int data)
  {
    throw new InternalError ("PlainSocketImpl::sendUrgentData not implemented");
  }
}
