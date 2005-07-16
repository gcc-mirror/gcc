/* SocketImpl.java -- Abstract socket implementation class
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003
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

import java.io.FileDescriptor;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;


/* Written using on-line Java Platform 1.2 API Specification.
 * Believed complete and correct.
 */

/**
 * This abstract class serves as the parent class for socket implementations.
 * The implementation class serves an intermediary to native routines that
 * perform system specific socket operations.
 * <p>
 * A default implementation is provided by the system, but this can be
 * changed via installing a <code>SocketImplFactory</code> (through a call
 * to the static method <code>Socket.setSocketImplFactory</code>).  A
 * subclass of <code>Socket</code> can also pass in a <code>SocketImpl</code>
 * to the <code>Socket(SocketImpl)</code> constructor to use an
 * implementation different from the system default without installing
 * a factory.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Per Bothner (bothner@cygnus.com)
 */
public abstract class SocketImpl implements SocketOptions
{
  /**
   * The address of the remote end of the socket connection
   */
  protected InetAddress address;

  /**
   * A FileDescriptor object representing this socket connection.
   */
  protected FileDescriptor fd;

  /**
   * The port number the socket is bound to locally
   */
  protected int localport = -1;

  /**
   * The port number of the remote end of the socket connection
   */
  protected int port;

  /**
   * Default, no-argument constructor for use by subclasses.
   */
  public SocketImpl()
  {
  }

  /**
   * Creates a new socket that is not bound to any local address/port and
   * is not connected to any remote address/port.  This will be created as
   * a stream socket if the stream parameter is true, or a datagram socket
   * if the stream parameter is false.
   *
   * @param stream true for a stream socket, false for a datagram socket
   *
   * @exception IOException If an error occurs
   */
  protected abstract void create(boolean stream) throws IOException;

  /**
   * Connects to the remote hostname and port specified as arguments.
   *
   * @param host The remote hostname to connect to
   * @param port The remote port to connect to
   *
   * @exception IOException If an error occurs
   */
  protected abstract void connect(String host, int port)
    throws IOException;

  /**
   * Connects to the remote address and port specified as arguments.
   *
   * @param host The remote address to connect to
   * @param port The remote port to connect to
   *
   * @exception IOException If an error occurs
   */
  protected abstract void connect(InetAddress host, int port)
    throws IOException;

  /**
   * Connects to the socket to the host specified in address. This
   * method blocks until successful connected or the timeout occurs.
   * A timeout of zero means no timout.
   *
   * @param address Data of remote host
   * @param timeout time to wait to stop connecting
   *
   * @exception IOException If an error occurs
   *
   * @since 1.4
   */
  protected abstract void connect(SocketAddress address, int timeout)
    throws IOException;

  /**
   * Binds to the specified port on the specified addr.  Note that this addr
   * must represent a local IP address.
   * <p>
   * Note that it is unspecified how to bind to all interfaces on the localhost
   * (INADDR_ANY).
   *
   * @param host The address to bind to
   * @param port The port number to bind to
   *
   * @exception IOException If an error occurs
   */
  protected abstract void bind(InetAddress host, int port)
    throws IOException;

  /**
   * Starts listening for connections on a socket. The backlog parameter
   * is how many pending connections will queue up waiting to be serviced
   * before being accept'ed.  If the queue of pending requests exceeds this
   * number, additional connections will be refused.
   *
   * @param backlog The length of the pending connection queue
   *
   * @exception IOException If an error occurs
   */
  protected abstract void listen(int backlog) throws IOException;

  /**
   * Accepts a connection on this socket.
   *
   * @param s The implementation object for the accepted connection.
   *
   * @exception IOException If an error occurs
   */
  protected abstract void accept(SocketImpl s) throws IOException;

  /**
   * Returns an <code>InputStream</code> object for reading from this socket.
   *
   * @return An <code>InputStream</code> for reading from this socket.
   *
   * @exception IOException If an error occurs
   */
  protected abstract InputStream getInputStream() throws IOException;

  /**
   * Returns an <code>OutputStream</code> object for writing to this socket
   *
   * @return An <code>OutputStream</code> for writing to this socket.
   *
   * @exception IOException If an error occurs.
   */
  protected abstract OutputStream getOutputStream() throws IOException;

  /**
   * Returns the number of bytes that the caller can read from this socket
   * without blocking.
   *
   * @return The number of readable bytes before blocking
   *
   * @exception IOException If an error occurs
   */
  protected abstract int available() throws IOException;

  /**
   * Closes the socket.  This will normally cause any resources, such as the
   * InputStream, OutputStream and associated file descriptors  to be freed.
   * <p>
   * Note that if the SO_LINGER option is set on this socket, then the
   * operation could block.
   *
   * @exception IOException If an error occurs
   */
  protected abstract void close() throws IOException;

  /**
   * Returns the FileDescriptor objects for this socket.
   *
   * @return A FileDescriptor for this socket.
   */
  protected FileDescriptor getFileDescriptor()
  {
    return fd;
  }

  /**
   * Returns the remote address this socket is connected to
   *
   * @return The remote address
   */
  protected InetAddress getInetAddress()
  {
    return address;
  }

  /**
   * Returns the remote port this socket is connected to
   *
   * @return The remote port
   */
  protected int getPort()
  {
    return port;
  }

  /**
   * Returns true or false when this socket supports sending urgent data
   * or not.
   *
   * @return true if the socket implementation supports sending urgent data,
   * false otherwise
   *
   * @since 1.4
   */
  protected boolean supportsUrgentData()
  {
    // This method has to be overwritten by socket classes that support
    // sending urgend data.
    return false;
  }

  /**
   * Sends one byte of urgent data to the socket.
   *
   * @param data The byte to send, the low eight bits of it
   *
   * @exception IOException If an error occurs
   *
   * @since 1.4
   */
  protected abstract void sendUrgentData(int data) throws IOException;

  /**
   * Returns the local port this socket is bound to
   *
   * @return The local port
   */
  protected int getLocalPort()
  {
    return localport;
  }

  /**
   * Returns a <code>String</code> representing the remote host and port of
   * this socket.
   *
   * @return A <code>String</code> for this socket.
   */
  public String toString()
  {
    return "[addr="
           + ((address == null) ? "0.0.0.0/0.0.0.0" : address.toString())
           + ",port=" + port + ",localport=" + localport + "]";
  }

  /**
   * Shut down the input side of this socket.  Subsequent reads will
   * return end-of-file.
   *
   * @exception IOException if an error occurs
   */
  protected void shutdownInput() throws IOException
  {
    throw new IOException("Not implemented in this socket class");
  }

  /**
   * Shut down the output side of this socket.  Subsequent writes will
   * fail with an IOException.
   *
   * @exception IOException if an error occurs
   */
  protected void shutdownOutput() throws IOException
  {
    throw new IOException("Not implemented in this socket class");
  }
}
