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

import gnu.java.nio.SocketChannelImpl;
import gnu.java.nio.VMChannel;

import java.io.InputStream;
import java.io.IOException;
import java.io.InterruptedIOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.SocketException;
import java.net.SocketImpl;
import java.net.SocketTimeoutException;
import java.nio.ByteBuffer;

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
public class PlainSocketImpl extends SocketImpl
{

  /**
   * The underlying plain socket VM implementation.
   */
  protected VMPlainSocketImpl impl;

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
   * The socket channel we use for IO operation. Package-private for
   * use by inner classes.
   */
  SocketChannelImpl channel;

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
   * Default do nothing constructor.
   */
  public PlainSocketImpl()
  {
    this.impl = new VMPlainSocketImpl();
  }

  /**
   * Sets the specified option on a socket to the passed in object.  For
   * options that take an integer argument, the passed in object is an
   * Integer.  The option_id parameter is one of the defined constants in
   * this interface.
   *
   * @param optionId The identifier of the option
   * @param value The value to set the option to
   *
   * @throws SocketException if an error occurs
   */
  public void setOption(int optionId, Object value) throws SocketException
  {
    switch (optionId)
      {
        case SO_LINGER:
        case IP_MULTICAST_LOOP:
        case SO_BROADCAST:
        case SO_KEEPALIVE:
        case SO_OOBINLINE:
        case TCP_NODELAY:
        case IP_TOS:
        case SO_RCVBUF:
        case SO_SNDBUF:
        case SO_TIMEOUT:
        case SO_REUSEADDR:
          impl.setOption(optionId, value);
          return;
        default:
          throw new SocketException("Unrecognized TCP option: " + optionId);
      }
  }

  /**
   * Returns the current setting of the specified option.  The Object returned
   * will be an Integer for options that have integer values.  The option_id
   * is one of the defined constants in this interface.
   *
   * @param optionId the option identifier
   *
   * @return the current value of the option
   *
   * @throws SocketException if an error occurs
   */
  public Object getOption(int optionId) throws SocketException
  {
    if (optionId == SO_BINDADDR)
      {
        try
          {
            return channel.getVMChannel().getLocalAddress().getAddress();
          }
        catch (IOException ioe)
          {
            SocketException se = new SocketException();
            se.initCause(ioe);
            throw se;
          }
      }

    // This filters options which are invalid for TCP.
    switch (optionId)
    {
      case SO_LINGER:
      case IP_MULTICAST_LOOP:
      case SO_BROADCAST:
      case SO_KEEPALIVE:
      case SO_OOBINLINE:
      case TCP_NODELAY:
      case IP_TOS:
      case SO_RCVBUF:
      case SO_SNDBUF:
      case SO_TIMEOUT:
      case SO_REUSEADDR:
        return impl.getOption(optionId);
      default:
        throw new SocketException("Unrecognized TCP option: " + optionId);
    }

  }

  public void shutdownInput() throws IOException
  {
    impl.shutdownInput();
  }

  public void shutdownOutput() throws IOException
  {
    impl.shutdownOutput();
  }

  /**
   * Creates a new socket that is not bound to any local address/port and
   * is not connected to any remote address/port.  The stream parameter will be
   * ignored since PlainSocketImpl always is a stream socket. Datagram sockets
   * are handled by PlainDatagramSocketImpl.
   *
   * @param stream <code>true</code> for stream sockets, <code>false</code> for
   *        datagram sockets
   */
  protected synchronized void create(boolean stream) throws IOException
  {
    channel = new SocketChannelImpl(false);
    VMChannel vmchannel = channel.getVMChannel();
    vmchannel.initSocket(stream);
    channel.configureBlocking(true);
    impl.getState().setChannelFD(vmchannel.getState());
  }

  /**
   * Connects to the remote hostname and port specified as arguments.
   *
   * @param hostname the remote hostname to connect to
   * @param port the remote port to connect to
   *
   * @throws IOException If an error occurs
   */
  protected synchronized void connect(String hostname, int port)
    throws IOException
  {
    connect(InetAddress.getByName(hostname), port);
  }

  /**
   * Connects to the remote address and port specified as arguments.
   *
   * @param addr the remote address to connect to
   * @param port the remote port to connect to
   *
   * @throws IOException If an error occurs
   */
  protected void connect(InetAddress addr, int port) throws IOException
  {
    connect(new InetSocketAddress(addr, port), 0);
  }

  /**
   * Connects to the remote socket address with a specified timeout.
   *
   * @param address the remote address to connect to
   * @param timeout the timeout to use for this connect, 0 means infinite.
   *
   * @throws IOException If an error occurs
   */
  protected synchronized void connect(SocketAddress address, int timeout)
    throws IOException
  {
    if (channel == null)
      create(true);
    boolean connected = channel.connect(address, timeout);
    if (!connected)
      throw new SocketTimeoutException("connect timed out");

    // Using the given SocketAddress is important to preserve
    // hostnames given by the caller.
    InetSocketAddress addr = (InetSocketAddress) address;
    this.address = addr.getAddress();
    this.port = addr.getPort();
  }

  /**
   * Binds to the specified port on the specified addr.  Note that this addr
   * must represent a local IP address.  **** How bind to INADDR_ANY? ****
   *
   * @param addr the address to bind to
   * @param port the port number to bind to
   *
   * @throws IOException if an error occurs
   */
  protected synchronized void bind(InetAddress addr, int port)
    throws IOException
  {
    if (channel == null)
      create(true);
    impl.bind(new InetSocketAddress(addr, port));
    localport = channel.getVMChannel().getLocalAddress().getPort();
  }

  /**
   * Starts listening for connections on a socket. The queuelen parameter
   * is how many pending connections will queue up waiting to be serviced
   * before being accept'ed.  If the queue of pending requests exceeds this
   * number, additional connections will be refused.
   *
   * @param queuelen The length of the pending connection queue
   *
   * @throws IOException If an error occurs
   */
  protected synchronized void listen(int queuelen)
    throws IOException
  {
    impl.listen(queuelen);
  }

  /**
   * Accepts a new connection on this socket and returns in in the
   * passed in SocketImpl.
   *
   * @param impl The SocketImpl object to accept this connection.
   */
  protected synchronized void accept(SocketImpl impl)
    throws IOException
  {
    if (channel == null)
        create(true);
    if (!(impl instanceof PlainSocketImpl))
      throw new IOException("incompatible SocketImpl: "
                            + impl.getClass().getName());
    PlainSocketImpl that = (PlainSocketImpl) impl;
    VMChannel c = channel.getVMChannel().accept();
    that.impl.getState().setChannelFD(c.getState());
    that.channel = new SocketChannelImpl(c);
    that.setOption(SO_REUSEADDR, Boolean.TRUE);
    // Reset the inherited timeout.
    that.setOption(SO_TIMEOUT, Integer.valueOf(0));

  }

  /**
   * Returns the number of bytes that the caller can read from this socket
   * without blocking.
   *
   * @return the number of readable bytes before blocking
   *
   * @throws IOException if an error occurs
   */
  protected int available() throws IOException
  {
    if (channel == null)
      throw new SocketException("not connected");
    return channel.getVMChannel().available();
  }

  /**
   * Closes the socket.  This will cause any InputStream or OutputStream
   * objects for this Socket to be closed as well.
   *
   * <p>
   * Note that if the SO_LINGER option is set on this socket, then the
   * operation could block.
   * </p>
   *
   * @throws IOException if an error occurs
   */
  protected void close() throws IOException
  {
    if (impl.getState().isValid())
      impl.close();

    address = null;
    port = -1;
  }

  public void sendUrgentData(int data) throws IOException
  {
    impl.sendUrgentData(data);
  }

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

  public VMChannel getVMChannel()
  {
    if (channel == null)
      return null;
    return channel.getVMChannel();
  }

  /* (non-Javadoc)
   * @see java.net.SocketImpl#getInetAddress()
   */
  protected InetAddress getInetAddress()
  {
    if (channel == null)
      return null;

    try
      {
        InetSocketAddress remote = channel.getVMChannel().getPeerAddress();
        if (remote == null)
          return null;
        // To mimic behavior of the RI the InetAddress instance which was
        // used to establish the connection is returned instead of one that
        // was created by the native layer (this preserves exact hostnames).
        if (address != null)
          return address;

        return remote.getAddress();
      }
    catch (IOException ioe)
      {
        return null;
      }
  }

  /* (non-Javadoc)
   * @see java.net.SocketImpl#getLocalPort()
   */
  protected int getLocalPort()
  {
    if (channel == null)
      return -1;
    try
      {
        InetSocketAddress local = channel.getVMChannel().getLocalAddress();
        if (local == null)
          return -1;
        return local.getPort();
      }
    catch (IOException ioe)
      {
        return -1;
      }
  }

  public InetSocketAddress getLocalAddress()
  {
    if (channel == null)
      return null;
    try
      {
        return channel.getVMChannel().getLocalAddress();
      }
    catch (IOException ioe)
      {
        return null;
      }
  }

  /* (non-Javadoc)
   * @see java.net.SocketImpl#getPort()
   */
  protected int getPort()
  {
    if (channel == null)
      return -1;

    try
      {
        InetSocketAddress remote = channel.getVMChannel().getPeerAddress();
        if (remote == null)
          return -1;
        return remote.getPort();
      }
    catch (IOException ioe)
      {
        return -1;
      }
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
    public int read() throws IOException
    {
      if (channel == null)
        throw new SocketException("not connected");
      while (true)
        {
          try
            {
              return channel.getVMChannel().read();
            }
          catch (SocketTimeoutException ste)
            {
              throw ste;
            }
          catch (InterruptedIOException iioe)
            {
              // Ignore; NIO may throw this; net io shouldn't
            }
        }
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
      if (channel == null)
        throw new SocketException("not connected");
      ByteBuffer b = ByteBuffer.wrap(buf, offset, len);
      while (true)
        {
          try
            {
              return channel.read(b);
            }
          catch (SocketTimeoutException ste)
            {
              throw ste;
            }
          catch (InterruptedIOException iioe)
            {
              // Ignored; NIO may throw this; net IO not.
            }
        }
    }
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
    public void write(int b) throws IOException
    {
      if (channel == null)
        throw new SocketException("not connected");
      while (true)
        {
          try
            {
              channel.getVMChannel().write(b);
              return;
            }
          catch (InterruptedIOException iioe)
            {
              // Ignored.
            }
        }
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
      if (channel == null)
        throw new SocketException("not connected");
      ByteBuffer b = ByteBuffer.wrap(buf, offset, len);
      while (b.hasRemaining())
        {
          try
            {
              if (channel.write(b) == -1)
                throw new IOException("channel has been closed");
            }
          catch (InterruptedIOException iioe)
            {
              // Ignored.
            }
        }
    }
  }
}
