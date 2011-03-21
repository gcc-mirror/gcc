/* VMChannel.java -- Native interface suppling channel operations.
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.java.nio;

import gnu.classpath.Configuration;

import java.io.IOException;
import java.net.Inet4Address;
import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.SocketException;
import java.nio.ByteBuffer;
import java.nio.MappedByteBuffer;

/**
 * Native interface to support configuring of channel to run in a non-blocking
 * manner and support scatter/gather io operations.
 *
 * @author Michael Barker <mike@middlesoft.co.uk>
 *
 */
public final class VMChannel
{
  /**
   * Our reference implementation uses an integer to store the native
   * file descriptor. Implementations without such support
   */
  private final State nfd;

  private Kind kind;

  public VMChannel()
  {
    // XXX consider adding security check here, so only Classpath
    // code may create instances.
    this.nfd = new State();
    kind = Kind.OTHER;
  }

  /**
   * This constructor is used by the POSIX reference implementation;
   * other virtual machines need not support it.
   *
   * <strong>Important:</strong> do not call this in library code that is
   * not specific to Classpath's reference implementation.
   *
   * @param native_fd The native file descriptor integer.
   * @throws IOException
   */
  VMChannel(final int native_fd) throws IOException
  {
    this();
    this.nfd.setNativeFD(native_fd);
  }

  public State getState()
  {
    return nfd;
  }

  static
  {
    // load the shared library needed for native methods.
    if (Configuration.INIT_LOAD_LIBRARY)
      {
        System.loadLibrary ("javanio");
      }
    initIDs();
  }

  public static VMChannel getStdin() throws IOException
  {
    return new VMChannel(stdin_fd());
  }

  public static VMChannel getStdout() throws IOException
  {
    return new VMChannel(stdout_fd());
  }

  public static VMChannel getStderr() throws IOException
  {
    return new VMChannel(stderr_fd());
  }

  private static native int stdin_fd();
  private static native int stdout_fd();
  private static native int stderr_fd();

  /**
   * Set the file descriptor to have the required blocking
   * setting.
   *
   * @param blocking The blocking flag to set.
   */
  public void setBlocking(boolean blocking) throws IOException
  {
    setBlocking(nfd.getNativeFD(), blocking);
  }

  private static native void setBlocking(int fd, boolean blocking)
    throws IOException;

  public int available() throws IOException
  {
    return available(nfd.getNativeFD());
  }

  private static native int available(int native_fd) throws IOException;

  /**
   * Reads a byte buffer directly using the supplied file descriptor.
   *
   * @param dst Direct Byte Buffer to read to.
   * @return Number of bytes read.
   * @throws IOException If an error occurs or dst is not a direct buffers.
   */
  public int read(ByteBuffer dst)
    throws IOException
  {
    return read(nfd.getNativeFD(), dst);
  }

  private static native int read(int fd, ByteBuffer dst) throws IOException;

  /**
   * Read a single byte.
   *
   * @return The byte read, or -1 on end of file.
   * @throws IOException
   */
  public int read() throws IOException
  {
    return read(nfd.getNativeFD());
  }

  private static native int read(int fd) throws IOException;

  /**
   * Reads into byte buffers directly using the supplied file descriptor.
   * Assumes that the buffer list contains DirectBuffers.  Will perform a
   * scattering read.
   *
   * @param dsts An array direct byte buffers.
   * @param offset Index of the first buffer to read to.
   * @param length The number of buffers to read to.
   * @return Number of bytes read.
   * @throws IOException If an error occurs or the dsts are not direct buffers.
   */
  public long readScattering(ByteBuffer[] dsts, int offset, int length)
    throws IOException
  {
    if (offset + length > dsts.length)
      throw new IndexOutOfBoundsException("offset + length > dsts.length");

    return readScattering(nfd.getNativeFD(), dsts, offset, length);
  }

  private static native long readScattering(int fd, ByteBuffer[] dsts,
                                            int offset, int length)
    throws IOException;

  /**
   * Receive a datagram on this channel, returning the host address
   * that sent the datagram.
   *
   * @param dst Where to store the datagram.
   * @return The host address that sent the datagram.
   * @throws IOException
   */
  public SocketAddress receive(ByteBuffer dst) throws IOException
  {
    if (kind != Kind.SOCK_DGRAM)
      throw new SocketException("not a datagram socket");
    ByteBuffer hostPort = ByteBuffer.allocateDirect(18);
    int hostlen = receive(nfd.getNativeFD(), dst, hostPort);
    if (hostlen == 0)
      return null;
    if (hostlen == 4) // IPv4
      {
        byte[] addr = new byte[4];
        hostPort.get(addr);
        int port = hostPort.getShort() & 0xFFFF;
        return new InetSocketAddress(Inet4Address.getByAddress(addr), port);
      }
    if (hostlen == 16) // IPv6
      {
        byte[] addr = new byte[16];
        hostPort.get(addr);
        int port = hostPort.getShort() & 0xFFFF;
        return new InetSocketAddress(Inet6Address.getByAddress(addr), port);
      }

    throw new SocketException("host address received with invalid length: "
                              + hostlen);
  }

  private static native int receive (int fd, ByteBuffer dst, ByteBuffer address)
    throws IOException;

  /**
   * Writes from a direct byte bufer using the supplied file descriptor.
   * Assumes the buffer is a DirectBuffer.
   *
   * @param src The source buffer.
   * @return Number of bytes written.
   * @throws IOException
   */
  public int write(ByteBuffer src) throws IOException
  {
    return write(nfd.getNativeFD(), src);
  }

  private native int write(int fd, ByteBuffer src) throws IOException;

  /**
   * Writes from byte buffers directly using the supplied file descriptor.
   * Assumes the that buffer list constains DirectBuffers.  Will perform
   * as gathering write.
   *
   * @param srcs
   * @param offset
   * @param length
   * @return Number of bytes written.
   * @throws IOException
   */
  public long writeGathering(ByteBuffer[] srcs, int offset, int length)
    throws IOException
  {
    if (offset + length > srcs.length)
      throw new IndexOutOfBoundsException("offset + length > srcs.length");

    // A gathering write is limited to 16 buffers; when writing, ensure
    // that we have at least one buffer with something in it in the 16
    // buffer window starting at offset.
    while (!srcs[offset].hasRemaining() && offset < srcs.length)
      offset++;

    // There are no buffers with anything to write.
    if (offset == srcs.length)
      return 0;

    // If we advanced `offset' so far that we don't have `length'
    // buffers left, reset length to only the remaining buffers.
    if (length > srcs.length - offset)
      length = srcs.length - offset;

    return writeGathering(nfd.getNativeFD(), srcs, offset, length);
  }

  private native long writeGathering(int fd, ByteBuffer[] srcs,
                                     int offset, int length)
    throws IOException;

  /**
   * Send a datagram to the given address.
   *
   * @param src The source buffer.
   * @param dst The destination address.
   * @return The number of bytes written.
   * @throws IOException
   */
  public int send(ByteBuffer src, InetSocketAddress dst)
    throws IOException
  {
    InetAddress addr = dst.getAddress();
    if (addr == null)
      throw new NullPointerException();
    if (addr instanceof Inet4Address)
      return send(nfd.getNativeFD(), src, addr.getAddress(), dst.getPort());
    else if (addr instanceof Inet6Address)
      return send6(nfd.getNativeFD(), src, addr.getAddress(), dst.getPort());
    else
      throw new SocketException("unrecognized inet address type");
  }

  // Send to an IPv4 address.
  private static native int send(int fd, ByteBuffer src, byte[] addr, int port)
    throws IOException;

  // Send to an IPv6 address.
  private static native int send6(int fd, ByteBuffer src, byte[] addr, int port)
    throws IOException;

  /**
   * Write a single byte.
   *
   * @param b The byte to write.
   * @throws IOException
   */
  public void write(int b) throws IOException
  {
    write(nfd.getNativeFD(), b);
  }

  private static native void write(int fd, int b) throws IOException;

  private native static void initIDs();

  // Network (socket) specific methods.

  /**
   * Create a new socket. This method will initialize the native file
   * descriptor state of this instance.
   *
   * @param stream Whether or not to create a streaming socket, or a datagram
   *  socket.
   * @throws IOException If creating a new socket fails, or if this
   *  channel already has its native descriptor initialized.
   */
  public void initSocket(boolean stream) throws IOException
  {
    if (nfd.isValid())
      throw new IOException("native FD already initialized");
    if (stream)
      kind = Kind.SOCK_STREAM;
    else
      kind = Kind.SOCK_DGRAM;
    nfd.setNativeFD(socket(stream));
  }

  /**
   * Create a new socket, returning the native file descriptor.
   *
   * @param stream Set to true for streaming sockets; false for datagrams.
   * @return The native file descriptor.
   * @throws IOException If creating the socket fails.
   */
  private static native int socket(boolean stream) throws IOException;

  /**
   * Connect the underlying socket file descriptor to the remote host.
   *
   * @param saddr The address to connect to.
   * @param timeout The connect timeout to use for blocking connects.
   * @return True if the connection succeeded; false if the file descriptor
   *  is in non-blocking mode and the connection did not immediately
   *  succeed.
   * @throws IOException If an error occurs while connecting.
   */
  public boolean connect(InetSocketAddress saddr, int timeout)
    throws SocketException
  {
    int fd;

    InetAddress addr = saddr.getAddress();

    // Translates an IOException into a SocketException to conform
    // to the throws clause.
    try
      {
        fd = nfd.getNativeFD();
      }
    catch (IOException ioe)
      {
        throw new SocketException(ioe.getMessage());
      }

    if (addr instanceof Inet4Address)
      return connect(fd, addr.getAddress(), saddr.getPort(),
                     timeout);
    if (addr instanceof Inet6Address)
      return connect6(fd, addr.getAddress(), saddr.getPort(),
                      timeout);
    throw new SocketException("unsupported internet address");
  }

  private static native boolean connect(int fd, byte[] addr, int port, int timeout)
    throws SocketException;

  private static native boolean connect6(int fd, byte[] addr, int port, int timeout)
    throws SocketException;

  /**
   * Disconnect this channel, if it is a datagram socket. Disconnecting
   * a datagram channel will disassociate it from any address, so the
   * socket will remain open, but can send and receive datagrams from
   * any address.
   *
   * @throws IOException If disconnecting this channel fails, or if this
   *  channel is not a datagram channel.
   */
  public void disconnect() throws IOException
  {
    if (kind != Kind.SOCK_DGRAM)
      throw new IOException("can only disconnect datagram channels");
    disconnect(nfd.getNativeFD());
  }

  private static native void disconnect(int fd) throws IOException;

  public InetSocketAddress getLocalAddress() throws IOException
  {
    if (!nfd.isValid())
      return null;
    ByteBuffer name = ByteBuffer.allocateDirect(18);
    int namelen = getsockname(nfd.getNativeFD(), name);
    if (namelen == 0) // not bound
      return null; // XXX return some wildcard?
    if (namelen == 4)
      {
        byte[] addr = new byte[4];
        name.get(addr);
        int port = name.getShort() & 0xFFFF;
        return new InetSocketAddress(Inet4Address.getByAddress(addr), port);
      }
    if (namelen == 16)
      {
        byte[] addr = new byte[16];
        name.get(addr);
        int port = name.getShort() & 0xFFFF;
        return new InetSocketAddress(Inet6Address.getByAddress(addr), port);
      }
    throw new SocketException("invalid address length");
  }

  private static native int getsockname(int fd, ByteBuffer name)
    throws IOException;

  /**
   * Returns the socket address of the remote peer this channel is connected
   * to, or null if this channel is not yet connected.
   *
   * @return The peer address.
   * @throws IOException
   */
  public InetSocketAddress getPeerAddress() throws IOException
  {
    if (!nfd.isValid())
      return null;
    ByteBuffer name = ByteBuffer.allocateDirect(18);
    int namelen = getpeername (nfd.getNativeFD(), name);
    if (namelen == 0) // not connected yet
      return null;
    if (namelen == 4) // IPv4
      {
        byte[] addr = new byte[4];
        name.get(addr);
        int port = name.getShort() & 0xFFFF;
        return new InetSocketAddress(Inet4Address.getByAddress(addr), port);
      }
    else if (namelen == 16) // IPv6
      {
        byte[] addr = new byte[16];
        name.get(addr);
        int port = name.getShort() & 0xFFFF;
        return new InetSocketAddress(Inet6Address.getByAddress(addr), port);
      }
    throw new SocketException("invalid address length");
  }

  /*
   * The format here is the peer address, followed by the port number.
   * The returned value is the length of the peer address; thus, there
   * will be LEN + 2 valid bytes put into NAME.
   */
  private static native int getpeername(int fd, ByteBuffer name)
    throws IOException;

  /**
   * Accept an incoming connection, returning a new VMChannel, or null
   * if the channel is nonblocking and no connection is pending.
   *
   * @return The accepted connection, or null.
   * @throws IOException If an IO error occurs.
   */
  public VMChannel accept() throws IOException
  {
    int new_fd = accept(nfd.getNativeFD());
    if (new_fd == -1) // non-blocking accept had no pending connection
      return null;
    return new VMChannel(new_fd);
  }

  private static native int accept(int native_fd) throws IOException;

  // File-specific methods.

  /**
   * Open a file at PATH, initializing the native state to operate on
   * that open file.
   *
   * @param path The absolute file path.
   * @throws IOException If the file cannot be opened, or if this
   *  channel was previously initialized.
   */
  public void openFile(String path, int mode) throws IOException
  {
    if (nfd.isValid() || nfd.isClosed())
      throw new IOException("can't reinitialize this channel");
    int fd = open(path, mode);
    nfd.setNativeFD(fd);
    kind = Kind.FILE;
  }

  private static native int open(String path, int mode) throws IOException;

  public long position() throws IOException
  {
    if (kind != Kind.FILE)
      throw new IOException("not a file");
    return position(nfd.getNativeFD());
  }

  private static native long position(int fd) throws IOException;

  public void seek(long pos) throws IOException
  {
    if (kind != Kind.FILE)
      throw new IOException("not a file");
    seek(nfd.getNativeFD(), pos);
  }

  private static native void seek(int fd, long pos) throws IOException;

  public void truncate(long length) throws IOException
  {
    if (kind != Kind.FILE)
      throw new IOException("not a file");
    truncate(nfd.getNativeFD(), length);
  }

  private static native void truncate(int fd, long len) throws IOException;

  public boolean lock(long pos, long len, boolean shared, boolean wait)
    throws IOException
  {
    if (kind != Kind.FILE)
      throw new IOException("not a file");
    return lock(nfd.getNativeFD(), pos, len, shared, wait);
  }

  private static native boolean lock(int fd, long pos, long len,
                                     boolean shared, boolean wait)
    throws IOException;

  public void unlock(long pos, long len) throws IOException
  {
    if (kind != Kind.FILE)
      throw new IOException("not a file");
    unlock(nfd.getNativeFD(), pos, len);
  }

  private static native void unlock(int fd, long pos, long len) throws IOException;

  public long size() throws IOException
  {
    if (kind != Kind.FILE)
      throw new IOException("not a file");
    return size(nfd.getNativeFD());
  }

  private static native long size(int fd) throws IOException;

  public MappedByteBuffer map(char mode, long position, int size)
    throws IOException
  {
    if (kind != Kind.FILE)
      throw new IOException("not a file");
    return map(nfd.getNativeFD(), mode, position, size);
  }

  private static native MappedByteBuffer map(int fd, char mode,
                                             long position, int size)
    throws IOException;

  public boolean flush(boolean metadata) throws IOException
  {
    if (kind != Kind.FILE)
      throw new IOException("not a file");
    return flush(nfd.getNativeFD(), metadata);
  }

  private static native boolean flush(int fd, boolean metadata) throws IOException;

  // Close.

  /**
   * Close this socket. The socket is also automatically closed when this
   * object is finalized.
   *
   * @throws IOException If closing the socket fails, or if this object has
   *  no open socket.
   */
  public void close() throws IOException
  {
    nfd.close();
  }

  static native void close(int native_fd) throws IOException;

  /**
   * <p>Provides a simple mean for the JNI code to find out whether the
   * current thread was interrupted by a call to Thread.interrupt().</p>
   *
   * @return
   */
  static boolean isThreadInterrupted()
  {
    return Thread.currentThread().isInterrupted();
  }

  // Inner classes.

  /**
   * A wrapper for a native file descriptor integer. This tracks the state
   * of an open file descriptor, and ensures that
   *
   * This class need not be fully supported by virtual machines; if a
   * virtual machine does not use integer file descriptors, or does and
   * wishes to hide that, then the methods of this class may be stubbed out.
   *
   * System-specific classes that depend on access to native file descriptor
   * integers SHOULD declare this fact.
   */
  public final class State
  {
    private int native_fd;
    private boolean valid;
    private boolean closed;

    State()
    {
      native_fd = -1;
      valid = false;
      closed = false;
    }

    public boolean isValid()
    {
      return valid;
    }

    public boolean isClosed()
    {
      return closed;
    }

    public int getNativeFD() throws IOException
    {
      if (!valid)
        throw new IOException("invalid file descriptor");
      return native_fd;
    }

    void setNativeFD(final int native_fd) throws IOException
    {
      if (valid)
        throw new IOException("file descriptor already initialized");
      this.native_fd = native_fd;
      valid = true;
    }

    public void close() throws IOException
    {
      if (!valid)
        throw new IOException("invalid file descriptor");
      try
        {
          VMChannel.close(native_fd);
        }
      finally
        {
          valid = false;
          closed = true;
        }
    }

    public String toString()
    {
      if (closed)
        return "<<closed>>";
      if (!valid)
        return "<<invalid>>";
      return String.valueOf(native_fd);
    }

    protected void finalize() throws Throwable
    {
      try
        {
          if (valid)
            close();
        }
      finally
        {
          super.finalize();
        }
    }
  }

  /**
   * An enumeration of possible kinds of channel.
   */
  static class Kind // XXX enum
  {
    /** A streaming (TCP) socket. */
    static final Kind SOCK_STREAM = new Kind();

    /** A datagram (UDP) socket. */
    static final Kind SOCK_DGRAM = new Kind();

    /** A file. */
    static final Kind FILE = new Kind();

    /** Something else; not a socket or file. */
    static final Kind OTHER = new Kind();

    private Kind() { }
  }
}
