// DatagramPacket.java - Represents packets in a connectionless protocol.

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date April 28, 1999.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

public final class DatagramPacket
{
  private byte[] buffer;
  private int offset;
  private int length;
  private InetAddress address;
  private int port;

  // JDK1.2
  public DatagramPacket(byte[] buf, int offset, int length)
  {
    // FIXME: We can't currently rely on NullPointerException being
    // thrown when we invoke a method on a null object.
    if (buf == null)
      throw new NullPointerException("Null buffer");
    if (offset < 0)
      throw new IllegalArgumentException("Invalid offset: " + offset);
    if (length < 0)
      throw new IllegalArgumentException("Invalid length: " + length);
    if (offset + length > buf.length)
      throw new IllegalArgumentException("Potential buffer overflow - offset: "
			+ offset + " length: " + length);

    buffer = buf;
    this.offset = offset;
    this.length = length;
    this.address = null;
    this.port = -1;
  }

  public DatagramPacket(byte[] buf, int length)
  {
    this(buf, 0, length);
  }

  // JDK1.2
  public DatagramPacket(byte[] buf, int offset, int length,
	InetAddress address, int port)
  {
    // FIXME: We can't currently rely on NullPointerException being
    // thrown when we invoke a method on a null object.
    if (buf == null)
      throw new NullPointerException("Null buffer");
    if (offset < 0)
      throw new IllegalArgumentException("Invalid offset: " + offset);
    if (length < 0)
      throw new IllegalArgumentException("Invalid length: " + length);
    if (offset + length > buf.length)
      throw new IllegalArgumentException("Potential buffer overflow - offset: "
			+ offset + " length: " + length);
    if (port < 0 || port > 65535)
      throw new IllegalArgumentException("Invalid port: " + port);
    if (address == null)
      throw new NullPointerException("Null address");

    buffer = buf;
    this.offset = offset;
    this.length = length;
    this.address = address;
    this.port = port;
  }

  public DatagramPacket(byte[] buf, int length, InetAddress address, int port)
  {
    this(buf, 0, length, address, port);
  }

  public synchronized InetAddress getAddress()
  {
    return address;
  }

  public synchronized int getPort()
  {
    return port;
  }

  public synchronized byte[] getData()
  {
    return buffer;
  }

  // JDK1.2
  public synchronized int getOffset()
  {
    return offset;
  }

  public synchronized int getLength()
  {
    return length;
  }

  public synchronized void setAddress(InetAddress iaddr)
  {
    if (iaddr == null)
      throw new NullPointerException("Null address");

    address = iaddr;
  }

  public synchronized void setPort(int iport)
  {
    if (iport < 0 || iport > 65535)
      throw new IllegalArgumentException("Invalid port: " + iport);

    port = iport;
  }

  public synchronized void setData(byte[] buf)
  {
    // This form of setData requires setLength to be called separately
    // and subsequently.
    if (buf == null)
      throw new NullPointerException("Null buffer");

    buffer = buf;
  }

  // JDK1.2
  public synchronized void setData(byte[] buf, int offset, int length)
  {
    // This form of setData must be used if offset is to be changed.

    // FIXME: We can't currently rely on NullPointerException being
    // thrown when we invoke a method on a null object.
    if (buf == null)
      throw new NullPointerException("Null buffer");
    if (offset < 0)
      throw new IllegalArgumentException("Invalid offset: " + offset);
    if (length < 0)
      throw new IllegalArgumentException("Invalid length: " + length);
    if (offset + length > buf.length)
      throw new IllegalArgumentException("Potential buffer overflow - offset: "
			+ offset + " length: " + length);

    buffer = buf;
    this.offset = offset;
    this.length = length;
  }

  public synchronized void setLength(int length)
  {
    if (length < 0)
      throw new IllegalArgumentException("Invalid length: " + length);
    if (offset + length > buffer.length)
      throw new IllegalArgumentException("Potential buffer overflow - offset: "
			+ offset + " length: " + length);

    this.length = length;
  }
}
