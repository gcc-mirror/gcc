/* DatagramPacket.java -- Class to model a packet to be sent via UDP
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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

/*
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

/**
  * This class models a packet of data that is to be sent across the network
  * using a connectionless protocol such as UDP.  It contains the data
  * to be send, as well as the destination address and port.  Note that
  * datagram packets can arrive in any order and are not guaranteed to be
  * delivered at all.
  * <p>
  * This class can also be used for receiving data from the network.
  * <p>
  * Note that for all method below where the buffer length passed by the
  * caller cannot exceed the actually length of the byte array passed as
  * the buffer, if this condition is not true, then the method silently
  * reduces the length value to maximum allowable value.
  *
  * Written using on-line Java Platform 1.2 API Specification, as well
  * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
  * Status:  Believed complete and correct.
  *
  * @author Warren Levy <warrenl@cygnus.com>
  * @author Aarom M. Renn (arenn@urbanophile.com) (Documentation comments)
  * @date April 28, 1999.
  */

public final class DatagramPacket
{
/**
  * The data buffer to send
  */
  private byte[] buffer;

/**
  * This is the offset into the buffer to start sending from or receiving to.
  */
  private int offset;

/**
  * The length of the data buffer to send
  */
  private int length;

/**
  * The address to which the packet should be sent or from which it
  * was received
  */
  private InetAddress address;

/**
  * The port to which the packet should be sent or from which it was
  * was received.
  */
  private int port;

/**
  * This method initializes a new instance of <code>DatagramPacket</code>
  * which has the specified buffer, offset, and length.
  *
  * @param buf The buffer for holding the incoming datagram.
  * @param offset The offset into the buffer to start writing.
  * @param length The maximum number of bytes to read.
  *
  * @since Java 1.2
  */
  public DatagramPacket(byte[] buf, int offset, int length)
  {
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

/**
  * Initializes a new instance of <code>DatagramPacket</code> for
  * receiving packets from the network.
  *
  * @param buf A buffer for storing the returned packet data
  * @param length The length of the buffer (must be <= buf.length)
  */
  public DatagramPacket(byte[] buf, int length)
  {
    this(buf, 0, length);
  }

/**
  * Initializes a new instance of <code>DatagramPacket</code> for
  * transmitting packets across the network.
  *
  * @param buf A buffer containing the data to send
  * @param offset The offset into the buffer to start writing from.
  * @param len The length of the buffer (must be <= buf.length)
  * @param addr The address to send to
  * @param port The port to send to
  *
  * @since Java 1.2
  */
  public DatagramPacket(byte[] buf, int offset, int length,
	InetAddress address, int port)
  {
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

/**
  * Initializes a new instance of <code>DatagramPacket</code> for
  * transmitting packets across the network.
  *
  * @param buf A buffer containing the data to send
  * @param length The length of the buffer (must be <= buf.length)
  * @param address The address to send to
  * @param port The port to send to
  */
  public DatagramPacket(byte[] buf, int length, InetAddress address, int port)
  {
    this(buf, 0, length, address, port);
  }

/**
  * Returns the address that this packet is being sent to or, if it was used
  * to receive a packet, the address that is was received from.  If the
  * constructor that doesn not take an address was used to create this object
  * and no packet was actually read into this object, then this method
  * returns <code>null</code>.
  *
  * @return The address for this packet.
  */
  public synchronized InetAddress getAddress()
  {
    return address;
  }

/**
  * Returns the port number this packet is being sent to or, if it was used
  * to receive a packet, the port that it was received from. If the
  * constructor that doesn not take an address was used to create this object
  * and no packet was actually read into this object, then this method
  * will return 0.
  *
  * @return The port number for this packet
  */
  public synchronized int getPort()
  {
    return port;
  }

/**
  * Returns the data buffer for this packet
  *
  * @return This packet's data buffer
  */
  public synchronized byte[] getData()
  {
    return buffer;
  }

/**
  * This method returns the current offset value into the data buffer
  * where data will be sent from.
  *
  * @return The buffer offset.
  *
  * @since Java 1.2
  */
  public synchronized int getOffset()
  {
    return offset;
  }

/**
  * Returns the length of the data in the buffer
  *
  * @return The length of the data
  */
  public synchronized int getLength()
  {
    return length;
  }

/**
  * This sets the address to which the data packet will be transmitted.
  *
  * @param addr The destination address
  */
  public synchronized void setAddress(InetAddress iaddr)
  {
    if (iaddr == null)
      throw new NullPointerException("Null address");

    address = iaddr;
  }

/**
  * This sets the port to which the data packet will be transmitted.
  *
  * @param port The destination port
  */
  public synchronized void setPort(int iport)
  {
    if (iport < 0 || iport > 65535)
      throw new IllegalArgumentException("Invalid port: " + iport);

    port = iport;
  }

/**
  * Sets the data buffer for this packet.
  *
  * @param buf The new buffer for this packet
  */
  public synchronized void setData(byte[] buf)
  {
    // This form of setData requires setLength to be called separately
    // and subsequently.
    if (buf == null)
      throw new NullPointerException("Null buffer");

    buffer = buf;
  }

/**
  * This method sets the data buffer for the packet.
  *
  * @param buf The byte array containing the data for this packet.
  * @param offset The offset into the buffer to start reading data from.
  * @param length The number of bytes of data in the buffer.
  *
  * @since Java 1.2
  */
  public synchronized void setData(byte[] buf, int offset, int length)
  {
    // This form of setData must be used if offset is to be changed.

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

/**
  * Sets the length of the data in the buffer. 
  *
  * @param length The new length.  (Where len <= buf.length)
  */
  public synchronized void setLength(int length)
  {
    if (length < 0)
      throw new IllegalArgumentException("Invalid length: " + length);
    if (offset + length > buffer.length)
      throw new IllegalArgumentException("Potential buffer overflow - offset: "
			+ offset + " length: " + length);

    this.length = length;
  }
} // class DatagramPacket

