/* JdwpPacket.java -- Base class for JDWP command and reply packets
   Copyright (C) 2005 Free Software Foundation

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
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.classpath.jdwp.transport;

/**
 * All command and reply packets in JDWP share
 * common header type information:
 *
 *    length (4 bytes) : size of entire packet, including length
 *    id     (4 bytes) : unique packet id
 *    flags  (1 byte)  : flag byte
 *    [command packet stuff | reply packet stuff]
 *    data   (variable) : unique command-/reply-specific data
 *
 * This class deal with everything except the command- and reply-specific
 * data, which get handled in {@link
 * gnu.classpath.jdwp.transport.JdwpCommandPacket} and {@link
 * gnu.classpath.jdwp.transprot.JdwpReplyPacket}.
 *
 * @author Keith Seitz  <keiths@redhat.com>
 */
public abstract class JdwpPacket
{
  // Last id of packet constructed
  protected static int _last_id = 0;

  // JDWP reply packet flag
  protected static final int JDWP_FLAG_REPLY = 0x80;

  /**
   * Minimum packet size excluding sub-class data
   * ( length (4) + id (4) + flags (1) )
   */
  protected static final int MINIMUM_SIZE = 9;

  /**
   * Id of command/reply
   */
  protected int _id;

  /**
   * Packet flags
   */
  protected byte _flags;

  /**
   * Packet-specific data
   */
  protected byte[] _data;

  /**
   * Constructor
   */
  public JdwpPacket ()
  {
    // By default, DON'T assign an id. This way when a packet
    // is constructed from fromBytes, _last_id won't increment (i.e.,
    // it won't leave holes in the outgoing packet ids).
  }

  /**
   * Constructs a <code>JdwpPacket</code> with the id
   * from the given packet.
   *
   * @param pkt  a packet whose id will be used in this new packet
   */
  public JdwpPacket (JdwpPacket pkt)
  {
    _id = pkt.getId ();
  }

  /**
   * Returns the packet id
   */
  public int getId ()
  { 
    return _id;
  }

  /**
   * Sets the packet id
   */
  public void setId (int id)
  { 
    _id = id;
  }

  /**
   * Returns the packet flags
   */
  public byte getFlags ()
  { 
    return _flags;
  }

  /**
   * Sets the packet flags
   */
  public void setFlags (byte flags)
  { 
    _flags = flags;
  }

  /**
   * Gets the command/reply-specific data in this packet
   */
  public byte[] getData ()
  { 
    return _data;
  }

  /**
   * Sets the command/reply-specific data in this packet
   */
  public void setData (byte[] data)
  { 
    _data = data;
  }

  /**
   * Returns the length of this entire packet
   */
  public int getLength ()
  { 
    return MINIMUM_SIZE + (_data == null ? 0 : _data.length);
  }

  /**
   * Allow subclasses to initialize from data
   * 
   * @param   bytes  packet data from the wire
   * @param   index  index into <code>bytes</code> to start processing
   * @return         number of bytes in <code>bytes</code> processed
   */
  protected abstract int myFromBytes (byte[] bytes, int index);

  /**
   * Convert the given bytes into a <code>JdwpPacket</code>. Uses the
   * abstract method <code>myFromBytes</code> to allow subclasses to
   * process data.
   *
   * If the given data does not represent a valid JDWP packet, it returns
   * <code>null</code>.
   * 
   * @param   bytes  packet data from the wire
   * @param   index  index into <code>bytes</code> to start processing
   * @return         number of bytes in <code>bytes</code> processed
   */
  public static JdwpPacket fromBytes (byte[] bytes)
  {
    int i = 0;
    int length = ((bytes[i++] & 0xff) << 24 | (bytes[i++] & 0xff) << 16
                  | (bytes[i++] & 0xff) << 8 | (bytes[i++] & 0xff));
    int id = 0;
    byte flags = 0;

    if (bytes.length == length)
      {
	id = ((bytes[i++] & 0xff) << 24 | (bytes[i++] & 0xff) << 16
              | (bytes[i++] & 0xff) << 8 | (bytes[i++] & 0xff));
	flags = bytes[i++];

	Class clazz = null;
	if (flags == 0)
	  clazz = JdwpCommandPacket.class;
	else if ((flags & JDWP_FLAG_REPLY) != 0)
	  clazz = JdwpReplyPacket.class;
	else
	  {
	    // Malformed packet. Discard it.
	    return null;
	  }

	JdwpPacket pkt = null;
	try
	  {
	    pkt = (JdwpPacket) clazz.newInstance ();
	  }
	catch (InstantiationException ie)
	  {
	    // Discard packet
	    return null;
	  }
	catch (IllegalAccessException iae)
	  {
	    // Discard packet
	    return null;
	  }

	pkt.setId (id);
	pkt.setFlags (flags);

	i += pkt.myFromBytes (bytes, i);
	byte[] data = new byte[length - i];
	System.arraycopy (bytes, i, data, 0, data.length);
	pkt.setData (data);

	return pkt;
      }
	
    return null;
  }

  // Put subclass information into bytes
  protected abstract int myToBytes (byte[] bytes, int index);

  // Convert this packet to it byte representation (ready to send on the wire)
  // NOTE: All integers should be big-endian.
  public byte[] toBytes ()
  {
    // Allocate a new array to hold contents of packet
    int length = getLength ();
    byte[] bytes = new byte[length];
	
    int i = 0;

    //
    // Packet layout: length, id, flags, packet-specific, data (optional)
    //

    // length
    bytes[i++] = (byte) (length >>> 24);
    bytes[i++] = (byte) (length >>> 16);
    bytes[i++] = (byte) (length >>> 8);
    bytes[i++] = (byte) length;

    // id
    bytes[i++] = (byte) (getId () >>> 24);
    bytes[i++] = (byte) (getId () >>> 16);
    bytes[i++] = (byte) (getId () >>> 8);
    bytes[i++] = (byte) getId ();

    // flag
    bytes[i++] = getFlags ();

    // packet-specific stuff
    i += myToBytes (bytes, i);

    // data (if any)
    byte[] data = getData ();
    if (data.length > 0 && i < length)
      {
	// Would it pay to be over cautious?
	System.arraycopy (data, 0, bytes, i, data.length);
      }

    return bytes;
  }
}
