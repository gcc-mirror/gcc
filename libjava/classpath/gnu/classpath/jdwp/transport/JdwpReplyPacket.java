/* JdwpReplyPacket.java -- JDWP reply packet
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
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.classpath.jdwp.transport;

import java.io.DataOutputStream;
import java.io.IOException;

/**
 * A class represents a JDWP reply packet.
 * This class adds an error code to the packet header information
 * in {@link gnu.classpath.jdwp.transport.JdwpPacket} and adds additional
 * reply packet-specific processing.
 *
 * @author Keith Seitz  <keiths@redhat.com>
 */
public class JdwpReplyPacket extends JdwpPacket
{
  /**
   * Error code
   */
  protected short _errorCode;

  // Minimum packet size [excluding super class] ( errorCode (2) )
  private static final int MINIMUM_LENGTH = 2;

  /**
   * Constructs a <code>JdwpReplyPacket</code>.
   */
  public JdwpReplyPacket ()
  {
    // Don't assign a packet id. This is called by JdwpPacket.fromBytes
    // which assigns a packet id. (Not that a VM would do that...)
  }

  /**
   * Constructs a <code>JdwpReplyPacket</code> with the
   * id from the given packet and error code
   *
   * @param pkt        the packet whose id this packet will use
   * @param errorCode  the error code
   */
  public JdwpReplyPacket (JdwpPacket pkt, short errorCode)
  {
    this(pkt);
    _errorCode = errorCode;
  }

  /**
   * Constructs a <code>JdwpReplyPacket</code> with the
   * id from the given packet and an empty error code
   *
   * @param pkt        the packet whose id this packet will use
   */
  public JdwpReplyPacket (JdwpPacket pkt)
  {
    super (pkt);
    _flags = (byte) JDWP_FLAG_REPLY;
  }

  /**
   * Returns the length of this packet
   */
  public int getLength ()
  {
    return MINIMUM_LENGTH + super.getLength ();
  }

  /**
   * Returns the error code
   */
  public short getErrorCode ()
  {
    return _errorCode;
  }

  /**
   * Sets the error code
   */
  public void setErrorCode (short ec)
  {
    _errorCode = ec;
  }

  // Reads command packet data from the given buffer, starting
  // at the given offset
  protected int myFromBytes (byte[] bytes, int index)
  {
    int i = 0;
    setErrorCode ((short) ((bytes[index + i++] & 0xff) << 8
			   | (bytes[index + i++] & 0xff)));
    return i;
  }

  // Writes the command packet data into the given buffer
  protected void myWrite (DataOutputStream dos)
    throws IOException
 {
    dos.writeShort (getErrorCode ());
  }
}
