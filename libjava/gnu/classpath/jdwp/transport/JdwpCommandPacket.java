/* JdwpCommandPacket.java -- JDWP command packet
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
 * A class representing a JDWP command packet.
 * This class adds command set and command to the packet header
 * information in {@link gnu.classpath.jdwp.transport.JdwpPacket}
 * and adds additional command packet-specific processing.
 *
 * @author Keith Seitz  <keiths@redhat.com>
 */
public class JdwpCommandPacket extends JdwpPacket
{
  /**
   * Command set
   */
  protected byte _commandSet;

  /**
   * Command
   */
  protected byte _command;

  // Minimum packet size [excluding super class]
  // ( commandSet (1) + command (1) )
  private static final int MINIMUM_LENGTH = 2;

  /**
   * Constructs a new <code>JdwpCommandPacket</code>
   */
  public JdwpCommandPacket ()
  {
    // Don't assign an id. This constructor is called by
    // JdwpPacket.fromBytes, and that will assign a packet id.
  }
  
  /**
   * Constructs a new <code>JdwpCommandPacket</code>
   * with the given command set and command
   *
   * @param set      the command set
   * @param command  the command
   */
  public JdwpCommandPacket (byte set, byte command)
  {
    _id = ++_last_id;
    _commandSet = set;
    _command = command;
  }

  /**
   * Retuns the length of this packet
   */
  public int getLength ()
  {
    return MINIMUM_LENGTH + super.getLength ();
  }

  /**
   * Returns the command set
   */
  public byte getCommandSet ()
  {
    return _commandSet;
  }

  /**
   * Sets the command set
   */
  public void setCommandSet (byte cs)
  { 
    _commandSet = cs;
  }

  /**
   * Returns the command
   */
  public byte getCommand ()
  {
    return _command;
  }

  /**
   * Sets the command
   */
  public void setCommand (byte cmd)
  {
    _command = cmd;
  }

  // Reads command packet data from the given buffer, starting
  // at the given offset
  protected int myFromBytes (byte[] bytes, int index)
  {
    int i = 0;
    setCommandSet (bytes[index + i++]);
    setCommand (bytes[index + i++]);
    return i;
  }

  // Writes the command packet data into the given buffer
  protected void myWrite (DataOutputStream dos)
    throws IOException
  {
    dos.writeByte (getCommandSet ());
    dos.writeByte (getCommand ());
  }
}
