/* MidiMessage.java -- base class for MIDI messages.
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package javax.sound.midi;

/**
 * The base class for all MIDI messages.
 *
 * @author Anthony Green (green@redhat.com)
 * @since 1.3
 *
 */
public abstract class MidiMessage implements Cloneable
{
  /**
   * MIDI message data.
   */
  protected byte data[];

  /**
   * The total length of the MIDI message.
   */
  protected int length;

  /**
   * MidiMessage contructor.
   *
   * @param data a valid MIDI message
   */
  protected MidiMessage(byte[] data)
  {
    this.data = data;
    this.length = data.length;
  }

  /**
   * Set the complete MIDI message.
   *
   * @param data The complete MIDI message.
   * @param length The length of the MIDI message.
   * @throws InvalidMidiDataException Thrown when the MIDI message is invalid.
   */
  protected void setMessage(byte[] data, int length)
    throws InvalidMidiDataException
  {
    this.data = new byte[length];
    System.arraycopy(data, 0, this.data, 0, length);
    this.length = length;
  }

  /**
   * Get the MIDI message data.
   *
   * @return an array containing the MIDI message data
   */
  public byte[] getMessage()
  {
    byte copy[] = new byte[length];
    System.arraycopy(data, 0, copy, 0, length);
    return copy;
  }

  /**
   * Get the status byte of the MIDI message (as an int)
   *
   * @return the status byte of the MIDI message (as an int), or zero if the message length is zero.
   */
  public int getStatus()
  {
    if (length > 0)
      return (data[0] & 0xff);
    else
      return 0;
  }

  /**
   * Get the length of the MIDI message.
   *
   * @return the length of the MIDI messsage
   */
  public int getLength()
  {
    return length;
  }

  /* Create a clone of this object.
   *
   * @see java.lang.Object#clone()
   */
  public abstract Object clone();
}
