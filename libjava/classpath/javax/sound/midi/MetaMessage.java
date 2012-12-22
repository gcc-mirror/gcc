/* MetaMessage.java -- A meta message for MIDI files.
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
 * A system exclusive MIDI message.
 *
 * @author Anthony Green (green@redhat.com)
 * @since 1.3
 *
 */
public class MetaMessage extends MidiMessage
{
  /**
   * The META status code.  Only valid for MIDI files, not the wire protocol.
   */
  public static final int META = 0xFF;

  // The length of the variable length data length encoding.
  private int lengthLength = 0;

  /**
   * Create a default valid meta message.
   *
   * The official specs don't specify what message is to be
   * created.  For now, we create a zero length meta message
   * with a type code of 0.
   */
  public MetaMessage()
  {
    super(new byte[4]);
    data[0] = (byte) META;
    data[1] = (byte) 0; // Type
    data[2] = (byte) 1; // Length length
    data[3] = (byte) 0; // Length
    lengthLength = 1;
  }

  /**
   * Create a MetaMessage object.
   * @param data a complete system exclusive message
   */
  protected MetaMessage(byte[] data)
  {
    super(data);
    int index = 2;
    lengthLength = 1;
    while ((data[index++] & 0x80) > 0)
      lengthLength++;
  }

  /**
   * Set the meta message.
   *
   * @param type the meta type byte (< 128)
   * @param data the message data
   * @param length the length of the message data
   * @throws InvalidMidiDataException if this message is invalid
   */
  public void setMessage(int type, byte[] data, int length)
    throws InvalidMidiDataException
  {
    if (type > 127)
      throw new InvalidMidiDataException("Meta type 0x"
                                         + Integer.toHexString(type)
                                         + " must be less than 128");

    // For a nice description of how variable length values are handled,
    // see http://www.borg.com/~jglatt/tech/midifile.htm

    // First compute the length of the length value
    lengthLength = 0;
    int lengthValue = length;
    do {
      lengthValue = lengthValue >> 7;
      lengthLength++;
    } while (lengthValue > 0);

    // Now allocate our data array
    this.length = 2 + lengthLength + length;
    this.data = new byte[this.length];
    this.data[0] = (byte) META;
    this.data[1] = (byte) type;

    // Now compute the length representation
    long buffer = length & 0x7F;
    // Avoid altering length variable; PR42551
    lengthValue = length;
    while ((lengthValue >>= 7) > 0)
    {
      buffer <<= 8;
      buffer |= ((lengthValue & 0x7F) | 0x80);
    }

    // Now store the variable length length value
    int index = 2;
    do
    {
      this.data[index++] = (byte) (buffer & 0xFF);
      if ((buffer & 0x80) == 0)
        break;
      buffer >>= 8;
    } while (true);

    // Now copy the real data.
    System.arraycopy(data, 0, this.data, index, length);
  }

  /**
   * Get the meta message type.
   *
   * @return the meta message type
   */
  public int getType()
  {
    return data[1];
  }

  /**
   * Get the data for this message, not including the status,
   * type, or length information.
   *
   * @return the message data, not including status, type or lenght info
   */
  public byte[] getData()
  {
    int dataLength = length - 2 - lengthLength;
    byte[] result = new byte[dataLength];
    System.arraycopy(data, 2 + lengthLength, result, 0, dataLength);
    return result;
  }

  /* Create a deep-copy clone of this object.
   * @see java.lang.Object#clone()
   */
  public Object clone()
  {
    byte message[] = new byte[length];
    System.arraycopy(data, 0, message, 0, length);
    return new MetaMessage(message);
  }
}
