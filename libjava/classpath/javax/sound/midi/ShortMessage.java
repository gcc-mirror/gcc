/* ShortMessage.java -- A MIDI message no longer than 3 bytes
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
 * A short MIDI message that is no longer than 3 bytes long.
 *
 * @author Anthony Green (green@redhat.com)
 * @since 1.3
 *
 */
public class ShortMessage extends MidiMessage
{
  /**
   * Status byte for Time Code message.
   */
  public static final int MIDI_TIME_CODE = 0xF1;

  /**
   * Status byte for Song Position Pointer message.
   */
  public static final int SONG_POSITION_POINTER = 0xF2;

  /**
   * Status byte for Song Select message.
   */
  public static final int SONG_SELECT = 0xF3;

  /**
   * Status byte for Tune Request message.
   */
  public static final int TUNE_REQUEST = 0xF6;

  /**
   * Status byte for End Of Exclusive message.
   */
  public static final int END_OF_EXCLUSIVE = 0xF7;

  /**
   * Status byte for Timing Clock message.
   */
  public static final int TIMING_CLOCK = 0xF8;

  /**
   * Status byte for Start message.
   */
  public static final int START = 0xFA;

  /**
   * Status byte for Continue message.
   */
  public static final int CONTINUE = 0xFB;

  /**
   * Status byte for Stop message.
   */
  public static final int STOP = 0xFC;

  /**
   * Status byte for Active Sensing message.
   */
  public static final int ACTIVE_SENSING = 0xFE;

  /**
   * Status byte for System Reset message.
   */
  public static final int SYSTEM_RESET = 0xFF;

  /**
   * Status nibble for Note Off message.
   */
  public static final int NOTE_OFF = 0x80;

  /**
   * Status nibble for Note On message.
   */
  public static final int NOTE_ON = 0x90;

  /**
   * Status nibble for Poly Pressure message.
   */
  public static final int POLY_PRESSURE = 0xA0;

  /**
   * Status nibble for Control Change message.
   */
  public static final int CONTROL_CHANGE = 0xB0;

  /**
   * Status nibble for Program Change message.
   */
  public static final int PROGRAM_CHANGE = 0xC0;

  /**
   * Statue nibble for Channel Pressure message.
   */
  public static final int CHANNEL_PRESSURE = 0xD0;

  /**
   * Status nibble for Pitch Bend message.
   */
  public static final int PITCH_BEND = 0xE0;

  // Create and initialize a default, arbitrary message.
  private static byte[] defaultMessage;
  static
  {
    defaultMessage = new byte[1];
    defaultMessage[0] = (byte) STOP;
  }

  /**
   * Create a short MIDI message.
   *
   * The spec requires that this represent a valid MIDI message, but doesn't
   * specify what it should be.  We've chosen the STOP message for our
   * implementation.
   */
  public ShortMessage()
  {
    this(defaultMessage);
  }

  /**
   * Create a short MIDI message.
   *
   * The data argument should be a valid MIDI message.  Unfortunately the spec
   * does not allow us to throw an InvalidMidiDataException if data is invalid.
   *
   * @param data the message data
   */
  protected ShortMessage(byte[] data)
  {
    super(data);
  }

  /**
   * Set the MIDI message.
   *
   * @param status the status byte for this message
   * @param data1 the first data byte for this message
   * @param data2 the second data byte for this message
   * @throws InvalidMidiDataException if status is bad, or data is out of range
   */
  public void setMessage(int status, int data1, int data2)
    throws InvalidMidiDataException
  {
    length = getDataLength(status);
    length++;
    if (data == null || data.length < length)
      data = new byte[length];
    data[0] = (byte) status;
    if (length > 1)
    {
      if (data1 < 0 || data1 > 127)
        throw new InvalidMidiDataException("data1 (" + data1
                                           + ") must be between 0 and 127.");
      data[1] = (byte) data1;
      if (length > 2)
      {
        if (data2 < 0 || data2 > 127)
          throw new InvalidMidiDataException("data2 (" + data2
                                             + ") must be between 0 and 127.");
        data[2] = (byte) data2;
      }
    }
  }

  public void setMessage(int command, int channel, int data1, int data2)
    throws InvalidMidiDataException
  {
    // TODO: This could probably stand some error checking.
    // It currently assumes command and channel are valid values.
    setMessage(command + channel, data1, data2);
  }

  /**
   * Set the MIDI message to one that requires no data bytes.
   *
   * @param status the status byte for this message
   * @throws InvalidMidiDataException if status is bad, or requires data
   */
  public void setMessage(int status) throws InvalidMidiDataException
  {
    int length = getDataLength(status);
    if (length != 0)
      throw new InvalidMidiDataException("Status byte 0x"
                                         + Integer.toHexString(status)
                                         + " requires "
                                         + length + " bytes of data.");
    setMessage(status, 0, 0);
  }


  /**
   * Return the number of data bytes needed for a given MIDI status byte.
   *
   * @param status the status byte for a short MIDI message
   * @return the number of data bytes needed for this status byte
   * @throws InvalidMidiDataException if status is an invalid status byte
   */
  protected final int getDataLength(int status) throws InvalidMidiDataException
  {
    int originalStatus = status;

    if ((status & 0xF0) != 0xF0)
      status &= 0xF0;

    switch (status)
    {
    case NOTE_OFF:
    case NOTE_ON:
    case POLY_PRESSURE:
    case CONTROL_CHANGE:
    case PITCH_BEND:
    case SONG_POSITION_POINTER:
      return 2;

    case PROGRAM_CHANGE:
    case CHANNEL_PRESSURE:
    case SONG_SELECT:
    case 0xF5:  // FIXME: unofficial bus select.  Not in spec??
      return 1;

    case TUNE_REQUEST:
    case END_OF_EXCLUSIVE:
    case TIMING_CLOCK:
    case START:
    case CONTINUE:
    case STOP:
    case ACTIVE_SENSING:
    case SYSTEM_RESET:
      return 0;

    default:
      throw new InvalidMidiDataException("Invalid status: 0x"
                                         + Integer.toHexString(originalStatus));
    }
  }

  /**
   * Get the channel information from this MIDI message, assuming it is a
   * MIDI channel message.
   *
   * @return the MIDI channel for this message
   */
  public int getChannel()
  {
    return data[0] & 0x0F;
  }

  /**
   * Get the command nibble from this MIDI message, assuming it is a MIDI
   * channel message.
   *
   * @return the MIDI command for this message
   */
  public int getCommand()
  {
    return data[0] & 0xF0;
  }

  /**
   * Get the first data byte from this message, assuming it exists, and
   * zero otherwise.
   *
   * @return the first data byte or zero if none exists.
   */
  public int getData1()
  {
    if (length > 1)
      return data[1];
    else
      return 0;
  }

  /**
   * Get the second data byte from this message, assuming it exists, and
   * zero otherwise.
   *
   * @return the second date byte or zero if none exists.
   */
  public int getData2()
  {
    if (length > 2)
      return data[2];
    else
      return 0;
  }

  /* Create a deep-copy clone of this object.
   * @see java.lang.Object#clone()
   */
  public Object clone()
  {
    byte message[] = new byte[length];
    System.arraycopy(data, 0, message, 0, length);
    return new ShortMessage(message);
  }
}
