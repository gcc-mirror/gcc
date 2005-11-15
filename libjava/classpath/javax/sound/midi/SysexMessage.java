/* SysexMessage.java -- System Exclusive MIDI message.
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
public class SysexMessage extends MidiMessage
{
  public static final int SYSTEM_EXCLUSIVE = 0xF0;
  
  public static final int SPECIAL_SYSTEM_EXCLUSIVE = 0xF7;
  
  /**
   * Create a default valid system exclusive message.
   * 
   * The official specs don't specify what message is to be
   * created.  Our implementation creates an empty 
   * system exclusive message.
   */
  public SysexMessage()
  {
    super(new byte[2]);
    data[0] = (byte) SYSTEM_EXCLUSIVE;
    data[1] = (byte) ShortMessage.END_OF_EXCLUSIVE;
  }
  
  /**
   * Create a SysexMessage object.
   * @param data a complete system exclusive message
   */
  public SysexMessage(byte[] data)
  {
    super(data);   
  }
  
  /**
   * Set the sysex message.  The first data byte (status) must be
   * 0xF0 or 0xF7.
   *  
   * @param data the message data
   * @param length the length of the message data
   * @throws InvalidMidiDataException if the status byte is not 0xF0 or 0xF7
   */
  public void setMessage(byte[] data, int length)
    throws InvalidMidiDataException
  {
    if (data[0] != SYSTEM_EXCLUSIVE
        || data[0] != SPECIAL_SYSTEM_EXCLUSIVE)
      throw new InvalidMidiDataException("Sysex message starts with 0x"
                                         + Integer.toHexString(data[0])
                                         + " instead of 0xF0 or 0xF7");
    super.setMessage(data, length);
  }

  /**
   * Set the sysex message.  status must be either 0xF0 or 0xF7.
   *  
   * @param status the sysex statys byte (0xF0 or 0xF7)
   * @param data the message data
   * @param length the length of the message data
   * @throws InvalidMidiDataException if status is not 0xF0 or 0xF7
   */
  public void setMessage(int status, byte[] data, int length)
    throws InvalidMidiDataException
  {
    if (status != SYSTEM_EXCLUSIVE
        || status != SPECIAL_SYSTEM_EXCLUSIVE)
      throw new InvalidMidiDataException("Sysex message starts with 0x"
                                         + Integer.toHexString(status)
                                         + " instead of 0xF0 or 0xF7");
    this.data = new byte[length+1];
    this.data[0] = (byte) status;
    System.arraycopy(data, 0, this.data, 1, length);
    this.length = length+1;
  }
  
  /**
   * Get the data for this message, not including the status byte.
   * @return the message data, not including the status byte
   */
  public byte[] getData()
  {
    byte[] result = new byte[length - 1];
    System.arraycopy(data, 1, result, 0, length - 1);
    return result;
  }
  
  /* Create a deep-copy clone of this object.
   * @see java.lang.Object#clone()
   */
  public Object clone()
  {
    byte message[] = new byte[length];
    System.arraycopy(data, 0, message, 0, length);
    return new SysexMessage(message); 
  }
}

