/* MidiFileFormat.java -- Information about a MIDI file
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
 * Describe a MIDI file, including specifics about its type, length and timing.
 *
 * @author Anthony Green (green@redhat.com)
 * @since 1.3
 *
 */
public class MidiFileFormat
{
  /**
   * The MIDI file type.  This is either 0, 1 or 2.
   *
   * Type 0 files contain a single track and represents a single song
   * performance.
   * Type 1 may contain multiple tracks for a single song performance.
   * Type 2 may contain multiple tracks, each representing a
   * separate song performance.
   *
   * See http://en.wikipedia.org/wiki/MIDI#MIDI_file_formats for more
   * information.
   */
  protected int type;

  /**
   * The division type of the MIDI file.
   */
  protected float divisionType;

  /**
   * The timing resolution of the MIDI file.
   */
  protected int resolution;

  /**
   * The size of the MIDI file in bytes.
   */
  protected int byteLength = UNKNOWN_LENGTH;

  /**
   * The length of the MIDI file in microseconds.
   */
  protected long microsecondLength = UNKNOWN_LENGTH;

  /**
   * A special value indicating an unknown quantity.
   */
  public static final int UNKNOWN_LENGTH = -1; // FIXME is this really -1?

  /**
   * Create a MidiFileFormat object from the given parameters.
   *
   * @param type the MIDI file type (0, 1, or 2)
   * @param divisionType the MIDI file division type
   * @param resolution the MIDI file timing resolution
   * @param bytes the MIDI file size in bytes
   * @param microseconds the MIDI file length in microseconds
   */
  public MidiFileFormat(int type, float divisionType,
                        int resolution, int bytes, long microseconds)
  {
    this.type = type;
    this.divisionType = divisionType;
    this.resolution = resolution;
    this.byteLength = bytes;
    this.microsecondLength = microseconds;
  }

  /**
   * Get the MIDI file type (0, 1, or 2).
   *
   * @return the MIDI file type (0, 1, or 2)
   */
  public int getType()
  {
    return type;
  }

  /**
   * Get the file division type.
   *
   * @return the file divison type
   */
  public float getDivisionType()
  {
    return divisionType;
  }

  /**
   * Get the file timing resolution.  If the division type is PPQ, then this
   * is value represents ticks per beat, otherwise it's ticks per frame (SMPTE).
   *
   * @return the timing resolution in ticks per beat or ticks per frame
   */
  public int getResolution()
  {
    return resolution;
  }

  /**
   * Get the file length in bytes.
   *
   * @return the file length in bytes or UNKNOWN_LENGTH
   */
  public int getByteLength()
  {
    return byteLength;
  }

  /**
   * Get the file length in microseconds.
   *
   * @return the file length in microseconds or UNKNOWN_LENGTH
   */
  public long getMicrosecondLength()
  {
    return microsecondLength;
  }
}
