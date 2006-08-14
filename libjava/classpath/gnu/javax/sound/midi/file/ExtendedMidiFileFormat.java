/* ExtendedMidiFileFormat.java -- extended with track count info.
   Copyright (C) 2006 Free Software Foundation, Inc.

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

package gnu.javax.sound.midi.file;

/**
 * ExtendedMidiFileFormat is a package private class that simply 
 * adds the number of MIDI tracks for the MidiFileFormat class.
 *
 * @author Anthony Green (green@redhat.com)
 */
class ExtendedMidiFileFormat
    extends javax.sound.midi.MidiFileFormat
{
  private int ntracks;
  
  /**
   * Get the number of tracks for this MIDI file.
   * 
   * @return the number of tracks for this MIDI file
   */
  public int getNumberTracks()
  {
    return ntracks;
  }
  
  /**
   * Create an ExtendedMidiFileFormat object from the given parameters.
   *
   * @param type the MIDI file type (0, 1, or 2)
   * @param divisionType the MIDI file division type
   * @param resolution the MIDI file timing resolution
   * @param bytes the MIDI file size in bytes
   * @param microseconds the MIDI file length in microseconds
   * @param ntracks the number of tracks
   */
  public ExtendedMidiFileFormat(int type, float divisionType, int resolution, 
                                int bytes, long microseconds, int ntracks)
  {
    super(type, divisionType, resolution, bytes, microseconds);
    this.ntracks = ntracks;
  }
}
