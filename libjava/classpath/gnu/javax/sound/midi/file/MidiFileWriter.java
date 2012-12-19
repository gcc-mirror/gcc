/* MidiFileWriter.java -- Write MIDI files.
   Copyright (C) 2006, 2012 Free Software Foundation, Inc.

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

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import javax.sound.midi.MetaMessage;
import javax.sound.midi.MidiEvent;
import javax.sound.midi.Sequence;
import javax.sound.midi.Track;

/**
 * A MIDI file writer.
 *
 * This code writes MIDI file types 0 and 1.
 *
 * There are many decent documents on the web describing the MIDI file
 * format.  I didn't bother looking for the official document.  If it
 * exists, I'm not even sure if it is freely available.  We should
 * update this comment if we find out anything helpful here.
 *
 * @author Anthony Green (green@redhat.com)
 *
 */
public class MidiFileWriter
    extends javax.sound.midi.spi.MidiFileWriter
{
  /* Return an array indicating which midi file types are supported.
   * @see javax.sound.midi.spi.MidiFileWriter#getMidiFileTypes()
   */
  public int[] getMidiFileTypes()
  {
    return new int[]{0, 1};
  }

  /* Return an array indicating which midi file types are supported
   * for a given Sequence.
   * @see javax.sound.midi.spi.MidiFileWriter#getMidiFileTypes(javax.sound.midi.Sequence)
   */
  public int[] getMidiFileTypes(Sequence sequence)
  {
    if (sequence.getTracks().length == 1)
      return new int[]{0};
    else
      return new int[]{1};
  }

  /* Write a sequence to an output stream in standard midi format.
   * @see javax.sound.midi.spi.MidiFileWriter#write(javax.sound.midi.Sequence, int, java.io.OutputStream)
   */
  public int write(Sequence in, int fileType, OutputStream out)
      throws IOException
  {
    MidiDataOutputStream dos = new MidiDataOutputStream (out);
    Track[] tracks = in.getTracks();
    dos.writeInt(0x4d546864); // MThd
    dos.writeInt(6);
    dos.writeShort(fileType);
    dos.writeShort(tracks.length);
    float divisionType = in.getDivisionType();
    int resolution = in.getResolution();
    // FIXME: division computation is incomplete.
    int division = 0;
    if (divisionType == Sequence.PPQ)
      division = resolution & 0x7fff;
    dos.writeShort(division);
    int length = 14;
    for (int i = 0; i < tracks.length; i++)
      length += writeTrack(tracks[i], dos);
    return length;
  }

  /**
   * Compute the length of a track as it will be written to the
   * output stream.
   *
   * @param track the track to measure
   * @param dos a MidiDataOutputStream used for helper method
   * @return the length of the track
   */
  private int computeTrackLength(Track track, MidiDataOutputStream dos)
  {
    int length = 0, i = 0, eventCount = track.size();
    long ptick = 0;
    while (i < eventCount)
      {
        MidiEvent me = track.get(i);
        long tick = me.getTick();
        length += dos.variableLengthIntLength((int) (tick - ptick));
        ptick = tick;
        length += me.getMessage().getLength();
        i++;
      }
    return length;
  }

  /**
   * Write a track to an output stream.
   *
   * @param track the track to write
   * @param dos a MidiDataOutputStream to write to
   * @return the number of bytes written
   */
  private int writeTrack(Track track, MidiDataOutputStream dos)
    throws IOException
  {
    int i = 0, elength = track.size(), trackLength;
    MidiEvent pme = null;
    dos.writeInt(0x4d54726b); // "MTrk"
    trackLength = computeTrackLength(track, dos);
    dos.writeInt(trackLength);
    while (i < elength)
      {
        MidiEvent me = track.get(i);
        int dtime = 0;
        if (pme != null)
          dtime = (int) (me.getTick() - pme.getTick());
        dos.writeVariableLengthInt(dtime);
        // FIXME: use running status byte
        byte msg[] = me.getMessage().getMessage();
        dos.write(msg);
        pme = me;
        i++;
      }

    // We're done if the last event was an End of Track meta message.
    if (pme != null && (pme.getMessage() instanceof MetaMessage))
      {
        MetaMessage mm = (MetaMessage) pme.getMessage();
        if (mm.getType() == 0x2f) // End of Track message
          return trackLength + 8;
      }

    // Write End of Track meta message
    dos.writeVariableLengthInt(0); // Delta time of 0
    dos.writeByte(0xff); // Meta Message
    dos.writeByte(0x2f); // End of Track message
    dos.writeVariableLengthInt(0); // Length of 0

    return trackLength + 8 + 4;
  }

  /* Write a Sequence to a file.
   * @see javax.sound.midi.spi.MidiFileWriter#write(javax.sound.midi.Sequence, int, java.io.File)
   */
  public int write(Sequence in, int fileType, File out) throws IOException
  {
    OutputStream os = new FileOutputStream(out);
    try
      {
        return write(in, fileType, os);
      }
    finally
      {
        os.close();
      }
  }

}
