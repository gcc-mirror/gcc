/* MidiFileReader.java -- Read MIDI files.
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

import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import javax.sound.midi.InvalidMidiDataException;
import javax.sound.midi.MetaMessage;
import javax.sound.midi.MidiEvent;
import javax.sound.midi.MidiFileFormat;
import javax.sound.midi.MidiMessage;
import javax.sound.midi.Sequence;
import javax.sound.midi.ShortMessage;
import javax.sound.midi.SysexMessage;
import javax.sound.midi.Track;

/**
 * A MIDI file reader.
 *
 * This code reads MIDI file types 0 and 1.
 *
 * There are many decent documents on the web describing the MIDI file
 * format.  I didn't bother looking for the official document.  If it
 * exists, I'm not even sure if it is freely available.  We should
 * update this comment if we find out anything helpful here.
 *
 * @author Anthony Green (green@redhat.com)
 *
 */
public class MidiFileReader extends javax.sound.midi.spi.MidiFileReader
{
  /* Get the MidiFileFormat for the given input stream.
   * @see javax.sound.midi.spi.MidiFileReader#getMidiFileFormat(java.io.InputStream)
   */
  public MidiFileFormat getMidiFileFormat(InputStream in)
    throws InvalidMidiDataException, IOException
  {
    DataInputStream din;
    if (in instanceof DataInputStream)
      din = (DataInputStream) in;
    else
      din = new DataInputStream(in);

    int type, ntracks, division, resolution, bytes;
    float divisionType;

    if (din.readInt() != 0x4d546864) // "MThd"
      throw new InvalidMidiDataException("Invalid MIDI chunk header.");

    bytes = din.readInt();
    if (bytes < 6)
      throw new
        InvalidMidiDataException("Invalid MIDI chunk header length: " + bytes);

    type = din.readShort();
    if (type < 0 || type > 2)
      throw new
        InvalidMidiDataException("Invalid MIDI file type value: " + type);

    ntracks = din.readShort();
    if (ntracks <= 0)
      throw new
        InvalidMidiDataException("Invalid number of MIDI tracks: " + ntracks);

    division = din.readShort();
    if ((division & 0x8000) != 0)
      {
        division = -((division >>> 8) & 0xFF);
        switch (division)
          {
          case 24:
            divisionType = Sequence.SMPTE_24;
            break;

          case 25:
            divisionType = Sequence.SMPTE_25;
            break;

          case 29:
            divisionType = Sequence.SMPTE_30DROP;
            break;

          case 30:
            divisionType = Sequence.SMPTE_30;
            break;

          default:
            throw new
              InvalidMidiDataException("Invalid MIDI frame division type: "
                                       + division);
          }
        resolution = division & 0xff;
      }
    else
      {
        divisionType = Sequence.PPQ;
        resolution = division & 0x7fff;
      }

    // If we haven't read every byte in the header now, just skip the rest.
    din.skip(bytes - 6);

    return new ExtendedMidiFileFormat(type, divisionType, resolution,
                                      MidiFileFormat.UNKNOWN_LENGTH,
                                      MidiFileFormat.UNKNOWN_LENGTH, ntracks);
  }

  /* Get the MidiFileFormat from the given URL.
   * @see javax.sound.midi.spi.MidiFileReader#getMidiFileFormat(java.net.URL)
   */
  public MidiFileFormat getMidiFileFormat(URL url)
    throws InvalidMidiDataException, IOException
  {
    InputStream is = url.openStream();
    try
      {
        return getMidiFileFormat(is);
      }
    finally
      {
        is.close();
      }
  }

  /* Get the MidiFileFormat from the given file.
   * @see javax.sound.midi.spi.MidiFileReader#getMidiFileFormat(java.io.File)
   */
  public MidiFileFormat getMidiFileFormat(File file)
    throws InvalidMidiDataException, IOException
  {
    InputStream is = new FileInputStream(file);
    try
      {
        return getMidiFileFormat(is);
      }
    finally
      {
        is.close();
      }
  }

  /* Get the MIDI Sequence found in this input stream.
   * @see javax.sound.midi.spi.MidiFileReader#getSequence(java.io.InputStream)
   */
  public Sequence getSequence(InputStream is) throws InvalidMidiDataException,
    IOException
  {
    MidiDataInputStream din = new MidiDataInputStream(is);
    ExtendedMidiFileFormat mff = (ExtendedMidiFileFormat) getMidiFileFormat(din);

    Sequence seq = new Sequence(mff.getDivisionType(), mff.getResolution());

    int ntracks = mff.getNumberTracks();

    while (ntracks-- > 0)
      {
        Track track = seq.createTrack();
        int Mtrk = din.readInt();
        if (Mtrk != 0x4d54726b)
          throw new InvalidMidiDataException("Invalid MIDI track header.");
        int length = din.readInt();

        int runningStatus = -1;
        int click = 0;

        // Set this to true when we've hit an End of Track meta event.
        boolean done = false;

        // Read all events.
        while (! done)
          {
            MidiMessage mm;
            int dtime = din.readVariableLengthInt();
            click += dtime;

            int sbyte = din.readUnsignedByte();

            if (sbyte < 0xf0)
              {
                ShortMessage sm;
                switch (sbyte & 0xf0)
                  {
                  case ShortMessage.NOTE_OFF:
                  case ShortMessage.NOTE_ON:
                  case ShortMessage.POLY_PRESSURE:
                  case ShortMessage.CONTROL_CHANGE:
                  case ShortMessage.PITCH_BEND:
                  case ShortMessage.SONG_POSITION_POINTER:
                    sm = new ShortMessage();
                    sm.setMessage(sbyte, din.readByte(), din.readByte());
                    runningStatus = sbyte;
                    break;

                  case ShortMessage.PROGRAM_CHANGE:
                  case ShortMessage.CHANNEL_PRESSURE:
                  case ShortMessage.SONG_SELECT:
                  case 0xF5: // FIXME: unofficial bus select. Not in spec??
                    sm = new ShortMessage();
                    sm.setMessage(sbyte, din.readByte(), 0);
                    runningStatus = sbyte;
                    break;

                  case ShortMessage.TUNE_REQUEST:
                  case ShortMessage.END_OF_EXCLUSIVE:
                  case ShortMessage.TIMING_CLOCK:
                  case ShortMessage.START:
                  case ShortMessage.CONTINUE:
                  case ShortMessage.STOP:
                  case ShortMessage.ACTIVE_SENSING:
                  case ShortMessage.SYSTEM_RESET:
                    sm = new ShortMessage();
                    sm.setMessage(sbyte, 0, 0);
                    runningStatus = sbyte;
                    break;

                  default:
                    if (runningStatus != - 1)
                      {
                        switch (runningStatus & 0xf0)
                          {
                          case ShortMessage.NOTE_OFF:
                          case ShortMessage.NOTE_ON:
                          case ShortMessage.POLY_PRESSURE:
                          case ShortMessage.CONTROL_CHANGE:
                          case ShortMessage.PITCH_BEND:
                          case ShortMessage.SONG_POSITION_POINTER:
                            sm = new ShortMessage();
                            sm.setMessage(runningStatus, sbyte, din.readByte());
                            break;

                          case ShortMessage.PROGRAM_CHANGE:
                          case ShortMessage.CHANNEL_PRESSURE:
                          case ShortMessage.SONG_SELECT:
                          case 0xF5: // FIXME: unofficial bus select. Not in
                                     // spec??
                            sm = new ShortMessage();
                            sm.setMessage(runningStatus, sbyte, 0);
                            continue;

                          case ShortMessage.TUNE_REQUEST:
                          case ShortMessage.END_OF_EXCLUSIVE:
                          case ShortMessage.TIMING_CLOCK:
                          case ShortMessage.START:
                          case ShortMessage.CONTINUE:
                          case ShortMessage.STOP:
                          case ShortMessage.ACTIVE_SENSING:
                          case ShortMessage.SYSTEM_RESET:
                            sm = new ShortMessage();
                            sm.setMessage(runningStatus, 0, 0);
                            continue;

                          default:
                            throw new
                              InvalidMidiDataException("Invalid Short MIDI Event: "
                                                       + sbyte);
                          }
                      }
                    else
                      throw new
                        InvalidMidiDataException("Invalid Short MIDI Event: "
                                                 + sbyte);
                  }
                mm = sm;
              }
            else if (sbyte == 0xf0 || sbyte == 0xf7)
              {
                // System Exclusive event
                int slen = din.readVariableLengthInt();
                byte sysex[] = new byte[slen];
                din.readFully(sysex);
                SysexMessage sm = new SysexMessage();
                sm.setMessage(sbyte, sysex, slen);
                mm = sm;
                runningStatus = - 1;
              }
            else if (sbyte == 0xff)
              {
                // Meta Message
                byte mtype = din.readByte();
                int mlen = din.readVariableLengthInt();
                byte meta[] = new byte[mlen];
                din.readFully(meta);
                MetaMessage metam = new MetaMessage();
                metam.setMessage(mtype, meta, mlen);
                mm = metam;

                if (mtype == 0x2f) // End of Track
                  done = true;

                runningStatus = - 1;
              }
            else
              {
                throw new InvalidMidiDataException("Invalid status byte: "
                                                   + sbyte);
              }

            track.add(new MidiEvent(mm, click));
          }
      }

    return seq;
  }

  /* Get the MIDI Sequence found at the given URL.
   * @see javax.sound.midi.spi.MidiFileReader#getSequence(java.net.URL)
   */
  public Sequence getSequence(URL url) throws InvalidMidiDataException,
    IOException
  {
    InputStream is = url.openStream();
    try
      {
        return getSequence(is);
      }
    finally
      {
        is.close();
      }
  }

  /* Get the MIDI Sequence found in the given file.
   * @see javax.sound.midi.spi.MidiFileReader#getSequence(java.io.File)
   */
  public Sequence getSequence(File file) throws InvalidMidiDataException,
    IOException
  {
    InputStream is = new FileInputStream(file);
    try
      {
        return getSequence(is);
      }
    finally
      {
        is.close();
      }
  }
}
