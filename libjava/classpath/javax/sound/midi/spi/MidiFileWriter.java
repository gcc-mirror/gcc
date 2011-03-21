/* MidiFileWriter.java -- MIDI file writing services
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


package javax.sound.midi.spi;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

import javax.sound.midi.Sequence;

/**
 * MidiFileWriter provides MIDI file writing services.
 *
 * There are three types of Standard MIDI File (SMF) formats,
 * represented by integers 0, 1, and 2.
 *
 * Type 0 files contain a single track and represents a single song
 * performance.
 * Type 1 may contain multiple tracks for a single song performance.
 * Type 2 may contain multiple tracks, each representing a
 * separate song performance.
 *
 * See http://en.wikipedia.org/wiki/MIDI#MIDI_file_formats for more
 * information.
 *
 * @author Anthony Green (green@redhat.com)
 * @since 1.3
 *
 */
public abstract class MidiFileWriter
{
  /**
   * Return the MIDI file types supported by this writer.
   *
   * @return the MIDI file types, or an empty array
   */
  public abstract int[] getMidiFileTypes();

  /**
   * Return the MIDI file types supported by this writer for the
   * given sequence.
   *
   * @param sequence the sequence we'd like to write
   * @return the MIDI file types, or an empty array
   */
  public abstract int[] getMidiFileTypes(Sequence sequence);

  /**
   * Returns true if this writer supports the given file type.
   *
   * @param fileType the file type we're asking about
   * @return true if this writer supports fileType, false otherwise
   */
  public boolean isFileTypeSupported(int fileType)
  {
    int types[] = getMidiFileTypes();
    for (int i = types.length; i > 0;)
    {
      if (types[--i] == fileType)
        return true;
    }
    return false;
  }

  /**
   * Returns true if this writer supports the given file type for the
   * given sequence.
   *
   * @param fileType the file type we're asking about
   * @param sequence the sequence we'd like to write
   * @return true if this writer supports fileType, false otherwise
   */
  public boolean isFileTypeSupported(int fileType, Sequence sequence)
  {
    int types[] = getMidiFileTypes(sequence);
    for (int i = types.length; i > 0;)
    {
      if (types[--i] == fileType)
        return true;
    }
    return false;
  }

  /**
   * Write a sequence to a stream using the specified MIDI file type.
   *
   * @param in the sequence to write
   * @param fileType the MIDI file type to use
   * @param out the output stream to write to
   * @return the number of byte written
   * @throws IOException if an I/O exception happens
   */
  public abstract int write(Sequence in, int fileType, OutputStream out)
    throws IOException;

  /**
   * Write a sequence to a file using the specified MIDI file type.
   *
   * @param in the sequence to write
   * @param fileType the MIDI file type to use
   * @param out the file to write to
   * @return the number of byte written
   * @throws IOException if an I/O exception happens
   */
  public abstract int write(Sequence in, int fileType, File out)
    throws IOException;
}
