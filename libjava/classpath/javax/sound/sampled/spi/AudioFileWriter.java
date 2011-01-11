/* Audio file writer API
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


package javax.sound.sampled.spi;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

import javax.sound.sampled.AudioFileFormat;
import javax.sound.sampled.AudioInputStream;

/**
 * This abstract class provides an API for writing audio files.  Concrete
 * subclasses implement the methods declared here.
 * @since 1.3
 */
public abstract class AudioFileWriter
{
  /**
   * Creat a new audio file writer.
   */
  public AudioFileWriter()
  {
  }

  /**
   * Return an array of all audio file format types supported by this
   * provider.
   */
  public abstract AudioFileFormat.Type[] getAudioFileTypes();

  /**
   * Return an array of all the audio file format types supported by this
   * provider, which can be written given the input stream.
   * @param ais the audio input stream
   */
  public abstract AudioFileFormat.Type[] getAudioFileTypes(AudioInputStream ais);

  /**
   * Return true if the indicated type is supported by this provider.
   * @param type the audio file format type
   */
  public boolean isFileTypeSupported(AudioFileFormat.Type type)
  {
    AudioFileFormat.Type[] types = getAudioFileTypes();
    for (int i = 0; i < types.length; ++i)
      {
        if (type.equals(types[i]))
          return true;
      }
    return false;
  }

  /**
   * Return true if the indicated type is supported by this provider,
   * and can be written from the given audio input stream.
   * @param type the audio file format type
   * @param ais the audio input stream to write
   */
  public boolean isFileTypeSupported(AudioFileFormat.Type type,
                                     AudioInputStream ais)
  {
    AudioFileFormat.Type[] types = getAudioFileTypes(ais);
    for (int i = 0; i < types.length; ++i)
      {
        if (type.equals(types[i]))
          return true;
      }
    return false;
  }

  /**
   * Write audio data to a file.
   * @param ais the audio input stream to write
   * @param type the desired audio file format type
   * @param out the file to write to
   * @return the number of bytes written
   * @throws IOException if an I/O error occurs when writing
   */
  public abstract int write(AudioInputStream ais, AudioFileFormat.Type type,
                            File out)
    throws IOException;

  /**
   * Write audio data to an output stream.
   * @param ais the audio input stream to write
   * @param type the desired audio file format type
   * @param os the output stream
   * @return the number of bytes written
   * @throws IOException if an I/O error occurs when writing
   */
  public abstract int write(AudioInputStream ais, AudioFileFormat.Type type,
                            OutputStream os)
    throws IOException;
}
