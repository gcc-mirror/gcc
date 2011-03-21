/* WAVReader.java -- Read WAV files.
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

package gnu.javax.sound.sampled.WAV;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.net.URL;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioFileFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.UnsupportedAudioFileException;
import javax.sound.sampled.spi.AudioFileReader;

/**
 * A WAV file reader.
 *
 * This code reads WAV files.
 *
 * There are many decent documents on the web describing the WAV file
 * format.  I didn't bother looking for the official document.  If it
 * exists, I'm not even sure if it is freely available.  We should
 * update this comment if we find out anything helpful here.  I used
 * http://www.sonicspot.com/guide/wavefiles.html
 *
 * @author Anthony Green (green@redhat.com)
 *
 */
public class WAVReader extends AudioFileReader
{
  private static long readUnsignedIntLE (DataInputStream is)
    throws IOException
  {
    byte[] buf = new byte[4];
    is.readFully(buf);
    return (buf[0] & 0xFF
            | ((buf[1] & 0xFF) << 8)
            | ((buf[2] & 0xFF) << 16)
            | ((buf[3] & 0xFF) << 24));
  }

  private static short readUnsignedShortLE (DataInputStream is)
    throws IOException
  {
    byte[] buf = new byte[2];
    is.readFully(buf);
    return (short) (buf[0] & 0xFF
                    | ((buf[1] & 0xFF) << 8));
  }

  /* Get an AudioFileFormat from the given File.
   * @see javax.sound.sampled.spi.AudioFileReader#getAudioFileFormat(java.io.File)
   */
  public AudioFileFormat getAudioFileFormat(File file)
      throws UnsupportedAudioFileException, IOException
  {
    InputStream is = new FileInputStream(file);
    try
      {
        return getAudioFileFormat(is);
      }
    finally
      {
        is.close();
      }
  }

  /* Get an AudioFileFormat from the given InputStream.
   * @see javax.sound.sampled.spi.AudioFileReader#getAudioFileFormat(java.io.InputStream)
   */
  public AudioFileFormat getAudioFileFormat(InputStream in)
      throws UnsupportedAudioFileException, IOException
  {
    DataInputStream din;

    if (in instanceof DataInputStream)
      din = (DataInputStream) in;
    else
      din = new DataInputStream(in);

    if (din.readInt() != 0x52494646) // "RIFF"
      throw new UnsupportedAudioFileException("Invalid WAV chunk header.");

    // Read the length of this RIFF thing.
    readUnsignedIntLE(din);

    if (din.readInt() != 0x57415645) // "WAVE"
      throw new UnsupportedAudioFileException("Invalid WAV chunk header.");

    boolean foundFmt = false;
    boolean foundData = false;

    short compressionCode = 0, numberChannels = 0, blockAlign = 0, bitsPerSample = 0;
    long sampleRate = 0, bytesPerSecond = 0;
    long chunkLength = 0;

    while (! foundData)
      {
        int chunkId = din.readInt();
        chunkLength = readUnsignedIntLE(din);
        switch (chunkId)
          {
          case 0x666D7420: // "fmt "
            foundFmt = true;
            compressionCode = readUnsignedShortLE(din);
            numberChannels = readUnsignedShortLE(din);
            sampleRate = readUnsignedIntLE(din);
            bytesPerSecond = readUnsignedIntLE(din);
            blockAlign = readUnsignedShortLE(din);
            bitsPerSample = readUnsignedShortLE(din);
            din.skip(chunkLength - 16);
            break;
          case 0x66616374: // "fact"
            // FIXME: hold compression format dependent data.
            din.skip(chunkLength);
            break;
          case 0x64617461: // "data"
            if (! foundFmt)
              throw new UnsupportedAudioFileException("This implementation requires WAV fmt chunks precede data chunks.");
            foundData = true;
            break;
          default:
            // Unrecognized chunk.  Skip it.
            din.skip(chunkLength);
          }
      }

    AudioFormat.Encoding encoding;

    switch (compressionCode)
      {
      case 1: // PCM/uncompressed
        if (bitsPerSample <= 8)
          encoding = AudioFormat.Encoding.PCM_UNSIGNED;
        else
          encoding = AudioFormat.Encoding.PCM_SIGNED;
        break;

      default:
        throw new UnsupportedAudioFileException("Unrecognized WAV compression code: 0x"
                                                + Integer.toHexString(compressionCode));
      }

    return new AudioFileFormat (AudioFileFormat.Type.WAVE,
                                new AudioFormat(encoding,
                                                (float) sampleRate,
                                                bitsPerSample,
                                                numberChannels,
                                                ((bitsPerSample + 7) / 8) * numberChannels,
                                                (float) bytesPerSecond, false),
                                (int) chunkLength);
  }

  /* Get an AudioFileFormat from the given URL.
   * @see javax.sound.sampled.spi.AudioFileReader#getAudioFileFormat(java.net.URL)
   */
  public AudioFileFormat getAudioFileFormat(URL url)
      throws UnsupportedAudioFileException, IOException
  {
    InputStream is = url.openStream();
    try
      {
        return getAudioFileFormat(is);
      }
    finally
      {
        is.close();
      }
  }

  /* Get an AudioInputStream from the given File.
   * @see javax.sound.sampled.spi.AudioFileReader#getAudioInputStream(java.io.File)
   */
  public AudioInputStream getAudioInputStream(File file)
      throws UnsupportedAudioFileException, IOException
  {
    return getAudioInputStream(new FileInputStream(file));
  }

  /* Get an AudioInputStream from the given InputStream.
   * @see javax.sound.sampled.spi.AudioFileReader#getAudioInputStream(java.io.InputStream)
   */
  public AudioInputStream getAudioInputStream(InputStream stream)
      throws UnsupportedAudioFileException, IOException
  {
    AudioFileFormat aff = getAudioFileFormat(stream);
    return new AudioInputStream(stream, aff.getFormat(), (long) aff.getFrameLength());
  }

  /* Get an AudioInputStream from the given URL.
   * @see javax.sound.sampled.spi.AudioFileReader#getAudioInputStream(java.net.URL)
   */
  public AudioInputStream getAudioInputStream(URL url)
      throws UnsupportedAudioFileException, IOException
  {
    return getAudioInputStream(url.openStream());
  }
}
