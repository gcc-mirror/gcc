/* Format conversion API
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

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioInputStream;

/**
 * A format conversion provider supplies methods for converting between
 * different audio formats.  This abstract class defines the interface
 * to this functionality; concrete subclasses will implement the methods
 * declared here.
 * @since 1.3
 */
public abstract class FormatConversionProvider
{
  /**
   * Create a new format conversion provider.
   */
  public FormatConversionProvider()
  {
  }

  /**
   * Return an audio input stream given the desired target encoding and
   * another audio input stream.  The data in the given stream will be
   * converted to the desired encoding.
   * @param encoding the encoding
   * @param source the source audio input stream
   * @return a new audio input stream
   * @throws IllegalArgumentException if the conversion is not supported
   */
  public abstract AudioInputStream getAudioInputStream(AudioFormat.Encoding encoding,
                                                       AudioInputStream source);

  /**
   * Return an audio input stream given the desired target format and
   * another audio input stream.  The data in the given stream will be
   * converted to the desired format.
   * @param format the format
   * @param source the source audio input stream
   * @return a new audio input stream
   * @throws IllegalArgumentException if the conversion is not supported
   */
  public abstract AudioInputStream getAudioInputStream(AudioFormat format,
                                                       AudioInputStream source);

  /**
   * Return an array of all the source encodings supported by this conversion
   * provider.
   */
  public abstract AudioFormat.Encoding[] getSourceEncodings();

  /**
   * Return an array of all the target encodings supported by this conversion
   * provider.
   */
  public abstract AudioFormat.Encoding[] getTargetEncodings();

  /**
   * Return an array of all the target encodings that are available for a given
   * source format.
   * @param fmt the source format
   * @return an array of supported target encodings
   */
  public abstract AudioFormat.Encoding[] getTargetEncodings(AudioFormat fmt);

  /**
   * Return a array of all the target formats that match given target encoding,
   * and to which this provider can convert the source format.
   * @param targ the target encoding to match
   * @param src the source format
   * @return an array of supported target formats
   */
  public abstract AudioFormat[] getTargetFormats(AudioFormat.Encoding targ,
                                                 AudioFormat src);

  /**
   * Return true if this provider supports conversion from the given
   * source format to the given target encoding.
   * @param targ the target encoding
   * @param src the source format
   * @return true if the conversion is supported
   */
  public boolean isConversionSupported(AudioFormat.Encoding targ,
                                       AudioFormat src)
  {
    AudioFormat.Encoding[] encodings = getTargetEncodings(src);
    for (int i = 0; i < encodings.length; ++i)
      {
        if (targ.equals(encodings[i]))
          return true;
      }
    return false;
  }

  /**
   * Return true if this provider supports conversions from the given
   * source format to the given target format.
   * @param targ the source format
   * @param src the target format
   * @return true if the conversion is supported
   */
  public boolean isConversionSupported(AudioFormat targ, AudioFormat src)
  {
    AudioFormat[] encodings = getTargetFormats(targ.getEncoding(), src);
    return encodings.length > 0;
  }

  /**
   * Return true if an encoding matching the argument is supported as a
   * source encoding by this provider.
   * @param src the source encoding
   * @return true if it is supported
   */
  public boolean isSourceEncodingSupported(AudioFormat.Encoding src)
  {
    AudioFormat.Encoding[] srcs = getSourceEncodings();
    for (int i = 0; i < srcs.length; ++i)
      {
        if (src.equals(srcs[i]))
          return true;
      }
    return false;
  }

  /**
   * Return true if an encoding matching the argument is supported as a
   * target encoding by this provider.
   * @param targ the target encoding
   * @return true if it is supported
   */
  public boolean isTargetEncodingSupported(AudioFormat.Encoding targ)
  {
    AudioFormat.Encoding[] encodings = getTargetEncodings();
    for (int i = 0; i < encodings.length; ++i)
      {
        if (targ.equals(encodings[i]))
          return true;
      }
    return false;
  }
}
