/* Main interface to audio system
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


package javax.sound.sampled;

import gnu.classpath.ServiceFactory;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.util.HashSet;
import java.util.Iterator;

import javax.sound.sampled.spi.AudioFileReader;
import javax.sound.sampled.spi.AudioFileWriter;
import javax.sound.sampled.spi.FormatConversionProvider;
import javax.sound.sampled.spi.MixerProvider;

/**
 * This clas is the primary interface to the audio system.  It contains
 * a number of static methods which can be used to access this package's
 * functionality.
 * 
 * @since 1.3
 */
public class AudioSystem
{
  /**
   * A constant which can be passed to a number of methods in this package,
   * to indicate an unspecified value.
   */
  public static final int NOT_SPECIFIED = -1;

  // This class is not instantiable.
  private AudioSystem()
  {
  }

  /**
   * Return the file format of a given File.
   * @param f the file to check
   * @return the format of the file
   * @throws UnsupportedAudioFileException if the file's format is not 
   * recognized
   * @throws IOException if there is an I/O error reading the file
   */
  public static AudioFileFormat getAudioFileFormat(File f)
    throws UnsupportedAudioFileException, IOException
  {
    Iterator i = ServiceFactory.lookupProviders(AudioFileReader.class);
    while (i.hasNext())
      {
        AudioFileReader reader = (AudioFileReader) i.next();
        try
          {
            return reader.getAudioFileFormat(f);
          }
        catch (UnsupportedAudioFileException _)
          {
            // Try the next provider.
          }
      }
    throw new UnsupportedAudioFileException("file type not recognized");
  }

  /**
   * Return the file format of a given input stream.
   * @param is the input stream to check
   * @return the format of the stream
   * @throws UnsupportedAudioFileException if the stream's format is not 
   * recognized
   * @throws IOException if there is an I/O error reading the stream
   */
  public static AudioFileFormat getAudioFileFormat(InputStream is)
    throws UnsupportedAudioFileException, IOException
  {
    Iterator i = ServiceFactory.lookupProviders(AudioFileReader.class);
    while (i.hasNext())
      {
        AudioFileReader reader = (AudioFileReader) i.next();
        try
          {
            return reader.getAudioFileFormat(is);
          }
        catch (UnsupportedAudioFileException _)
          {
            // Try the next provider.
          }
      }
    throw new UnsupportedAudioFileException("input stream type not recognized");
  }

  /**
   * Return the file format of a given URL.
   * @param url the URL to check
   * @return the format of the URL
   * @throws UnsupportedAudioFileException if the URL's format is not 
   * recognized
   * @throws IOException if there is an I/O error reading the URL
   */
  public static AudioFileFormat getAudioFileFormat(URL url)
    throws UnsupportedAudioFileException, IOException
  {
    Iterator i = ServiceFactory.lookupProviders(AudioFileReader.class);
    while (i.hasNext())
      {
        AudioFileReader reader = (AudioFileReader) i.next();
        try
          {
            return reader.getAudioFileFormat(url);
          }
        catch (UnsupportedAudioFileException _)
          {
            // Try the next provider.
          }
      }
    throw new UnsupportedAudioFileException("URL type not recognized");
  }

  /**
   * Return an array of all the supported AudioFileFormat types.
   * @return an array of unique types
   */
  public static AudioFileFormat.Type[] getAudioFileTypes()
  {
    HashSet<AudioFileFormat.Type> result
      = new HashSet<AudioFileFormat.Type>();
    Iterator i = ServiceFactory.lookupProviders(AudioFileWriter.class);
    while (i.hasNext())
      {
        AudioFileWriter writer = (AudioFileWriter) i.next();
        AudioFileFormat.Type[] types = writer.getAudioFileTypes();
        for (int j = 0; j < types.length; ++j)
          result.add(types[j]);
      }
    return result.toArray(new AudioFileFormat.Type[result.size()]);
  }

  /**
   * Return an array of all the supported AudioFileFormat types which match the
   * given audio input stream
   * @param ais the audio input stream
   * @return an array of unique types
   */
  public static AudioFileFormat.Type[] getAudioFileTypes(AudioInputStream ais)
  {
    HashSet<AudioFileFormat.Type> result
      = new HashSet<AudioFileFormat.Type>();
    Iterator i = ServiceFactory.lookupProviders(AudioFileWriter.class);
    while (i.hasNext())
      {
        AudioFileWriter writer = (AudioFileWriter) i.next();
        AudioFileFormat.Type[] types = writer.getAudioFileTypes(ais);
        for (int j = 0; j < types.length; ++j)
          result.add(types[j]);
      }
    return result.toArray(new AudioFileFormat.Type[result.size()]);
  }

  /**
   * Given an audio input stream, this will try to create a new audio input
   * stream whose encoding matches the given target encoding.  If no provider
   * offers this conversion, an exception is thrown. 
   * @param targ the target encoding
   * @param ais the original audio stream
   * @return a new audio stream
   * @throws IllegalArgumentException if the conversion cannot be made
   */
  public static AudioInputStream getAudioInputStream(AudioFormat.Encoding targ,
						     AudioInputStream ais)
  {
    Iterator i = ServiceFactory.lookupProviders(FormatConversionProvider.class);
    while (i.hasNext())
      {
        FormatConversionProvider prov = (FormatConversionProvider) i.next();
        if (! prov.isConversionSupported(targ, ais.getFormat()))
          continue;
        return prov.getAudioInputStream(targ, ais);
      }
    throw new IllegalArgumentException("encoding not supported for stream");
 }

  /**
   * Given an audio input stream, this will try to create a new audio input
   * stream whose format matches the given target format.  If no provider
   * offers this conversion, an exception is thrown. 
   * @param targ the target format
   * @param ais the original audio stream
   * @return a new audio stream
   * @throws IllegalArgumentException if the conversion cannot be made
   */
  public static AudioInputStream getAudioInputStream(AudioFormat targ,
						     AudioInputStream ais)
  {
    Iterator i = ServiceFactory.lookupProviders(FormatConversionProvider.class);
    while (i.hasNext())
      {
        FormatConversionProvider prov = (FormatConversionProvider) i.next();
        if (! prov.isConversionSupported(targ, ais.getFormat()))
          continue;
        return prov.getAudioInputStream(targ, ais);
      }
    throw new IllegalArgumentException("format not supported for stream");
   }

  /**
   * Return an audio input stream for the file.
   * @param f the file to read
   * @return an audio input stream for the file
   * @throws UnsupportedAudioFileException if the file's audio format is not
   * recognized
   * @throws IOException if there is an error while reading the file
   */
  public static AudioInputStream getAudioInputStream(File f)
    throws UnsupportedAudioFileException, IOException
  {
    Iterator i = ServiceFactory.lookupProviders(AudioFileReader.class);
    while (i.hasNext())
      {
        AudioFileReader reader = (AudioFileReader) i.next();
        try
          {
            return reader.getAudioInputStream(f);
          }
        catch (UnsupportedAudioFileException _)
          {
            // Try the next provider.
          }
      }
    throw new UnsupportedAudioFileException("file type not recognized");
  }

  /**
   * Return an audio input stream given an input stream.
   * @param is the input stream
   * @return an audio input stream
   * @throws UnsupportedAudioFileException if the input stream's audio format
   * is not supported by any of the installed providers
   * @throws IOException if there is an error while reading the input stream
   */
  public static AudioInputStream getAudioInputStream(InputStream is)
    throws UnsupportedAudioFileException, IOException
  {
    Iterator i = ServiceFactory.lookupProviders(AudioFileReader.class);
    while (i.hasNext())
      {
        AudioFileReader reader = (AudioFileReader) i.next();
        try
          {
            return reader.getAudioInputStream(is);
          }
        catch (UnsupportedAudioFileException _)
          {
            // Try the next provider.
          }
      }
    throw new UnsupportedAudioFileException("input stream type not recognized");
  }

  /**
   * Return an audio input stream for the given URL.
   * @param url the URL
   * @return an audio input stream
   * @throws UnsupportedAudioFileException if the URL's audio format is not
   * supported by any of the installed providers
   * @throws IOException if there is an error while reading the URL
   */
  public static AudioInputStream getAudioInputStream(URL url)
    throws UnsupportedAudioFileException, IOException
  {
    Iterator i = ServiceFactory.lookupProviders(AudioFileReader.class);
    while (i.hasNext())
      {
        AudioFileReader reader = (AudioFileReader) i.next();
        try
          {
            return reader.getAudioInputStream(url);
          }
        catch (UnsupportedAudioFileException _)
          {
            // Try the next provider.
          }
      }
    throw new UnsupportedAudioFileException("URL type not recognized");
  }

  /**
   * Return a new clip which can be used for playing back an audio stream.
   * @throws LineUnavailableException if a clip is not available for some
   * reason
   * @throws SecurityException if a clip cannot be made for security reasons
   * @since 1.5
   */
  public static Clip getClip()
    throws LineUnavailableException
  {
    Mixer.Info[] infos = getMixerInfo();
    for (int i = 0; i < infos.length; ++i)
      {
        Mixer mix = getMixer(infos[i]);
        Line[] lines = mix.getSourceLines();
        for (int j = 0; j < lines.length; ++j)
          {
            if (lines[j] instanceof Clip)
              return (Clip) lines[j];
          }
      }
    throw new LineUnavailableException("no Clip available");
  }

  /**
   * Return a new clip which can be used for playing back an audio stream.
   * The clip is obtained from the indicated mixer.
   * @param info the mixer to use
   * @throws LineUnavailableException if a clip is not available for some
   * reason
   * @throws SecurityException if a clip cannot be made for security reasons
   * @since 1.5
   */
  public static Clip getClip(Mixer.Info info)
    throws LineUnavailableException
  {
    Mixer mix = getMixer(info);
    Line[] lines = mix.getSourceLines();
    for (int j = 0; j < lines.length; ++j)
      {
        if (lines[j] instanceof Clip)
          return (Clip) lines[j];
      }
    throw new LineUnavailableException("no Clip available");
  }

  /**
   * Return a line matching the provided description.  All the providers
   * on the system are searched for a matching line.
   * @param info description of the line
   * @return the matching line
   * @throws LineUnavailableException if no provider supplies a matching line
   */
  public static Line getLine(Line.Info info) throws LineUnavailableException
  {
    Mixer.Info[] infos = getMixerInfo();
    for (int i = 0; i < infos.length; ++i)
      {
        Mixer mix = getMixer(infos[i]);
        try
        {
          return mix.getLine(info);
        }
        catch (LineUnavailableException _)
        {
          // Try the next provider.
        }
      }
    throw new LineUnavailableException("no Clip available");
  }

  /**
   * Return a mixer matching the provided description.  All the providers
   * on the system are searched for a matching mixer.
   * @param info description of the mixer
   * @return the matching mixer
   * @throws IllegalArgumentException if no provider supplies a matching mixer
   */
  public static Mixer getMixer(Mixer.Info info)
  {
    Iterator i = ServiceFactory.lookupProviders(MixerProvider.class);
    while (i.hasNext())
      {
        MixerProvider prov = (MixerProvider) i.next();
        if (prov.isMixerSupported(info))
          return prov.getMixer(info);
      }
    throw new IllegalArgumentException("mixer not found");
  }

  /**
   * Return an array of descriptions of all the mixers provided on the system.
   */
  public static Mixer.Info[] getMixerInfo()
  {
    HashSet<Mixer.Info> result = new HashSet<Mixer.Info>();
    Iterator i = ServiceFactory.lookupProviders(MixerProvider.class);
    while (i.hasNext())
      {
        MixerProvider prov = (MixerProvider) i.next();
        Mixer.Info[] is = prov.getMixerInfo();
        for (int j = 0; j < is.length; ++j)
          result.add(is[j]);
      }
    return result.toArray(new Mixer.Info[result.size()]);
  }

  /**
   * Return a source data line matching the given audio format.
   * @param fmt the audio format
   * @throws LineUnavailableException if no source data line matching
   * this format is available
   * @since 1.5
   */
  public static SourceDataLine getSourceDataLine(AudioFormat fmt)
    throws LineUnavailableException
  {
    DataLine.Info info = new DataLine.Info(SourceDataLine.class, fmt);
    Mixer.Info[] mixers = getMixerInfo();
    for (int i = 0; i < mixers.length; ++i)
      {
        Mixer mix = getMixer(mixers[i]);
        if (mix.isLineSupported(info))
          return (SourceDataLine) mix.getLine(info);
      }
    throw new LineUnavailableException("source data line not found");
  }

  /**
   * Return a target data line matching the given audio format.
   * @param fmt the audio format
   * @throws LineUnavailableException if no target data line matching
   * this format is available
   * @since 1.5
   */
  public static SourceDataLine getSourceDataLine(AudioFormat fmt,
						 Mixer.Info mixer)
    throws LineUnavailableException
  {
    DataLine.Info info = new DataLine.Info(SourceDataLine.class, fmt);
    Mixer mix = getMixer(mixer);
    if (mix.isLineSupported(info))
      return (SourceDataLine) mix.getLine(info);
    throw new LineUnavailableException("source data line not found");
  }

  /**
   * Return an array of descriptions of all the source lines matching
   * the given line description.
   * @param info description of the lines to match
   */
  public static Line.Info[] getSourceLineInfo(Line.Info info)
  {
    HashSet<Line.Info> result = new HashSet<Line.Info>();
    Mixer.Info[] infos = getMixerInfo();
    for (int i = 0; i < infos.length; ++i)
      {
        Mixer mix = getMixer(infos[i]);
        Line.Info[] srcs = mix.getSourceLineInfo(info);
        for (int j = 0; j < srcs.length; ++j)
          result.add(srcs[j]);
      }
    return result.toArray(new Line.Info[result.size()]);
  }

  /**
   * Find and return a target data line matching the given audio format.
   * @param fmt the format to match
   * @throws LineUnavailableException if no matching line was found 
   * @since 1.5
   */
  public static TargetDataLine getTargetDataLine(AudioFormat fmt)
    throws LineUnavailableException
  {
    DataLine.Info info = new DataLine.Info(TargetDataLine.class, fmt);
    Mixer.Info[] mixers = getMixerInfo();
    for (int i = 0; i < mixers.length; ++i)
      {
        Mixer mix = getMixer(mixers[i]);
        if (mix.isLineSupported(info))
          return (TargetDataLine) mix.getLine(info);
      }
    throw new LineUnavailableException("target data line not found");
  }

  /**
   * Return a target data line matching the given audio format and
   * mixer.
   * @param fmt the audio format
   * @param mixer the mixer description
   * @return a target data line
   * @throws LineUnavailableException if no matching target data line was
   * found
   * @since 1.5
   */
  public static TargetDataLine getTargetDataLine(AudioFormat fmt,
						 Mixer.Info mixer)
    throws LineUnavailableException
  {
    DataLine.Info info = new DataLine.Info(TargetDataLine.class, fmt);
    Mixer mix = getMixer(mixer);
    if (mix.isLineSupported(info))
      return (TargetDataLine) mix.getLine(info);
    throw new LineUnavailableException("target data line not found");
  }

  /**
   * Given a source encoding, return an array of all target encodings to which
   * data in this form can be converted.
   * @param source the source encoding
   */
  public static AudioFormat.Encoding[] getTargetEncodings(AudioFormat.Encoding source)
  {
    HashSet<AudioFormat.Encoding> result
      = new HashSet<AudioFormat.Encoding>();
    Iterator i = ServiceFactory.lookupProviders(FormatConversionProvider.class);
    while (i.hasNext())
      {
        FormatConversionProvider prov = (FormatConversionProvider) i.next();
        if (! prov.isSourceEncodingSupported(source))
          continue;
        AudioFormat.Encoding[] es = prov.getTargetEncodings();
        for (int j = 0; j < es.length; ++j)
          result.add(es[j]);
      }
    return result.toArray(new AudioFormat.Encoding[result.size()]);
  }

  /**
   * Given a source format, return an array of all the target encodings to
   * which data in this format can be converted.
   * @param source the source format
   */
  public static AudioFormat.Encoding[] getTargetEncodings(AudioFormat source)
  {
    HashSet<AudioFormat.Encoding> result
      = new HashSet<AudioFormat.Encoding>();
    Iterator i = ServiceFactory.lookupProviders(FormatConversionProvider.class);
    while (i.hasNext())
      {
        FormatConversionProvider prov = (FormatConversionProvider) i.next();
        AudioFormat.Encoding[] es = prov.getTargetEncodings(source);
        for (int j = 0; j < es.length; ++j)
          result.add(es[j]);
      }
    return result.toArray(new AudioFormat.Encoding[result.size()]);
  }

  /**
   * Given a target encoding and a source audio format, return an array of all
   * matching audio formats to which data in this source format can be converted. 
   * @param encoding the target encoding
   * @param sourceFmt the source format
   */
  public static AudioFormat[] getTargetFormats(AudioFormat.Encoding encoding,
					       AudioFormat sourceFmt)
  {
    HashSet<AudioFormat> result = new HashSet<AudioFormat>();
    Iterator i = ServiceFactory.lookupProviders(FormatConversionProvider.class);
    while (i.hasNext())
      {
        FormatConversionProvider prov = (FormatConversionProvider) i.next();
        AudioFormat[] es = prov.getTargetFormats(encoding, sourceFmt);
        for (int j = 0; j < es.length; ++j)
          result.add(es[j]);
      }
    return result.toArray(new AudioFormat[result.size()]);
  }

  /**
   * Given a line description, return an array of descriptions of all
   * the matching target lines.
   * @param info the line description
   */
  public static Line.Info[] getTargetLineInfo(Line.Info info)
  {
    HashSet<Line.Info> result = new HashSet<Line.Info>();
    Mixer.Info[] infos = getMixerInfo();
    for (int i = 0; i < infos.length; ++i)
      {
        Mixer mix = getMixer(infos[i]);
        Line.Info[] targs = mix.getTargetLineInfo(info);
        for (int j = 0; j < targs.length; ++j)
          result.add(targs[j]);
      }
    return result.toArray(new Line.Info[result.size()]);
  }

  /**
   * Return true if the currently installed providers are able to
   * convert data from the given source format to the given target encoding.
   * @param targ the target encoding
   * @param source the source format
   */
  public static boolean isConversionSupported(AudioFormat.Encoding targ,
					      AudioFormat source)
  {
    Iterator i 
      = ServiceFactory.lookupProviders(FormatConversionProvider.class);
    while (i.hasNext())
      {
        FormatConversionProvider prov = (FormatConversionProvider) i.next();
        if (prov.isConversionSupported(targ, source))
          return true;
      }
    return false;
  }

  /**
   * Return true if the currently installed providers are able to convert
   * the given source format to the given target format.
   * @param targ the target format
   * @param source the source format
   */
  public static boolean isConversionSupported(AudioFormat targ,
					      AudioFormat source)
  {
    Iterator i 
      = ServiceFactory.lookupProviders(FormatConversionProvider.class);
    while (i.hasNext())
      {
        FormatConversionProvider prov = (FormatConversionProvider) i.next();
        if (prov.isConversionSupported(targ, source))
          return true;
      }
    return false;
  }

  private static boolean isFileTypeSupported(AudioFileFormat.Type[] types,
                                             AudioFileFormat.Type type)
  {
    for (int i = 0; i < types.length; ++i)
      {
        if (types[i].equals(type))
          return true;
      }
    return false;
  }

  /**
   * Return true if the given audio file format is supported by one of
   * the providers installed on the system.
   * @param type the audio file format type
   */
  public static boolean isFileTypeSupported(AudioFileFormat.Type type)
  {
    return isFileTypeSupported(getAudioFileTypes(), type);
  }

  /**
   * Return true if the given audio file format is supported for the
   * given audio input stream by one of the providers installed on the 
   * system.
   * @param type the audio file format type
   * @param ais the audio input stream
   */
  public static boolean isFileTypeSupported(AudioFileFormat.Type type,
					    AudioInputStream ais)
  {
    return isFileTypeSupported(getAudioFileTypes(ais), type);
  }

  /**
   * Return true if some provider on the system supplies a line
   * matching the argument. 
   * @param info the line to match
   */
  public static boolean isLineSupported(Line.Info info)
  {
    Mixer.Info[] infos = getMixerInfo();
    for (int i = 0; i < infos.length; ++i)
      {
        if (getMixer(infos[i]).isLineSupported(info))
          return true;
      }
    return false;
  }

  /**
   * Write an audio input stream to the given file, using the specified
   * audio file format.  All the providers installed on the system will
   * be searched to find one that supports this operation.
   * @param ais the audio input stream to write
   * @param type the desired audio file format type
   * @param out the file to write to
   * @return the number of bytes written
   * @throws IOException if an I/O error occurs while writing
   * @throws IllegalArgumentException if the file type is not supported
   */
  public static int write(AudioInputStream ais, AudioFileFormat.Type type,
			  File out)
    throws IOException
  {
    Iterator i = ServiceFactory.lookupProviders(AudioFileWriter.class);
    while (i.hasNext())
      {
        AudioFileWriter w = (AudioFileWriter) i.next();
        if (w.isFileTypeSupported(type, ais))
          return w.write(ais, type, out);
      }
    throw new IllegalArgumentException("file type not supported by system");
  }

  /**
   * Write an audio input stream to the given output stream, using the
   * specified audio file format.  All the providers installed on the
   * system will be searched to find one that supports this operation.
   * @param ais the audio input stream to write
   * @param type the desired audio file format type
   * @param os the output stream to write to
   * @return the number of bytes written
   * @throws IOException if an I/O error occurs while writing
   * @throws IllegalArgumentException if the file type is not supported
   */
  public static int write(AudioInputStream ais, AudioFileFormat.Type type,
			  OutputStream os)
    throws IOException
  {
    Iterator i = ServiceFactory.lookupProviders(AudioFileWriter.class);
    while (i.hasNext())
      {
        AudioFileWriter w = (AudioFileWriter) i.next();
        if (w.isFileTypeSupported(type, ais))
          return w.write(ais, type, os);
      }
    throw new IllegalArgumentException("file type not supported by system");
  }
}
