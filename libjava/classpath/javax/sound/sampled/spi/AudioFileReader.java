/* Audio file reader API
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
import java.io.InputStream;
import java.net.URL;

import javax.sound.sampled.AudioFileFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.UnsupportedAudioFileException;

/**
 * This abstract class defines the interface to audio file readers.
 * A concrete provider subclass will implement the methods declared
 * here.  These methods can be used to determine the format of
 * files, and to retrieve an AudioInputStream for a file. 
 * @since 1.3
 */
public abstract class AudioFileReader
{
  /**
   * The default constructor.  Note that this class is abstract and
   * thus not directly instantiable.
   */
  public AudioFileReader()
  {
  }

  /**
   * Return the format of the given file as deduced by this provider.
   * If the format of the file is not recognized, throws an exception.
   * This will also throw an exception if there is an I/O error when
   * reading the file.
   * @param file the file to examine
   * @return the audio file format
   * @throws UnsupportedAudioFileException if the file's format is not
   * recognized
   * @throws IOException if there is an I/O error while reading the file
   */
  public abstract AudioFileFormat getAudioFileFormat(File file)
    throws UnsupportedAudioFileException, IOException;

  /**
   * Return the format of the given input stream as deduced by this provider.
   * If the format of the stream is not recognized, throws an exception.
   * This will also throw an exception if there is an I/O error when
   * reading the stream.  Note that providers typically use mark and reset
   * on the stream when examining the data, and as a result an IOException
   * may be thrown if the stream does not support these.
   * @param is the stream to examine
   * @return the audio file format
   * @throws UnsupportedAudioFileException if the stream's format is not
   * recognized
   * @throws IOException if there is an I/O error while reading the stream
   */
  public abstract AudioFileFormat getAudioFileFormat(InputStream is)
    throws UnsupportedAudioFileException, IOException;

  /**
   * Return the format of the given URL as deduced by this provider.
   * If the format of the URL is not recognized, throws an exception.
   * This will also throw an exception if there is an I/O error when
   * reading the URL.
   * @param url the URL to examine
   * @return the audio file format
   * @throws UnsupportedAudioFileException if the URL's format is not
   * recognized
   * @throws IOException if there is an I/O error while reading the URL
   */
  public abstract AudioFileFormat getAudioFileFormat(URL url)
    throws UnsupportedAudioFileException, IOException;

  /**
   * Return an AudioInputStream for the given file.  The file is assumed
   * to hold valid audio data.  
   * @param file the file to read
   * @return an AudioInputStream for the file
   * @throws UnsupportedAudioFileException if the file's type is not
   * recognized
   * @throws IOException if there is an error while reading the file 
   */
  public abstract AudioInputStream getAudioInputStream(File file)
    throws UnsupportedAudioFileException, IOException;

  /**
   * Return an AudioInputStream wrapping the given input stream.  The stream
   * is assumed to hold valid audio data.  
   * @param is the input stream to wrap
   * @return an AudioInputStream for the stream
   * @throws UnsupportedAudioFileException if the stream's type is not
   * recognized
   * @throws IOException if there is an error while reading the stream 
   */
  public abstract AudioInputStream getAudioInputStream(InputStream is)
    throws UnsupportedAudioFileException, IOException;

  /**
   * Return an AudioInputStream for the given URL.  The URL is assumed
   * to hold valid audio data.  
   * @param url the URL to read
   * @return an AudioInputStream for the URL
   * @throws UnsupportedAudioFileException if the URL's type is not
   * recognized
   * @throws IOException if there is an error while reading the URL 
   */
  public abstract AudioInputStream getAudioInputStream(URL url)
    throws UnsupportedAudioFileException, IOException;
}
