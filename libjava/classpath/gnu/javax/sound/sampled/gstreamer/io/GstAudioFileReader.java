/*GstAudioFileReader -- GNU Classpath GStreamer AudioFileReader.
 Copyright (C) 2007 Free Software Foundation, Inc.

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

package gnu.javax.sound.sampled.gstreamer.io;

import gnu.java.lang.CPStringBuilder;

import gnu.javax.sound.sampled.gstreamer.GStreamerMixer;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import javax.sound.sampled.AudioFileFormat;
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.UnsupportedAudioFileException;
import javax.sound.sampled.spi.AudioFileReader;

/**
 * An implementation of a general AudioFileReader. Uses GStreamer to
 * parse and retrieve informations about the file passed as input. 
 * 
 * @author Mario Torre <neugens@limasoftware.net>
 */
public class GstAudioFileReader
    extends AudioFileReader
{ 
  @Override
  public AudioFileFormat getAudioFileFormat(File file)
      throws UnsupportedAudioFileException, IOException
  {
    CPStringBuilder name = new CPStringBuilder(file.getName());
    String _name = name.substring(name.lastIndexOf(".") + 1);
    
    return getAudioFileFormat(
               new BufferedInputStream(new FileInputStream(file)), _name);
  }

  @Override
  public AudioFileFormat getAudioFileFormat(InputStream is)
      throws UnsupportedAudioFileException, IOException
  {
    return getAudioFileFormat(is, null);
  }

  private AudioFileFormat getAudioFileFormat(InputStream is, String extension)
    throws UnsupportedAudioFileException
    {
      AudioFormat format = null;
      try
        {
          format = GstAudioFileReaderNativePeer.getAudioFormat(is);
        }
      catch (Exception e)
        {
          UnsupportedAudioFileException ex =
            new UnsupportedAudioFileException("Unsupported encoding.");
         
          ex.initCause(ex.getCause());
          throw ex;
        }
      
      if (format == null)
        throw new UnsupportedAudioFileException("Unsupported encoding.");
      
      String name = format.getProperty(GStreamerMixer.GST_DECODER).toString();
      
      if (extension == null)
        {
          extension =
            format.getProperty(GStreamerMixer.GST_FILE_EXTENSION).toString();
        }
      
      AudioFileFormat.Type type =
        new AudioFileFormat.Type(name, extension);
      
      // TODO: we should calculate this in some way. We don't need it, but
      // application may want to use this data.
      return new AudioFileFormat(type, format, AudioSystem.NOT_SPECIFIED);
    }
  
  @Override
  public AudioFileFormat getAudioFileFormat(URL url)
      throws UnsupportedAudioFileException, IOException
  {
    return getAudioFileFormat(new BufferedInputStream(url.openStream()));
  }

  @Override
  public AudioInputStream getAudioInputStream(File file)
      throws UnsupportedAudioFileException, IOException
  { 
    InputStream stream = new FileInputStream(file);
    long length = file.length();
    
    AudioFormat format = null;
    
    try
      {
        format = GstAudioFileReaderNativePeer.getAudioFormat(file);
      }
    catch (Exception e)
      {
        UnsupportedAudioFileException ex =
          new UnsupportedAudioFileException("Unsupported encoding.");
       
        ex.initCause(ex.getCause());
        throw ex;
      }
    
    // get the header size
    if (format == null)
      throw new UnsupportedAudioFileException("Unsupported encoding.");

    return new AudioInputStream(stream, format, length);
  }

  @Override
  public AudioInputStream getAudioInputStream(InputStream is)
      throws UnsupportedAudioFileException, IOException
  {
    AudioFormat format = null;
    
    try
      {
        format = GstAudioFileReaderNativePeer.getAudioFormat(is);
      }
    catch (Exception e)
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    
    // get the header size
    if (format == null)
      throw new UnsupportedAudioFileException("Unsupported encoding.");

    return new AudioInputStream(is, format, AudioSystem.NOT_SPECIFIED);
  }

  @Override
  public AudioInputStream getAudioInputStream(URL url)
      throws UnsupportedAudioFileException, IOException
  {
    return getAudioInputStream(new BufferedInputStream(url.openStream()));
  }
}
