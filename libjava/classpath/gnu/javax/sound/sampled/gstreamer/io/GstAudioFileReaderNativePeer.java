/*GstAudioFileReaderNativePeer -- GNU Classpath GStreamer AudioFileReader
  native peer class.
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

import gnu.javax.sound.sampled.gstreamer.GStreamerMixer;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.AudioFormat.Encoding;

/**
 * GStreamer native peer for GstAudioFileReader. 
 * 
 * @author Mario Torre <neugens@limasoftware.net>
 */
final class GstAudioFileReaderNativePeer
{
  private static final String GST_ENCODING = "GStreamer Generic Audio Reader";
  
  private static class GstHeader
  {
    /* 
     * NOTE: these properties are accessed by the native code, be careful
     * if you change them.
     * Not all the fields are necessarily set.
     * 
     */
    public String file = null;
    
    public String suffix = null;
    
    public String name = null;
    
    public String mimetype = null;
    
    public String endianness = null;

    public String channels = null;

    public String rate = null;

    public String width = null;

    public String depth = null;
    
    public String isSigned = null;
    
    public String layer = null;
    public String bitrate = null;
    public String framed = null;
    public String type = null;
  }
  
  public static AudioFormat getAudioFormat(File file) throws Exception
  {
    GstHeader header = new GstHeader();
    header.file = file.getAbsolutePath();
    
    if (!gstreamer_get_audio_format_file(header))
      return null;
    
    return getAudioFormat(header);
  }
  
  public static AudioFormat getAudioFormat(InputStream is) throws Exception
  {
    GstHeader header = new GstHeader();
    
    BufferedInputStream stream = new BufferedInputStream(is);
    if(!stream.markSupported()) 
      throw new IOException("Stream must support marking."); 
    
    stream.mark(0);
    
    if (!gstreamer_get_audio_format_stream(header, stream))
      return null;
    
    return getAudioFormat(header);
  }
  
  public static AudioFormat getAudioFormat(URL url) throws Exception
  {
    GstHeader header = new GstHeader();
    header.file = url.toExternalForm();
    
    BufferedInputStream stream = new BufferedInputStream(url.openStream());
    if(!stream.markSupported()) 
      throw new IOException("Stream must support marking."); 
    
    stream.mark(0);
    
    if (!gstreamer_get_audio_format_stream(header, stream))
      return null;
    
    return getAudioFormat(header);
  }
  
  private static Encoding getEncoding(GstHeader header)
  {
    StringBuilder buffer = new StringBuilder();
    
    if (header.name == null)
      {
        buffer.append(GST_ENCODING);
        if (header.mimetype != null)
          {
            buffer.append(" ");
            buffer.append(header.mimetype);
          }
       
        header.name = buffer.toString();
      }
    else
      {
        // strip the "decoder" word from the name, if any
        // this is a bit ugly, the alternative would be to still output the
        // full name of the decoder/demuxer
        String lowerCase = header.name.toLowerCase();
        int index = lowerCase.indexOf("decoder");
        if (index == -1)
          {
            index = lowerCase.indexOf("demuxer");
          }
        
        if (index == -1)
          index = lowerCase.length();

        buffer.append(header.name.substring(0, index));
          
      }
    
    return new Encoding(buffer.toString().trim());
  }
  
  private static AudioFormat getAudioFormat(GstHeader header)
    throws Exception
  {
    int na = AudioSystem.NOT_SPECIFIED;
    
    /* we use mimetype as an header, but this could have some side effects */
    Encoding encoding = getEncoding(header);
    
    float sampleRate = ((header.rate != null) ?
                         new Float(header.rate).floatValue() : na);
                        
    int sampleSizeInBits = ((header.depth != null) ?
                             new Integer(header.depth).intValue() : na);
    
    int channels = ((header.channels != null) ?
                     new Integer(header.channels).intValue() : na);
    
    boolean bigEndian = false;
    if (header.endianness != null)
      {
        if (header.endianness.compareTo("4321") == 0)
          bigEndian = true;
      }
    
    int frameSize = na;
    float frameRate = na;
    String lowerCase = header.name.toLowerCase();
    
    // FIXME: frameRate = sampleRate in these cases under all the tests so far
    // but I'm not sure if this is always correct...
    if (lowerCase.contains("law") || lowerCase.contains("au") ||
        lowerCase.contains("x-au"))
      {
        frameSize = (sampleSizeInBits >> 3) * channels;
        frameRate = sampleRate;
      }
    else if (lowerCase.contains("wav"))
      {
        frameSize = ((sampleSizeInBits + 7) / 8) * channels;
        frameRate = sampleRate;
      }
    else if (lowerCase.contains("iff"))
      {
        frameSize = (sampleSizeInBits * channels) / 8;
        frameRate = sampleRate;
      }
    
    // write all the additional properties we got to identify
    // the gstreamer plugin actually used to deal with this stream
    Map<String, Object> properties = new HashMap<String, Object>();
    properties.put(GStreamerMixer.GST_BACKEND, true);
    properties.put(GStreamerMixer.GST_DECODER, header.name);
    
    /* now we put in some of the additional properties if we have them */
    if (header.type != null) properties.put("type", header.type);
    if (header.framed != null) properties.put("framed", header.framed);
    if (header.bitrate != null) properties.put("bitrate", header.bitrate);
    if (header.isSigned != null) properties.put("isSigned", header.isSigned);
    if (header.depth != null) properties.put("depth", header.depth);
    if (header.mimetype != null) properties.put("mimetype", header.mimetype);
    
    AudioFormat format = new AudioFormat(encoding,
                                         sampleRate,
                                         sampleSizeInBits,
                                         channels,
                                         frameSize,
                                         frameRate,
                                         bigEndian,
                                         properties);
    return format;
  }
  
  /* ***** native methods ***** */
  
  /**
   * Retrieve header information about the file being played.
   * 
   * @param info
   * @return
   */
  native static final
  protected boolean gstreamer_get_audio_format_stream(GstHeader info,
                                               BufferedInputStream istream);
  
  /**
   * Retrieve header information about the file being played.
   * 
   * @param info
   * @return
   */
  native static final
  protected boolean gstreamer_get_audio_format_file(GstHeader info);
  
  static
  {
    System.loadLibrary("gstreamerpeer"); //$NON-NLS-1$
  }
}
