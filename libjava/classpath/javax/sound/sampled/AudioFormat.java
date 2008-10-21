/* An audio format
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

import gnu.java.lang.CPStringBuilder;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * This class describes an audio format, including its encoding,
 * the number of channels, its frame rate, etc.
 * @since 1.3
 */
public class AudioFormat
{
  /**
   * This describes a given audio format encoding.
   * @since 1.3
   */
  public static class Encoding
  {
    /** The ALAW encoding.  */
    public static final Encoding ALAW = new Encoding("alaw");

    /** The signed PCM encoding.  */
    public static final Encoding PCM_SIGNED = new Encoding("pcm_signed");

    /** The unsigned PCM encoding.  */
    public static final Encoding PCM_UNSIGNED = new Encoding("pcm_unsigned");

    /** The ULAW encoding.  */
    public static final Encoding ULAW = new Encoding("ulaw");

    private String name;

    /**
     * Create a new encoding descriptor, given its name.
     * @param name the name
     */
    public Encoding(String name)
    {
      this.name = name;
    }

    public final boolean equals(Object o)
    {
      return super.equals(o);
    }

    public final int hashCode()
    {
      return super.hashCode();
    }

    /**
     * Return the name of this encoding.
     */
    public final String toString()
    {
      return name;
    }
  }

  /**
   * True if the audio data is stored big-endian.
   */
  protected boolean bigEndian;

  /**
   * The number of channels of data in this format.
   */
  protected int channels;

  /**
   * The encoding of this format.
   */
  protected Encoding encoding;

  /**
   * The frame rate of this format.  This is the number of frames
   * per second.
   */
  protected float frameRate;

  /**
   * The number of bytes per frame in this format.
   */
  protected int frameSize;

  /**
   * The number of samples per second.
   */
  protected float sampleRate;

  /**
   * The number of bits in each sample.
   */
  protected int sampleSizeInBits;

  private Map<String, Object> properties;

  /**
   * Create a new audio format, given various attributes of it.
   * The properties map for this format will be empty.
   * 
   * @param encoding the encoding for this format
   * @param sampleRate the sample rate
   * @param sampleSizeInBits the sample size, in bits
   * @param channels the number of channels
   * @param frameSize the frame size, in bytes
   * @param frameRate the frame rate, in frames per second
   * @param bigEndian true if the data is stored big-endian
   */
  public AudioFormat(Encoding encoding, float sampleRate, int sampleSizeInBits,
		     int channels, int frameSize, float frameRate,
		     boolean bigEndian)
  {
    this.encoding = encoding;
    this.sampleRate = sampleRate;
    this.sampleSizeInBits = sampleSizeInBits;
    this.channels = channels;
    this.frameSize = frameSize;
    this.frameRate = frameRate;
    this.bigEndian = bigEndian;
    this.properties = Collections.<String, Object> emptyMap();
  }

  /**
   * Create a new audio format, given various attributes of it.
   * The properties map is copied by this constructor, so changes
   * to the argument Map will not affect the new object.
   *
   * @param encoding the encoding for this format
   * @param sampleRate the sample rate
   * @param sampleSizeInBits the sample size, in bits
   * @param channels the number of channels
   * @param frameSize the frame size, in bytes
   * @param frameRate the frame rate, in frames per second
   * @param bigEndian true if the data is stored big-endian
   * @param properties a map describing properties of this format
   */
  public AudioFormat(Encoding encoding, float sampleRate, int sampleSizeInBits,
		     int channels, int frameSize, float frameRate,
		     boolean bigEndian, Map<String, Object> properties)
  {
    this.encoding = encoding;
    this.sampleRate = sampleRate;
    this.sampleSizeInBits = sampleSizeInBits;
    this.channels = channels;
    this.frameSize = frameSize;
    this.frameRate = frameRate;
    this.bigEndian = bigEndian;
    this.properties = Collections.unmodifiableMap(new HashMap<String, Object>(properties));
  }

  /**
   * Create a new PCM-based audio format, given various attributes of it.
   * The encoding will either be Encoding#PCM_SIGNED or Encoding#PCM_UNSIGNED.
   * The frame size for this format will be derived from the sample size in
   * bits and the number of channels, unless one of those is
   * AudioSystem#NOT_SPECIFIED.  The frame rate will be the same as the sample
   * rate, and the properties map will be empty.
   * 
   * @param sampleRate the sample rate
   * @param sampleSizeInBits the sample size, in bits
   * @param channels the number of channels
   * @param signed true if this is a signed encoding
   * @param bigEndian true if the data is stored big-endian
   */
  public AudioFormat(float sampleRate, int sampleSizeInBits,
		     int channels, boolean signed, boolean bigEndian)
  {
    this.encoding = signed ? Encoding.PCM_SIGNED : Encoding.PCM_UNSIGNED;
    this.sampleRate = sampleRate;
    this.sampleSizeInBits = sampleSizeInBits;
    this.channels = channels;
    // It isn't clear whether channels can be NOT_SPECIFIED.
    if (sampleSizeInBits == AudioSystem.NOT_SPECIFIED
        || channels == AudioSystem.NOT_SPECIFIED)
      this.frameSize = AudioSystem.NOT_SPECIFIED;
    else
      this.frameSize = (sampleSizeInBits + 7) / 8 * channels;
    this.frameRate = sampleRate;
    this.bigEndian = bigEndian;
    this.properties = Collections.<String, Object> emptyMap();
  }

  /**
   * Return the number of channels in this format.
   */
  public int getChannels()
  {
    return channels;
  }

  /**
   * Return the encoding of this format.
   */
  public Encoding getEncoding()
  {
    return encoding;
  }

  /**
   * Return the frame rate of this format.
   */
  public float getFrameRate()
  {
    return frameRate;
  }

  /**
   * Return the frame size of this format.
   */
  public int getFrameSize()
  {
    return frameSize;
  }

  /**
   * Given a key, return a property associated with this format;
   * or null if this property is not set. 
   * @param key the name of the property
   * @return the value of the property, or null if the property is not set
   */
  public Object getProperty(String key)
  {
    return properties.get(key);
  }

  /**
   * Return the sample rate of this format.
   */
  public float getSampleRate()
  {
    return sampleRate;
  }

  /**
   * Return the sample size of this format, in bits.
   */
  public int getSampleSizeInBits()
  {
    return sampleSizeInBits;
  }

  /**
   * Return true if this format is big endian, false otherwise.
   * This only matters for formats whose sample size is greater than
   * one byte.
   */
  public boolean isBigEndian()
  {
    return bigEndian;
  }

  /**
   * Return true if this audio format matches another.
   * @param fmt the format to match against
   * @return true if they match, false otherwise
   */
  public boolean matches(AudioFormat fmt)
  {
    if (! encoding.equals(fmt.encoding)
        || channels != fmt.channels
        || sampleSizeInBits != fmt.sampleSizeInBits
        || frameSize != fmt.frameSize)
      return false;
    if (sampleRate != AudioSystem.NOT_SPECIFIED
        && fmt.sampleRate != AudioSystem.NOT_SPECIFIED
        && sampleRate != fmt.sampleRate)
      return false;
    if (frameRate != AudioSystem.NOT_SPECIFIED
        && fmt.frameRate != AudioSystem.NOT_SPECIFIED
        && frameRate != fmt.frameRate)
      return false;
    if (sampleSizeInBits > 8)
      return bigEndian == fmt.bigEndian;
    return true;
  }

  /**
   * Return a read-only Map holding the properties associated with 
   * this format.
   */
  public Map<String, Object> properties()
  {
    return properties;
  }

  /**
   * Return a description of this format.
   */
  public String toString()
  {
    CPStringBuilder result = new CPStringBuilder();
    
    // usually at least encoding should be somewhat specified
    result.append(encoding);
    
    if (sampleRate != AudioSystem.NOT_SPECIFIED)
      {
        result.append(" ");
        result.append(sampleRate);
        result.append(" Hz");
      }
    
    if (sampleSizeInBits != AudioSystem.NOT_SPECIFIED)
      {
        result.append(" ");
        result.append(sampleSizeInBits);
        result.append(" bits");
      }
    
    if (channels != AudioSystem.NOT_SPECIFIED)
      {
        result.append(" ");
        result.append(channels);
        result.append(" channel");
        if (channels > 1) result.append("s");
      }
    
    if (sampleSizeInBits > 8)
      result.append(bigEndian ? " big endian" : " little endian");
    
    return result.toString();
  }
}
