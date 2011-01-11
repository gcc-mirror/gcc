/* Audio file format
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

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * This describes an audio file, including information about its length,
 * the format of the audio data, and other things.
 * @since 1.3
 */
public class AudioFileFormat
{
  /**
   * An instance of this type describes a standard audio file format.
   * @since 1.3
   */
  public static class Type
  {
    // This is kind of goofy since there are multiple extensions for
    // some of these.

    /** The AIFC format.  */
    public static final Type AIFC = new Type("AIFC", "aifc");

    /** The AIFF format.  */
    public static final Type AIFF = new Type("AIFF", "aiff");

    /** The AU format.  */
    public static final Type AU = new Type("AU", "au");

    /** The SND format.  */
    public static final Type SND = new Type("SND", "snd");

    /** The WAVE format.  */
    public static final Type WAVE = new Type ("WAVE", "wav");

    private String name;
    private String extension;

    /**
     * Create a new Type given its name and file extension.
     * The file extension does not include the ".".
     * @param name the type's name
     * @param extension the file extension
     */
    public Type(String name, String extension)
    {
      this.name = name;
      this.extension = extension;
    }

    public final boolean equals(Object o)
    {
      if (! (o instanceof Type))
        return false;
      Type other = (Type) o;
      return name.equals(other.name) && extension.equals(other.extension);
    }

    public final int hashCode()
    {
      return name.hashCode() + extension.hashCode();
    }

    /**
     * Return the extension associated with this Type.
     */
    public String getExtension()
    {
      return extension;
    }

    /**
     * Return the name of this Type.
     */
    public final String toString()
    {
      return name;
    }
  }

  private int byteLength;
  private AudioFormat format;
  private Type type;
  private int frameLength;
  private Map<String, Object> properties;

  /**
   * Create a new AudioFileFormat given the type, the format, and the
   * frame length.  The new object will have an unspecified byte length,
   * and an empty properties map.
   * @param type the type
   * @param fmt the format
   * @param frameLen the frame length
   */
  public AudioFileFormat(Type type, AudioFormat fmt, int frameLen)
  {
    this.byteLength = AudioSystem.NOT_SPECIFIED;
    this.format = fmt;
    this.type = type;
    this.frameLength = frameLen;
    this.properties = Collections.<String, Object> emptyMap();
  }

  /**
   * Create a new AudioFileFormat given the type, the format, the
   * frame length, and some properties.  The new object will have an
   * unspecified byte length.  A copy of the properties argument will
   * be made, so changes to the map passed in will not affect the
   * new AudioFileFormat.
   * @param type the type
   * @param fmt the format
   * @param frameLen the frame length
   * @param properties the properties
   */
  public AudioFileFormat(Type type, AudioFormat fmt, int frameLen,
                         Map<String, Object> properties)
  {
    this.byteLength = AudioSystem.NOT_SPECIFIED;
    this.format = fmt;
    this.type = type;
    this.frameLength = frameLen;
    this.properties = Collections.unmodifiableMap(new HashMap<String, Object>(properties));
  }

  /**
   * Create a new AudioFileFormat given the type, the byte length, the format,
   * and the frame length.  The new object will have an empty properties map.
   * @param type the type
   * @param byteLen the byte length
   * @param fmt the format
   * @param frameLen the frame length
   */
  protected AudioFileFormat(Type type, int byteLen, AudioFormat fmt,
                            int frameLen)
  {
    this.byteLength = byteLen;
    this.format = fmt;
    this.type = type;
    this.frameLength = frameLen;
    this.properties = Collections.<String, Object> emptyMap();
  }

  /**
   * Return the byte length of this file format.
   */
  public int getByteLength()
  {
    return byteLength;
  }

  /**
   * Return the AudioFormat associated with this file format.
   */
  public AudioFormat getFormat()
  {
    return format;
  }

  /**
   * Return the frame length of this file format.
   */
  public int getFrameLength()
  {
    return frameLength;
  }

  /**
   * Return the value of a property defined in this format.
   * @param key the property name
   * @return the value of the property, or null if the property is not defined
   */
  public Object getProperty(String key)
  {
    return properties.get(key);
  }

  /**
   * Return the Type associated with this file format.
   */
  public Type getType()
  {
    return type;
  }

  /**
   * Return the properties associated with this format, as a Map.
   * The returned Map is unmodifiable.
   */
  public Map<String, Object> properties()
  {
    return properties;
  }

  /**
   * Return a description of this AudioFileFormat.
   */
  public String toString()
  {
    return ("byteLength=" + byteLength + "; format=" + format
            + "; type=" + type + "; frameLength=" + frameLength);
  }
}
