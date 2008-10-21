/* 
   Copyright (C) 2005-2007 Free Software Foundation, Inc.

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

/**
 * The DataLine interface adds data-related functionality to the Line
 * interface.  For example, it adds methods to start and stop the data
 * on the line.
 * @since 1.3 
 */
public interface DataLine extends Line
{
  /**
   * This class extends Line.Info with information specific to DataLine.
   * In particular it adds information about buffer sizes, and about supported
   * audio formats.
   * @since 1.3
   */
  class Info extends Line.Info
  {
    private int minBufferSize;
    private int maxBufferSize;
    private AudioFormat[] formats;

    /**
     * Create a new Info given the line's class and a supported
     * audio format.  The buffer sizes default to AudioSystem.NOT_SPECIFIED.
     * @param klass the class of the line
     * @param fmt the supported format
     */
    public Info(Class<?> klass, AudioFormat fmt)
    {
      super(klass);
      this.minBufferSize = AudioSystem.NOT_SPECIFIED;
      this.maxBufferSize = AudioSystem.NOT_SPECIFIED;
      this.formats = new AudioFormat[] { fmt };
    }

    /**
     * Create a new Info given the line's class, the supported audio formats,
     * the minimum buffer size, and the maximum buffer size.
     * @param klass the class of the linee
     * @param fmts the supported audio formats
     * @param minSize the minimum buffer size
     * @param maxSize the maximum buffer size
     */
    public Info(Class<?> klass, AudioFormat[] fmts, int minSize, int maxSize)
    {
      super(klass);
      this.minBufferSize = minSize;
      this.maxBufferSize = maxSize;
      this.formats = fmts;
    }

    /**
     * Create a new Info given the line's class, a supported
     * audio format, and a buffer size.  Both the minimum and maximum
     * sizes are set from this size.
     * @param klass the class of the line
     * @param fmt the supported format
     * @param size the buffer size
     */
    public Info(Class<?> klass, AudioFormat fmt, int size)
    {
      super(klass);
      this.minBufferSize = size;
      this.maxBufferSize = size;
      this.formats = new AudioFormat[] { fmt };
    }

    /**
     * Return the supported audio formats.
     */
    public AudioFormat[] getFormats()
    {
      // FIXME: clone?
      return formats;
    }

    /**
     * Return the maximum buffer size.
     */
    public int getMaxBufferSize()
    {
      return maxBufferSize;
    }

    /**
     * Return the minimum buffer size.
     */
    public int getMinBufferSize()
    {
      return minBufferSize;
    }

    /**
     * Return true if the indicated audio format is supported by this
     * Info, false otherwise.
     * @param fmt the audio format
     * @return true if the format is supported
     */
    public boolean isFormatSupported(AudioFormat fmt)
    {
      for (int i = 0; i < formats.length; ++i)
        {
          if (fmt.matches(formats[i]))
            return true;
        }
      return false;
    }

    /**
     * Return true if this Info matches another Info object.
     */
    public boolean matches(Line.Info o)
    {
      if (! super.matches(o) || ! (o instanceof Info))
        return false;

      Info other = (Info) o;
      if (minBufferSize < other.minBufferSize ||
          maxBufferSize > other.maxBufferSize)
        return false;
      
      for (int i = 0; i < formats.length; ++i)
        {
          boolean ok = false;
          for (int j = 0; j < other.formats.length; ++j)
            {
              if (formats[i].matches(other.formats[j]))
                {
                  ok = true;
                  break;
                }
            }
          if (! ok)
            return false;
        }
      
      return true;
    }

    /**
     * Return a description of this Info object.
     */
    public String toString()
    {
      CPStringBuilder result = new CPStringBuilder();
      result.append("formats: [");
      for (int i = 0; i < formats.length; ++i)
        {
          if (i > 0)
            result.append(", ");
          result.append(formats[i].toString());
        }
      
      result.append("]; minBufferSize: ");
      result.append(minBufferSize);
      result.append("; maxBufferSize: ");
      result.append(maxBufferSize);
      return result.toString();
    }
    
  } // end class: Info

  /**
   * Return the number of bytes currently available on this DataLine.
   */
  int available();

  /**
   * This method blocks until whatever data is buffered in the
   * DataLine's internal buffer has been drained.
   */
  void drain();

  /**
   * This flushes the DataLine by discarding any buffered data.
   */
  void flush();

  /**
   * Returns the size of the DataLine's internal buffer, in bytes.
   */
  int getBufferSize();

  /**
   * Return the current format of the data associated with this DataLine.
   */
  AudioFormat getFormat();

  /**
   * Return the current frame position.
   */
  int getFramePosition();

  /**
   * Return the volume level for this DataLine.
   */
  float getLevel();

  /**
   * Return the current frame position. 
   * @since 1.5
   */
  long getLongFramePosition();

  /**
   * Return the number of microseconds this DataLine has been playing.
   */
  long getMicrosecondPosition();

  /**
   * Return true if this line is active, meaning that it is actively
   * performing audio I/O.
   */
  boolean isActive();

  /**
   * Return true if this line is running, meaning that it has been
   * started.  When the line is stopped, this method will return false.
   */
  boolean isRunning();

  /**
   * Start processing data.  This will emit a START event.
   */
  void start();

  /**
   * Stop processing data.  This will emit a STOP event.
   */
  void stop();
}
