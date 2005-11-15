/* 
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

import java.io.IOException;

/**
 * A Clip represents some pre-loaded audio data.
 * @since 1.3
 */
public interface Clip extends DataLine
{
  /**
   * This can be passed to {@link #loop(int)} to indicate that looping
   * should be done continuously.
   */
  int LOOP_CONTINUOUSLY = -1;

  /**
   * Return the frame length of this clip.
   */
  int getFrameLength();

  /**
   * Return the length of the clip in microseconds.
   */
  long getMicrosecondLength();

  /**
   * Start looping the clip.  Looping will occur count times, or, if count
   * is LOOP_CONTINUOUSLY, will be done continuously.  A count of 0 indicates
   * that any current looping should stop.
   * @param count the number of times to loop
   */
  void loop(int count);

  /**
   * Open a clip, given an audio format and some data.
   * @param fmt the format of the data
   * @param data a byte array containing the audio data
   * @param offset the offset of the first byte of data in the array
   * @param len the length of the audio data in the array, in bytes
   * @throws LineUnavailableException if the line cannot be opened
   * @throws SecurityException if the line cannot be opened for security
   * reasons
   */
  void open(AudioFormat fmt, byte[] data, int offset, int len)
    throws LineUnavailableException;

  /**
   * Open a clip, given an audio input stream.
   * @param ais the input stream
   * @throws LineUnavailableException if the line cannot be opened
   * @throws SecurityException if the line cannot be opened for security
   * reasons
   * @throws IOException if there is an I/O error while reading the stream
   */
  void open(AudioInputStream ais)
    throws LineUnavailableException, IOException;

  /**
   * Set the position to the indicated frame.
   * @param where new frame position
   */
  void setFramePosition(int where);

  /**
   * Set the loop begin and end points.  These are used by loop(int).
   * @param begin the starting point
   * @param end the ending point
   */
  void setLoopPoints(int begin, int end);

  /**
   * Set the position to the indicated microsecond.
   * @param ms the new position in microseconds
   */
  void setMicrosecondPosition(long ms);
}
