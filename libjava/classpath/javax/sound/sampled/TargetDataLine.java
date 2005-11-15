/* Input data line.
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

/**
 * This is a DataLine from which data may be read.
 * @since 1.3
 */
public interface TargetDataLine extends DataLine
{
  /**
   * Open the line using the indicated audio format.
   * @param fmt the format to use
   * @throws LineUnavailableException if the line is not available for
   * some reason
   * @throws SecurityException if this operation is prevented by the
   * security manager
   */
  void open(AudioFormat fmt)
    throws LineUnavailableException;

  /**
   * Open the line using the indicated audio format and buffer size.
   * @param fmt the format to use
   * @throws LineUnavailableException if the line is not available for
   * some reason
   * @throws SecurityException if this operation is prevented by the
   * security manager
   */
  void open(AudioFormat fmt, int size)
    throws LineUnavailableException;

  /**
   * Read data from the line into the given buffer.  The requested data
   * should be an integral number of framaes, as determined by the audio
   * format.
   * @param buf the buffer into which the data is put
   * @param offset the initial offset at which to write
   * @param length the maximum number of bytes to read
   * @return the actual number of bytes read
   */
  int read(byte[] buf, int offset, int length);
}
