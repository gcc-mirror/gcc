/* Checksum.java - Interface to compute a data checksum
   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

package java.util.zip;

/*
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

/**
 * Interface to compute a data checksum used by checked input/output streams.
 * A data checksum can be updated by one byte or with a byte array. After each
 * update the value of the current checksum can be returned by calling
 * <code>getValue</code>. The complete checksum object can also be reset
 * so it can be used again with new data.
 *
 * @see CheckedInputStream
 * @see CheckedOutputStream
 *
 * @author Per Bothner
 * @author Jochen Hoenicke
 */
public interface Checksum
{
  /**
   * Returns the data checksum computed so far.
   */
  public long getValue ();

  /**
   * Resets the data checksum as if no update was ever called.
   */
  public void reset ();

  /**
   * Adds one byte to the data checksum.
   *
   * @param bval the data value to add. The high byte of the int is ignored.
   */
  public void update (int bval);

  /**
   * Adds the byte array to the data checksum.
   *
   * @param buf the buffer which contains the data
   * @param off the offset in the buffer where the data starts
   * @param len the length of the data
   */
  public void update (byte[] buf, int off, int len);
}
