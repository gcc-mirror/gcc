/* CheckedOutputStream.java - Compute checksum of data being written.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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

import java.io.FilterOutputStream;
import java.io.OutputStream;
import java.io.IOException;

/* Written using on-line Java Platform 1.2 API Specification
 * and JCL book.
 * Believed complete and correct.
 */

/**
 * OutputStream that computes a checksum of data being written using a
 * supplied Checksum object.
 *
 * @see Checksum
 *
 * @author Tom Tromey
 * @date May 17, 1999
 */
public class CheckedOutputStream extends FilterOutputStream
{
  /**
   * Creates a new CheckInputStream on top of the supplied OutputStream
   * using the supplied Checksum.
   */
  public CheckedOutputStream (OutputStream out, Checksum cksum)
  {
    super (out);
    this.sum = cksum;
  }

  /**
   * Returns the Checksum object used. To get the data checksum computed so
   * far call <code>getChecksum.getValue()</code>.
   */
  public Checksum getChecksum ()
  {
    return sum;
  }

  /**
   * Writes one byte to the OutputStream and updates the Checksum.
   */
  public void write (int bval) throws IOException
  {
    out.write(bval);
    sum.update(bval);
  }

  /**
   * Writes the byte array to the OutputStream and updates the Checksum.
   */
  public void write (byte[] buf, int off, int len) throws IOException
  {
    out.write(buf, off, len);
    sum.update(buf, off, len);
  }

  /** The checksum object. */
  private Checksum sum;
}
