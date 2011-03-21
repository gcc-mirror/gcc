/* Blob.java -- Access a SQL Binary Large OBject.
   Copyright (C) 1999, 2000, 2002, 2006 Free Software Foundation, Inc.

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

package java.sql;

import java.io.InputStream;
import java.io.OutputStream;

/**
 * This interface specified methods for accessing a SQL BLOB (Binary Large
 * OBject) type.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @since 1.2
 */
public interface Blob
{
  /**
   * This method returns the number of bytes in this <code>Blob</code>.
   *
   * @return The number of bytes in this <code>Blob</code>.
   * @exception SQLException If an error occurs.
   */
  long length() throws SQLException;

  /**
   * This method returns up to the requested bytes of this <code>Blob</code>
   * as a <code>byte</code> array.
   *
   * @param start The index into this <code>Blob</code> to start returning
   *              bytes from.
   * @param count The requested number of bytes to return.
   * @return The requested bytes from this <code>Blob</code>.
   * @exception SQLException If an error occurs.
   */
  byte[] getBytes(long start, int count) throws SQLException;

  /**
   * This method returns a stream that will read the bytes of this
   * <code>Blob</code>.
   *
   * @return A stream that will read the bytes of this <code>Blob</code>.
   * @exception SQLException If an error occurs.
   */
  InputStream getBinaryStream() throws SQLException;

  /**
   * This method returns the index into this <code>Blob</code> at which the
   * first instance of the specified bytes occur. The searching starts at the
   * specified index into this <code>Blob</code>.
   *
   * @param pattern The byte pattern to search for.
   * @param start The index into this <code>Blob</code> to start searching for
   *              the pattern.
   * @return The offset at which the pattern is first found, or -1 if the
   *         pattern is not found.
   * @exception SQLException If an error occurs.
   */
  long position(byte[] pattern, long start) throws SQLException;

  /**
   * This method returns the index into this <code>Blob</code> at which the
   * first instance of the specified pattern occurs. The searching starts at the
   * specified index into this <code>Blob</code>. The bytes in the specified
   * <code>Blob</code> are used as the search pattern.
   *
   * @param pattern The <code>Blob</code> containing the byte pattern to
   *                search for.
   * @param start The index into this <code>Blob</code> to start searching for
   *              the pattern.
   * @return The offset at which the pattern is first found, or -1 if the
   *         pattern is not found.
   * @exception SQLException If an error occurs.
   */
  long position(Blob pattern, long start) throws SQLException;

  /**
   * Writes the specified data into this <code>Blob</code>, starting at the
   * specified index.
   *
   * @param start The index at which the writing starts.
   * @param bytes The data to write.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  int setBytes(long start, byte[] bytes) throws SQLException;

  /**
   * Writes a portion of the specified data into this <code>Blob</code>,
   * starting at the specified index.
   *
   * @param startWrite The index into this <code>Blob</code> at which writing
   *                   starts.
   * @param bytes The data to write a portion of.
   * @param startRead The offset into the data where the portion to copy starts.
   * @param count The number of bytes to write.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  int setBytes(long startWrite, byte[] bytes, int startRead, int count)
      throws SQLException;

  /**
   * Returns a binary stream that writes into this <code>Blob</code>,
   * starting at the specified index.
   *
   * @param start The index at which the writing starts.
   * @return A binary stream to write into this <code>Blob</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  OutputStream setBinaryStream(long start) throws SQLException;

  /**
   * Truncates this <code>Blob</code> to be at most the specified number of
   * bytes long.
   *
   * @param count The length this <code>Blob</code> is truncated to.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void truncate(long count) throws SQLException;
}
