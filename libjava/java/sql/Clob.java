/* Clob.java -- Access Character Large OBjects
   Copyright (C) 1999, 2000, 2002 Free Software Foundation, Inc.

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
import java.io.Reader;
import java.io.Writer;

/**
 * This interface contains methods for accessing a SQL CLOB (Character
 * Large OBject) type.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public interface Clob 
{
 /**
  * This method returns the number of characters in the CLOB.
  *
  * @return The number of characters in the CLOB.
  * @exception SQLException If an error occurs.
  * @since 1.2
  */
  long length() throws SQLException;

  /**
   * This method returns the specified portion of the CLOB as a 
   * <code>String</code>. 
   *
   * @param offset The index into the CLOB (index values start at 1) to 
   *        start returning characters from.
   * @param length The requested number of characters to return.
   * @return The requested CLOB section, as a <code>String</code>.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  String getSubString(long pos, int length) throws SQLException;

  /**
   * This method returns a character stream that reads the contents of the
   * CLOB.
   *
   * @return A character stream to read the CLOB's contents.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  Reader getCharacterStream() throws SQLException;

  /**
   * This method returns a byte stream that reads the contents of the
   * CLOB as a series of ASCII bytes.
   *
   * @return A stream to read the CLOB's contents.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  InputStream getAsciiStream() throws SQLException;

  /**
   * This method returns the index into the CLOB of the first occurrence of
   * the specified character pattern (supplied by the caller as a
   * <code>String</code>).  The search begins at the specified index.
   *
   * @param searchstr The character pattern to search for, passed as a
   *        <code>String</code>.
   * @param start.  The index into the CLOB to start search (indexes start
   *        at 1).
   * @return The index at which the pattern was found (indexes start at 1),
   *         or -1 if the pattern was not found.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  long position(String searchstr, long start) throws SQLException;

  /**
   * This method returns the index into the CLOB of the first occurrence of
   * the specified character pattern (supplied by the caller as a
   * <code>Clob</code>).  The search begins at the specified index.
   *
   * @param searchstr The character pattern to search for, passed as a
   *        <code>Clob</code>.
   * @param start.  The index into the CLOB to start search (indexes start
   *        at 1).
   * @return The index at which the pattern was found (indexes start at 1),
   *         or -1 if the pattern was not found.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  long position(Clob searchstr, long start) throws SQLException;

  /**
   * @since 1.4
   */
  int setString(long pos, String str) throws SQLException;

  /**
   * @since 1.4
   */
  int setString(long pos, String str, int offset, int len)
    throws SQLException;

  /**
   * @since 1.4
   */
  OutputStream setAsciiStream(long pos) throws SQLException;

  /**
   * @since 1.4
   */
  Writer setCharacterStream(long pos) throws SQLException;

  /**
   * @since 1.4
   */
  void truncate(long len) throws SQLException;
}
