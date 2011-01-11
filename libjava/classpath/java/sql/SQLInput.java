/* SQLInput.java -- Read SQL values from a stream
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
import java.io.Reader;
import java.math.BigDecimal;
import java.net.URL;

/**
 * This interface provides methods for reading values from a stream
 * that is connected to a SQL structured or distinct type.  It is used
 * for custom mapping of user defined data types.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public interface SQLInput
{
  /**
   * This method reads the next item from the stream a Java
   * <code>String</code>.
   *
   * @return The value read from the stream as a <code>String</code>.
   * @exception SQLException If an error occurs.
   */
  String readString() throws SQLException;

  /**
   * This method reads the next item from the stream a Java
   * <code>boolean</code>.
   *
   * @return The value read from the stream as a <code>boolean</code>.
   * @exception SQLException If an error occurs.
   */
  boolean readBoolean() throws SQLException;

  /**
   * This method reads the next item from the stream a Java
   * <code>byte</code>.
   *
   * @return The value read from the stream as a <code>byte</code>.
   * @exception SQLException If an error occurs.
   */
  byte readByte() throws SQLException;

  /**
   * This method reads the next item from the stream a Java
   * <code>short</code>.
   *
   * @return The value read from the stream as a <code>short</code>.
   * @exception SQLException If an error occurs.
   */
  short readShort() throws SQLException;

  /**
   * This method reads the next item from the stream a Java
   * <code>int</code>.
   *
   * @return The value read from the stream as an <code>int</code>.
   * @exception SQLException If an error occurs.
   */
  int readInt() throws SQLException;

  /**
   * This method reads the next item from the stream a Java
   * <code>long</code>.
   *
   * @return The value read from the stream as a <code>long</code>.
   * @exception SQLException If an error occurs.
   */
  long readLong() throws SQLException;

  /**
   * This method reads the next item from the stream a Java
   * <code>float</code>.
   *
   * @return The value read from the stream as a <code>float</code>.
   * @exception SQLException If an error occurs.
   */
  float readFloat() throws SQLException;

  /**
   * This method reads the next item from the stream a Java
   * <code>double</code>.
   *
   * @return The value read from the stream as a <code>double</code>.
   * @exception SQLException If an error occurs.
   */
  double readDouble() throws SQLException;

  /**
   * This method reads the next item from the stream a Java
   * <code>BigDecimal</code>.
   *
   * @return The value read from the stream as a <code>BigDecimal</code>.
   * @exception SQLException If an error occurs.
   */
  BigDecimal readBigDecimal() throws SQLException;

  /**
   * This method reads the next item from the stream a Java
   * byte array
   *
   * @return The value read from the stream as a byte array.
   * @exception SQLException If an error occurs.
   */
  byte[] readBytes() throws SQLException;

  /**
   * This method reads the next item from the stream a Java
   * <code>java.sql.Date</code>.
   *
   * @return The value read from the stream as a <code>java.sql.Date</code>.
   * @exception SQLException If an error occurs.
   */
  Date readDate() throws SQLException;

  /**
   * This method reads the next item from the stream a Java
   * <code>java.sql.Time</code>.
   *
   * @return The value read from the stream as a <code>java.sql.Time</code>.
   * @exception SQLException If an error occurs.
   */
  Time readTime() throws SQLException;

  /**
   * This method reads the next item from the stream a Java
   * <code>java.sql.Timestamp</code>.
   *
   * @return The value read from the stream as a <code>java.sql.Timestamp</code>.
   * @exception SQLException If an error occurs.
   */
  Timestamp readTimestamp() throws SQLException;

  /**
   * This method reads the next item from the stream a character
   * <code>Reader</code>.
   *
   * @return The value read from the stream as a <code>Reader</code>.
   * @exception SQLException If an error occurs.
   */
  Reader readCharacterStream() throws SQLException;

  /**
   * This method reads the next item from the stream a ASCII text
   * <code>InputStream</code>.
   *
   * @return The value read from the stream as an <code>InputStream</code>.
   * @exception SQLException If an error occurs.
   */
  InputStream readAsciiStream() throws SQLException;

  /**
   * This method reads the next item from the stream a binary
   * <code>InputStream</code>.
   *
   * @return The value read from the stream as an <code>InputStream</code>.
   * @exception SQLException If an error occurs.
   */
  InputStream readBinaryStream() throws SQLException;

  /**
   * This method reads the next item from the stream a Java
   * <code>Object</code>.
   *
   * @return The value read from the stream as an <code>Object</code>.
   * @exception SQLException If an error occurs.
   */
  Object readObject() throws SQLException;

  /**
   * This method reads the next item from the stream a Java SQL
   * <code>Ref</code>.
   *
   * @return The value read from the stream as an <code>Ref</code>.
   * @exception SQLException If an error occurs.
   */
  Ref readRef() throws SQLException;

  /**
   * This method reads the next item from the stream a Java SQL
   * <code>Blob</code>.
   *
   * @return The value read from the stream as a <code>Blob</code>.
   * @exception SQLException If an error occurs.
   */
  Blob readBlob() throws SQLException;

  /**
   * This method reads the next item from the stream a Java SQL
   * <code>Clob</code>.
   *
   * @return The value read from the stream as a <code>Clob</code>.
   * @exception SQLException If an error occurs.
   */
  Clob readClob() throws SQLException;

  /**
   * This method reads the next item from the stream a Java SQL
   * <code>Array</code>.
   *
   * @return The value read from the stream as an <code>Array</code>.
   * @exception SQLException If an error occurs.
   */
  Array readArray() throws SQLException;

  /**
   * This method tests whether or not the last value read was a SQL
   * NULL value.
   *
   * @return <code>true</code> if the last value read was a NULL,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean wasNull() throws SQLException;

  /**
   * @since 1.4
   */
  URL readURL() throws SQLException;
}
