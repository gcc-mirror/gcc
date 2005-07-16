/* CallableStatement.java -- A statement for calling stored procedures.
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
import java.util.Calendar;
import java.util.Map;

/**
 * This interface provides a mechanism for calling stored procedures.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public interface CallableStatement extends PreparedStatement 
{
  /**
   * This method registers the specified parameter as an output parameter
   * of the specified SQL type.
   *
   * @param index The index of the parameter to register as output.
   * @param type The SQL type value from <code>Types</code>.
   * @exception SQLException If an error occurs.
   */   
  void registerOutParameter(int parameterIndex, int sqlType)
    throws SQLException;

  /**
   * This method registers the specified parameter as an output parameter
   * of the specified SQL type and scale.
   *
   * @param index The index of the parameter to register as output.
   * @param type The SQL type value from <code>Types</code>.
   * @param scale The scale of the value that will be returned.
   * @exception SQLException If an error occurs.
   */   
  void registerOutParameter(int parameterIndex, int sqlType, int scale)
    throws SQLException;

  /**
   * This method tests whether the value of the last parameter that was fetched
   * was actually a SQL NULL value.
   *
   * @return <code>true</code> if the last parameter fetched was a NULL,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean wasNull() throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>String</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>String</code>.
   * @exception SQLException If an error occurs.
   */
  String getString(int parameterIndex) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>boolean</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>boolean</code>.
   * @exception SQLException If an error occurs.
   */
  boolean getBoolean(int parameterIndex) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>byte</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>byte</code>.
   * @exception SQLException If an error occurs.
   */
  byte getByte(int parameterIndex) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>short</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>short</code>.
   * @exception SQLException If an error occurs.
   */
  short getShort(int parameterIndex) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>int</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>int</code>.
   * @exception SQLException If an error occurs.
   */
  int getInt(int parameterIndex) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>long</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>long</code>.
   * @exception SQLException If an error occurs.
   */
  long getLong(int parameterIndex) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>float</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>float</code>.
   * @exception SQLException If an error occurs.
   */
  float getFloat(int parameterIndex) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>double</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>double</code>.
   * @exception SQLException If an error occurs.
   */
  double getDouble(int parameterIndex) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>BigDecimal</code>.
   *
   * @param parameterIndex The index of the parameter to return.
   * @param scale The number of digits to the right of the decimal to return.
   * @return The parameter value as a <code>BigDecimal</code>.
   * @exception SQLException If an error occurs.
   * @deprecated Use getBigDecimal(int parameterIndex)
   *             or getBigDecimal(String parameterName) instead.
   */
  BigDecimal getBigDecimal(int parameterIndex, int scale)
    throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * byte array.
   *
   * @param parameterIndex The index of the parameter to return.
   * @return The parameter value as a byte array
   * @exception SQLException If an error occurs.
   */
  byte[] getBytes(int parameterIndex) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>java.sql.Date</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>java.sql.Date</code>.
   * @exception SQLException If an error occurs.
   */
  Date getDate(int parameterIndex) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>java.sql.Time</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>java.sql.Time</code>.
   * @exception SQLException If an error occurs.
   */
  Time getTime(int parameterIndex) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>java.sql.Timestamp</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>java.sql.Timestamp</code>.
   * @exception SQLException If an error occurs.
   */
  Timestamp getTimestamp(int parameterIndex) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>Object</code>.
   *
   * @param parameterIndex The index of the parameter to return.
   * @return The parameter value as an <code>Object</code>.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  Object getObject(int parameterIndex) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>BigDecimal</code>.
   *
   * @param parameterIndex The index of the parameter to return.
   * @return The parameter value as a <code>BigDecimal</code>.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  BigDecimal getBigDecimal(int parameterIndex) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>Object</code>.
   *
   * @param index The index of the parameter to return.
   * @param map The mapping to use for conversion from SQL to Java types.
   * @return The parameter value as an <code>Object</code>.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  Object getObject(int index, Map map) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>Ref</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>Ref</code>.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  Ref getRef(int index) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>Blob</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>Blob</code>.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */   
  Blob getBlob(int index) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>Clob</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>Clob</code>.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  Clob getClob(int index) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>Array</code>.
   *
   * @param parameterIndex The index of the parameter to return.
   * @return The parameter value as a <code>Array</code>.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  Array getArray(int index) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>java.sql.Date</code>.
   *
   * @param parameterIndex The index of the parameter to return.
   * @param cal The <code>Calendar</code> to use for timezone and locale.
   * @return The parameter value as a <code>java.sql.Date</code>.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  Date getDate(int parameterIndex, Calendar cal) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>java.sql.Time</code>.
   *
   * @param parameterIndex The index of the parameter to return.
   * @param cal The <code>Calendar</code> to use for timezone and locale.
   * @return The parameter value as a <code>java.sql.Time</code>.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  Time getTime(int parameterIndex, Calendar cal) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>java.sql.Timestamp</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>java.sql.Timestamp</code>.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  Timestamp getTimestamp(int parameterIndex, Calendar cal)
    throws SQLException;

  /**
   * This method registers the specified parameter as an output parameter
   * of the specified SQL type.
   *
   * @param index The index of the parameter to register as output.
   * @param type The SQL type value from <code>Types</code>.
   * @param name The user defined data type name.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  void registerOutParameter(int paramIndex, int sqlType,
				   String typeName)
    throws SQLException;

  /**
   * This method registers the specified parameter as an output parameter
   * of the specified SQL type.
   *
   * @param parameterName The name of the parameter to register as output.
   * @param sqlType The SQL type value from <code>Types</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void registerOutParameter(String parameterName, int sqlType)
    throws SQLException;

  /**
   * This method registers the specified parameter as an output parameter
   * of the specified SQL type.  This version of registerOutParameter is used 
   * for NUMERIC or DECIMAL types.
   *
   * @param parameterName The name of the parameter to register as output.
   * @param sqlType The SQL type value from <code>Types</code>.
   * @param scale Number of digits to the right of the decimal point.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void registerOutParameter(String parameterName, int sqlType,
				   int scale)
    throws SQLException;


  /**
   * This method registers the specified parameter as an output parameter
   * of the specified SQL type.  This version of registerOutParameter is used 
   * for user-named or REF types. If the type of the output parameter does
   * not have such a type, the typeName argument is ignored.
   *
   * @param parameterName The name of the parameter to register as output.
   * @param sqlType The SQL type value from <code>Types</code>.
   * @param typeName The SQL structured type name.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void registerOutParameter(String parameterName, int sqlType,
				   String typeName) 
    throws SQLException;

  /**
   * @since 1.4
   */
  URL getURL(int parameterIndex) throws SQLException;

  /**
   * @since 1.4
   */
  void setURL(String parameterName, URL val) throws SQLException;

  /**
   * @since 1.4
   */
  void setNull(String parameterName, int sqlType) throws SQLException;

  /**
   * @since 1.4
   */
  void setBoolean(String parameterName, boolean x) throws SQLException;

  /**
   * @since 1.4
   */
  void setByte(String parameterName, byte x) throws SQLException;

  /**
   * @since 1.4
   */
  void setShort(String parameterName, short x) throws SQLException;

  /**
   * @since 1.4
   */
  void setInt(String parameterName, int x) throws SQLException;

  /**
   * @since 1.4
   */
  void setLong(String parameterName, long x) throws SQLException;

  /**
   * @since 1.4
   */
  void setFloat(String parameterName, float x) throws SQLException;

  /**
   * @since 1.4
   */
  void setDouble(String parameterName, double x) throws SQLException;

  /**
   * @since 1.4
   */
  void setBigDecimal(String parameterName, BigDecimal x)
    throws SQLException;

  /**
   * @since 1.4
   */
  void setString(String parameterName, String x) throws SQLException;

  /**
   * @since 1.4
   */
  void setBytes(String parameterName, byte[] x) throws SQLException;

  /**
   * @since 1.4
   */
  void setDate(String parameterName, Date x) throws SQLException;

  /**
   * @since 1.4
   */
  void setTime(String parameterName, Time x) throws SQLException;

  /**
   * @since 1.4
   */
  void setTimestamp(String parameterName, Timestamp x)
    throws SQLException;

  /**
   * @since 1.4
   */
  void setAsciiStream(String parameterName, InputStream x, int length)
      throws SQLException;

  /**
   * @since 1.4
   */
  void setBinaryStream(String parameterName, InputStream x, int length)
      throws SQLException;

  /**
   * @since 1.4
   */
  void setObject(String parameterName, Object x, int targetSqlType,
			int scale)
    throws SQLException;

  /**
   * @since 1.4
   */
  void setObject(String parameterName, Object x, int targetSqlType)
    throws SQLException;

  /**
   * @since 1.4
   */
  void setObject(String parameterName, Object x) throws SQLException;

  /**
   * @since 1.4
   */
  void setCharacterStream(String parameterName, Reader reader,
				 int length)
    throws SQLException;

  /**
   * @since 1.4
   */
  void setDate(String parameterName, Date x, Calendar cal)
    throws SQLException;

  /**
   * @since 1.4
   */
  void setTime(String parameterName, Time x, Calendar cal)
    throws SQLException;

  /**
   * @since 1.4
   */
  void setTimestamp(String parameterName, Timestamp x, Calendar cal)
    throws SQLException;

  /**
   * @since 1.4
   */
  void setNull(String parameterName, int sqlType, String typeName)
    throws SQLException;

  /**
   * @since 1.4
   */
  String getString(String parameterName) throws SQLException;

  /**
   * @since 1.4
   */
  boolean getBoolean(String parameterName) throws SQLException;

  /**
   * @since 1.4
   */
  byte getByte(String parameterName) throws SQLException;

  /**
   * @since 1.4
   */
  short getShort(String parameterName) throws SQLException;

  /**
   * @since 1.4
   */
  int getInt(String parameterName) throws SQLException;

  /**
   * @since 1.4
   */
  long getLong(String parameterName) throws SQLException;

  /**
   * @since 1.4
   */
  float getFloat(String parameterName) throws SQLException;

  /**
   * @since 1.4
   */
  double getDouble(String parameterName) throws SQLException;

  /**
   * @since 1.4
   */
  byte[] getBytes(String parameterName) throws SQLException;

  /**
   * @since 1.4
   */
  Date getDate(String parameterName) throws SQLException;

  /**
   * @since 1.4
   */
  Time getTime(String parameterName) throws SQLException;

  /**
   * @since 1.4
   */
  Timestamp getTimestamp(String parameterName) throws SQLException;

  /**
   * @since 1.4
   */
  Object getObject(String parameterName) throws SQLException;

  /**
   * @since 1.4
   */
  BigDecimal getBigDecimal(String parameterName) throws SQLException;

  /**
   * @since 1.4
   */
  Object getObject(String parameterName, Map map) throws SQLException;

  /**
   * @since 1.4
   */
  Ref getRef(String parameterName) throws SQLException;

  /**
   * @since 1.4
   */
  Blob getBlob(String parameterName) throws SQLException;

  /**
   * @since 1.4
   */
  Clob getClob(String parameterName) throws SQLException;

  /**
   * @since 1.4
   */
  Array getArray(String parameterName) throws SQLException;

  /**
   * @since 1.4
   */
  Date getDate(String parameterName, Calendar cal) throws SQLException;

  /**
   * @since 1.4
   */
  Time getTime(String parameterName, Calendar cal) throws SQLException;

  /**
   * @since 1.4
   */
  Timestamp getTimestamp(String parameterName, Calendar cal)
    throws SQLException;

  /**
   * @since 1.4
   */
  URL getURL(String parameterName) throws SQLException;
}
