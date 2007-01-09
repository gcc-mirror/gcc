/* CallableStatement.java -- A statement for calling stored procedures.
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
   * @param sqlType The SQL type value from <code>Types</code>.
   * @exception SQLException If an error occurs.
   */   
  void registerOutParameter(int index, int sqlType)
    throws SQLException;

  /**
   * This method registers the specified parameter as an output parameter
   * of the specified SQL type and scale.
   *
   * @param index The index of the parameter to register as output.
   * @param sqlType The SQL type value from <code>Types</code>.
   * @param scale The scale of the value that will be returned.
   * @exception SQLException If an error occurs.
   */   
  void registerOutParameter(int index, int sqlType, int scale)
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
  String getString(int index) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>boolean</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>boolean</code>.
   * @exception SQLException If an error occurs.
   */
  boolean getBoolean(int index) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>byte</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>byte</code>.
   * @exception SQLException If an error occurs.
   */
  byte getByte(int index) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>short</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>short</code>.
   * @exception SQLException If an error occurs.
   */
  short getShort(int index) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>int</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>int</code>.
   * @exception SQLException If an error occurs.
   */
  int getInt(int index) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>long</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>long</code>.
   * @exception SQLException If an error occurs.
   */
  long getLong(int index) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>float</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>float</code>.
   * @exception SQLException If an error occurs.
   */
  float getFloat(int index) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>double</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>double</code>.
   * @exception SQLException If an error occurs.
   */
  double getDouble(int index) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>BigDecimal</code>.
   *
   * @param index The index of the parameter to return.
   * @param scale The number of digits to the right of the decimal to return.
   * @return The parameter value as a <code>BigDecimal</code>.
   * @exception SQLException If an error occurs.
   * @deprecated Use getBigDecimal(int index)
   *             or getBigDecimal(String name) instead.
   */
  BigDecimal getBigDecimal(int index, int scale)
    throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * byte array.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a byte array
   * @exception SQLException If an error occurs.
   */
  byte[] getBytes(int index) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>java.sql.Date</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>java.sql.Date</code>.
   * @exception SQLException If an error occurs.
   */
  Date getDate(int index) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>java.sql.Time</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>java.sql.Time</code>.
   * @exception SQLException If an error occurs.
   */
  Time getTime(int index) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>java.sql.Timestamp</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>java.sql.Timestamp</code>.
   * @exception SQLException If an error occurs.
   */
  Timestamp getTimestamp(int index) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>Object</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as an <code>Object</code>.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  Object getObject(int index) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>BigDecimal</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>BigDecimal</code>.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  BigDecimal getBigDecimal(int index) throws SQLException;

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
  Object getObject(int index, Map<String, Class<?>> map) throws SQLException;

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
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>Array</code>.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  Array getArray(int index) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>java.sql.Date</code>.
   *
   * @param index The index of the parameter to return.
   * @param cal The <code>Calendar</code> to use for timezone and locale.
   * @return The parameter value as a <code>java.sql.Date</code>.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  Date getDate(int index, Calendar cal) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>java.sql.Time</code>.
   *
   * @param index The index of the parameter to return.
   * @param cal The <code>Calendar</code> to use for timezone and locale.
   * @return The parameter value as a <code>java.sql.Time</code>.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  Time getTime(int index, Calendar cal) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>java.sql.Timestamp</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>java.sql.Timestamp</code>.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  Timestamp getTimestamp(int index, Calendar cal)
    throws SQLException;

  /**
   * This method registers the specified parameter as an output parameter
   * of the specified SQL type.
   *
   * @param index The index of the parameter to register as output.
   * @param sqlType The SQL type value from <code>Types</code>.
   * @param typeName The user defined data type name.
   * @exception SQLException If an error occurs.
   * @since 1.2
   */
  void registerOutParameter(int index, int sqlType, String typeName)
    throws SQLException;

  /**
   * This method registers the specified parameter as an output parameter
   * of the specified SQL type.
   *
   * @param name The name of the parameter to register as output.
   * @param sqlType The SQL type value from <code>Types</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void registerOutParameter(String name, int sqlType)
    throws SQLException;

  /**
   * This method registers the specified parameter as an output parameter
   * of the specified SQL type.  This version of registerOutParameter is used 
   * for NUMERIC or DECIMAL types.
   *
   * @param name The name of the parameter to register as output.
   * @param sqlType The SQL type value from <code>Types</code>.
   * @param scale Number of digits to the right of the decimal point.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void registerOutParameter(String name, int sqlType, int scale)
    throws SQLException;


  /**
   * This method registers the specified parameter as an output parameter
   * of the specified SQL type.  This version of registerOutParameter is used 
   * for user-named or REF types. If the type of the output parameter does
   * not have such a type, the typeName argument is ignored.
   *
   * @param name The name of the parameter to register as output.
   * @param sqlType The SQL type value from <code>Types</code>.
   * @param typeName The SQL structured type name.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void registerOutParameter(String name, int sqlType, String typeName) 
    throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>java.net.URL</code>.
   *
   * @param index The index of the parameter to return.
   * @return The parameter value as a <code>URL</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  URL getURL(int index) throws SQLException;

  /**
   * This method sets the value of the specified parameter to the specified
   * <code>java.net.URL</code>
   * 
   * @param name The name of the parameter to set.
   * @param value The value the parameter.
   * @since 1.4
   */
  void setURL(String name, URL value) throws SQLException;

  /**
   * This method populates the specified parameter with a SQL NULL value
   * for the specified type.
   *
   * @param name The name of the parameter to set.
   * @param sqlType The SQL type identifier of the parameter from 
   *                <code>Types</code>
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setNull(String name, int sqlType) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>boolean</code> value.
   *
   * @param name The name of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setBoolean(String name, boolean value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>byte</code> value.
   *
   * @param name The name of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setByte(String name, byte value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>short</code> value.
   *
   * @param name The name of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setShort(String name, short value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>int</code> value.
   *
   * @param name The name of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setInt(String name, int value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>long</code> value.
   *
   * @param name The name of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setLong(String name, long value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>float</code> value.
   *
   * @param name The name of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setFloat(String name, float value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>double</code> value.
   *
   * @param name The name of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setDouble(String name, double value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>BigDecimal</code> value.
   *
   * @param name The name of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setBigDecimal(String name, BigDecimal value)
    throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>String</code> value.
   *
   * @param name The name of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setString(String name, String value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>byte</code> array value.
   *
   * @param name The name of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setBytes(String name, byte[] value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>java.sql.Date</code> value.
   *
   * @param name The name of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setDate(String name, Date value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>java.sql.Time</code> value.
   *
   * @param name The name of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setTime(String name, Time value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>java.sql.Timestamp</code> value.
   *
   * @param name The name of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setTimestamp(String name, Timestamp value)
    throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * ASCII <code>InputStream</code> value.
   *
   * @param name The name of the parameter value to set.
   * @param stream The stream from which the parameter value is read.
   * @param count The number of bytes in the stream.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setAsciiStream(String name, InputStream stream, int count)
      throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * binary <code>InputStream</code> value.
   *
   * @param name The name of the parameter value to set.
   * @param stream The stream from which the parameter value is read.
   * @param count The number of bytes in the stream.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setBinaryStream(String name, InputStream stream, int count)
      throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>Object</code> value.  The specified SQL object type will be used.
   *
   * @param name The name of the parameter value to set.
   * @param value The value of the parameter.
   * @param sqlType The SQL type to use for the parameter, from 
   *                <code>Types</code>
   * @param scale The scale of the value, for numeric values only.
   * @exception SQLException If an error occurs.
   * @see Types
   * @since 1.4
   */
  void setObject(String name, Object value, int sqlType, int scale)
    throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>Object</code> value.  The specified SQL object type will be used.
   *
   * @param name The name of the parameter value to set.
   * @param value The value of the parameter.
   * @param sqlType The SQL type to use for the parameter, from 
   *                <code>Types</code>
   * @exception SQLException If an error occurs.
   * @see Types
   * @since 1.4
   */
  void setObject(String name, Object value, int sqlType)
    throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>Object</code> value.  The default object type to SQL type mapping
   * will be used.
   *
   * @param name The name of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setObject(String name, Object value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * character <code>Reader</code> value.
   *
   * @param name The name of the parameter value to set.
   * @param reader The reader from which the parameter value is read.
   * @param count The number of characters in the stream.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setCharacterStream(String name, Reader reader, int count)
    throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>java.sql.Date</code> value.
   *
   * @param name The name of the parameter value to set.
   * @param value The value of the parameter.
   * @param cal The <code>Calendar</code> to use for timezone and locale.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setDate(String name, Date value, Calendar cal)
    throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>java.sql.Time</code> value.
   *
   * @param name The name of the parameter value to set.
   * @param value The value of the parameter.
   * @param cal The <code>Calendar</code> to use for timezone and locale.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setTime(String name, Time value, Calendar cal)
    throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>java.sql.Timestamp</code> value.
   *
   * @param name The name of the parameter value to set.
   * @param value The value of the parameter.
   * @param cal The <code>Calendar</code> to use for timezone and locale.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setTimestamp(String name, Timestamp value, Calendar cal)
    throws SQLException;

  /**
   * This method populates the specified parameter with a SQL NULL value
   * for the specified type.
   *
   * @param name The name of the parameter to set.
   * @param sqlType The SQL type identifier of the parameter from
   *                <code>Types</code>
   * @param typeName The name of the data type, for user defined types.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setNull(String name, int sqlType, String typeName)
    throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>String</code>.
   *
   * @param name The name of the parameter to return.
   * @return The parameter value as a <code>String</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  String getString(String name) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>boolean</code>.
   *
   * @param name The name of the parameter to return.
   * @return The parameter value as a <code>boolean</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  boolean getBoolean(String name) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>byte</code>.
   *
   * @param name The name of the parameter to return.
   * @return The parameter value as a <code>byte</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  byte getByte(String name) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>short</code>.
   *
   * @param name The name of the parameter to return.
   * @return The parameter value as a <code>short</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  short getShort(String name) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>int</code>.
   *
   * @param name The name of the parameter to return.
   * @return The parameter value as a <code>int</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  int getInt(String name) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>long</code>.
   *
   * @param name The name of the parameter to return.
   * @return The parameter value as a <code>long</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  long getLong(String name) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>float</code>.
   *
   * @param name The name of the parameter to return.
   * @return The parameter value as a <code>float</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  float getFloat(String name) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>double</code>.
   *
   * @param name The name of the parameter to return.
   * @return The parameter value as a <code>double</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  double getDouble(String name) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>byte</code> array.
   *
   * @param name The name of the parameter to return.
   * @return The parameter value as a <code>byte[]</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  byte[] getBytes(String name) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>java.sql.Date</code>.
   *
   * @param name The name of the parameter to return.
   * @return The parameter value as a <code>java.sql.Date</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  Date getDate(String name) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>java.sql.Time</code>.
   *
   * @param name The name of the parameter to return.
   * @return The parameter value as a <code>java.sql.Time</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  Time getTime(String name) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>java.sql.Timestamp</code>.
   *
   * @param name The name of the parameter to return.
   * @return The parameter value as a <code>java.sql.Timestamp</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  Timestamp getTimestamp(String name) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>Object</code>.
   *
   * @param name The name of the parameter to return.
   * @return The parameter value as a <code>Object</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  Object getObject(String name) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>BigDecimal</code>.
   *
   * @param name The name of the parameter to return.
   * @return The parameter value as a <code>BigDecimal</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  BigDecimal getBigDecimal(String name) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>Object</code> using the specified mapping for conversion from
   * SQL to Java types.
   *
   * @param name The name of the parameter to return.
   * @param map The mapping to use for conversion from SQL to Java types.
   * @return The parameter value as an <code>Object</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  Object getObject(String name, Map<String, Class<?>> map) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>Ref</code>.
   *
   * @param name The name of the parameter to return.
   * @return The parameter value as a <code>Ref</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  Ref getRef(String name) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>Blob</code>.
   *
   * @param name The name of the parameter to return.
   * @return The parameter value as a <code>Blob</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  Blob getBlob(String name) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>Clob</code>.
   *
   * @param name The name of the parameter to return.
   * @return The parameter value as a <code>Clob</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  Clob getClob(String name) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>Array</code>.
   *
   * @param name The name of the parameter to return.
   * @return The parameter value as a <code>Array</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  Array getArray(String name) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>java.sql.Date</code>.
   *
   * @param name The name of the parameter to return.
   * @param cal The <code>Calendar</code> to use for timezone and locale.
   * @return The parameter value as a <code>java.sql.Date</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  Date getDate(String name, Calendar cal) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>java.sql.Time</code>.
   *
   * @param name The name of the parameter to return.
   * @param cal The <code>Calendar</code> to use for timezone and locale.
   * @return The parameter value as a <code>java.sql.Time</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  Time getTime(String name, Calendar cal) throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>java.sql.Timestamp</code>.
   *
   * @param name The name of the parameter to return.
   * @param cal The <code>Calendar</code> to use for timezone and locale.
   * @return The parameter value as a <code>java.sql.Timestamp</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  Timestamp getTimestamp(String name, Calendar cal)
    throws SQLException;

  /**
   * This method returns the value of the specified parameter as a Java
   * <code>java.net.URL</code>.
   *
   * @param name The name of the parameter to return.
   * @return The parameter value as a <code>java.net.URL</code>.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  URL getURL(String name) throws SQLException;
}
