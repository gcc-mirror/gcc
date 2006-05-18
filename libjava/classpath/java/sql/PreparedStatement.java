/* PreparedStatement.java -- Interface for pre-compiled statements.
   Copyright (C) 1999, 2000, 2006 Free Software Foundation, Inc.

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

/**
 * This interface provides a mechanism for executing pre-compiled
 * statements.  This provides greater efficiency when calling the same
 * statement multiple times.  Parameters are allowed in a statement,
 * providings for maximum reusability.
 * 
 * <p> Note that in this class parameter indices start at 1, not 0.</p>
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public interface PreparedStatement extends Statement 
{
  /**
   * This method executes a prepared SQL query and returns its ResultSet.
   *
   * @return The ResultSet of the SQL statement.
   * @exception SQLException If an error occurs.
   */
  ResultSet executeQuery() throws SQLException;

  /**
   * This method executes an SQL INSERT, UPDATE or DELETE statement.  SQL
   * statements that return nothing such as SQL DDL statements can be executed.
   *
   * @return The result is either the row count for INSERT, UPDATE or DELETE
   *         statements; or 0 for SQL statements that return nothing.
   * @exception SQLException If an error occurs.
   */
  int executeUpdate() throws SQLException;

  /**
   * This method populates the specified parameter with a SQL NULL value
   * for the specified type.
   *
   * @param index The index of the parameter to set.
   * @param sqlType The SQL type identifier of the parameter from 
   *                <code>Types</code>
   *
   * @exception SQLException If an error occurs.
   */
  void setNull(int index, int sqlType) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>boolean</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  void setBoolean(int index, boolean value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>byte</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  void setByte(int index, byte value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>short</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  void setShort(int index, short value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>int</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  void setInt(int index, int value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>long</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  void setLong(int index, long value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>float</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  void setFloat(int index, float value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>double</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  void setDouble(int index, double value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>java.math.BigDecimal</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  void setBigDecimal(int index, BigDecimal value) throws
      SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>String</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  void setString(int index, String value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>byte</code> array value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  void setBytes(int index, byte[] value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>java.sql.Date</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  void setDate(int index, Date value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>java.sql.Time</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  void setTime(int index, Time value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>java.sql.Timestamp</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  void setTimestamp(int index, Timestamp value)
    throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * ASCII <code>InputStream</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param stream The stream from which the parameter value is read.
   * @param count The number of bytes in the stream.
   * @exception SQLException If an error occurs.
   */
  void setAsciiStream(int index, InputStream stream, int count)
    throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * Unicode UTF-8 <code>InputStream</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param stream The stream from which the parameter value is read.
   * @param count The number of bytes in the stream.
   * @exception SQLException If an error occurs.
   * @deprecated
   */
  void setUnicodeStream(int index, InputStream stream, int count)
    throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * binary <code>InputStream</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param stream The stream from which the parameter value is read.
   * @param count The number of bytes in the stream.
   * @exception SQLException If an error occurs.
   */
  void setBinaryStream(int index, InputStream stream, int count)
    throws SQLException;

  /**
   * This method clears all of the input parameter that have been
   * set on this statement.
   *
   * @exception SQLException If an error occurs.
   */
  void clearParameters() throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>Object</code> value.  The specified SQL object type will be used.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @param sqlType The SQL type to use for the parameter, from 
   *                <code>Types</code>
   * @param scale The scale of the value, for numeric values only.
   * @exception SQLException If an error occurs.
   * @see Types
   */
  void setObject(int index, Object value, int sqlType, int scale)
    throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>Object</code> value.  The specified SQL object type will be used.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @param sqlType The SQL type to use for the parameter, from 
   *                      <code>Types</code>
   * @exception SQLException If an error occurs.
   * @see Types
   */
  void setObject(int index, Object value, int sqlType)
    throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>Object</code> value.  The default object type to SQL type mapping
   * will be used.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  void setObject(int index, Object value) throws SQLException;

  /**
   * This method executes a prepared SQL query.
   * Some prepared statements return multiple results; the execute method
   * handles these complex statements as well as the simpler form of
   * statements handled by executeQuery and executeUpdate.
   *
   * @return The result of the SQL statement.
   * @exception SQLException If an error occurs.
   */
  boolean execute() throws SQLException;

  /**
   * This method adds a set of parameters to the batch for JDBC 2.0.
   * @exception SQLException If an error occurs.
   */
  void addBatch() throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * character <code>Reader</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param reader The reader from which the parameter value is read.
   * @param count The number of characters in the stream.
   * @exception SQLException If an error occurs.
   */
  void setCharacterStream(int index, Reader reader, int count)
  throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>Ref</code> value.  The default object type to SQL type mapping
   * will be used.
   *
   * @param index The index of the parameter value to set.
   * @param value The <code>Ref</code> used to set the value of the parameter.
   * @exception SQLException If an error occurs.
   */
  void setRef(int index, Ref value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>Blob</code> value.  The default object type to SQL type mapping
   * will be used.
   *
   * @param index The index of the parameter value to set.
   * @param value The <code>Blob</code> used to set the 
   *             value of the parameter.
   * @exception SQLException If an error occurs.
   */
  void setBlob(int index, Blob value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>Clob</code> value.  The default object type to SQL type mapping
   * will be used.
   *
   * @param index The index of the parameter value to set.
   * @param value The <code>Clob</code> used to set the
   *              value of the parameter.
   * @exception SQLException If an error occurs.
   */
  void setClob(int index, Clob value) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>Array</code> value.  The default object type to SQL type mapping
   * will be used.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  void setArray(int index, Array value) throws SQLException;

  /**
   * This method returns meta data for the result set from this statement.
   *
   * @return Meta data for the result set from this statement.
   * @exception SQLException If an error occurs.
   */
  ResultSetMetaData getMetaData() throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>java.sql.Date</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @param cal The <code>Calendar</code> to use for timezone and locale.
   * @exception SQLException If an error occurs.
   */
  void setDate(int index, Date value, Calendar cal)
    throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>java.sql.Time</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @param cal The <code>Calendar</code> to use for timezone and locale.
   * @exception SQLException If an error occurs.
   */
  void setTime(int index, Time value, Calendar cal)
    throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>java.sql.Timestamp</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @param cal The <code>Calendar</code> to use for timezone and locale.
   * @exception SQLException If an error occurs.
   */
  void setTimestamp(int index, Timestamp value, Calendar cal)
    throws SQLException;

  /**
   * This method populates the specified parameter with a SQL NULL value
   * for the specified type.
   *
   * @param index The index of the parameter to set.
   * @param sqlType The SQL type identifier of the parameter from
   *                <code>Types</code>
   * @param typeName The name of the data type, for user defined types.
   * @exception SQLException If an error occurs.
   */
  void setNull(int index, int sqlType, String typeName)
    throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>java.net.URL</code> value.
   * 
   * @param index The index of the parameter to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   * @since 1.4
   */
  void setURL(int index, URL value) throws SQLException;

  /**
   * Returns information about the parameters set on this 
   * <code>PreparedStatement</code> (see {@link ParameterMetaData} for a
   * detailed description of the provided information).
   * 
   * @return Meta data for the parameters of this statement.
   * @see ParameterMetaData
   * @since 1.4
   */
  ParameterMetaData getParameterMetaData() throws SQLException;
}
