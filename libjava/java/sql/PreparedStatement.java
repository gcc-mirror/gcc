/* PreparedStatement.java -- Interface for pre-compiled statements.
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
  public ResultSet executeQuery() throws SQLException;

  /**
   * This method executes an SQL INSERT, UPDATE or DELETE statement.  SQL
   * statements that return nothing such as SQL DDL statements can be executed.
   *
   * @return The result is either the row count for INSERT, UPDATE or DELETE
   *         statements; or 0 for SQL statements that return nothing.
   * @exception SQLException If an error occurs.
   */
  public int executeUpdate() throws SQLException;

  /**
   * This method populates the specified parameter with a SQL NULL value
   * for the specified type.
   *
   * @param index The index of the parameter to set.
   * @param type The SQL type identifier of the parameter from <code>Types</code>
   *
   * @exception SQLException If an error occurs.
   */
  public void setNull(int parameterIndex, int sqlType) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>boolean</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  public void setBoolean(int parameterIndex, boolean x) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>byte</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  public void setByte(int parameterIndex, byte x) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>short</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  public void setShort(int parameterIndex, short x) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>int</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  public void setInt(int parameterIndex, int x) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>long</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  public void setLong(int parameterIndex, long x) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>float</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  public void setFloat(int parameterIndex, float x) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>double</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  public void setDouble(int parameterIndex, double x) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>java.math.BigDecimal</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  public void setBigDecimal(int parameterIndex, BigDecimal x) throws
      SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>String</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  public void setString(int parameterIndex, String x) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>byte</code> array value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  public void setBytes(int parameterIndex, byte[] x) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>java.sql.Date</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  public void setDate(int parameterIndex, Date x) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>java.sql.Time</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  public void setTime(int parameterIndex, Time x) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>java.sql.Timestamp</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  public void setTimestamp(int parameterIndex, Timestamp x)
    throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * ASCII <code>InputStream</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @param length The number of bytes in the stream.
   * @exception SQLException If an error occurs.
   */
  public void setAsciiStream(int parameterIndex, InputStream x, int length)
    throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * Unicode UTF-8 <code>InputStream</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @param length The number of bytes in the stream.
   * @exception SQLException If an error occurs.
   * @deprecated
   */
  public void setUnicodeStream(int parameterIndex, InputStream x, int length)
    throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * binary <code>InputStream</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @param length The number of bytes in the stream.
   * @exception SQLException If an error occurs.
   */
  public void setBinaryStream(int parameterIndex, InputStream x, int length)
    throws SQLException;

  /**
   * This method clears all of the input parameter that have been
   * set on this statement.
   *
   * @exception SQLException If an error occurs.
   */
  public void clearParameters() throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>Object</code> value.  The specified SQL object type will be used.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @param type The SQL type to use for the parameter, from <code>Types</code>
   * @param scale The scale of the value, for numeric values only.
   * @exception SQLException If an error occurs.
   * @see Types
   */
  public void setObject(int parameterIndex, Object x, int targetSqlType,
    int scale) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>Object</code> value.  The specified SQL object type will be used.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @param type The SQL type to use for the parameter, from <code>Types</code>
   * @exception SQLException If an error occurs.
   * @see Types
   */
  public void setObject(int parameterIndex, Object x, int targetSqlType)
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
  public void setObject(int parameterIndex, Object x) throws SQLException;

  /**
   * This method executes a prepared SQL query.
   * Some prepared statements return multiple results; the execute method
   * handles these complex statements as well as the simpler form of
   * statements handled by executeQuery and executeUpdate.
   *
   * @return The result of the SQL statement.
   * @exception SQLException If an error occurs.
   */
  public boolean execute() throws SQLException;

  /**
   * This method adds a set of parameters to the batch for JDBC 2.0.
   * @exception SQLException If an error occurs.
   */
  public void addBatch() throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * character <code>Reader</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @param length The number of bytes in the stream.
   * @exception SQLException If an error occurs.
   */
  public void setCharacterStream(int parameterIndex, Reader reader,
    int length) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>Ref</code> value.  The default object type to SQL type mapping
   * will be used.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  public void setRef(int i, Ref x) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>Blob</code> value.  The default object type to SQL type mapping
   * will be used.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  public void setBlob(int i, Blob x) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>Clob</code> value.  The default object type to SQL type mapping
   * will be used.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  public void setClob(int i, Clob x) throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>Array</code> value.  The default object type to SQL type mapping
   * will be used.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @exception SQLException If an error occurs.
   */
  public void setArray(int i, Array x) throws SQLException;

  /**
   * This method returns meta data for the result set from this statement.
   *
   * @return Meta data for the result set from this statement.
   * @exception SQLException If an error occurs.
   */
  public ResultSetMetaData getMetaData() throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>java.sql.Date</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @param calendar The <code>Calendar</code> to use for timezone and locale.
   * @exception SQLException If an error occurs.
   */
  public void setDate(int parameterIndex, Date x, Calendar cal)
    throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>java.sql.Time</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @param calendar The <code>Calendar</code> to use for timezone and locale.
   * @exception SQLException If an error occurs.
   */
  public void setTime(int parameterIndex, Time x, Calendar cal)
    throws SQLException;

  /**
   * This method sets the specified parameter from the given Java
   * <code>java.sql.Timestamp</code> value.
   *
   * @param index The index of the parameter value to set.
   * @param value The value of the parameter.
   * @param calendar The <code>Calendar</code> to use for timezone and locale.
   * @exception SQLException If an error occurs.
   */
  public void setTimestamp(int parameterIndex, Timestamp x, Calendar cal)
    throws SQLException;

  /**
   * This method populates the specified parameter with a SQL NULL value
   * for the specified type.
   *
   * @param index The index of the parameter to set.
   * @param type The SQL type identifier of the parameter from <code>Types</code>
   * @param name The name of the data type, for user defined types.
   * @exception SQLException If an error occurs.
   */
  public void setNull(int paramIndex, int sqlType, String typeName)
    throws SQLException;

  /**
   * @since 1.4
   */
  public void setURL(int parameterIndex, URL x) throws SQLException;

  /**
   * @since 1.4
   */
  public ParameterMetaData getParameterMetaData() throws SQLException;
}
