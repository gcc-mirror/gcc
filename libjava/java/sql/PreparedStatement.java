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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.sql;

import java.io.InputStream;
import java.io.Reader;
import java.math.BigDecimal;

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
  * This method populates the specified parameter with a SQL NULL value
  * for the specified type.
  *
  * @param index The index of the parameter to set.
  * @param type The SQL type identifier of the parameter from <code>Types</code>
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setNull(int index, int type) throws SQLException;
  
/*************************************************************************/

/**
  * This method sets the specified parameter from the given Java
  * <code>boolean</code> value.
  *
  * @param index The index of the parameter value to set.
  * @param value The value of the parameter.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setBoolean(int index, boolean value) throws SQLException;

/*************************************************************************/

/**
  * This method sets the specified parameter from the given Java
  * <code>byte</code> value.
  *
  * @param index The index of the parameter value to set.
  * @param value The value of the parameter.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setByte(int index, byte value) throws SQLException;

/*************************************************************************/

/**
  * This method sets the specified parameter from the given Java
  * <code>short</code> value.
  *
  * @param index The index of the parameter value to set.
  * @param value The value of the parameter.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setShort(int index, short value) throws SQLException;

/*************************************************************************/

/**
  * This method sets the specified parameter from the given Java
  * <code>int</code> value.
  *
  * @param index The index of the parameter value to set.
  * @param value The value of the parameter.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setInt(int index, int value) throws SQLException;

/*************************************************************************/

/**
  * This method sets the specified parameter from the given Java
  * <code>long</code> value.
  *
  * @param index The index of the parameter value to set.
  * @param value The value of the parameter.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setLong(int index, long value) throws SQLException;

/*************************************************************************/

/**
  * This method sets the specified parameter from the given Java
  * <code>float</code> value.
  *
  * @param index The index of the parameter value to set.
  * @param value The value of the parameter.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setFloat(int index, float value) throws SQLException;

/*************************************************************************/

/**
  * This method sets the specified parameter from the given Java
  * <code>double</code> value.
  *
  * @param index The index of the parameter value to set.
  * @param value The value of the parameter.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setDouble(int index, double value) throws SQLException;

/*************************************************************************/

/**
  * This method sets the specified parameter from the given Java
  * <code>String</code> value.
  *
  * @param index The index of the parameter value to set.
  * @param value The value of the parameter.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setString(int index, String value) throws SQLException;

/*************************************************************************/

/**
  * This method sets the specified parameter from the given Java
  * <code>byte</code> array value.
  *
  * @param index The index of the parameter value to set.
  * @param value The value of the parameter.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setBytes(int index, byte[] value) throws SQLException;

/*************************************************************************/

/**
  * This method sets the specified parameter from the given Java
  * <code>java.math.BigDecimal</code> value.
  *
  * @param index The index of the parameter value to set.
  * @param value The value of the parameter.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setBigDecimal(int index, java.math.BigDecimal value) throws SQLException;

/*************************************************************************/

/**
  * This method sets the specified parameter from the given Java
  * <code>java.sql.Date</code> value.
  *
  * @param index The index of the parameter value to set.
  * @param value The value of the parameter.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setDate(int index, java.sql.Date value) throws SQLException;

/*************************************************************************/

/**
  * This method sets the specified parameter from the given Java
  * <code>java.sql.Time</code> value.
  *
  * @param index The index of the parameter value to set.
  * @param value The value of the parameter.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setTime(int index, java.sql.Time value) throws SQLException;

/*************************************************************************/

/**
  * This method sets the specified parameter from the given Java
  * <code>java.sql.Timestamp</code> value.
  *
  * @param index The index of the parameter value to set.
  * @param value The value of the parameter.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setTimestamp(int index, java.sql.Timestamp value) throws SQLException;

/*************************************************************************/

/**
  * This method sets the specified parameter from the given Java
  * ASCII <code>InputStream</code> value.
  *
  * @param index The index of the parameter value to set.
  * @param value The value of the parameter.
  * @param length The number of bytes in the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setAsciiStream(int index, InputStream value, int length) throws SQLException;

/*************************************************************************/

/**
  * This method sets the specified parameter from the given Java
  * Unicode UTF-8 <code>InputStream</code> value.
  *
  * @param index The index of the parameter value to set.
  * @param value The value of the parameter.
  * @param length The number of bytes in the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setUnicodeStream(int index, InputStream value, int length) throws SQLException;

/*************************************************************************/

/**
  * This method sets the specified parameter from the given Java
  * binary <code>InputStream</code> value.
  *
  * @param index The index of the parameter value to set.
  * @param value The value of the parameter.
  * @param length The number of bytes in the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setBinaryStream(int index, InputStream value, int length) throws SQLException;

/*************************************************************************/

/**
  * This method sets the specified parameter from the given Java
  * <code>Object</code> value.  The default object type to SQL type mapping
  * will be used.
  *
  * @param index The index of the parameter value to set.
  * @param value The value of the parameter.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setObject(int index, Object value) throws SQLException;

/*************************************************************************/

/**
  * This method sets the specified parameter from the given Java
  * <code>Object</code> value.  The specified SQL object type will be used.
  *
  * @param index The index of the parameter value to set.
  * @param value The value of the parameter.
  * @param type The SQL type to use for the parameter, from <code>Types</code>
  *
  * @exception SQLException If an error occurs.
  *
  * @see Types
  */
public abstract void
setObject(int index, Object value, int type) throws SQLException;

/*************************************************************************/

/**
  * This method sets the specified parameter from the given Java
  * <code>Object</code> value.  The specified SQL object type will be used.
  *
  * @param index The index of the parameter value to set.
  * @param value The value of the parameter.
  * @param type The SQL type to use for the parameter, from <code>Types</code>
  * @param scale The scale of the value, for numeric values only.
  *
  * @exception SQLException If an error occurs.
  *
  * @see Types
  */
public abstract void
setObject(int index, Object value, int type, int scale) throws SQLException;

/*************************************************************************/

/**
  * This method clears all of the input parameter that have been
  * set on this statement.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
clearParameters() throws SQLException;

/*************************************************************************/

/**
  * This method executes a prepared SQL query.
  * Some prepared statements return multiple results; the execute method
  * handles these complex statements as well as the simpler form of
  * statements handled by executeQuery and executeUpdate.
  *
  * @return The result of the SQL statement.
  *
  * @exception SQLException If an error occurs.
  */
public abstract boolean
execute() throws SQLException;

/*************************************************************************/

/**
  * This method executes a prepared SQL query and returns its ResultSet.
  *
  * @return The ResultSet of the SQL statement.
  *
  * @exception SQLException If an error occurs.
  */
public abstract ResultSet
executeQuery() throws SQLException;

/*************************************************************************/

/**
  * This method executes an SQL INSERT, UPDATE or DELETE statement.  SQL
  * statements that return nothing such as SQL DDL statements can be executed.
  *
  * @return The result is either the row count for INSERT, UPDATE or DELETE
  * statements; or 0 for SQL statements that return nothing.
  *
  * @exception SQLException If an error occurs.
  */
public abstract int
executeUpdate() throws SQLException;

} // interface PreparedStatement

