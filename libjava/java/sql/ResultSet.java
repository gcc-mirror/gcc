/* ResultSet.java -- A SQL statement result set.
   Copyright (C) 1999 Free Software Foundation, Inc.

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
  * This interface provides access to the data set returned by a SQL
  * statement.  An instance of this interface is returned by the various
  * execution methods in the <code>Statement</code.
  * <p>
  * This class models a cursor, which can be stepped through one row at a
  * time.  Methods are provided for accessing columns by column name or by
  * index.
  * <p>
  * Note that a result set is invalidated if the statement that returned
  * it is closed.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public interface ResultSet
{

/**
  * This method advances to the next row in the result set.  Any streams
  * open on the current row are closed automatically.
  *
  * @return <code>true</code> if the next row exists, <code>false</code>
  * otherwise.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract boolean 
next() throws SQLException;

/*************************************************************************/

/**
  * This method closes the result set and frees any associated resources.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract void
close() throws SQLException;

/*************************************************************************/

/**
  * This method tests whether the value of the last column that was fetched
  * was actually a SQL NULL value.
  *
  * @return <code>true</code> if the last column fetched was a NULL,
  * <code>false</code> otherwise.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract boolean
wasNull() throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>String</code>.
  *
  * @param index The index of the column to return.
  *
  * @return The column value as a <code>String</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract String
getString(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>Object</code>.
  *
  * @param index The index of the column to return.
  *
  * @return The column value as an <code>Object</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract Object
getObject(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>boolean</code>.
  *
  * @param index The index of the column to return.
  *
  * @return The column value as a <code>boolean</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract boolean
getBoolean(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>byte</code>.
  *
  * @param index The index of the column to return.
  *
  * @return The column value as a <code>byte</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract byte
getByte(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>short</code>.
  *
  * @param index The index of the column to return.
  *
  * @return The column value as a <code>short</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract short
getShort(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>int</code>.
  *
  * @param index The index of the column to return.
  *
  * @return The column value as a <code>int</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract int
getInt(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>long</code>.
  *
  * @param index The index of the column to return.
  *
  * @return The column value as a <code>long</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract long
getLong(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>float</code>.
  *
  * @param index The index of the column to return.
  *
  * @return The column value as a <code>float</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract float
getFloat(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>double</code>.
  *
  * @param index The index of the column to return.
  *
  * @return The column value as a <code>double</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract double
getDouble(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>BigDecimal</code>.
  *
  * @param index The index of the column to return.
  * @param scale The number of digits to the right of the decimal to return.
  *
  * @return The column value as a <code>BigDecimal</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract BigDecimal
getBigDecimal(int index, int scale) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * byte array.
  *
  * @param index The index of the column to return.
  *
  * @return The column value as a byte array
  *
  * @exception SQLException If an error occurs.
  */
public abstract byte[]
getBytes(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>java.sql.Date</code>.
  *
  * @param index The index of the column to return.
  *
  * @return The column value as a <code>java.sql.Date</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract java.sql.Date
getDate(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>java.sql.Time</code>.
  *
  * @param index The index of the column to return.
  *
  * @return The column value as a <code>java.sql.Time</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract java.sql.Time
getTime(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>java.sql.Timestamp</code>.
  *
  * @param index The index of the column to return.
  *
  * @return The column value as a <code>java.sql.Timestamp</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract java.sql.Timestamp
getTimestamp(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as an ASCII 
  * stream.  Note that all the data from this stream must be read before
  * fetching the value of any other column.  Please also be aware that 
  * calling <code>next()</code> or <code>close()</code> on this result set
  * will close this stream as well.
  *
  * @param index The index of the column to return.
  *
  * @return The column value as an ASCII <code>InputStream</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract InputStream
getAsciiStream(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Unicode UTF-8
  * stream.  Note that all the data from this stream must be read before
  * fetching the value of any other column.  Please also be aware that 
  * calling <code>next()</code> or <code>close()</code> on this result set
  * will close this stream as well.
  *
  * @param index The index of the column to return.
  *
  * @return The column value as a Unicode UTF-8 <code>InputStream</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract InputStream
getUnicodeStream(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a raw byte
  * stream.  Note that all the data from this stream must be read before
  * fetching the value of any other column.  Please also be aware that 
  * calling <code>next()</code> or <code>close()</code> on this result set
  * will close this stream as well.
  *
  * @param index The index of the column to return.
  *
  * @return The column value as a raw byte <code>InputStream</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract InputStream
getBinaryStream(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>String</code>.
  *
  * @param column The name of the column to return.
  *
  * @return The column value as a <code>String</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract String
getString(String column) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>Object</code>.
  *
  * @param column The name of the column to return.
  *
  * @return The column value as an <code>Object</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract Object
getObject(String column) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>boolean</code>.
  *
  * @param column The name of the column to return.
  *
  * @return The column value as a <code>boolean</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract boolean
getBoolean(String column) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>byte</code>.
  *
  * @param column The name of the column to return.
  *
  * @return The column value as a <code>byte</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract byte
getByte(String column) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>short</code>.
  *
  * @param column The name of the column to return.
  *
  * @return The column value as a <code>short</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract short
getShort(String column) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>int</code>.
  *
  * @param column The name of the column to return.
  *
  * @return The column value as a <code>int</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract int
getInt(String column) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>long</code>.
  *
  * @param column The name of the column to return.
  *
  * @return The column value as a <code>long</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract long
getLong(String column) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>float</code>.
  *
  * @param column The name of the column to return.
  *
  * @return The column value as a <code>float</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract float
getFloat(String column) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>double</code>.
  *
  * @param column The name of the column to return.
  *
  * @return The column value as a <code>double</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract double
getDouble(String column) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>BigDecimal</code>.
  *
  * @param column The name of the column to return.
  * @param scale The number of digits to the right of the decimal to return.
  *
  * @return The column value as a <code>BigDecimal</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract BigDecimal
getBigDecimal(String column, int scale) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * byte array.
  *
  * @param column The name of the column to return.
  *
  * @return The column value as a byte array
  *
  * @exception SQLException If an error occurs.
  */
public abstract byte[]
getBytes(String column) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>java.sql.Date</code>.
  *
  * @param column The name of the column to return.
  *
  * @return The column value as a <code>java.sql.Date</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract java.sql.Date
getDate(String column) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>java.sql.Time</code>.
  *
  * @param column The name of the column to return.
  *
  * @return The column value as a <code>java.sql.Time</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract java.sql.Time
getTime(String column) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>java.sql.Timestamp</code>.
  *
  * @param column The name of the column to return.
  *
  * @return The column value as a <code>java.sql.Timestamp</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract java.sql.Timestamp
getTimestamp(String column) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as an ASCII 
  * stream.  Note that all the data from this stream must be read before
  * fetching the value of any other column.  Please also be aware that 
  * calling <code>next()</code> or <code>close()</code> on this result set
  * will close this stream as well.
  *
  * @param column The name of the column to return.
  *
  * @return The column value as an ASCII <code>InputStream</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract InputStream
getAsciiStream(String column) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Unicode UTF-8
  * stream.  Note that all the data from this stream must be read before
  * fetching the value of any other column.  Please also be aware that 
  * calling <code>next()</code> or <code>close()</code> on this result set
  * will close this stream as well.
  *
  * @param column The name of the column to return.
  *
  * @return The column value as a Unicode UTF-8 <code>InputStream</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract InputStream
getUnicodeStream(String column) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a raw byte
  * stream.  Note that all the data from this stream must be read before
  * fetching the value of any other column.  Please also be aware that 
  * calling <code>next()</code> or <code>close()</code> on this result set
  * will close this stream as well.
  *
  * @param column The name of the column to return.
  *
  * @return The column value as a raw byte <code>InputStream</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract InputStream
getBinaryStream(String column) throws SQLException;

/*************************************************************************/

/**
  * This method returns the first SQL warning associated with this result
  * set.  Any additional warnings will be chained to this one.
  *
  * @return The first SQLWarning for this result set, or <code>null</code> if
  * there are no warnings.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract SQLWarning 
getWarnings() throws SQLException;

/*************************************************************************/

/**
  * This method clears all warnings associated with this result set.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
clearWarnings() throws SQLException;

/*************************************************************************/

/**
  * This method returns the name of the database cursor used by this
  * result set.
  *
  * @return The name of the database cursor used by this result set.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract String
getCursorName() throws SQLException;

/*************************************************************************/

/**
  * This method returns data about the columns returned as part of the
  * result set as a <code>ResultSetMetaData</code> instance.
  *
  * @return The <code>ResultSetMetaData</code> instance for this result set.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract ResultSetMetaData
getMetaData() throws SQLException;

/*************************************************************************/

/**
  * This method returns the column index of the specified named column.
  *
  * @param column The name of the column.
  *
  * @return The index of the column.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract int
findColumn(String column) throws SQLException;

} // interface ResultSet

