/* ResultSet.java -- A SQL statement result set.
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
import java.util.Calendar;
import java.util.Map;

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
  * The rows will be processed in order from first to last.
  */
public static final int FETCH_FORWARD = 0;

/**
  * The rows will be processed in order from last to first.
  */
public static final int FETCH_REVERSE = 1;

/**
  * The rows will be processed in an unknown order
  */
public static final int FETCH_UNKNOWN = 2;

/**
  * This type of result set may only step forward through the rows returned.
  */
public static final int TYPE_FORWARD_ONLY = 0;

/**
  * This type of result set is scrollable and is not sensitive to changes
  * made by other statements.
  */
public static final int TYPE_SCROLL_INSENSITIVE = 1;

/**
  * This type of result set is scrollable and is also sensitive to changes
  * made by other statements.
  */
public static final int TYPE_SCROLL_SENSITIVE = 1;

/**
  * The concurrency mode of for the result set may not be modified.
  */
public static final int CONCUR_READ_ONLY = 0;

/**
  * The concurrency mode of for the result set may be modified.
  */
public static final int CONCUR_UPDATABLE = 1;

/*************************************************************************/

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
  * This method moves the current position to the previous row in the
  * result set.
  *
  * @return <code>true</code> if the previous row exists, <code>false</code>
  * otherwise.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract boolean
previous() throws SQLException;

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
  *
  * @return The column value as a <code>BigDecimal</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract BigDecimal
getBigDecimal(int index) throws SQLException;

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
  * This method returns the value of the specified column as a character
  * stream.  Note that all the data from this stream must be read before
  * fetching the value of any other column.  Please also be aware that 
  * calling <code>next()</code> or <code>close()</code> on this result set
  * will close this stream as well.
  *
  * @param index The index of the column to return.
  *
  * @return The column value as an character <code>Reader</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract Reader
getCharacterStream(int index) throws SQLException;

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
  *
  * @return The column value as a <code>BigDecimal</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract BigDecimal
getBigDecimal(String column) throws SQLException;

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
  * This method returns the value of the specified column as a character
  * stream.  Note that all the data from this stream must be read before
  * fetching the value of any other column.  Please also be aware that 
  * calling <code>next()</code> or <code>close()</code> on this result set
  * will close this stream as well.
  *
  * @param column The name of the column to return.
  *
  * @return The column value as an character <code>Reader</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract Reader
getCharacterStream(String column) throws SQLException;

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

/*************************************************************************/

/**
  * This method tests whether or not the cursor is before the first row
  * in the result set.
  *
  * @return <code>true</code> if the cursor is positioned before the first
  * row, <code>false</code> otherwise.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract boolean
isBeforeFirst() throws SQLException;

/*************************************************************************/

/**
  * This method tests whether or not the cursor is after the last row
  * in the result set.
  *
  * @return <code>true</code> if the cursor is positioned after the last
  * row, <code>false</code> otherwise.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract boolean
isAfterLast() throws SQLException;

/*************************************************************************/

/**
  * This method tests whether or not the cursor is positioned on the first
  * row in the result set.
  *
  * @return <code>true</code> if the cursor is positioned on the first
  * row, <code>false</code> otherwise.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract boolean
isFirst() throws SQLException;

/*************************************************************************/

/**
  * This method tests whether or not the cursor is on the last row
  * in the result set.
  *
  * @return <code>true</code> if the cursor is positioned on the last
  * row, <code>false</code> otherwise.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract boolean
isLast() throws SQLException;

/*************************************************************************/

/**
  * This method repositions the cursor to before the first row in the
  * result set.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract void
beforeFirst() throws SQLException;

/*************************************************************************/

/**
  * This method repositions the cursor to after the last row in the result
  * set.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract void
afterLast() throws SQLException;

/*************************************************************************/

/**
  * This method repositions the cursor on the first row in the
  * result set.
  *
  * @return <code>true</code> if the cursor is on a valid row;
  * <code>false</code> if there are no rows in the result set.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract boolean
first() throws SQLException;

/*************************************************************************/

/**
  * This method repositions the cursor on the last row in the result
  * set.
  * 
  * @return <code>true</code> if the cursor is on a valid row;
  * <code>false</code> if there are no rows in the result set.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract boolean
last() throws SQLException;

/*************************************************************************/

/**
  * This method returns the current row number in the cursor.  Numbering
  * begins at index 1.
  *
  * @return The current row number, or 0 if there is not current row.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract int
getRow() throws SQLException;

/*************************************************************************/

/**
  * This method positions the result set to the specified absolute row.
  * Positive numbers are row offsets from the beginning of the result
  * set (numbering starts from row 1) and negative numbers are row offsets
  * from the end of the result set (numbering starts from -1).
  *
  * @param row The row to position the result set to.
  *
  * @return <code>true</code> if the current position was changed,
  * <code>false</code> otherwise.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract boolean
absolute(int row) throws SQLException;

/*************************************************************************/

/**
  * This method moves the result set position relative to the current row.
  * The offset can be positive or negative.
  *
  * @param row The relative row position to move to.
  *
  * @return <code>true</code> if the current position was changed,
  * <code>false</code> otherwise.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract boolean
relative(int row) throws SQLException;

/*************************************************************************/

/**
  * This method provides a hint to the driver about which direction the
  * result set will be processed in. 
  *
  * @param direction The direction in which rows will be processed. (Values?)
  * 
  * @exception SQLException If an error occurs.
  */
public abstract void
setFetchDirection(int direction) throws SQLException;
 
/*************************************************************************/

/**
  * This method returns the current fetch direction for this result set.
  *
  * @return The fetch direction for this result set.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract int
getFetchDirection() throws SQLException;

/*************************************************************************/

/**
  * This method provides a hint to the driver about how many rows at a
  * time it should fetch from the database.
  *
  * @param rows The number of rows the driver should fetch per call.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract void
setFetchSize(int rows) throws SQLException;

/*************************************************************************/

/**
  * This method returns the current number of rows that will be fetched 
  * from the database at a time.
  *
  * @return The current fetch size for this result set.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract int
getFetchSize() throws SQLException;

/*************************************************************************/

/**
  * This method returns the result set type of this result set.  This will
  * be one of the TYPE_* constants defined in this interface.
  *
  * @return The result set type.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract int
getType() throws SQLException;

/*************************************************************************/

/**
  * This method returns the concurrency type of this result set.  This will
  * be one of the CONCUR_* constants defined in this interface.
  *
  * @return The result set concurrency type.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract int
getConcurrency() throws SQLException;

/*************************************************************************/

/**
  * This method tests whether or not the current row in the result set
  * has been updated.  Updates must be visible in order of this method to
  * detect the update.
  *
  * @return <code>true</code> if the row has been updated, <code>false</code>
  * otherwise.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract boolean
rowUpdated() throws SQLException;

/*************************************************************************/

/**
  * This method tests whether or not the current row in the result set
  * has been inserted.  Inserts must be visible in order of this method to
  * detect the insert.
  *
  * @return <code>true</code> if the row has been inserted, <code>false</code>
  * otherwise.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract boolean
rowInserted() throws SQLException;

/*************************************************************************/

/**
  * This method tests whether or not the current row in the result set
  * has been deleted.  Deletes must be visible in order of this method to
  * detect the deletion.
  *
  * @return <code>true</code> if the row has been deleted, <code>false</code>
  * otherwise.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract boolean
rowDeleted() throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a NULL value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @return index The index of the column to update.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateNull(int index) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a boolean value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param index The index of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateBoolean(int index, boolean value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a byte value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param index The index of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateByte(int index, byte value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a short value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param index The index of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateShort(int index, short value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have an int value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param index The index of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateInt(int index, int value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a long value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param index The index of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateLong(int index, long value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a float value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param index The index of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateFloat(int index, float value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a double value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param index The index of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateDouble(int index, double value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a BigDecimal value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param index The index of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateBigDecimal(int index, BigDecimal value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a String value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param index The index of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateString(int index, String value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a byte array value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param index The index of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateBytes(int index, byte[] value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a java.sql.Date value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param index The index of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateDate(int index, java.sql.Date value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a java.sql.Time value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param index The index of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateTime(int index, java.sql.Time value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a java.sql.Timestamp value.  
  * This does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param index The index of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateTimestamp(int index, java.sql.Timestamp value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column from an ASCII text stream.
  * This does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param index The index of the column to update.
  * @param value The new value of the column.
  * @param length The length of the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateAsciiStream(int index, InputStream value, int length) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column from a binary stream.
  * This does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param index The index of the column to update.
  * @param value The new value of the column.
  * @param length The length of the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateBinaryStream(int index, InputStream value, int length) 
                   throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column from a character stream.
  * This does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param index The index of the column to update.
  * @param value The new value of the column.
  * @param length The length of the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateCharacterStream(int index, Reader value, int length) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have an Object value.  
  * This does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param index The index of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateObject(int index, Object value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have an Object value.  
  * This does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param index The index of the column to update.
  * @param value The new value of the column.
  * @param scale The scale of the object in question, which is used only
  * for numeric type objects.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateObject(int index, Object value, int scale) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a NULL value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @return name The name of the column to update.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateNull(String name) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a boolean value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param name The name of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateBoolean(String name, boolean value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a byte value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param name The name of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateByte(String name, byte value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a short value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param name The name of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateShort(String name, short value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have an int value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param name The name of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateInt(String name, int value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a long value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param name The name of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateLong(String name, long value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a float value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param name The name of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateFloat(String name, float value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a double value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param name The name of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateDouble(String name, double value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a BigDecimal value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param name The name of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateBigDecimal(String name, BigDecimal value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a String value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param name The name of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateString(String name, String value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a byte array value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param name The name of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateBytes(String name, byte[] value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a java.sql.Date value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param name The name of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateDate(String name, java.sql.Date value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a java.sql.Time value.  This
  * does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param name The name of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateTime(String name, java.sql.Time value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have a java.sql.Timestamp value.  
  * This does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param name The name of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateTimestamp(String name, java.sql.Timestamp value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column from an ASCII text stream.
  * This does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param name The name of the column to update.
  * @param value The new value of the column.
  * @param length The length of the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateAsciiStream(String name, InputStream value, int length) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column from a binary stream.
  * This does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param name The name of the column to update.
  * @param value The new value of the column.
  * @param length The length of the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateBinaryStream(String name, InputStream value, int length) 
                   throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column from a character stream.
  * This does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param name The name of the column to update.
  * @param value The new value of the column.
  * @param length The length of the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateCharacterStream(String name, Reader value, int length) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have an Object value.  
  * This does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param name The name of the column to update.
  * @param value The new value of the column.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateObject(String name, Object value) throws SQLException;

/*************************************************************************/

/**
  * This method updates the specified column to have an Object value.  
  * This does not update the actual database.  <code>updateRow</code> must be
  * called in order to do that.
  *
  * @param name The name of the column to update.
  * @param value The new value of the column.
  * @param scale The scale of the object in question, which is used only
  * for numeric type objects.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateObject(String name, Object value, int scale) throws SQLException;

/*************************************************************************/

/**
  * This method inserts the current row into the database.  The result set
  * must be positioned on the insert row in order to call this method
  * successfully.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
insertRow() throws SQLException;

/*************************************************************************/

/**
  * This method updates the current row in the database.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
updateRow() throws SQLException;

/*************************************************************************/

/**
  * This method deletes the current row in the database.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
deleteRow() throws SQLException;

/*************************************************************************/

/**
  * This method refreshes the contents of the current row from the database.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
refreshRow() throws SQLException;

/*************************************************************************/

/**
  * This method cancels any changes that have been made to a row.  If 
  * the <code>rowUpdate</code> method has been called, then the changes
  * cannot be undone.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
cancelRowUpdates() throws SQLException;

/*************************************************************************/

/**
  * This method positions the result set to the "insert row", which allows
  * a new row to be inserted into the database from the result set.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
moveToInsertRow() throws SQLException;

/*************************************************************************/

/**
  * This method moves the result set position from the insert row back to
  * the current row that was selected prior to moving to the insert row.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
moveToCurrentRow() throws SQLException;

/*************************************************************************/

/**
  * This method returns a the <code>Statement</code> that was used to
  * produce this result set.
  *
  * @return The <code>Statement</code> used to produce this result set.
  *
  * @exception SQLException If an error occurs.
  */
public abstract Statement
getStatement() throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>Object</code> using the specified SQL type to Java type map.
  *
  * @param index The index of the column to return.
  * @param map The SQL type to Java type map to use.
  *
  * @return The value of the column as an <code>Object</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract Object
getObject(int index, Map map) throws SQLException;

/*************************************************************************/

/**
  * This method returns a <code>Ref</code> for the specified column which
  * represents the structured type for the column.
  *
  * @param index  The index of the column to return.
  *
  * @return A <code>Ref</code> object for the column
  *
  * @exception SQLException If an error occurs.
  */
public Ref
getRef(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the specified column value as a BLOB.
  *
  * @param index The index of the column value to return.
  *
  * @return The value of the column as a BLOB.
  *
  * @exception SQLException If an error occurs.
  */
public abstract Blob
getBlob(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the specified column value as a CLOB.
  *
  * @param index The index of the column value to return.
  *
  * @return The value of the column as a CLOB.
  *
  * @exception SQLException If an error occurs.
  */
public abstract Clob
getClob(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the specified column value as an <code>Array</code>.
  *
  * @param index The index of the column value to return.
  *
  * @return The value of the column as an <code>Array</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract Array
getArray(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified column as a Java
  * <code>Object</code> using the specified SQL type to Java type map.
  *
  * @param name The name of the column to return.
  * @param map The SQL type to Java type map to use.
  *
  * @return The value of the column as an <code>Object</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract Object
getObject(String name, Map map) throws SQLException;

/*************************************************************************/

/**
  * This method returns a <code>Ref</code> for the specified column which
  * represents the structured type for the column.
  *
  * @param index  The index of the column to return.
  *
  * @return A <code>Ref</code> object for the column
  *
  * @exception SQLException If an error occurs.
  */
public Ref
getRef(String name) throws SQLException;

/*************************************************************************/

/**
  * This method returns the specified column value as a BLOB.
  *
  * @param name The name of the column value to return.
  *
  * @return The value of the column as a BLOB.
  *
  * @exception SQLException If an error occurs.
  */
public abstract Blob
getBlob(String name) throws SQLException;

/*************************************************************************/

/**
  * This method returns the specified column value as a CLOB.
  *
  * @param name The name of the column value to return.
  *
  * @return The value of the column as a CLOB.
  *
  * @exception SQLException If an error occurs.
  */
public abstract Clob
getClob(String name) throws SQLException;

/*************************************************************************/

/**
  * This method returns the specified column value as an <code>Array</code>.
  *
  * @param name The name of the column value to return.
  *
  * @return The value of the column as an <code>Array</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract Array
getArray(String name) throws SQLException;

/*************************************************************************/

/**
  * This method returns the specified column value as a 
  * <code>java.sql.Date</code>.  The specified <code>Calendar</code> is used
  * to generate a value for the date if the database does not support
  * timezones.
  *
  * @param index The index of the column value to return.
  * @param cal The <code>Calendar</code> to use for calculating timezones.
  *
  * @return The value of the column as a <code>java.sql.Date</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract java.sql.Date
getDate(int index, Calendar cal) throws SQLException;

/*************************************************************************/

/**
  * This method returns the specified column value as a 
  * <code>java.sql.Time</code>.  The specified <code>Calendar</code> is used
  * to generate a value for the time if the database does not support
  * timezones.
  *
  * @param index The index of the column value to return.
  * @param cal The <code>Calendar</code> to use for calculating timezones.
  *
  * @return The value of the column as a <code>java.sql.Time</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract java.sql.Time
getTime(int index, Calendar cal) throws SQLException;

/*************************************************************************/

/**
  * This method returns the specified column value as a 
  * <code>java.sql.Timestamp</code>.  The specified <code>Calendar</code> is used
  * to generate a value for the timestamp if the database does not support
  * timezones.
  *
  * @param index The index of the column value to return.
  * @param cal The <code>Calendar</code> to use for calculating timezones.
  *
  * @return The value of the column as a <code>java.sql.Timestamp</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract java.sql.Timestamp
getTimestamp(int index, Calendar cal) throws SQLException;

/*************************************************************************/

/**
  * This method returns the specified column value as a 
  * <code>java.sql.Date</code>.  The specified <code>Calendar</code> is used
  * to generate a value for the date if the database does not support
  * timezones.
  *
  * @param name The name of the column value to return.
  * @param cal The <code>Calendar</code> to use for calculating timezones.
  *
  * @return The value of the column as a <code>java.sql.Date</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract java.sql.Date
getDate(String name, Calendar cal) throws SQLException;

/*************************************************************************/

/**
  * This method returns the specified column value as a 
  * <code>java.sql.Time</code>.  The specified <code>Calendar</code> is used
  * to generate a value for the time if the database does not support
  * timezones.
  *
  * @param name The name of the column value to return.
  * @param cal The <code>Calendar</code> to use for calculating timezones.
  *
  * @return The value of the column as a <code>java.sql.Time</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract java.sql.Time
getTime(String name, Calendar cal) throws SQLException;

/*************************************************************************/

/**
  * This method returns the specified column value as a 
  * <code>java.sql.Timestamp</code>.  The specified <code>Calendar</code> is used
  * to generate a value for the timestamp if the database does not support
  * timezones.
  *
  * @param name The name of the column value to return.
  * @param cal The <code>Calendar</code> to use for calculating timezones.
  *
  * @return The value of the column as a <code>java.sql.Timestamp</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract java.sql.Timestamp
getTimestamp(String name, Calendar cal) throws SQLException;

} // interface ResultSet

