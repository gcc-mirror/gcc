/* ResultSetMetaData.java -- Returns information about the ResultSet
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

/**
 * This interface provides a mechanism for obtaining information about
 * the columns that are present in a <code>ResultSet</code>.
 * 
 * <p> Note that in this class column indices start at 1, not 0.</p>
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public interface ResultSetMetaData 
{
  /**
   * The column does not allow NULL's.
   */
  int columnNoNulls = 0;

  /**
   * The column allows NULL's.
   */
  int columnNullable = 1;

  /**
   * It is unknown whether or not the column allows NULL's.
   */
  int columnNullableUnknown = 2;

  /**
   * This method returns the number of columns in the result set.
   *
   * @return The number of columns in the result set.
   * @exception SQLException If an error occurs.
   */
  int getColumnCount() throws SQLException;

  /**
   * This method test whether or not the column is an auto-increment column.
   * Auto-increment columns are read-only.
   *
   * @param columnIndex The index of the column to test.
   * @return <code>true</code> if the column is auto-increment, <code>false</code>
   *         otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean isAutoIncrement(int columnIndex) throws SQLException;

  /**
   * This method tests whether or not a column is case sensitive in its values.
   *
   * @param columnIndex The index of the column to test.
   * @return <code>true</code> if the column value is case sensitive,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean isCaseSensitive(int columnIndex) throws SQLException;

  /**
   * This method tests whether not the specified column can be used in 
   * a WHERE clause.
   *
   * @param columnIndex The index of the column to test.
   * @return <code>true</code> if the column may be used in a WHERE clause,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean isSearchable(int columnIndex) throws SQLException;

  /**
   * This method tests whether or not the column stores a monetary value.
   *
   * @param columnIndex The index of the column to test.
   * @return <code>true</code> if the column contains a monetary value,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean isCurrency(int columnIndex) throws SQLException;

  /**
   * This method returns a value indicating whether or not the specified
   * column may contain a NULL value.
   *
   * @param columnIndex The index of the column to test.
   * @return A constant indicating whether or not the column can contain NULL,
   *         which will be one of <code>columnNoNulls</code>,
   *         <code>columnNullable</code>, or <code>columnNullableUnknown</code>.
   * @exception SQLException If an error occurs.
   */
  int isNullable(int columnIndex) throws SQLException;

  /**
   * This method tests whether or not the value of the specified column
   * is signed or unsigned.
   *
   * @param columnIndex The index of the column to test.
   * @return <code>true</code> if the column value is signed, <code>false</code>
   *         otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean isSigned(int columnIndex) throws SQLException;

  /**
   * This method returns the maximum number of characters that can be used
   * to display a value in this column.
   *
   * @param columnIndex The index of the column to check.
   * @return The maximum number of characters that can be used to display a
   *         value for this column.
   * @exception SQLException If an error occurs.
   */
  int getColumnDisplaySize(int columnIndex) throws SQLException;

  /**
   * This method returns a string that should be used as a caption for this
   * column for user display purposes.
   *
   * @param columnIndex The index of the column to check.
   * @return A display string for the column.
   * @exception SQLException If an error occurs.
   */
  String getColumnLabel(int columnIndex) throws SQLException;

  /**
   * This method returns the name of the specified column.
   *
   * @param columnIndex The index of the column to return the name of.
   * @return The name of the column.
   * @exception SQLException If an error occurs.
   */
  String getColumnName(int columnIndex) throws SQLException;

  /**
   * This method returns the name of the schema that contains the specified
   * column.
   *
   * @param columnIndex The index of the column to check the schema name for.
   * @return The name of the schema that contains the column.
   * @exception SQLException If an error occurs.
   */
  String getSchemaName(int columnIndex) throws SQLException;

  /**
   * This method returns the precision of the specified column, which is the
   * number of decimal digits it contains.
   *
   * @param columnIndex The index of the column to check the precision on.
   * @return The precision of the specified column.
   * @exception SQLException If an error occurs.
   */
  int getPrecision(int columnIndex) throws SQLException;

  /**
   * This method returns the scale of the specified column, which is the
   * number of digits to the right of the decimal point.
   *
   * @param columnIndex The index column to check the scale of.
   * @return The scale of the column.
   * @exception SQLException If an error occurs.
   */
  int getScale(int columnIndex) throws SQLException;

  /**
   * This method returns the name of the table containing the specified
   * column.
   *
   * @param columnIndex The index of the column to check the table name for.
   * @return The name of the table containing the column.
   * @exception SQLException If an error occurs.
   */
  String getTableName(int columnIndex) throws SQLException;

  /**
   * This method returns the name of the catalog containing the specified
   * column.
   *
   * @param columnIndex The index of the column to check the catalog name for.
   * @return The name of the catalog containing the column.
   * @exception SQLException If an error occurs.
   */
  String getCatalogName(int columnIndex) throws SQLException;

  /**
   * This method returns the SQL type of the specified column.  This will
   * be one of the constants from <code>Types</code>.
   *
   * @param columnIndex The index of the column to check the SQL type of.
   * @return The SQL type for this column.
   * @exception SQLException If an error occurs.
   * @see Types
   */
  int getColumnType(int columnIndex) throws SQLException;

  /**
   * This method returns the name of the SQL type for this column.
   *
   * @param columnIndex The index of the column to check the SQL type name for.
   * @return The name of the SQL type for this column.
   * @exception SQLException If an error occurs.
   */
  String getColumnTypeName(int columnIndex) throws SQLException;

  /**
   * This method tests whether or not the specified column is read only.
   *
   * @param columnIndex The index of the column to check.
   * @return <code>true</code> if the column is read only, <code>false</code>
   *         otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean isReadOnly(int columnIndex) throws SQLException;

  /**
   * This method tests whether or not the column may be writable.  This
   * does not guarantee that a write will be successful.
   *
   * @param columnIndex The index of the column to check for writability.
   * @return <code>true</code> if the column may be writable,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean isWritable(int columnIndex) throws SQLException;

  /**
   * This method tests whether or not the column is writable.  This
   * does guarantee that a write will be successful.
   *
   * @param columnIndex The index of the column to check for writability.
   * @return <code>true</code> if the column is writable,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean isDefinitelyWritable(int columnIndex) throws SQLException;

  /**
   * This method returns the name of the Java class which will be used to
   * create objects representing the data in this column.
   *
   * @param columnIndex The index of the column to check.
   * @return The name of the Java class that will be used for values in
   *         this column.
   * @exception SQLException If an error occurs.
   */
  String getColumnClassName(int columnIndex) throws SQLException;
}
