/* Statement.java -- Interface for executing SQL statements.
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

/**
 * This interface provides a mechanism for executing SQL statements.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public interface Statement 
{
  int CLOSE_CURRENT_RESULT = 1;
  int KEEP_CURRENT_RESULT = 2;
  int CLOSE_ALL_RESULTS = 3;
  int SUCCESS_NO_INFO = -2;
  int EXECUTE_FAILED = -3;
  int RETURN_GENERATED_KEYS = 1;
  int NO_GENERATED_KEYS = 2;

  /**
   * This method executes the specified SQL SELECT statement and returns a
   * (possibly empty) <code>ResultSet</code> with the results of the query.
   *
   * @param sql The SQL statement to execute.
   * @return The result set of the SQL statement.
   * @exception SQLException If an error occurs.
   */
  ResultSet executeQuery(String sql) throws SQLException;

  /**
   * This method executes the specified SQL INSERT, UPDATE, or DELETE statement
   * and returns the number of rows affected, which may be 0.
   * 
   * @param sql The SQL statement to execute.
   * @return The number of rows affected by the SQL statement.
   * @exception SQLException If an error occurs.
   */
  int executeUpdate(String sql) throws SQLException;

  /**
   * This method closes the statement and frees any associated resources.
   *
   * @exception SQLException If an error occurs.
   */
  void close() throws SQLException;

  /**
   * This method returns the maximum length of any column value in bytes.
   *
   * @return The maximum length of any column value in bytes.
   * @exception SQLException If an error occurs.
   */
  int getMaxFieldSize() throws SQLException;

  /**
   * This method sets the limit for the maximum length of any column in bytes.
   *
   * @param maxsize The new maximum length of any column in bytes.
   * @exception SQLException If an error occurs.
   */
  void setMaxFieldSize(int max) throws SQLException;

  /**
   * This method returns the maximum possible number of rows in a result set.
   *
   * @return The maximum possible number of rows in a result set.
   * @exception SQLException If an error occurs.
   */
  int getMaxRows() throws SQLException;

  /**
   * This method sets the maximum number of rows that can be present in a
   * result set.
   *
   * @param maxrows The maximum possible number of rows in a result set.
   * @exception SQLException If an error occurs.
   */
  void setMaxRows(int max) throws SQLException;

  /**
   * This method sets the local escape processing mode on or off.  The
   * default value is on.
   *
   * @param escape <code>true</code> to enable local escape processing, 
   *        <code>false</code> to disable it.
   * @exception SQLException If an error occurs.
   */
  void setEscapeProcessing(boolean enable) throws SQLException;

  /**
   * The method returns the number of seconds a statement may be in process
   * before timing out.  A value of 0 means there is no timeout.
   *
   * @return The SQL statement timeout in seconds.
   * @exception SQLException If an error occurs.
   */
  int getQueryTimeout() throws SQLException;

  /**
   * This method sets the number of seconds a statement may be in process
   * before timing out.  A value of 0 means there is no timeout.
   *
   * @param timeout The new SQL statement timeout value.
   * @exception SQLException If an error occurs.
   */
  void setQueryTimeout(int seconds) throws SQLException;

  /**
   * This method cancels an outstanding statement, if the database supports
   * that operation.
   *
   * @exception SQLException If an error occurs.
   */
  void cancel() throws SQLException;

  /**
   * This method returns the first SQL warning attached to this statement.
   * Subsequent warnings will be chained to this one.
   *
   * @return The first SQL warning for this statement.
   * @exception SQLException If an error occurs.
   */
  SQLWarning getWarnings() throws SQLException;

  /**
   * This method clears any SQL warnings that have been attached to this
   * statement.
   *
   * @exception SQLException If an error occurs.
   */
  void clearWarnings() throws SQLException;

  /**
   * This method sets the cursor name that will be used by the result set.
   *
   * @param name The cursor name to use for this statement.
   * @exception SQLException If an error occurs.
   */
  void setCursorName(String name) throws SQLException;

  /**
   * This method executes an arbitrary SQL statement of any time.  The
   * methods <code>getResultSet</code>, <code>getMoreResults</code> and
   * <code>getUpdateCount</code> retrieve the results.
   *
   * @return <code>true</code> if a result set was returned, <code>false</code>
   *         if an update count was returned.
   * @exception SQLException If an error occurs.
   */
  boolean execute(String sql) throws SQLException;

  /**
   * This method returns the result set of the SQL statement that was
   * executed.  This should be called only once per result set returned.
   *
   * @return The result set of the query, or <code>null</code> if there was
   *         no result set (for example, if the statement was an UPDATE).
   * @exception SQLException If an error occurs.
   * @see execute
   */
  ResultSet getResultSet() throws SQLException;

  /**
   * This method returns the update count of the SQL statement that was
   * executed.  This should be called only once per executed SQL statement.
   *
   * @return The update count of the query, or -1 if there was no update
   *         count (for example, if the statement was a SELECT).
   * @exception SQLException If an error occurs.
   * @see execute
   */
  int getUpdateCount() throws SQLException;

  /**
   * This method advances the result set pointer to the next result set, 
   * which can then be retrieved using <code>getResultSet</code>
   *
   * @return <code>true</code> if there is another result set, 
   *         <code>false</code> otherwise (for example, the next result is an
   *         update count).
   * @exception SQLException If an error occurs.
   * @see execute
   */
  boolean getMoreResults() throws SQLException;

  /**
   * This method informs the driver which direction the result set will
   * be accessed in.
   *
   * @param direction The direction the result set will be accessed in (?????)
   * @exception SQLException If an error occurs.
   */
  void setFetchDirection(int direction) throws SQLException;

  /**
   * This method returns the current direction that the driver thinks the
   * result set will be accessed int.
   *
   * @return The direction the result set will be accessed in (????)
   * @exception SQLException If an error occurs.
   */
  int getFetchDirection() throws SQLException;

  /**
   * This method informs the driver how many rows it should fetch from the
   * database at a time.
   *
   * @param numrows The number of rows the driver should fetch at a time
   *        to populate the result set.
   * @exception SQLException If an error occurs.
   */
  void setFetchSize(int rows) throws SQLException;

  /**
   * This method returns the number of rows the driver believes should be
   * fetched from the database at a time.
   *
   * @return The number of rows that will be fetched from the database at a time.
   * @exception SQLException If an error occurs.
   */
  int getFetchSize() throws SQLException;

  /**
   * This method returns the concurrency type of the result set for this
   * statement. This will be one of the concurrency types defined in
   * <code>ResultSet</code>.
   *
   * @return The concurrency type of the result set for this statement.
   * @exception SQLException If an error occurs.
   * @see ResultSet
   */
  int getResultSetConcurrency() throws SQLException;

  /**
   * This method returns the result set type for this statement.  This will
   * be one of the result set types defined in <code>ResultSet</code>.
   *
   * @return The result set type for this statement.
   * @exception SQLException If an error occurs.
   * @see ResultSet
   */
  int getResultSetType() throws SQLException;

  /**
   * This method adds a SQL statement to a SQL batch.  A driver is not
   * required to implement this method.
   *
   * @param sql The sql statement to add to the batch.
   * @exception SQLException If an error occurs.
   */
  void addBatch(String sql) throws SQLException;

  /**
   * This method clears out any SQL statements that have been populated in
   * the current batch.  A driver is not required to implement this method.
   *
   * @exception SQLException If an error occurs.
   */
  void clearBatch() throws SQLException;

  /**
   * This method executes the SQL batch and returns an array of update
   * counts - one for each SQL statement in the batch - ordered in the same
   * order the statements were added to the batch.  A driver is not required
   * to implement this method.
   *
   * @return An array of update counts for this batch.
   * @exception SQLException If an error occurs.
   */
  int[] executeBatch() throws SQLException;

  /**
   * This method returns the <code>Connection</code> instance that was
   * used to create this object.
   *
   * @return The connection used to create this object.
   * @exception SQLException If an error occurs.
   */
  Connection getConnection() throws SQLException;

  /**
   * @since 1.4
   */
  boolean getMoreResults(int current) throws SQLException;

  /**
   * @since 1.4
   */
  ResultSet getGeneratedKeys() throws SQLException;

  /**
   * @since 1.4
   */
  int executeUpdate(String sql, int autoGeneratedKeys)
    throws SQLException;

  /**
   * @since 1.4
   */
  int executeUpdate(String sql, int[] columnIndexes)
    throws SQLException;

  /**
   * @since 1.4
   */
  int executeUpdate(String sql, String[] columnNames)
    throws SQLException;

  /**
   * @since 1.4
   */
  boolean execute(String sql, int autoGeneratedKeys)
    throws SQLException;

  /**
   * @since 1.4
   */
  boolean execute(String sql, int[] columnIndexes) throws SQLException;

  /**
   * @since 1.4
   */
  boolean execute(String sql, String[] columnNames)
    throws SQLException;

  /**
   * @since 1.4
   */
  int getResultSetHoldability() throws SQLException;
}
