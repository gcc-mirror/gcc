/* Statement.java -- Interface for executing SQL statements.
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

/**
  * This interface provides a mechanism for executing SQL statements.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public interface Statement
{

/**
  * This method executes the specified SQL SELECT statement and returns a
  * (possibly empty) <code>ResultSet</code> with the results of the query.
  *
  * @param sql The SQL statement to execute.
  *
  * @return The result set of the SQL statement.
  *
  * @exception SQLException If an error occurs.
  */
public abstract ResultSet
executeQuery(String sql) throws SQLException;

/*************************************************************************/

/**
  * This method executes the specified SQL INSERT, UPDATE, or DELETE statement
  * and returns the number of rows affected, which may be 0.
  * 
  * @param sql The SQL statement to execute.
  *
  * @return The number of rows affected by the SQL statement.
  *
  * @exception SQLException If an error occurs.
  */
public abstract int
executeUpdate(String sql) throws SQLException;

/*************************************************************************/

/**
  * This method closes the statement and frees any associated resources.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
close() throws SQLException;

/*************************************************************************/

/**
  * This method returns the maximum length of any column value in bytes.
  *
  * @return The maximum length of any column value in bytes.
  *
  * @exception SQLException If an error occurs.
  */
public abstract int
getMaxFieldSize() throws SQLException;

/*************************************************************************/

/**
  * This method sets the limit for the maximum length of any column in bytes.
  *
  * @param maxsize The new maximum length of any column in bytes.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setMaxFieldSize(int maxsize) throws SQLException;

/*************************************************************************/

/**
  * This method returns the maximum possible number of rows in a result set.
  *
  * @return The maximum possible number of rows in a result set.
  *
  * @exception SQLException If an error occurs.
  */
public abstract int
getMaxRows() throws SQLException;

/*************************************************************************/

/**
  * This method sets the maximum number of rows that can be present in a
  * result set.
  *
  * @param maxrows The maximum possible number of rows in a result set.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setMaxRows(int maxrows) throws SQLException;

/*************************************************************************/

/**
  * This method sets the local escape processing mode on or off.  The
  * default value is on.
  *
  * @param escape <code>true</code> to enable local escape processing, 
  * <code>false</code> to disable it.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setEscapeProcessing(boolean esacpe) throws SQLException;

/*************************************************************************/

/**
  * The method returns the number of seconds a statement may be in process
  * before timing out.  A value of 0 means there is no timeout.
  *
  * @return The SQL statement timeout in seconds.
  *
  * @exception SQLException If an error occurs.
  */
public abstract int
getQueryTimeout() throws SQLException;

/*************************************************************************/

/**
  * This method sets the number of seconds a statement may be in process
  * before timing out.  A value of 0 means there is no timeout.
  *
  * @param timeout The new SQL statement timeout value.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setQueryTimeout(int timeout) throws SQLException;

/*************************************************************************/

/**
  * This method cancels an outstanding statement, if the database supports
  * that operation.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
cancel() throws SQLException;

/*************************************************************************/

/**
  * This method returns the first SQL warning attached to this statement.
  * Subsequent warnings will be chained to this one.
  *
  * @return The first SQL warning for this statement.
  *
  * @exception SQLException If an error occurs.
  */
public abstract SQLWarning
getWarnings() throws SQLException;

/*************************************************************************/

/**
  * This method clears any SQL warnings that have been attached to this
  * statement.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
clearWarnings() throws SQLException;

/*************************************************************************/

/**
  * This method sets the cursor name that will be used by the result set.
  *
  * @param name The cursor name to use for this statement.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
setCursorName(String name) throws SQLException;

/*************************************************************************/

/**
  * This method executes an arbitrary SQL statement of any time.  The
  * methods <code>getResultSet</code>, <code>getMoreResults</code> and
  * <code>getUpdateCount</code> retrieve the results.
  *
  * @return <code>true</code> if a result set was returned, <code>false</code>
  * if an update count was returned.
  *
  * @exception SQLException If an error occurs.
  */
public abstract boolean
execute(String sql) throws SQLException;

/*************************************************************************/

/**
  * This method returns the result set of the SQL statement that was
  * executed.  This should be called only once per result set returned.
  *
  * @return The result set of the query, or <code>null</code> if there was
  * no result set (for example, if the statement was an UPDATE).
  *
  * @exception SQLException If an error occurs.
  *
  * @see execute
  */
public abstract ResultSet
getResultSet() throws SQLException;

/*************************************************************************/

/**
  * This method returns the update count of the SQL statement that was
  * executed.  This should be called only once per executed SQL statement.
  *
  * @return The update count of the query, or -1 if there was no update
  * count (for example, if the statement was a SELECT).
  *
  * @exception SQLException If an error occurs.
  *
  * @see execute
  */
public abstract int
getUpdateCount() throws SQLException;

/*************************************************************************/

/**
  * This method advances the result set pointer to the next result set, 
  * which can then be retrieved using <code>getResultSet</code>
  *
  * @return <code>true</code> if there is another result set, 
  * <code>false</code> otherwise (for example, the next result is an
  * update count).
  *
  * @exception SQLException If an error occurs.
  *
  * @see execute
  */
public abstract boolean
getMoreResults() throws SQLException;

} // interface Statement

