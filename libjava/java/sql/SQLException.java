/* SQLException.java -- General SQL exception
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
  * This exception is thrown when a database error occurs.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class SQLException extends Exception implements java.io.Serializable
{

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * This is the next exception in the chain
  * @serialized
  */
private SQLException next;

/**
  * This is the state of the SQL statement at the time of the error.
  * @serialized
  */
private String SQLState;

/**
  * The vendor error code for this error
  * @serialized
  */
private int vendorCode;

/*************************************************************************/

/*
 * Constructors
 */

/**
  * This method initializes a new instance of <code>SQLException</code>
  * that does not have a descriptive messages and SQL state, and which
  * has a vendor error code of 0.
  */
public 
SQLException()
{
  this(null, null, 0);
}

/*************************************************************************/

/**
  * This method initializes a new instance of <code>SQLException</code>
  * with the specified descriptive error message.  The SQL state of this
  * instance will be <code>null</code> and the vendor error code will be 0.
  *
  * @param message A string describing the nature of the error.
  */
public 
SQLException(String message)
{
  this(message, null, 0);
}

/*************************************************************************/

/**
  * This method initializes a new instance of <code>SQLException</code>
  * with the specified descriptive error message and SQL state string.
  * The vendor error code of this instance will be 0.
  *
  * @param message A string describing the nature of the error.
  * @param SQLState A string containing the SQL state of the error.
  */
public
SQLException(String message, String SQLState)
{
  this(message, SQLState, 0);
}

/*************************************************************************/

/**
  * This method initializes a nwe instance of <code>SQLException</code>
  * with the specified descriptive error message, SQL state string, and
  * vendor code.
  *
  * @param message A string describing the nature of the error.
  * @param SQLState A string containing the SQL state of the error.
  * @param vendorCode The vendor error code associated with this error.
  */
public
SQLException(String message, String SQLState, int vendorCode)
{
  super(message);

  this.SQLState = SQLState;
  this.vendorCode = vendorCode;
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * This method returns the SQLState information associated with this
  * error.  The value returned is a <code>String</code> which is formatted
  * using the XOPEN SQL state conventions.
  *
  * @return The SQL state, which may be <code>null</code>.
  */
public String
getSQLState()
{
  return(SQLState);
}

/*************************************************************************/

/**
  * This method returns the vendor specific error code associated with 
  * this error.
  *
  * @return The vendor specific error code associated with this error.
  */
public int
getErrorCode()
{
  return(vendorCode);
}

/*************************************************************************/

/**
  * This method returns the exception that is chained to this object.
  *
  * @return The exception chained to this object, which may be 
  * <code>null</code>.
  */
public SQLException
getNextException()
{
  return(next);
}

/*************************************************************************/

/**
  * This method adds a new exception to the end of the chain of exceptions
  * that are chained to this object.
  *
  * @param e The exception to add to the end of the chain.
  */
public void
setNextException(SQLException e)
{
  if (e == null)
    return;

  SQLException list_entry = this;
  while (list_entry.getNextException() != null)
    list_entry = list_entry.getNextException();

  list_entry.next = e;
}

} // class SQLException

