/* SQLWarning.java -- Database access warnings.
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
  * This exception is thrown when a database warning occurs.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class SQLWarning extends SQLException implements java.io.Serializable
{

/*************************************************************************/

/*
 * Constructors
 */

/**
  * This method initializes a new instance of <code>SQLWxception</code>
  * that does not have a descriptive messages and SQL state, and which
  * has a vendor error code of 0.
  */
public 
SQLWarning()
{
  this(null, null, 0);
}

/*************************************************************************/

/**
  * This method initializes a new instance of <code>SQLWarning</code>
  * with the specified descriptive error message.  The SQL state of this
  * instance will be <code>null</code> and the vendor error code will be 0.
  *
  * @param message A string describing the nature of the error.
  */
public 
SQLWarning(String message)
{
  this(message, null, 0);
}

/*************************************************************************/

/**
  * This method initializes a new instance of <code>SQLWarning</code>
  * with the specified descriptive error message and SQL state string.
  * The vendor error code of this instance will be 0.
  *
  * @param message A string describing the nature of the error.
  * @param SQLState A string containing the SQL state of the error.
  */
public
SQLWarning(String message, String SQLState)
{
  this(message, SQLState, 0);
}

/*************************************************************************/

/**
  * This method initializes a nwe instance of <code>SQLWarning</code>
  * with the specified descriptive error message, SQL state string, and
  * vendor code.
  *
  * @param message A string describing the nature of the error.
  * @param SQLState A string containing the SQL state of the error.
  * @param vendorCode The vendor error code associated with this error.
  */
public
SQLWarning(String message, String SQLState, int vendorCode)
{
  super(message, SQLState, vendorCode);
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * This method returns the exception that is chained to this object.
  *
  * @return The exception chained to this object, which may be 
  * <code>null</code>.
  */
public SQLWarning
getNextWarning()
{
  return((SQLWarning)super.getNextException());
}

/*************************************************************************/

/**
  * This method adds a new exception to the end of the chain of exceptions
  * that are chained to this object.
  *
  * @param e The exception to add to the end of the chain.
  */
public void
setNextWarning(SQLWarning e)
{
  super.setNextException(e);
}

} // class SQLWarning

