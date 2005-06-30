/* SQLWarning.java -- Database access warnings.
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
 * This exception is thrown when a database warning occurs.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public class SQLWarning extends SQLException 
{
  static final long serialVersionUID = 3917336774604784856L;

  /**
   * This method initializes a nwe instance of <code>SQLWarning</code>
   * with the specified descriptive error message, SQL state string, and
   * vendor code.
   *
   * @param message A string describing the nature of the error.
   * @param SQLState A string containing the SQL state of the error.
   * @param vendorCode The vendor error code associated with this error.
   */
  public SQLWarning(String reason, String SQLState, int vendorCode)
  {
    super(reason, SQLState, vendorCode);
  }

  /**
   * This method initializes a new instance of <code>SQLWarning</code>
   * with the specified descriptive error message and SQL state string.
   * The vendor error code of this instance will be 0.
   *
   * @param message A string describing the nature of the error.
   * @param SQLState A string containing the SQL state of the error.
   */
  public SQLWarning(String message, String SQLState)
  {
    super(message, SQLState);
  }

  /**
   * This method initializes a new instance of <code>SQLWarning</code>
   * with the specified descriptive error message.  The SQL state of this
   * instance will be <code>null</code> and the vendor error code will be 0.
   *
   * @param message A string describing the nature of the error.
   */
  public SQLWarning(String message)
  {
    super(message);
  }

  /**
   * This method initializes a new instance of <code>SQLWarning</code>
   * that does not have a descriptive messages and SQL state, and which
   * has a vendor error code of 0.
   */
  public SQLWarning()
  {
    super();
  }

  /**
   * This method returns the exception that is chained to this object.
   *
   * @return The exception chained to this object, which may be 
   *         <code>null</code>.
   */
  public SQLWarning getNextWarning()
  {
    return (SQLWarning) super.getNextException();
  }

  /**
   * This method adds a new exception to the end of the chain of exceptions
   * that are chained to this object.
   *
   * @param w The exception to add to the end of the chain.
   */
  public void setNextWarning(SQLWarning w)
  {
    super.setNextException(w);
  }
}
