/* Clob.java -- Access Character Large OBjects
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

/**
  * This interface contains methods for accessing a SQL CLOB (Character
  * Large OBject) type.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public interface Clob
{

/**
  * This method returns the number of characters in the CLOB.
  *
  * @return The number of characters in the CLOB.
  *
  * @exception SQLException If an error occurs.
  */
public abstract long
length() throws SQLException;

/*************************************************************************/

/**
  * This method returns the specified portion of the CLOB as a 
  * <code>String</code>. 
  *
  * @param offset The index into the CLOB (index values start at 1) to 
  * start returning characters from.
  * @param length The requested number of characters to return.
  *
  * @return The requested CLOB section, as a <code>String</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract String
getSubString(long offset, int length) throws SQLException;

/*************************************************************************/

/**
  * This method returns a byte stream that reads the contents of the
  * CLOB as a series of ASCII bytes.
  *
  * @return A stream to read the CLOB's contents.
  *
  * @exception SQLException If an error occurs.
  */
public abstract InputStream
getAsciiStream() throws SQLException;

/*************************************************************************/

/**
  * This method returns a character stream that reads the contents of the
  * CLOB.
  *
  * @return A character stream to read the CLOB's contents.
  *
  * @exception SQLException If an error occurs.
  */
public abstract Reader
getCharacterStream() throws SQLException;

/*************************************************************************/

/**
  * This method returns the index into the CLOB of the first occurrence of
  * the specified character pattern (supplied by the caller as a
  * <code>String</code>).  The search begins at the specified index.
  *
  * @param pattern The character pattern to search for, passed as a
  * <code>String</code>.
  * @param offset.  The index into the CLOB to start search (indexes start
  * at 1).
  *
  * @return The index at which the pattern was found (indexes start at 1),
  * or -1 if the pattern was not found.
  *
  * @exception SQLException If an error occurs.
  */
public abstract long
position(String pattern, long offset) throws SQLException;

/*************************************************************************/

/**
  * This method returns the index into the CLOB of the first occurrence of
  * the specified character pattern (supplied by the caller as a
  * <code>Clob</code>).  The search begins at the specified index.
  *
  * @param pattern The character pattern to search for, passed as a
  * <code>Clob</code>.
  * @param offset.  The index into the CLOB to start search (indexes start
  * at 1).
  *
  * @return The index at which the pattern was found (indexes start at 1),
  * or -1 if the pattern was not found.
  *
  * @exception SQLException If an error occurs.
  */
public abstract long
position(Clob pattern, long offset) throws SQLException;

} // interface Clob

