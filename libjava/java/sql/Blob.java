/* Blob.java -- Access a SQL Binary Large OBject.
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

/**
  * This interface specified methods for accessing a SQL BLOB (Binary
  * Large OBject) type.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public interface Blob
{

/*************************************************************************/

/**
  * This method returns the number of bytes in the BLOB.
  *
  * @return The number of bytes in the BLOB.
  *
  * @exception SQLException If an error occurs.
  */
public abstract long
length() throws SQLException;

/*************************************************************************/

/**
  * This method returns up to the requested bytes of this BLOB as a 
  * <code>byte</code> array.
  *
  * @param offset The index into the BLOB to start returning bytes from.
  * @param length The requested number of bytes to return.
  *
  * @return The requested bytes from the BLOB.
  *
  * @exception SQLException If an error occurs.
  */
public abstract byte[]
getBytes(long offset, int length) throws SQLException;

/*************************************************************************/

/**
  * This method returns a stream that will read the bytes of the BLOB.
  *
  * @return A stream that will read the bytes of the BLOB.
  *
  * @exception SQLException If an error occurs.
  */
public abstract InputStream
getBinaryStream() throws SQLException;

/*************************************************************************/

/**
  * This method returns the index into the BLOB at which the first instance
  * of the specified bytes occur.  The searching starts at the specified
  * index into the BLOB.
  *
  * @param pattern The byte pattern to search for.
  * @param offset The index into the BLOB to starting searching for the pattern.
  *
  * @return The offset at which the pattern is first found, or -1 if the
  * pattern is not found.
  *
  * @exception SQLException If an error occurs.
  */
public abstract long
position(byte[] pattern, long offset) throws SQLException;

/*************************************************************************/

/**
  * This method returns the index into the BLOB at which the first instance
  * of the specified pattern occurs.  The searching starts at the specified
  * index into this BLOB.  The bytes in the specified <code>Blob</code> are
  * used as the search pattern.
  *
  * @param pattern The <code>Blob</code> containing the byte pattern to
  * search for.
  * @param offset The index into the BLOB to starting searching for the pattern.
  *
  * @return The offset at which the pattern is first found, or -1 if the
  * pattern is not found.
  *
  * @exception SQLException If an error occurs.
  */
public abstract long
position(Blob pattern, long offset) throws SQLException;

} // interface Blob

