/* SQLData.java -- Custom mapping for a user defined datatype
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

/**
  * This interface is used for mapping SQL data to user defined datatypes.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public interface SQLData
{

/**
  * This method returns the user defined datatype name for this object.
  *
  * @return The user defined data type name for this object.
  *
  * @exception SQLException If an error occurs.
  */
public abstract String
getSQLTypeName() throws SQLException;

/*************************************************************************/

/**
  * This method populates the data in the object from the specified stream.
  *
  * @param stream The stream to read the data from.
  * @param name The data type name of the data on the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
readSQL(SQLInput stream, String name) throws SQLException;

/*************************************************************************/

/**
  * This method writes the data in this object to the specified stream.
  *
  * @param stream The stream to write the data to.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
writeSQL(SQLOutput stream) throws SQLException;

} // interface SQLData

