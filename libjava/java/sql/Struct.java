/* Struct.java -- Mapping for a SQL structured type.
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

import java.util.Map;

/**
  * This interface implements the standard type mapping for a SQL 
  * structured type.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public interface Struct
{

/**
  * This method returns the name of the SQL structured type for this
  * object.
  *
  * @return The SQL structured type name.
  *
  * @exception SQLException If an error occurs.
  */
public abstract String
getSQLTypeName() throws SQLException;

/*************************************************************************/

/**
  * This method returns the attributes of this SQL structured type.
  *
  * @return The attributes of this structure type.
  *
  * @exception SQLException If an error occurs.
  */
public abstract Object[]
getAttributes() throws SQLException;

/*************************************************************************/

/**
  * This method returns the attributes of this SQL structured type.
  * The specified map of type mappings overrides the default mappings.
  *
  * @param map The map of SQL type mappings.
  *
  * @return The attributes of this structure type.
  *
  * @exception SQLException If a error occurs.
  */
public abstract Object[]
getAttributes(Map map) throws SQLException;

} // interface Struct

