/* Array.java -- Interface for accessing SQL array object
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

import java.util.Map;

/**
  * This interface provides methods for accessing SQL array types
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public interface Array
{

/**
  * This method returns the name of the SQL type of the elements in this
  * array.  This name is database specific.
  *
  * @param The name of the SQL type of the elements in this array.
  *
  * @exception SQLException If an error occurs.
  */
public abstract String
getBaseTypeName() throws SQLException;

/*************************************************************************/

/**
  * This method returns the JDBC type identifier of the elements in this
  * array.  This will be one of the values defined in the <code>Types</code>
  * class.
  *
  * @return The JDBC type of the elements in this array.
  *
  * @exception SQLException If an error occurs.
  * 
  * @see Types
  */
public abstract int
getBaseType() throws SQLException;

/*************************************************************************/

/**
  * This method returns the contents of this array.  This object returned
  * will be an array of Java objects of the appropriate types.
  *
  * @return The contents of the array as an array of Java objects.
  *
  * @exception SQLException If an error occurs.
  */
public abstract Object
getArray() throws SQLException;

/*************************************************************************/

/**
  * This method returns the contents of this array.  The specified
  * <code>Map</code> will be used to override selected mappings between
  * SQL types and Java classes.
  * 
  * @param map A mapping of SQL types to Java classes.
  *
  * @return The contents of the array as an array of Java objects.
  *
  * @exception SQLException If an error occurs.
  */
public abstract Object
getArray(Map map) throws SQLException;

/*************************************************************************/

/**
  * This method returns a portion of this array starting at index
  * <code>offset</code> into the array and continuing for <code>length</code>
  * elements.  Fewer than the requested number of elements will be
  * returned if the array does not contain the requested number of elements.
  * The object returned will be an array of Java objects of
  * the appropriate types.
  *
  * @param offset The offset into this array to start returning elements from.
  * @param count The requested number of elements to return.
  *
  * @return The requested portion of the array.
  *
  * @exception SQLException If an error occurs.
  */
public abstract Object
getArray(long offset, int count) throws SQLException;

/*************************************************************************/

/**
  * This method returns a portion of this array starting at index
  * <code>offset</code> into the array and continuing for <code>length</code>
  * elements.  Fewer than the requested number of elements will be
  * returned if the array does not contain the requested number of elements.
  * The object returned will be an array of Java objects.  The specified
  * <code>Map</code> will be used for overriding selected SQL type to
  * Java class mappings.
  *
  * @param offset The offset into this array to start returning elements from.
  * @param count The requested number of elements to return.
  * @param map A mapping of SQL types to Java classes.
  *
  * @return The requested portion of the array.
  *
  * @exception SQLException If an error occurs.
  */
public abstract Object
getArray(long index, int count, Map map) throws SQLException;

/*************************************************************************/

/**
  * This method returns the elements in the array as a <code>ResultSet</code>.
  * Each row of the result set will have two columns.  The first will be
  * the index into the array of that row's contents.  The second will be
  * the actual value of that array element.
  *
  * @return The elements of this array as a <code>ResultSet</code>.
  *
  * @exception SQLException If an error occurs.
  *
  * @see ResultSet
  */
public abstract ResultSet
getResultSet() throws SQLException;

/*************************************************************************/

/**
  * This method returns the elements in the array as a <code>ResultSet</code>.
  * Each row of the result set will have two columns.  The first will be
  * the index into the array of that row's contents.  The second will be
  * the actual value of that array element.  The specified <code>Map</code>
  * will be used to override selected default mappings of SQL types to
  * Java classes.
  *
  * @param map A mapping of SQL types to Java classes.
  *
  * @return The elements of this array as a <code>ResultSet</code>.
  *
  * @exception SQLException If an error occurs.
  *
  * @see ResultSet
  */
public abstract ResultSet
getResultSet(Map map) throws SQLException;

/*************************************************************************/

/**
  * This method returns a portion of the array as a <code>ResultSet</code>.
  * The returned portion will start at index <code>offset</code> into the
  * array and up to <code>length</code> elements will be returned.
  * <p>
  * Each row of the result set will have two columns.  The first will be
  * the index into the array of that row's contents.  The second will be
  * the actual value of that array element.
  *
  * @param offset The index into the array to start returning elements from.
  * @param length The requested number of elements to return.
  *
  * @return The requested elements of this array as a <code>ResultSet</code>.
  *
  * @exception SQLException If an error occurs.
  *
  * @see ResultSet
  */
public abstract ResultSet
getResultSet(long index, int count) throws SQLException;

/*************************************************************************/

/**
  * This method returns a portion of the array as a <code>ResultSet</code>.
  * The returned portion will start at index <code>offset</code> into the
  * array and up to <code>length</code> elements will be returned.
  * <p>
  * Each row of the result set will have two columns.  The first will be
  * the index into the array of that row's contents.  The second will be
  * the actual value of that array element.  The specified <code>Map</code>
  * will be used to override selected default mappings of SQL types to
  * Java classes.
  *
  * @param offset The index into the array to start returning elements from.
  * @param length The requested number of elements to return.
  * @param map A mapping of SQL types to Java classes.
  *
  * @return The requested elements of this array as a <code>ResultSet</code>.
  *
  * @exception SQLException If an error occurs.
  *
  * @see ResultSet
  */
public abstract ResultSet
getResultSet(long index, int count, Map map) throws SQLException;

} // interface Array

