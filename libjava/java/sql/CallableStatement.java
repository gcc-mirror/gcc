/* CallableStatement.java -- A statement for calling stored procedures.
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

import java.io.InputStream;
import java.io.Reader;
import java.math.BigDecimal;
import java.util.Calendar;

/**
  * This interface provides a mechanism for calling stored procedures.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public interface CallableStatement extends PreparedStatement
{

/*************************************************************************/

/**
  * This method tests whether the value of the last parameter that was fetched
  * was actually a SQL NULL value.
  *
  * @return <code>true</code> if the last parameter fetched was a NULL,
  * <code>false</code> otherwise.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract boolean
wasNull() throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified parameter as a Java
  * <code>String</code>.
  *
  * @param index The index of the parameter to return.
  *
  * @return The parameter value as a <code>String</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract String
getString(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified parameter as a Java
  * <code>Object</code>.
  *
  * @param index The index of the parameter to return.
  *
  * @return The parameter value as an <code>Object</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract Object
getObject(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified parameter as a Java
  * <code>boolean</code>.
  *
  * @param index The index of the parameter to return.
  *
  * @return The parameter value as a <code>boolean</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract boolean
getBoolean(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified parameter as a Java
  * <code>byte</code>.
  *
  * @param index The index of the parameter to return.
  *
  * @return The parameter value as a <code>byte</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract byte
getByte(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified parameter as a Java
  * <code>short</code>.
  *
  * @param index The index of the parameter to return.
  *
  * @return The parameter value as a <code>short</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract short
getShort(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified parameter as a Java
  * <code>int</code>.
  *
  * @param index The index of the parameter to return.
  *
  * @return The parameter value as a <code>int</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract int
getInt(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified parameter as a Java
  * <code>long</code>.
  *
  * @param index The index of the parameter to return.
  *
  * @return The parameter value as a <code>long</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract long
getLong(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified parameter as a Java
  * <code>float</code>.
  *
  * @param index The index of the parameter to return.
  *
  * @return The parameter value as a <code>float</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract float
getFloat(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified parameter as a Java
  * <code>double</code>.
  *
  * @param index The index of the parameter to return.
  *
  * @return The parameter value as a <code>double</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract double
getDouble(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified parameter as a Java
  * <code>BigDecimal</code>.
  *
  * @param index The index of the parameter to return.
  * @param scale The number of digits to the right of the decimal to return.
  *
  * @return The parameter value as a <code>BigDecimal</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract BigDecimal
getBigDecimal(int index, int scale) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified parameter as a Java
  * byte array.
  *
  * @param index The index of the parameter to return.
  *
  * @return The parameter value as a byte array
  *
  * @exception SQLException If an error occurs.
  */
public abstract byte[]
getBytes(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified parameter as a Java
  * <code>java.sql.Date</code>.
  *
  * @param index The index of the parameter to return.
  *
  * @return The parameter value as a <code>java.sql.Date</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract java.sql.Date
getDate(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified parameter as a Java
  * <code>java.sql.Time</code>.
  *
  * @param index The index of the parameter to return.
  *
  * @return The parameter value as a <code>java.sql.Time</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract java.sql.Time
getTime(int index) throws SQLException;

/*************************************************************************/

/**
  * This method returns the value of the specified parameter as a Java
  * <code>java.sql.Timestamp</code>.
  *
  * @param index The index of the parameter to return.
  *
  * @return The parameter value as a <code>java.sql.Timestamp</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract java.sql.Timestamp
getTimestamp(int index) throws SQLException;

/*************************************************************************/

/**
  * This method registers the specified parameter as an output parameter
  * of the specified SQL type.
  *
  * @param index The index of the parameter to register as output.
  * @param type The SQL type value from <code>Types</code>.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
registerOutParameter(int index, int type) throws SQLException;

/*************************************************************************/

/**
  * This method registers the specified parameter as an output parameter
  * of the specified SQL type and scale.
  *
  * @param index The index of the parameter to register as output.
  * @param type The SQL type value from <code>Types</code>.
  * @param scale The scale of the value that will be returned.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
registerOutParameter(int index, int type, int scale) throws SQLException;

} // interface CallableStatement


