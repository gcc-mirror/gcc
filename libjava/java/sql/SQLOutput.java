/* SQLOutput.java -- Write SQL values to a stream
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

import java.io.InputStream;
import java.io.Reader;
import java.math.BigDecimal;

/**
  * This interface provides methods for writing Java types to a SQL stream.
  * It is used for implemented custom type mappings for user defined data
  * types.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public interface SQLOutput
{

/*************************************************************************/

/**
  * This method writes the specified Java <code>String</code>
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
writeString(String value) throws SQLException;

/*************************************************************************/

/**
  * This method writes the specified Java <code>boolean</code>
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
writeBoolean(boolean value) throws SQLException;

/*************************************************************************/

/**
  * This method writes the specified Java <code>byte</code>
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
writeByte(byte value) throws SQLException;

/*************************************************************************/

/**
  * This method writes the specified Java <code>short</code>
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
writeShort(short value) throws SQLException;

/*************************************************************************/

/**
  * This method writes the specified Java <code>int</code>
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
writeInt(int value) throws SQLException;

/*************************************************************************/

/**
  * This method writes the specified Java <code>long</code>
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
writeLong(long value) throws SQLException;

/*************************************************************************/

/**
  * This method writes the specified Java <code>float</code>
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
writeFloat(float value) throws SQLException;

/*************************************************************************/

/**
  * This method writes the specified Java <code>double</code>
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
writeDouble(double value) throws SQLException;

/*************************************************************************/

/**
  * This method writes the specified Java <code>BigDecimal</code>
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
writeBigDecimal(BigDecimal value) throws SQLException;

/*************************************************************************/

/**
  * This method writes the specified Java <code>byte</code> array
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
writeBytes(byte[] value) throws SQLException;

/*************************************************************************/

/**
  * This method writes the specified Java <code>java.sql.Date</code> 
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
writeDate(java.sql.Date value) throws SQLException;

/*************************************************************************/

/**
  * This method writes the specified Java <code>java.sql.Time</code> 
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
writeTime(java.sql.Time value) throws SQLException;

/*************************************************************************/

/**
  * This method writes the specified Java <code>java.sql.Timestamp</code> 
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
writeTimestamp(java.sql.Timestamp value) throws SQLException;

/*************************************************************************/

/**
  * This method writes the specified Java character stream
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
writeCharacterStream(Reader value) throws SQLException;

/*************************************************************************/

/**
  * This method writes the specified uninterpreted binary byte stream
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract void
writeBinaryStream(InputStream value) throws SQLException;

/*************************************************************************/

/**
  * This method writes the specified ASCII text stream
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  * 
  * @exception SQLException If an error occurs.
  */
public abstract void
writeAsciiStream(InputStream value) throws SQLException;

/*************************************************************************/

/**
  * This method writes the specified Java <code>SQLData</code> object
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
writeObject(SQLData value) throws SQLException;

/*************************************************************************/

/**
  * This method writes the specified Java SQL <code>Ref</code> object
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
writeRef(Ref value) throws SQLException;

/*************************************************************************/

/**
  * This method writes the specified Java SQL <code>Blob</code> object
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
writeBlob(Blob value) throws SQLException;

/*************************************************************************/

/**
  * This method writes the specified Java SQL <code>Clob</code> object
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
writeClob(Clob value) throws SQLException;

/*************************************************************************/

/**
  * This method writes the specified Java SQL <code>Struct</code> object
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
writeStruct(Struct value) throws SQLException;

/*************************************************************************/

/**
  * This method writes the specified Java SQL <code>Array</code> object
  * to the SQL stream.
  *
  * @param value The value to write to the stream.
  *
  * @exception SQLException If an error occurs.
  */
public abstract void
writeArray(Array value) throws SQLException;

} // interface SQLOutput

