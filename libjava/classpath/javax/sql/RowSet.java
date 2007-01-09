/* RowSet.java 
   Copyright (C) 2002 Free Software Foundation, Inc.

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


package javax.sql;

import java.io.InputStream;
import java.io.Reader;
import java.math.BigDecimal;
import java.sql.Array;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.Date;
import java.sql.Ref;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Time;
import java.sql.Timestamp;
import java.util.Calendar;
import java.util.Map;

/**
 * @since 1.4
 */
public interface RowSet extends ResultSet 
{
  String getUrl() throws SQLException;

  void setUrl(String url) throws SQLException;

  String getDataSourceName();

  void setDataSourceName(String name) throws SQLException;

  String getUsername();

  void setUsername(String name) throws SQLException;

  String getPassword();

  void setPassword(String password) throws SQLException;

  int getTransactionIsolation();

  void setTransactionIsolation(int level) throws SQLException;

  Map<String, Class<?>> getTypeMap() throws SQLException;

  void setTypeMap(Map<String, Class<?>> map) throws SQLException;

  String getCommand();

  void setCommand(String cmd) throws SQLException;

  boolean isReadOnly();

  void setReadOnly(boolean value) throws SQLException;

  int getMaxFieldSize() throws SQLException;

  void setMaxFieldSize(int max) throws SQLException;

  int getMaxRows() throws SQLException;

  void setMaxRows(int max) throws SQLException;

  boolean getEscapeProcessing() throws SQLException;

  void setEscapeProcessing(boolean enable) throws SQLException;

  int getQueryTimeout() throws SQLException;

  void setQueryTimeout(int seconds) throws SQLException;

  void setType(int type) throws SQLException;

  void setConcurrency(int concurrency) throws SQLException;

  void setNull(int parameterIndex, int sqlType) throws SQLException;

  void setNull(int paramIndex, int sqlType, String typeName) throws
      SQLException;

  void setBoolean(int parameterIndex, boolean x) throws SQLException;

  void setByte(int parameterIndex, byte x) throws SQLException;

  void setShort(int parameterIndex, short x) throws SQLException;

  void setInt(int parameterIndex, int x) throws SQLException;

  void setLong(int parameterIndex, long x) throws SQLException;

  void setFloat(int parameterIndex, float x) throws SQLException;

  void setDouble(int parameterIndex, double x) throws SQLException;

  void setBigDecimal(int parameterIndex, BigDecimal x) throws
      SQLException;

  void setString(int parameterIndex, String x) throws SQLException;

  void setBytes(int parameterIndex, byte[] x) throws SQLException;

  void setDate(int parameterIndex, Date x) throws SQLException;

  void setTime(int parameterIndex, Time x) throws SQLException;

  void setTimestamp(int parameterIndex, Timestamp x) throws
      SQLException;

  void setAsciiStream(int parameterIndex, InputStream x, int length)
      throws SQLException;

  void setBinaryStream(int parameterIndex, InputStream x, int length)
      throws SQLException;

  void setCharacterStream(int parameterIndex, Reader reader, int
      length) throws SQLException;

  void setObject(int parameterIndex, Object x, int targetSqlType, int
      scale) throws SQLException;

  void setObject(int parameterIndex, Object x, int targetSqlType)
      throws SQLException;

  void setObject(int parameterIndex, Object x) throws SQLException;

  void setRef(int i, Ref x) throws SQLException;

  void setBlob(int i, Blob x) throws SQLException;

  void setClob(int i, Clob x) throws SQLException;

  void setArray(int i, Array x) throws SQLException;

  void setDate(int parameterIndex, Date x, Calendar cal) throws
      SQLException;

  void setTime(int parameterIndex, Time x, Calendar cal) throws
      SQLException;

  void setTimestamp(int parameterIndex, Timestamp x, Calendar cal)
      throws SQLException;

  void clearParameters() throws SQLException;

  void execute() throws SQLException;

  void addRowSetListener(RowSetListener listener);

  void removeRowSetListener(RowSetListener listener);
}
