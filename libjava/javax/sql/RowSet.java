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
  /**
   * @since 1.4
   */
  public String getUrl() throws SQLException;

  /**
   * @since 1.4
   */
  public void setUrl(String url) throws SQLException;

  /**
   * @since 1.4
   */
  public String getDataSourceName();

  /**
   * @since 1.4
   */
  public void setDataSourceName(String name) throws SQLException;

  /**
   * @since 1.4
   */
  public String getUsername();

  /**
   * @since 1.4
   */
  public void setUsername(String name) throws SQLException;

  /**
   * @since 1.4
   */
  public String getPassword();

  /**
   * @since 1.4
   */
  public void setPassword(String password) throws SQLException;

  /**
   * @since 1.4
   */
  public int getTransactionIsolation();

  /**
   * @since 1.4
   */
  public void setTransactionIsolation(int level) throws SQLException;

  /**
   * @since 1.4
   */
  public Map getTypeMap() throws SQLException;

  /**
   * @since 1.4
   */
  public void setTypeMap(Map map) throws SQLException;

  /**
   * @since 1.4
   */
  public String getCommand();

  /**
   * @since 1.4
   */
  public void setCommand(String cmd) throws SQLException;

  /**
   * @since 1.4
   */
  public boolean isReadOnly();

  /**
   * @since 1.4
   */
  public void setReadOnly(boolean value) throws SQLException;

  /**
   * @since 1.4
   */
  public int getMaxFieldSize() throws SQLException;

  /**
   * @since 1.4
   */
  public void setMaxFieldSize(int max) throws SQLException;

  /**
   * @since 1.4
   */
  public int getMaxRows() throws SQLException;

  /**
   * @since 1.4
   */
  public void setMaxRows(int max) throws SQLException;

  /**
   * @since 1.4
   */
  public boolean getEscapeProcessing() throws SQLException;

  /**
   * @since 1.4
   */
  public void setEscapeProcessing(boolean enable) throws SQLException;

  /**
   * @since 1.4
   */
  public int getQueryTimeout() throws SQLException;

  /**
   * @since 1.4
   */
  public void setQueryTimeout(int seconds) throws SQLException;

  /**
   * @since 1.4
   */
  public void setType(int type) throws SQLException;

  /**
   * @since 1.4
   */
  public void setConcurrency(int concurrency) throws SQLException;

  /**
   * @since 1.4
   */
  public void setNull(int parameterIndex, int sqlType) throws SQLException;

  /**
   * @since 1.4
   */
  public void setNull(int paramIndex, int sqlType, String typeName) throws
      SQLException;

  /**
   * @since 1.4
   */
  public void setBoolean(int parameterIndex, boolean x) throws SQLException;

  /**
   * @since 1.4
   */
  public void setByte(int parameterIndex, byte x) throws SQLException;

  /**
   * @since 1.4
   */
  public void setShort(int parameterIndex, short x) throws SQLException;

  /**
   * @since 1.4
   */
  public void setInt(int parameterIndex, int x) throws SQLException;

  /**
   * @since 1.4
   */
  public void setLong(int parameterIndex, long x) throws SQLException;

  /**
   * @since 1.4
   */
  public void setFloat(int parameterIndex, float x) throws SQLException;

  /**
   * @since 1.4
   */
  public void setDouble(int parameterIndex, double x) throws SQLException;

  /**
   * @since 1.4
   */
  public void setBigDecimal(int parameterIndex, BigDecimal x) throws
      SQLException;

  /**
   * @since 1.4
   */
  public void setString(int parameterIndex, String x) throws SQLException;

  /**
   * @since 1.4
   */
  public void setBytes(int parameterIndex, byte[] x) throws SQLException;

  /**
   * @since 1.4
   */
  public void setDate(int parameterIndex, Date x) throws SQLException;

  /**
   * @since 1.4
   */
  public void setTime(int parameterIndex, Time x) throws SQLException;

  /**
   * @since 1.4
   */
  public void setTimestamp(int parameterIndex, Timestamp x) throws
      SQLException;

  /**
   * @since 1.4
   */
  public void setAsciiStream(int parameterIndex, InputStream x, int length)
      throws SQLException;

  /**
   * @since 1.4
   */
  public void setBinaryStream(int parameterIndex, InputStream x, int length)
      throws SQLException;

  /**
   * @since 1.4
   */
  public void setCharacterStream(int parameterIndex, Reader reader, int
      length) throws SQLException;

  /**
   * @since 1.4
   */
  public void setObject(int parameterIndex, Object x, int targetSqlType, int
      scale) throws SQLException;

  /**
   * @since 1.4
   */
  public void setObject(int parameterIndex, Object x, int targetSqlType)
      throws SQLException;

  /**
   * @since 1.4
   */
  public void setObject(int parameterIndex, Object x) throws SQLException;

  /**
   * @since 1.4
   */
  public void setRef(int i, Ref x) throws SQLException;

  /**
   * @since 1.4
   */
  public void setBlob(int i, Blob x) throws SQLException;

  /**
   * @since 1.4
   */
  public void setClob(int i, Clob x) throws SQLException;

  /**
   * @since 1.4
   */
  public void setArray(int i, Array x) throws SQLException;

  /**
   * @since 1.4
   */
  public void setDate(int parameterIndex, Date x, Calendar cal) throws
      SQLException;

  /**
   * @since 1.4
   */
  public void setTime(int parameterIndex, Time x, Calendar cal) throws
      SQLException;

  /**
   * @since 1.4
   */
  public void setTimestamp(int parameterIndex, Timestamp x, Calendar cal)
      throws SQLException;

  /**
   * @since 1.4
   */
  public void clearParameters() throws SQLException;

  /**
   * @since 1.4
   */
  public void execute() throws SQLException;

  /**
   * @since 1.4
   */
  public void addRowSetListener(RowSetListener listener);

  /**
   * @since 1.4
   */
  public void removeRowSetListener(RowSetListener listener);
}
