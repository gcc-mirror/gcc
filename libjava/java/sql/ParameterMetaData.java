/* ParameterMetaData.java
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

package java.sql;

/**
 * @since 1.4
 */
public interface ParameterMetaData 
{
  public static final int parameterNoNulls = 0;

  public static final int parameterNullable = 1;

  public static final int parameterNullableUnknown = 2;

  public static final int parameterModeUnknown = 0;

  public static final int parameterModeIn = 1;

  public static final int parameterModeInOut = 2;

  public static final int parameterModeOut = 4;

  /**
   * @since 1.4
   */
  public int getParameterCount() throws SQLException;

  /**
   * @since 1.4
   */
  public int isNullable(int param) throws SQLException;

  /**
   * @since 1.4
   */
  public boolean isSigned(int param) throws SQLException;

  /**
   * @since 1.4
   */
  public int getPrecision(int param) throws SQLException;

  /**
   * @since 1.4
   */
  public int getScale(int param) throws SQLException;

  /**
   * @since 1.4
   */
  public int getParameterType(int param) throws SQLException;

  /**
   * @since 1.4
   */
  public String getParameterTypeName(int param) throws SQLException;

  /**
   * @since 1.4
   */
  public String getParameterClassName(int param) throws SQLException;

  /**
   * @since 1.4
   */
  public int getParameterMode(int param) throws SQLException;
}
