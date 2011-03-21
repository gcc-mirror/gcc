/* Types.java -- SQL type constants
   Copyright (C) 1999, 2001 Free Software Foundation, Inc.

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


package java.sql;

/**
 * This class contains constants that are used to identify SQL data types.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public class Types
{
  // These should be self explanatory.  People need a SQL book, not
  // Javadoc comments for these.
  public static final int BIT = -7;
  public static final int TINYINT = -6;
  public static final int SMALLINT = 5;
  public static final int INTEGER = 4;
  public static final int BIGINT = -5;
  public static final int FLOAT = 6;
  public static final int REAL = 7;
  public static final int DOUBLE = 8;
  public static final int NUMERIC = 2;
  public static final int DECIMAL = 3;
  public static final int CHAR = 1;
  public static final int VARCHAR = 12;
  public static final int LONGVARCHAR = -1;
  public static final int DATE = 91;
  public static final int TIME = 92;
  public static final int TIMESTAMP = 93;
  public static final int BINARY = -2;
  public static final int VARBINARY = -3;
  public static final int LONGVARBINARY = -4;
  public static final int NULL = 0;
  public static final int OTHER = 1111;
  public static final int JAVA_OBJECT = 2000;
  public static final int DISTINCT = 2001;
  public static final int STRUCT = 2002;
  public static final int ARRAY = 2003;
  public static final int BLOB = 2004;
  public static final int CLOB = 2005;
  public static final int REF = 2006;
  public static final int DATALINK = 70;
  public static final int BOOLEAN = 16;

  // This class can't be instantiated.
  private Types()
  {
  }
}
