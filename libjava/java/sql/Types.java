/* Types.java -- SQL type constants
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

} // class Types 

