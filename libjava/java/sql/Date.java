/* Date.java -- Wrapper around java.util.Date
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

import java.text.SimpleDateFormat;

/**
  * This class is a wrapper around java.util.Date to allow the JDBC
  * driver to identify the value as a SQL Date.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class Date extends java.util.Date implements java.io.Serializable
{

/*
 * Class Variables
 */

/**
  * Used for parsing and formatting this date.
  */
private static SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");

/*************************************************************************/

/*
 * Class Methods
 */

/**
  * This method returns a new instance of this class by parsing a
  * date in JDBC format into a Java date.
  *
  * @param str The string to parse.
  *
  * @return The resulting <code>java.sql.Date</code> value. 
  */
public static Date
valueOf(String str)
{
  try
    {
      java.util.Date d = (java.util.Date)sdf.parseObject(str);
      return(new Date(d.getTime()));
    }
  catch(Exception e)
    {
      return(null);
    }
}

/*************************************************************************/

/*
 * Constructors
 */

/**
  * This method initializes a new instance of this class with the
  * specified year, month, and day.
  *
  * @param year The year of this date minue 1900.
  * @param month The month of this date (0-11).
  * @param day The day of this date (1-31).
  *
  * @deprecated
  */
public 
Date(int year, int month, int day)
{
  super(year, month, day);
}

/*************************************************************************/

/**
  * This method initializes a new instance of this class with the
  * specified time value representing the number of seconds since 
  * Jan 1, 1970 at 12:00 midnight GMT.
  *
  * @param time The time value to intialize this date to.
  */
public
Date(long date)
{
  super(date);
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * This method returns this date in JDBC format.
  *
  * @return This date as a string.
  */
public String
toString()
{
  return(sdf.format(this));
}

} // class Date

