/* Date.java -- Wrapper around java.util.Date
   Copyright (C) 1999, 2000, 2003 Free Software Foundation, Inc.

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

import java.text.ParseException;
import java.text.SimpleDateFormat;

/**
  * This class is a wrapper around java.util.Date to allow the JDBC
  * driver to identify the value as a SQL Date.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class Date extends java.util.Date 
{
  static final long serialVersionUID = 1511598038487230103L;

  /**
   * Used for parsing and formatting this date.
   */
  private static SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");

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
  public Date(int year, int month, int day)
  {
    super(year, month, day);  
  }

  /**
   * This method initializes a new instance of this class with the
   * specified time value representing the number of seconds since 
   * Jan 1, 1970 at 12:00 midnight GMT.
   *
   * @param time The time value to intialize this date to.
   */
  public Date(long date)
  {
    super(date);
  }

  /**
   * This method always throws an IllegalArgumentException.
   *
   * @throws IllegalArgumentException when it's called.
   * @deprecated
   */
  public int getHours() throws IllegalArgumentException
  {
    throw new IllegalArgumentException();
  }

  /**
   * This method always throws an IllegalArgumentException.
   *
   * @throws IllegalArgumentException when it's called.
   * @deprecated
   */
  public int getMinutes() throws IllegalArgumentException
  {
    throw new IllegalArgumentException();
  }

  /**
   * This method always throws an IllegalArgumentException.
   *
   * @throws IllegalArgumentException when it's called.
   * @deprecated
   */
  public int getSeconds() throws IllegalArgumentException
  {
    throw new IllegalArgumentException();
  }

  /**
   * This method always throws an IllegalArgumentException.
   *
   * @throws IllegalArgumentException when it's called.
   * @deprecated
   */
  public void setHours(int newValue) throws IllegalArgumentException
  {
    throw new IllegalArgumentException();
  }

  /**
   * This method always throws an IllegalArgumentException.
   *
   * @throws IllegalArgumentException when it's called.
   * @deprecated
   */
  public void setMinutes(int newValue) throws IllegalArgumentException
  {
    throw new IllegalArgumentException();
  }

  /**
   * This method always throws an IllegalArgumentException.
   *
   * @throws IllegalArgumentException when it's called.
   * @deprecated
   */
  public void setSeconds(int newValue) throws IllegalArgumentException
  {
    throw new IllegalArgumentException();
  }

  /**
   * This method returns a new instance of this class by parsing a
   * date in JDBC format into a Java date.
   *
   * @param str The string to parse.
   * @return The resulting <code>java.sql.Date</code> value.
   *
   * @deprecated
   */
  public static Date valueOf (String str)
  {
    try 
      {
	java.util.Date d = (java.util.Date) sdf.parseObject(str);

	if (d == null)
	  throw new IllegalArgumentException(str);
	else
	  return new Date(d.getTime());
      }
    catch (ParseException e)
      {
	throw new IllegalArgumentException(str);
      }
  }

  /**
   * This method returns this date in JDBC format.
   *
   * @return This date as a string.
   *
   * @deprecated
   */
  public String toString()
  {
    return sdf.format(this);
  }
}
