/* Time.java -- Wrapper around java.util.Date
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
  * driver to identify the value as a SQL Timestamp.  Note that this
  * class also adds an additional field for nano-seconds, and so 
  * is not completely identical to <code>java.util.Date</code> as
  * the <code>java.sql.Date</code> and <code>java.sql.Time</code>
  * classes are.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class Timestamp extends java.util.Date implements java.io.Serializable
{

/*
 * Class Variables
 */

/**
  * Used for parsing and formatting this date.
  */
  // Millisecond will have to be close enough for now.
private static SimpleDateFormat parse_sdf = 
  new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSS");

private static SimpleDateFormat format_sdf =
  new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * @serial The nanosecond value for this object
  */
private int nanos;

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
  * @return The resulting <code>java.sql.Timestamp</code> value. 
  */
public static Timestamp
valueOf(String str)
{
  try
    {
      Date d = (Date)parse_sdf.parseObject(str);
      return(new Timestamp(d.getTime()));
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
  * @param year The year for this Timestamp (year - 1900)
  * @param month The month for this Timestamp (0-11)
  * @param day The day for this Timestamp (1-31)
  * @param hour The hour for this Timestamp (0-23)
  * @param minute The minute for this Timestamp (0-59)
  * @param second The second for this Timestamp (0-59)
  * @param nanos The nanosecond value for this Timestamp (0 to 999,999,9999)
  *
  * @deprecated
  */
public 
Timestamp(int year, int month, int day, int hour, int minute, int second,
          int nanos)
{
  super(year, month, day, hour, minute, second);

  this.nanos = nanos;
}

/*************************************************************************/

/**
  * This method initializes a new instance of this class with the
  * specified time value representing the number of seconds since 
  * Jan 1, 1970 at 12:00 midnight GMT.
  *
  * @param time The time value to intialize this <code>Time</code> to.
  */
public
Timestamp(long date)
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
  return(format_sdf.format(this) + "." + getNanos());
}

/*************************************************************************/

/**
  * This method returns the nanosecond value for this object.
  *
  * @return The nanosecond value for this object.
  */
public int
getNanos()
{
  return(nanos);
}

/*************************************************************************/

/**
  * This method sets the nanosecond value for this object.
  *
  * @param nanos The nanosecond value for this object.
  */
public void
setNanos(int nanos)
{
  this.nanos = nanos;
}

/*************************************************************************/

/**
  * This methods tests whether this object is earlier than the specified
  * object.
  *
  * @param ts The other <code>Timestamp</code> to test against.
  *
  * @return <code>true</code> if this object is earlier than the other object,
  * <code>false</code> otherwise.
  */
public boolean
before(Timestamp ts)
{
  if (ts.getTime() > getTime())
    return(true);

  if (ts.getNanos() > getNanos())
    return(true);

  return(false);
}

/*************************************************************************/

/**
  * This methods tests whether this object is later than the specified
  * object.
  *
  * @param ts The other <code>Timestamp</code> to test against.
  *
  * @return <code>true</code> if this object is later than the other object,
  * <code>false</code> otherwise.
  */
public boolean
after(Timestamp ts)
{
  if (ts.getTime() < getTime())
    return(true);

  if (ts.getNanos() < getNanos())
    return(true);

  return(false);
}

/*************************************************************************/

/**
  * This method these the specified <code>Object</code> for equality
  * against this object.  This will be true if an only if the specified
  * object is an instance of <code>Timestamp</code> and has the same
  * time value fields.
  *
  * @param obj The object to test against for equality.
  *
  * @return <code>true</code> if the specified object is equal to this
  * object, <code>false</code> otherwise.
  */
public boolean
equals(Object obj)
{
  if (obj == null)
    return(false);

  if (!(obj instanceof Timestamp))
    return(false);

  return(equals((Timestamp)obj));
}

/*************************************************************************/

/**
  * This method tests the specified timestamp for equality against this
  * object.  This will be true if and only if the specified object is
  * not <code>null</code> and contains all the same time value fields
  * as this object.
  *
  * @param ts The <code>Timestamp</code> to test against for equality.
  *
  * @return <code>true</code> if the specified object is equal to this
  * object, <code>false</code> otherwise.
  */
public boolean
equals(Timestamp ts)
{
  if (ts == null)
    return(false);

  if (ts.getTime() != getTime())
    return(false);

  if (ts.getNanos() != getNanos())
    return(false);

  return(true);
}

} // class Timestamp

