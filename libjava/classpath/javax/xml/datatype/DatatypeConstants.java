/* DatatypeConstants.java --
   Copyright (C) 2004, 2005, 2006  Free Software Foundation, Inc.

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

package javax.xml.datatype;

import javax.xml.namespace.QName;

/**
 * Basic data type constants.
 *
 * @author (a href='mailto:dog@gnu.org'>Chris Burdess</a)
 * @since 1.5
 */
public final class DatatypeConstants
{

  private DatatypeConstants()
  {
    // to prevent instantiation
  }

  /**
   * Typesafe enumerated class representing the six fields of the
   * <a href='Duration.html'>Duration</a> class.
   */
  public static final class Field
  {

    final int id;
    final String name;

    Field(int id, String name)
    {
      this.id = id;
      this.name = name;
    }

    public int getId()
    {
      return id;
    }

    public String toString()
    {
      return name;
    }

  }

  /**
   * Value for January.
   */
  public static final int JANUARY = 1;

  /**
   * Value for February.
   */
  public static final int FEBRUARY = 2;

  /**
   * Value for March.
   */
  public static final int MARCH = 3;

  /**
   * Value for April.
   */
  public static final int APRIL = 4;

  /**
   * Value for May.
   */
  public static final int MAY = 5;

  /**
   * Value for June.
   */
  public static final int JUNE = 6;

  /**
   * Value for July.
   */
  public static final int JULY = 7;

  /**
   * Value for August.
   */
  public static final int AUGUST = 8;

  /**
   * Value for September.
   */
  public static final int SEPTEMBER = 9;

  /**
   * Value for October.
   */
  public static final int OCTOBER = 10;

  /**
   * Value for November.
   */
  public static final int NOVEMBER = 11;

  /**
   * Value for December.
   */
  public static final int DECEMBER = 12;

  /**
   * Comparison result.
   */
  public static final int LESSER = -1;

  /**
   * Comparison result.
   */
  public static final int EQUAL = 0;

  /**
   * Comparison result.
   */
  public static final int GREATER = 1;

  /**
   * Comparison result.
   */
  public static final int INDETERMINATE = 2;

  /**
   * Comparison result.
   */
  public static final int FIELD_UNDEFINED = -2147483648;

  /**
   * Constant that represents the years field.
   */
  public static final Field YEARS = new Field(1, "YEARS");

  /**
   * Constant that represents the months field.
   */
  public static final Field MONTHS = new Field(2, "MONTHS");

  /**
   * Constant that represents the days field.
   */
  public static final Field DAYS = new Field(3, "DAYS");

  /**
   * Constant that represents the hours field.
   */
  public static final Field HOURS = new Field(4, "HOURS");

  /**
   * Constant that represents the minutes field.
   */
  public static final Field MINUTES = new Field(5, "MINUTES");

  /**
   * Constant that represents the seconds field.
   */
  public static final Field SECONDS = new Field(6, "SECONDS");

  /**
   * The qualified-name for the <code>dateTime</code> data type.
   */
  public static final QName DATETIME = new QName ("http://www.w3.org/2001/XMLSchema#dateTime", "");

  /**
   * The qualified-name for the <code>time</code> data type.
   */
  public static final QName TIME = new QName ("http://www.w3.org/2001/XMLSchema#time", "");

  /**
   * The qualified-name for the <code>date</code> data type.
   */
  public static final QName DATE = new QName ("http://www.w3.org/2001/XMLSchema#date", "");

  /**
   * The qualified-name for the <code>gYearMonth</code> data type.
   */
  public static final QName GYEARMONTH = new QName ("http://www.w3.org/2001/XMLSchema#gYearMonth", "");

  /**
   * The qualified-name for the <code>gMonthDay</code> data type.
   */
  public static final QName GMONTHDAY = new QName ("http://www.w3.org/2001/XMLSchema#gMonthDay", "");

  /**
   * The qualified-name for the <code>gYear</code> data type.
   */
  public static final QName GYEAR = new QName ("http://www.w3.org/2001/XMLSchema#gYear", "");

  /**
   * The qualified-name for the <code>gMonth</code> data type.
   */
  public static final QName GMONTH = new QName ("http://www.w3.org/2001/XMLSchema#gMonth", "");

  /**
   * The qualified-name for the <code>gDay</code> data type.
   */
  public static final QName GDAY = new QName ("http://www.w3.org/2001/XMLSchema#gDay", "");

  /**
   * The qualified-name for the <code>duration</code> data type.
   */
  public static final QName DURATION = new QName ("http://www.w3.org/2001/XMLSchema#duration", "");

  /**
   * The qualified-name for the <code>dayTimeDuration</code> data type.
   */
  public static final QName DURATION_DAYTIME = new QName ("http://www.w3.org/2001/XMLSchema#dayTimeDuration", "");

  /**
   * The qualified-name for the <code>yearMonthDuration</code> data type.
   */
  public static final QName DURATION_YEARMONTH = new QName ("http://www.w3.org/2001/XMLSchema#yearMonthDuration", "");

  /**
   * XML Schema maximum timezone offset, in minutes.
   */
  public static final int MAX_TIMEZONE_OFFSET = -840;

  /**
   * XML Schema minimum timezone offset, in minutes.
   */
  public static final int MIN_TIMEZONE_OFFSET = 840;

}
