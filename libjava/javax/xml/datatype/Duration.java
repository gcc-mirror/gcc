/* Duration.java -- 
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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

import java.math.BigDecimal;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import javax.xml.datatype.DatatypeConstants;
import javax.xml.namespace.QName;

/**
 * An immutable time space as specified in XML Schema 1.0.
 *
 * @author (a href='mailto:dog@gnu.org'>Chris Burdess</a)
 * @since 1.3
 */
public abstract class Duration
{

  /**
   * Returns the name of the XML Schema data type this value maps to.
   */
  public QName getXMLSchemaType()
  {
    int state = 0;
    state |= isSet(DatatypeConstants.YEARS) ? 32 : 0;
    state |= isSet(DatatypeConstants.MONTHS) ? 16 : 0;
    state |= isSet(DatatypeConstants.DAYS) ? 8 : 0;
    state |= isSet(DatatypeConstants.HOURS) ? 4 : 0;
    state |= isSet(DatatypeConstants.MINUTES) ? 2 : 0;
    state |= isSet(DatatypeConstants.SECONDS) ? 1 : 0;
    switch (state)
      {
      case 63:
        return DatatypeConstants.DURATION;
      case 15:
        return DatatypeConstants.DURATION_DAYTIME;
      case 48:
        return DatatypeConstants.DURATION_YEARMONTH;
      default:
        throw new IllegalStateException();
      }
  }

  /**
   * Returns the sign of this value.
   */
  public abstract int getSign();

  /**
   * Returns the years in this duration as an int, or 0 if not present.
   */
  public int getYears()
  {
    Number val = getField(DatatypeConstants.YEARS);
    return (val == null) ? 0 : val.intValue();
  }
  
  /**
   * Returns the months in this duration as an int, or 0 if not present.
   */
  public int getMonths()
  {
    Number val = getField(DatatypeConstants.MONTHS);
    return (val == null) ? 0 : val.intValue();
  }
  
  /**
   * Returns the days in this duration as an int, or 0 if not present.
   */
  public int getDays()
  {
    Number val = getField(DatatypeConstants.DAYS);
    return (val == null) ? 0 : val.intValue();
  }
  
  /**
   * Returns the hours in this duration as an int, or 0 if not present.
   */
  public int getHours()
  {
    Number val = getField(DatatypeConstants.HOURS);
    return (val == null) ? 0 : val.intValue();
  }
  
  /**
   * Returns the minutes in this duration as an int, or 0 if not present.
   */
  public int getMinutes()
  {
    Number val = getField(DatatypeConstants.MINUTES);
    return (val == null) ? 0 : val.intValue();
  }
  
  /**
   * Returns the seconds in this duration as an int, or 0 if not present.
   */
  public int getSeconds()
  {
    Number val = getField(DatatypeConstants.SECONDS);
    return (val == null) ? 0 : val.intValue();
  }

  /**
   * Returns the duration length in milliseconds.
   * Because the length of a month or year may vary depending on the year,
   * the <code>startInstant</code> parameter is used to specify the duration
   * offset.
   */
  public long getTimeInMillis(Calendar startInstant)
  {
    Calendar cal = (Calendar) startInstant.clone();
    long t1 = cal.getTimeInMillis();
    addTo(cal);
    long t2 = cal.getTimeInMillis();
    return t2 - t1;
  }

  /**
   * Returns the duration length in milliseconds.
   * Because the length of a month or year may vary depending on the year,
   * the <code>startInstant</code> parameter is used to specify the duration
   * offset.
   */
  public long getTimeInMillis(Date startInstant)
  {
    Date date = (Date) startInstant.clone();
    long t1 = date.getTime();
    addTo(date);
    long t2 = date.getTime();
    return t2 - t1;
  }

  /**
   * Returns the value of the specified field, or <code>null</code> if the
   * field is undefined.
   */
  public abstract Number getField(DatatypeConstants.Field field);

  /**
   * Indicates whether the specified field is set.
   */
  public abstract boolean isSet(DatatypeConstants.Field field);

  /**
   * Returns the result of adding the specified duration to this duration.
   */
  public abstract Duration add(Duration rhs);

  /**
   * Adds this duration to the specified calendar.
   */
  public abstract void addTo(Calendar calendar);
  /*{
    switch (getSign())
      {
      case -1:
        calendar.add(Calendar.YEAR, -getYears());
        calendar.add(Calendar.MONTH, -getMonths());
        calendar.add(Calendar.DATE, -getDays());
        calendar.add(Calendar.HOUR, -getHours());
        calendar.add(Calendar.MINUTE, -getMinutes());
        calendar.add(Calendar.SECOND, -getSeconds());
        break;
      case 1:
        calendar.add(Calendar.YEAR, getYears());
        calendar.add(Calendar.MONTH, getMonths());
        calendar.add(Calendar.DATE, getDays());
        calendar.add(Calendar.HOUR, getHours());
        calendar.add(Calendar.MINUTE, getMinutes());
        calendar.add(Calendar.SECOND, getSeconds());
      }
  }*/
  
  /**
   * Adds this duration to the specified date.
   */
  public void addTo(Date date)
  {
    Calendar calendar = new GregorianCalendar();
    calendar.setTimeInMillis(date.getTime());
    addTo(calendar);
    date.setTime(calendar.getTimeInMillis());
  }

  /**
   * Returns the result of subtracting the given duration from this
   * duration.
   */
  public Duration subtract(Duration rhs)
  {
    // TODO
    throw new UnsupportedOperationException();
  }

  /**
   * Returns the result of multiplying this duration by the given factor.
   */
  public Duration multiply(int factor)
  {
    return multiply(BigDecimal.valueOf((long) factor));
  }

  /**
   * Returns the result of multiplying this duration by the given factor.
   */
  public Duration multiply(BigDecimal factor)
  {
    // TODO
    throw new UnsupportedOperationException();
  }

  /**
   * Returns the unary negative of this duration.
   */
  public abstract Duration negate();
  
  /**
   * Converts the years and months fields into the days field using a
   * specific time instant as the reference point.
   */
  public abstract Duration normalizeWith(Calendar startTimeInstant);

  /**
   * Partial order relation comparison with this duration, in accordance
   * with XML Schema 1.0 Part 2, Section 3.2.7.6.2.
   */
  public abstract int compare(Duration duration);

  public boolean isLongerThan(Duration duration)
  {
    // TODO
    throw new UnsupportedOperationException();
  }

  public boolean isShorterThan(Duration duration)
  {
    // TODO
    throw new UnsupportedOperationException();
  }

  public boolean equals(java.lang.Object duration)
  {
    // TODO
    throw new UnsupportedOperationException();
  }
  
  public abstract int hashCode();

  /**
   * Returns the lexical representation of this duration.
   */
  public String toString()
  {
    // TODO
    throw new UnsupportedOperationException();
  }
  
}
