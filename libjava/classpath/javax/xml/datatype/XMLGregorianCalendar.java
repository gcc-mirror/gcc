/* XMLGregorianCalendar.java --
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
import java.math.BigInteger;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;
import javax.xml.namespace.QName;

/**
 * An XML Schema 1.0 date/time data type.
 *
 * @author (a href='mailto:dog@gnu.org'>Chris Burdess</a)
 * @since 1.3
 */
public abstract class XMLGregorianCalendar
  implements Cloneable
{

  /**
   * Resets all fields to undefined.
   */
  public abstract void clear();

  /**
   * Resets all fields to their original values.
   */
  public abstract void reset();

  public abstract void setYear(BigInteger year);

  public abstract void setYear(int year);

  public abstract void setMonth(int month);

  public abstract void setDay(int day);

  public abstract void setTimezone(int offset);

  public void setTime(int hour, int minute, int second)
  {
    setHour(hour);
    setMinute(minute);
    setSecond(second);
  }

  public abstract void setHour(int hour);

  public abstract void setMinute(int minute);

  public abstract void setSecond(int second);

  public abstract void setMillisecond(int millisecond);

  public abstract void setFractionalSecond(BigDecimal fractional);

  public void setTime(int hour, int minute, int second, BigDecimal fractional)
  {
    setHour(hour);
    setMinute(minute);
    setSecond(second);
    setFractionalSecond(fractional);
  }

  public void setTime(int hour, int minute, int second, int millisecond)
  {
    setHour(hour);
    setMinute(minute);
    setSecond(second);
    setMillisecond(millisecond);
  }

  public abstract BigInteger getEon();

  public abstract int getYear();

  public abstract BigInteger getEonAndYear();

  public abstract int getMonth();

  public abstract int getDay();

  public abstract int getTimezone();

  public abstract int getHour();

  public abstract int getMinute();

  public abstract int getSecond();

  public int getMillisecond()
  {
    BigDecimal factor = BigDecimal.valueOf(1000L);
    BigDecimal val = getFractionalSecond().multiply(factor);
    return val.intValue();
  }

  public abstract BigDecimal getFractionalSecond();

  public abstract int compare(XMLGregorianCalendar xmlGregorianCalendar);

  public abstract XMLGregorianCalendar normalize();

  public boolean equals(Object obj)
  {
    if (obj instanceof XMLGregorianCalendar)
      {
        XMLGregorianCalendar xgc = (XMLGregorianCalendar) obj;
        BigInteger y1 = getEonAndYear();
        BigInteger y2 = xgc.getEonAndYear();
        BigDecimal f1 = getFractionalSecond();
        BigDecimal f2 = xgc.getFractionalSecond();
        return ((y1 == null && y2 == null) || (y1 != null && y1.equals(y2))) &&
          getMonth() == xgc.getMonth() &&
          getDay() == xgc.getDay() &&
          getTimezone() == xgc.getTimezone() &&
          getHour() == xgc.getHour() &&
          getMinute() == xgc.getMinute() &&
          getSecond() == xgc.getSecond() &&
          ((f1 == null && f2 == null) || (f1 != null && f1.equals(f2)));
      }
    return false;
  }

  public int hashCode()
  {
    int hash = 0;
    BigInteger y = getEonAndYear();
    BigDecimal f = getFractionalSecond();
    if (y != null)
      {
        hash *= 31 + y.hashCode();
      }
    hash *= 31 + getMonth();
    hash *= 31 + getDay();
    hash *= 31 + getTimezone();
    hash *= 31 + getHour();
    hash *= 31 + getMinute();
    hash *= 31 + getSecond();
    if (f != null)
      {
        hash *= 31 + f.hashCode();
      }
    return hash;
  }

  /**
   * Returns the XML Schema lexical representation of this calendar.
   */
  public abstract String toXMLFormat();

  public abstract QName getXMLSchemaType();

  public String toString()
  {
    return toXMLFormat();
  }

  /**
   * Determines the validity of this calendar by
   * <code>getXMLSchemaType</code> constraints.
   */
  public abstract boolean isValid();

  /**
   * Adds the specified duration to this calendar.
   */
  public abstract void add(Duration duration);

  public abstract GregorianCalendar toGregorianCalendar();

  public abstract GregorianCalendar toGregorianCalendar(TimeZone timezone,
                                                        Locale locale,
                                                        XMLGregorianCalendar defaults);

  public abstract TimeZone getTimeZone(int defaultZoneoffset);

  public abstract Object clone();

}
