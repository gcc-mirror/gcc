/* DatatypeFactory.java --
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

import java.io.File;
import java.io.FileInputStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.Properties;
import java.util.ServiceLoader;

/**
 * Factory class to create new datatype objects mapping XML to and from Java
 * objects.
 *
 * @author Chris Burdess
 * @since 1.5
 */
public abstract class DatatypeFactory
{

  /**
   * JAXP 1.3 default property name.
   */
  public static final String DATATYPEFACTORY_PROPERTY = "javax.xml.datatype.DatatypeFactory";

  /**
   * JAXP 1.3 default implementation class name.
   */
  public static final String DATATYPEFACTORY_IMPLEMENTATION_CLASS = "gnu.xml.datatype.JAXPDatatypeFactory";

  protected DatatypeFactory()
  {
  }

  /**
   * Returns a new factory instance.
   */
  public static DatatypeFactory newInstance()
    throws DatatypeConfigurationException
  {
    try
      {
        // 1. system property
        String className = System.getProperty(DATATYPEFACTORY_PROPERTY);
        if (className != null)
          return (DatatypeFactory) Class.forName(className).newInstance();
        // 2. jaxp.properties property
        File javaHome = new File(System.getProperty("java.home"));
        File javaHomeLib = new File(javaHome, "lib");
        File jaxpProperties = new File(javaHomeLib, "jaxp.properties");
        if (jaxpProperties.exists())
          {
            FileInputStream in = new FileInputStream(jaxpProperties);
            Properties p = new Properties();
            p.load(in);
            in.close();
            className = p.getProperty(DATATYPEFACTORY_PROPERTY);
            if (className != null)
              return (DatatypeFactory) Class.forName(className).newInstance();
          }
        // 3. services
        Iterator<DatatypeFactory> i = ServiceLoader.load(DatatypeFactory.class).iterator();
        if (i.hasNext())
          return i.next();
        // 4. fallback
        Class<?> t = Class.forName(DATATYPEFACTORY_IMPLEMENTATION_CLASS);
        return (DatatypeFactory) t.newInstance();
      }
    catch (Exception e)
      {
        throw new DatatypeConfigurationException(e);
      }
  }

  /**
   * Returns a new duration from its string representation.
   * @param lexicalRepresentation the lexical representation of the
   * duration, as specified in XML Schema 1.0 section 3.2.6.1.
   */
  public abstract Duration newDuration(String lexicalRepresentation);

  /**
   * Returns a new duration.
   * @param durationInMilliSeconds the duration in milliseconds
   */
  public abstract Duration newDuration(long durationInMilliSeconds);

  /**
   * Returns a new duration by specifying the individual components.
   * @param isPositive whether the duration is positive
   * @param years the number of years
   * @param months the number of months
   * @param days the number of days
   * @param hours the number of hours
   * @param minutes th number of minutes
   * @param seconds the number of seconds
   */
  public abstract Duration newDuration(boolean isPositive,
                                       BigInteger years,
                                       BigInteger months,
                                       BigInteger days,
                                       BigInteger hours,
                                       BigInteger minutes,
                                       BigDecimal seconds);

  /**
   * Returns a new duration by specifying the individual components.
   * @param isPositive whether the duration is positive
   * @param years the number of years
   * @param months the number of months
   * @param days the number of days
   * @param hours the number of hours
   * @param minutes th number of minutes
   * @param seconds the number of seconds
   */
  public Duration newDuration(boolean isPositive,
                              int years,
                              int months,
                              int days,
                              int hours,
                              int minutes,
                              int seconds)
  {
    return newDuration(isPositive,
                       BigInteger.valueOf((long) years),
                       BigInteger.valueOf((long) months),
                       BigInteger.valueOf((long) days),
                       BigInteger.valueOf((long) hours),
                       BigInteger.valueOf((long) minutes),
                       BigDecimal.valueOf((long) seconds));
  }

  /**
   * Returns a new dayTimeDuration from its string representation.
   * @param lexicalRepresentation the lexical representation of the
   * duration, as specified in XML Schema 1.0 section 3.2.6.1.
   */
  public Duration newDurationDayTime(String lexicalRepresentation)
  {
    return newDuration(lexicalRepresentation);
  }

  /**
   * Returns a new dayTimeDuration.
   * @param durationInMilliseconds the duration in milliseconds
   */
  public Duration newDurationDayTime(long durationInMilliseconds)
  {
    // TODO xmlSchemaType
    return newDuration(durationInMilliseconds);
  }

  /**
   * Returns a new dayTimeDuration by specifying the individual components.
   * @param isPositive whether the duration is positive
   * @param days the number of days
   * @param hours the number of hours
   * @param minutes th number of minutes
   * @param seconds the number of seconds
   */
  public Duration newDurationDayTime(boolean isPositive,
                                     BigInteger days,
                                     BigInteger hours,
                                     BigInteger minutes,
                                     BigInteger seconds)
  {
    return newDuration(isPositive,
                       null,
                       null,
                       days,
                       hours,
                       minutes,
                       new BigDecimal(seconds));
  }

  /**
   * Returns a new dayTimeDuration by specifying the individual components.
   * @param isPositive whether the duration is positive
   * @param days the number of days
   * @param hours the number of hours
   * @param minutes th number of minutes
   * @param seconds the number of seconds
   */
  public Duration newDurationDayTime(boolean isPositive,
                                     int days,
                                     int hours,
                                     int minutes,
                                     int seconds)
  {
    return newDuration(isPositive,
                       null,
                       null,
                       BigInteger.valueOf((long) days),
                       BigInteger.valueOf((long) hours),
                       BigInteger.valueOf((long) minutes),
                       BigDecimal.valueOf((long) seconds));
  }

  /**
   * Returns a new yearMonthDuration from its string representation.
   * @param lexicalRepresentation the lexical representation of the
   * duration, as specified in XML Schema 1.0 section 3.2.6.1.
   */
  public Duration newDurationYearMonth(String lexicalRepresentation)
  {
    return newDuration(lexicalRepresentation);
  }

  /**
   * Returns a new yearMonthDuration.
   * @param durationInMilliseconds the duration in milliseconds
   */
  public Duration newDurationYearMonth(long durationInMilliseconds)
  {
    // TODO xmlSchemaType
    return newDuration(durationInMilliseconds);
  }

  /**
   * Returns a new yearMonthDuration by specifying the individual components.
   * @param isPositive whether the duration is positive
   * @param years the number of years
   * @param months the number of months
   */
  public Duration newDurationYearMonth(boolean isPositive,
                                       BigInteger years,
                                       BigInteger months)
  {
    return newDuration(isPositive,
                       years,
                       months,
                       null,
                       null,
                       null,
                       null);
  }

  /**
   * Returns a new yearMonthDuration by specifying the individual components.
   * @param isPositive whether the duration is positive
   * @param years the number of years
   * @param months the number of months
   */
  public Duration newDurationYearMonth(boolean isPositive,
                                       int years,
                                       int months)
  {
    return newDuration(isPositive,
                       BigInteger.valueOf((long) years),
                       BigInteger.valueOf((long) months),
                       null,
                       null,
                       null,
                       null);
  }

  /**
   * Returns a new XMLGregorianCalendar with no fields initialized.
   */
  public abstract XMLGregorianCalendar newXMLGregorianCalendar();

  /**
   * Returns a new XMLGregorianCalendar from a string representation.
   * @param lexicalRepresentation the lexical representation as specified in
   * XML Schema 1.0 Part 2, section 3.2.[7-14].1.
   */
  public abstract XMLGregorianCalendar newXMLGregorianCalendar(String lexicalRepresentation);

  /**
   * Returns a new XMLGregorianCalendar based on the specified Gregorian
   * calendar.
   */
  public abstract XMLGregorianCalendar newXMLGregorianCalendar(GregorianCalendar cal);

  /**
   * Returns a new XMLGregorianCalendar with the specified components.
   */
  public abstract XMLGregorianCalendar newXMLGregorianCalendar(BigInteger year,
                                                               int month,
                                                               int day,
                                                               int hour,
                                                               int minute,
                                                               int second,
                                                               BigDecimal fractionalSecond,
                                                               int timezone);

  /**
   * Returns a new XMLGregorianCalendar with the specified components.
   */
  public XMLGregorianCalendar newXMLGregorianCalendar(int year,
                                                      int month,
                                                      int day,
                                                      int hour,
                                                      int minute,
                                                      int second,
                                                      int millisecond,
                                                      int timezone)
  {
    return newXMLGregorianCalendar(BigInteger.valueOf((long) year),
                                   month,
                                   day,
                                   hour,
                                   minute,
                                   second,
                                   new BigDecimal(((double) millisecond) / 1000.0),
                                   timezone);
  }

  /**
   * Returns a new XMLGregorianCalendar with the specified components.
   */
  public XMLGregorianCalendar newXMLGregorianCalendarDate(int year,
                                                          int month,
                                                          int day,
                                                          int timezone)
  {
    return newXMLGregorianCalendar(BigInteger.valueOf((long) year),
                                   month,
                                   day,
                                   DatatypeConstants.FIELD_UNDEFINED,
                                   DatatypeConstants.FIELD_UNDEFINED,
                                   DatatypeConstants.FIELD_UNDEFINED,
                                   null,
                                   timezone);
  }

  /**
   * Returns a new XMLGregorianCalendar with the specified components.
   */
  public XMLGregorianCalendar newXMLGregorianCalendarTime(int hours,
                                                          int minutes,
                                                          int seconds,
                                                          int timezone)
  {
    return newXMLGregorianCalendar(null,
                                   DatatypeConstants.FIELD_UNDEFINED,
                                   DatatypeConstants.FIELD_UNDEFINED,
                                   hours,
                                   minutes,
                                   seconds,
                                   null,
                                   timezone);
  }

  /**
   * Returns a new XMLGregorianCalendar with the specified components.
   */
  public XMLGregorianCalendar newXMLGregorianCalendarTime(int hours,
                                                          int minutes,
                                                          int seconds,
                                                          BigDecimal fractionalSecond,
                                                          int timezone)
  {
    return newXMLGregorianCalendar(null,
                                   DatatypeConstants.FIELD_UNDEFINED,
                                   DatatypeConstants.FIELD_UNDEFINED,
                                   hours,
                                   minutes,
                                   seconds,
                                   fractionalSecond,
                                   timezone);
  }

  /**
   * Returns a new XMLGregorianCalendar with the specified components.
   */
  public XMLGregorianCalendar newXMLGregorianCalendarTime(int hours,
                                                          int minutes,
                                                          int seconds,
                                                          int milliseconds,
                                                          int timezone)
  {
    return newXMLGregorianCalendar(null,
                                   DatatypeConstants.FIELD_UNDEFINED,
                                   DatatypeConstants.FIELD_UNDEFINED,
                                   hours,
                                   minutes,
                                   seconds,
                                   new BigDecimal(((double) milliseconds) / 1000.0),
                                   timezone);
  }

}
