/* java.util.Date
   Copyright (C) 1998, 1999, 2000, 2001, 2005  Free Software Foundation, Inc.

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

package java.util;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

/**
 * <p>
 * This class represents a specific time in milliseconds since the epoch.
 * The epoch is 1970, January 1 00:00:00.0000 UTC.  
 * </p>
 * <p>
 * <code>Date</code> is intended to reflect universal time coordinate (UTC),
 * but this depends on the underlying host environment.  Most operating systems 
 * don't handle the leap second, which occurs about once every year or
 * so.  The leap second is added to the last minute of the day on either
 * the 30th of June or the 31st of December, creating a minute 61 seconds
 * in length.
 * </p>
 * <p>
 * The representations of the date fields are as follows:
 * <ul>
 * <li>
 * Years are specified as the difference between the year
 * and 1900.  Thus, the final year used is equal to
 * 1900 + y, where y is the input value.
 * </li>
 * <li>
 * Months are represented using zero-based indexing,
 * making 0 January and 11 December.
 * </li>
 * <li>
 * Dates are represented with the usual values of
 * 1 through to 31.
 * </li>
 * <li>
 * Hours are represented in the twenty-four hour clock,
 * with integer values from 0 to 23.  12am is 0, and
 * 12pm is 12.
 * </li>
 * <li>
 * Minutes are again as usual, with values from 0 to 59.
 * </li>
 * <li>
 * Seconds are represented with the values 0 through to 61,
 * with 60 and 61 being leap seconds (as per the ISO C standard).
 * </li>
 * </ul>
 * </p>
 * <p>
 * Prior to JDK 1.1, this class was the sole class handling date and time
 * related functionality.  However, this particular solution was not
 * amenable to internationalization.  The new <code>Calendar</code>
 * class should now be used to handle dates and times, with <code>Date</code>
 * being used only for values in milliseconds since the epoch.  The
 * <code>Calendar</code> class, and its concrete implementations, handle
 * the interpretation of these values into minutes, hours, days, months
 * and years.  The formatting and parsing of dates is left to the
 * <code>DateFormat</code> class, which is able to handle the different
 * types of date format which occur in different locales.
 * </p>
 *
 * @see Calendar
 * @see GregorianCalendar
 * @see java.text.DateFormat
 * @author Jochen Hoenicke
 * @author Per Bothner (bothner@cygnus.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 */
public class Date
    implements Cloneable, Comparable, Serializable
{
  /**
   * This is the serialization UID for this class
   * for compatability with Sun's JDK.
   */
  private static final long serialVersionUID = 7523967970034938905L;

  /**
   * The time in milliseconds since the epoch.
   */
  private transient long time;

  /**
   * An array of week names used to map names to integer values.
   */
  private static final String[] weekNames = { "Sun", "Mon", "Tue", "Wed",
					      "Thu", "Fri", "Sat" };
  /**
   * An array of month names used to map names to integer values.
   */
  private static final String[] monthNames = { "Jan", "Feb", "Mar", "Apr",
					       "May", "Jun", "Jul", "Aug",
					       "Sep", "Oct", "Nov", "Dec" };
  /**
   * Creates a new Date Object representing the current time.
   */
  public Date()
  {
    time = System.currentTimeMillis();
  }

  /**
   * Creates a new Date Object representing the given time.
   *
   * @param time the time in milliseconds since the epoch.
   */
  public Date(long time)
  {
    this.time = time;
  }

  /**
   * Creates a new Date Object representing the given time.
   *
   * @deprecated use <code>new GregorianCalendar(year+1900, month,
   * day)</code> instead.
   * @param year the difference between the required year and 1900.
   * @param month the month as a value between 0 and 11.
   * @param day the day as a value between 0 and 31.
   */
  public Date(int year, int month, int day)
  {
    this(year, month, day, 0, 0, 0);
  }

  /**
   * Creates a new Date Object representing the given time.
   *
   * @deprecated use <code>new GregorianCalendar(year+1900, month,
   * day, hour, min)</code> instead.
   * @param year the difference between the required year and 1900.
   * @param month the month as a value between 0 and 11.
   * @param day the day as a value between 0 and 31.
   * @param hour the hour as a value between 0 and 23, in 24-hour
   *        clock notation.
   * @param min the minute as a value between 0 and 59.
   */
  public Date(int year, int month, int day, int hour, int min)
  {
    this(year, month, day, hour, min, 0);
  }

  /**
   * Creates a new Date Object representing the given time.
   *
   * @deprecated use <code>new GregorianCalendar(year+1900, month,
   * day, hour, min, sec)</code> instead. 
   * @param year the difference between the required year and 1900.
   * @param month the month as a value between 0 and 11.
   * @param day the day as a value between 0 and 31.
   * @param hour the hour as a value between 0 and 23, in 24-hour
   *        clock notation.
   * @param min the minute as a value between 0 and 59.
   * @param sec the second as a value between 0 and 61 (with 60
   *        and 61 being leap seconds).
   */
  public Date(int year, int month, int day, int hour, int min, int sec)
  {
    GregorianCalendar cal =
	new GregorianCalendar(year + 1900, month, day, hour, min, sec);
    time = cal.getTimeInMillis();
  }

  /**
   * Creates a new Date from the given string representation.  This
   * does the same as <code>new Date(Date.parse(s))</code>
   * @see #parse
   * @deprecated use <code>java.text.DateFormat.parse(s)</code> instead.  
   */
  public Date(String s)
  {
    time = parse(s);
  }

  /**
   * Returns a copy of this <code>Date</code> object.
   *
   * @return a copy, or null if the object couldn't be
   *         cloned.
   * @see Object#clone()
   */
  public Object clone()
  {
    try
      {
	return super.clone();
      }
    catch (CloneNotSupportedException ex)
      {
	return null;
      }
  }

  /**
   * Returns the number of milliseconds since the epoch
   * specified by the given arguments.  The arguments are
   * interpreted relative to UTC rather than the local
   * time zone.
   *
   * @deprecated Use <code>Calendar</code> with a UTC
   *             <code>TimeZone</code> instead.
   * @param year the difference between the required year and 1900.
   * @param month the month as a value between 0 and 11.
   * @param day the day as a value between 0 and 31.
   * @param hour the hour as a value between 0 and 23, in 24-hour
   *        clock notation.
   * @param min the minute as a value between 0 and 59.
   * @param sec the second as a value between 0 and 61 (with 60
   *        and 61 being leap seconds).
   * @return the time in milliseconds since the epoch.
   */
  public static long UTC(int year, int month, int date,
			 int hrs, int min, int sec)
  {
    GregorianCalendar cal =
      new GregorianCalendar(year + 1900, month, date, hrs, min, sec);
    cal.set(Calendar.ZONE_OFFSET, 0);
    cal.set(Calendar.DST_OFFSET, 0);
    return cal.getTimeInMillis();
  }

  /**
   * Gets the time represented by this object.
   *
   * @return the time in milliseconds since the epoch.
   */
  public long getTime()
  {
    return time;
  }

  /**
   * Returns the number of minutes offset used with UTC to give the time
   * represented by this object in the current time zone.  The date information
   * from this object is also used to determine whether or not daylight savings
   * time is in effect.  For example, the offset for the UK would be 0 if the
   * month of the date object was January, and 1 if the month was August.
   * 
   * @deprecated use
   * <code>Calendar.get(Calendar.ZONE_OFFSET)+Calendar.get(Calendar.DST_OFFSET)</code>
   * instead.
   * @return The time zone offset in minutes of the local time zone
   * relative to UTC.  The time represented by this object is used to
   * determine if we should use daylight savings.
   */
  public int getTimezoneOffset()
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    return - (cal.get(Calendar.ZONE_OFFSET)
	    + cal.get(Calendar.DST_OFFSET)) / (60 * 1000);
  }

  /**
   * Sets the time which this object should represent.
   *
   * @param time the time in milliseconds since the epoch.  
   */
  public void setTime(long time)
  {
    this.time = time;
  }

  /**
   * Tests if this date is after the specified date.
   *
   * @param when the other date
   * @return true, if the date represented by this object is
   * strictly later than the time represented by when.  
   */
  public boolean after(Date when)
  {
    return time > when.time;
  }

  /**
   * Tests if this date is before the specified date.
   *
   * @param when the other date
   * @return true, if the date represented by when is strictly later
   * than the time represented by this object.
   */
  public boolean before(Date when)
  {
    return time < when.time;
  }

  /**
   * Compares two dates for equality.
   *
   * @param obj the object to compare.
   * @return true, if obj is a Date object and the time represented
   * by obj is exactly the same as the time represented by this
   * object.  
   */
  public boolean equals(Object obj)
  {
    return (obj instanceof Date && time == ((Date) obj).time);
  }

  /**
   * Compares two dates.
   *
   * @param when the other date.
   * @return 0, if the date represented
   * by obj is exactly the same as the time represented by this
   * object, a negative if this Date is before the other Date, and
   * a positive value otherwise.  
   */
  public int compareTo(Date when)
  {
    return (time < when.time) ? -1 : (time == when.time) ? 0 : 1;
  }

  /**
   * Compares this Date to another object.  This behaves like
   * <code>compareTo(Date)</code>, but it takes a generic object
   * and throws a <code>ClassCastException</code> if obj is
   * not a <code>Date</code>.
   * 
   * @param obj the other date.
   * @return 0, if the date represented
   * by obj is exactly the same as the time represented by this
   * object, a negative if this Date is before the other Date, and
   * a positive value otherwise.  
   * @exception ClassCastException if obj is not of type Date.
   */
  public int compareTo(Object obj)
  {
    return compareTo((Date) obj);
  }

  /**
   * Computes the hash code of this <code>Date</code> as the
   * XOR of the most significant and the least significant
   * 32 bits of the 64 bit milliseconds value.
   *
   * @return the hash code.
   */
  public int hashCode()
  {
    return (int) time ^ (int) (time >>> 32);
  }

  /**
   * <p>
   * Returns a string representation of this date using
   * the following date format:
   * </p>
   * <p>
   * <code>day mon dd hh:mm:ss zz yyyy</code>
   * </p>
   * <p>where the fields used here are:
   * <ul>
   * <li>
   * <code>day</code> -- the day of the week
   * (Sunday through to Saturday).
   * </li>
   * <li>
   * <code>mon</code> -- the month (Jan to Dec).
   * </li>
   * <li>
   * <code>dd</code> -- the day of the month
   * as two decimal digits (01 to 31).
   * </li>
   * <li>
   * <code>hh</code> -- the hour of the day
   * as two decimal digits in 24-hour clock notation
   * (01 to 23).
   * </li>
   * <li>
   * <code>mm</code> -- the minute of the day
   * as two decimal digits (01 to 59).
   * </li>
   * <li>
   * <code>ss</code> -- the second of the day
   * as two decimal digits (01 to 61).
   * </li>
   * <li>
   * <code>zz</code> -- the time zone information if available.
   * The possible time zones used include the abbreviations
   * recognised by <code>parse()</code> (e.g. GMT, CET, etc.)
   * and may reflect the fact that daylight savings time is in
   * effect.  The empty string is used if there is no time zone
   * information.
   * </li>
   * <li>
   * <code>yyyy</code> -- the year as four decimal digits.
   * </li>
   * </ul>
   * <p>
   * The <code>DateFormat</code> class should now be 
   * preferred over using this method.
   * </p>
   *
   * @return A string of the form 'day mon dd hh:mm:ss zz yyyy'
   * @see #parse(String)
   * @see DateFormat
   */
  public String toString()
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    String day = "0" + cal.get(Calendar.DATE);
    String hour = "0" + cal.get(Calendar.HOUR_OF_DAY);
    String min = "0" + cal.get(Calendar.MINUTE);
    String sec = "0" + cal.get(Calendar.SECOND);
    String year = "000" + cal.get(Calendar.YEAR);
    return weekNames[cal.get(Calendar.DAY_OF_WEEK) - 1] + " "
      + monthNames[cal.get(Calendar.MONTH)] + " "
      + day.substring(day.length() - 2) + " "
      + hour.substring(hour.length() - 2) + ":"
      + min.substring(min.length() - 2) + ":"
      + sec.substring(sec.length() - 2) + " "
      +
      cal.getTimeZone().getDisplayName(cal.getTimeZone().inDaylightTime(this),
				       TimeZone.SHORT) + " " +
      year.substring(year.length() - 4);
  }

  /** 
   * Returns a locale-dependent string representation of this
   * <code>Date</code> object.
   *
   * @deprecated Use DateFormat.format(Date)
   * @return A locale-dependent string representation.
   * @see #parse(String)
   * @see DateFormat
   */
  public String toLocaleString()
  {
    return java.text.DateFormat.getInstance().format(this);
  }

  /** 
   * <p>
   * Returns a string representation of this <code>Date</code>
   * object using GMT rather than the local timezone.
   * The following date format is used:
   * </p>
   * <p>
   * <code>d mon yyyy hh:mm:ss GMT</code>
   * </p>
   * <p>where the fields used here are:
   * <ul>
   * <li>
   * <code>d</code> -- the day of the month
   * as one or two decimal digits (1 to 31).
   * </li>
   * <li>
   * <code>mon</code> -- the month (Jan to Dec).
   * </li>
   * <li>
   * <code>yyyy</code> -- the year as four decimal digits.
   * </li>
   * <li>
   * <code>hh</code> -- the hour of the day
   * as two decimal digits in 24-hour clock notation
   * (01 to 23).
   * </li>
   * <li>
   * <code>mm</code> -- the minute of the day
   * as two decimal digits (01 to 59).
   * </li>
   * <li>
   * <code>ss</code> -- the second of the day
   * as two decimal digits (01 to 61).
   * </li>
   * <li>
   * <code>GMT</code> -- the literal string "GMT"
   * indicating Greenwich Mean Time as opposed to
   * the local timezone.
   * </li>
   * </ul>
   * 
   * @deprecated Use DateFormat.format(Date) with a GMT TimeZone.
   * @return A string of the form 'd mon yyyy hh:mm:ss GMT' using
   *         GMT as opposed to the local timezone.
   * @see #parse(String)
   * @see DateFormat
   */
  public String toGMTString()
  {
    java.text.DateFormat format = java.text.DateFormat.getInstance();
    format.setTimeZone(TimeZone.getTimeZone("GMT"));
    return format.format(this);
  }

  /**
   * Parses the time zone string.
   *
   * @param tok The token containing the time zone.
   * @param sign The sign (+ or -) used by the time zone.
   * @return An integer representing the number of minutes offset
   *         from GMT for the time zone.
   */
  private static int parseTz(String tok, char sign)
    throws IllegalArgumentException
  {
    int num;

    try
      {
	// parseInt doesn't handle '+' so strip off sign.
	num = Integer.parseInt(tok.substring(1));
      }
    catch (NumberFormatException ex)
      {
	throw new IllegalArgumentException(tok);
      }

    // Convert hours to minutes.
    if (num < 24)
      num *= 60;
    else
      num = (num / 100) * 60 + num % 100;

    return sign == '-' ? -num : num;
  }

  /**
   * Parses the month string.
   *
   * @param tok the token containing the month.
   * @return An integer between 0 and 11, representing
   *         a month from January (0) to December (11),
   *         or -1 if parsing failed.
   */
  private static int parseMonth(String tok)
  {
    // Initialize strings for month names.
    // We could possibly use the fields of DateFormatSymbols but that is
    // localized and thus might not match the English words specified.
    String months[] = { "JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY",
			"JUNE", "JULY", "AUGUST", "SEPTEMBER", "OCTOBER",
			"NOVEMBER", "DECEMBER" };

    int i;
    for (i = 0; i < 12; i++)
      if (months[i].startsWith(tok))
        return i;

    // Return -1 if not found.
    return -1;
  }

  /**
   * Parses the day of the week string.
   *
   * @param tok the token containing the day of the week.
   * @return true if the token was parsed successfully.
   */
  private static boolean parseDayOfWeek(String tok)
  {
    // Initialize strings for days of the week names.
    // We could possibly use the fields of DateFormatSymbols but that is
    // localized and thus might not match the English words specified.
    String daysOfWeek[] = { "SUNDAY", "MONDAY", "TUESDAY", "WEDNESDAY",
			    "THURSDAY", "FRIDAY", "SATURDAY" };

    int i;
    for (i = 0; i < 7; i++)
      if (daysOfWeek[i].startsWith(tok))
        return true;

    return false;
  }

  /** 
   * <p>
   * Parses a String and returns the time, in milliseconds since the
   * epoch, it represents.  Most syntaxes are handled, including
   * the IETF date standard "day, dd mon yyyy hh:mm:ss zz" (see
   * <code>toString()</code> for definitions of these fields).
   * Standard U.S. time zone abbreviations are recognised, in
   * addition to time zone offsets in positive or negative minutes.
   * If a time zone is specified, the specified time is assumed to
   * be in UTC and the appropriate conversion is applied, following
   * parsing, to convert this to the local time zone.  If no zone
   * is specified, the time is assumed to already be in the local
   * time zone.
   * </p>
   * <p>
   * The method parses the string progressively from left to right.
   * At the end of the parsing process, either a time is returned
   * or an <code>IllegalArgumentException</code> is thrown to signify
   * failure.  The ASCII characters A-Z, a-z, 0-9, and ',', '+', '-',
   * ':' and '/' are the only characters permitted within the string,
   * besides whitespace and characters enclosed within parantheses
   * '(' and ')'.  
   * </p>
   * <p>
   * A sequence of consecutive digits are recognised as a number,
   * and interpreted as follows:
   * <ul>
   * <li>
   * A number preceded by a sign (+ or -) is taken to be a time zone
   * offset.  The time zone offset can be specified in either hours
   * or minutes.  The former is assumed if the number is less than 24.
   * Otherwise, the offset is assumed to be in minutes.  A - indicates
   * a time zone west of GMT, while a + represents a time zone to the
   * east of GMT.  The time zones are always assumed to be relative
   * to GMT, and a (redundant) specification of this can be included
   * with the time zone.  For example, '-9', 'utc-9' and 'GMT-9' all
   * represent a time zone nine hours west of GMT.  Similarly,
   * '+4', 'ut+4' and 'UTC+4' all give 4 hours east of GMT.
   * </li>
   * <li>
   * A number equal to or greater than 70 is regarded as a year specification.
   * Values lower than 70 are only assumed to indicate a year if both the
   * day of the month and the month itself have already been recognised.
   * Year values less than 100 are interpreted as being relative to the current
   * century when the <code>Date</code> class is initialised..  Given a century,
   * x, the year is assumed to be within the range x - 80 to x + 19.  The value
   * itself is then used as a match against the two last digits of one of these
   * years.  For example, take x to be 2004.  A two-digit year is assumed to fall
   * within the range x - 80 (1924) and x + 19 (2023).  Thus, any intepreted value
   * between 0 and 23 is assumed to be 2000 to 2023 and values between 24 and 99
   * are taken as being 1924 to 1999.  This only applies for the case of 2004.
   * With a different year, the values will be interpreted differently. 2005
   * will used 0 to 24 as 2000 to 2024 and 25 to 99 as 1925 to 1999, for example.
   * This behaviour differs from that of <code>SimpleDateFormat</code> and is
   * time-dependent (a two-digit year will be interpreted differently depending
   * on the time the code is run).
   * </li>
   * <li>
   * Numbers followed by a colon are interpreted by first an hour, and then
   * as a minute, once an hour has been found.
   * </li>
   * <li>
   * <li>
   * Numbers followed by a slash are regarded first as a month, and then as
   * a day of the month once the month has been found.  This follows the
   * U.S. date format of mm/dd, rather than the European dd/mm.  Months
   * are converted to the recognised value - 1 before storage, in order
   * to put the number within the range 0 to 11.
   * </li>
   * <li>
   * Numbers followed by commas, whitespace, hyphens or the end of the string
   * are interpreted in the following order: hour, minute, second, day of month.
   * The first type not already recognised in the current string being parsed is
   * assumed.
   * </li>
   * </ul>
   * </p>
   * <p>
   * A sequence of consecutive alphabetic characters is recognised as a word,
   * and interpreted as follows, in a case-insentive fashion:
   * <ul>
   * <li>
   * The characters 'AM' or 'PM' restrict the hour value to a value between 0
   * and 12.  In the latter case, 12 is added to the hour value before storage.
   * </li>
   * <li>
   * Any words which match any prefix of one of the days of the week ('Monday',
   * 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday' and 'Sunday'),
   * are simply ignored.
   * </li>
   * <li>
   * Any words which match any prefix of one of the months of the year ('January',
   * 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September',
   * 'October', 'November', 'December') are recognised and interpreted as the
   * appropriate value between 0 and 11.  The first match made against a
   * month is the one used, in the order specified here.  For example, 'Ma' is
   * intepreted as 'March' (2) and not as 'May' (4).  Similarly, 'Ju' is 'June',
   * and not 'July'.
   * </li>
   * <li>
   * The words 'GMT', 'UT' and 'UTC' are interpreted as specifying UTC as the
   * time zone in use for this date.
   * </li>
   * <li>
   * The word pairs 'EST'/'EDT', 'CST'/'CDT', 'MST'/'MDT' and 'PST'/'PDT' are
   * interpreted as the appropriate U.S. time zone abbreviation.  Each pair
   * is the standard and daylight savings time zone specification, respectively,
   * for each zone within the U.S, these being Eastern Standard/Daylight Time
   * (-5), Central Standard/Daylight Time (-6), Mountain Standard/Daylight Time
   * (-7) and Pacific Standard/Daylight Time (-8).
   * </li>
   * </ul>
   *
   * @param s The String to parse.
   * @return The time in milliseconds since the epoch.
   * @throws IllegalArgumentException if the string fails to parse.
   * @deprecated Use DateFormat.parse(String)
   * @see #toString()
   * @see SimpleDateFormat
   */
  public static long parse(String string)
  {
    // Initialize date/time fields before parsing begins.
    int year = -1;
    int month = -1;
    int day = -1;
    int hour = -1;
    int minute = -1;
    int second = -1;
    int timezone = 0;
    boolean localTimezone = true;

    // Trim out any nested stuff in parentheses now to make parsing easier.
    StringBuffer buf = new StringBuffer();
    int parenNesting = 0;
    int len = string.length();
    for (int i = 0;  i < len;  i++)
      {
	char ch = string.charAt(i);
	if (ch >= 'a' && ch <= 'z')
	  ch -= 'a' - 'A';
	if (ch == '(')
	  parenNesting++;
	else if (parenNesting == 0)
	  buf.append(ch);
	else if (ch == ')')
	  parenNesting--;
      }
    int tmpMonth;

    // Make all chars upper case to simplify comparisons later.
    // Also ignore commas; treat them as delimiters.
    StringTokenizer strtok = new StringTokenizer(buf.toString(), " \t\n\r,");

    while (strtok.hasMoreTokens())
      {
	String tok = strtok.nextToken();
	char firstch = tok.charAt(0);
	if ((firstch == '+' || firstch == '-') && year >= 0)
	  {
	    timezone = parseTz(tok, firstch);
	    localTimezone = false;
	  }
	else if (firstch >= '0' && firstch <= '9')
	  {
	    while (tok != null && tok.length() > 0)
	      {
		int punctOffset = tok.length();
		int num = 0;
		int punct;
		for (int i = 0;  ;  i++)
		  {
		    if (i >= punctOffset)
		      {
			punct = -1;
			break;
		      }
		    else
		      {
			punct = tok.charAt(i);
			if (punct >= '0' && punct <= '9')
			  {
			    if (num > 999999999) // in case of overflow
			      throw new IllegalArgumentException(tok);
			    num = 10 * num + (punct - '0');
			  }
			else
			  {
			    punctOffset = i;
			    break;
			  }
		      }
		      
		  }

		if (punct == ':')
		  {
		    if (hour < 0)
		      hour = num;
		    else
		      minute = num;
		  }
	        else if ((num >= 70
			  && (punct == ' ' || punct == ','
			      || punct == '/' || punct < 0))
			 || (num < 70 && day >= 0 && month >= 0 && year < 0))
		  {
		    if (num >= 100)
		      year = num;
		    else
		      {
			int curYear = 1900 + new Date().getYear();
			int firstYear = curYear - 80;
			year = firstYear / 100 * 100 + num;
			if (year < firstYear)
			  year += 100;
		      }
		  }
		else if (punct == '/')
		  {
		    if (month < 0)
		      month = num - 1;
		    else
		      day = num;
		  }
		else if (hour >= 0 && minute < 0)
		  minute = num;
		else if (minute >= 0 && second < 0)
		  second = num;
		else if (day < 0)
		  day = num;
		else
		  throw new IllegalArgumentException(tok);

		// Advance string if there's more to process in this token.
		if (punct < 0 || punctOffset + 1 >= tok.length())
		  tok = null;
		else
		  tok = tok.substring(punctOffset + 1);
	      }
	  }
	else if (firstch >= 'A' && firstch <= 'Z')
	  {
	    if (tok.equals("AM"))
	      {
		if (hour < 1 || hour > 12)
		  throw new IllegalArgumentException(tok);
		if (hour == 12)
		  hour = 0;
	      }
	    else if (tok.equals("PM"))
	      {
		if (hour < 1 || hour > 12)
		  throw new IllegalArgumentException(tok);
		if (hour < 12)
		  hour += 12;
	      }
	    else if (parseDayOfWeek(tok))
	      ; // Ignore it; throw the token away.
	    else if (tok.equals("UT") || tok.equals("UTC") || tok.equals("GMT"))
	      localTimezone = false;
	    else if (tok.startsWith("UT") || tok.startsWith("GMT"))
	      {
		int signOffset = 3;
		if (tok.charAt(1) == 'T' && tok.charAt(2) != 'C')
		  signOffset = 2;

	        char sign = tok.charAt(signOffset);
		if (sign != '+' && sign != '-')
		  throw new IllegalArgumentException(tok);

	        timezone = parseTz(tok.substring(signOffset), sign);
	        localTimezone = false;
	      }
	    else if ((tmpMonth = parseMonth(tok)) >= 0)
	      month = tmpMonth;
	    else if (tok.length() == 3 && tok.charAt(2) == 'T')
	      {
		// Convert timezone offset from hours to minutes.
		char ch = tok.charAt(0);
		if (ch == 'E')
		  timezone = -5 * 60;
		else if (ch == 'C')
		  timezone = -6 * 60;
		else if (ch == 'M')
		  timezone = -7 * 60;
		else if (ch == 'P')
		  timezone = -8 * 60;
		else
		  throw new IllegalArgumentException(tok);

		// Shift 60 minutes for Daylight Savings Time.
		if (tok.charAt(1) == 'D')
		  timezone += 60;
		else if (tok.charAt(1) != 'S')
		  throw new IllegalArgumentException(tok);

	        localTimezone = false;
	      }
	    else
	      throw new IllegalArgumentException(tok);
	  }
	else
	  throw new IllegalArgumentException(tok);
      }

    // Unspecified hours, minutes, or seconds should default to 0.
    if (hour < 0)
      hour = 0;
    if (minute < 0)
      minute = 0;
    if (second < 0)
      second = 0;

    // Throw exception if any other fields have not been recognized and set.
    if (year < 0 || month < 0 || day < 0)
      throw new IllegalArgumentException("Missing field");

    // Return the time in either local time or relative to GMT as parsed.
    // If no time-zone was specified, get the local one (in minutes) and
    // convert to milliseconds before adding to the UTC.
    GregorianCalendar cal
      = new GregorianCalendar(year, month, day, hour, minute, second);
    if (!localTimezone)
      {
	cal.set(Calendar.ZONE_OFFSET, timezone * 60 * 1000);
	cal.set(Calendar.DST_OFFSET, 0);
      }
    return cal.getTimeInMillis();
  }

  /**
   * Returns the difference between the year represented by this
   * <code>Date</code> object and 1900.
   *
   * @return the year minus 1900 represented by this date object.
   * @deprecated Use Calendar instead of Date, and use get(Calendar.YEAR)
   * instead.  Note the 1900 difference in the year.
   * @see Calendar
   * @see #setYear(int)
   */
  public int getYear()
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    return cal.get(Calendar.YEAR) - 1900;
  }

  /**
   * Sets the year to the specified year, plus 1900.  The other
   * fields are only altered as required to match the same date
   * and time in the new year.  Usually, this will mean that
   * the fields are not changed at all, but in the case of
   * a leap day or leap second, the fields will change in
   * relation to the existence of such an event in the new year.
   * For example, if the date specifies February the 29th, 2000,
   * then this will become March the 1st if the year is changed
   * to 2001, as 2001 is not a leap year.  Similarly, a seconds
   * value of 60 or 61 may result in the seconds becoming 0 and
   * the minute increasing by 1, if the new time does not include
   * a leap second.
   *
   * @param year the year minus 1900.
   * @deprecated Use Calendar instead of Date, and use
   * set(Calendar.YEAR, year) instead.  Note about the 1900
   * difference in year.  
   * @see #getYear()
   * @see Calendar
   */
  public void setYear(int year)
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    cal.set(Calendar.YEAR, 1900 + year);
    time = cal.getTimeInMillis();
  }

  /**
   * Returns the month represented by this <code>Date</code> object,
   * as a value between 0 (January) and 11 (December).
   *
   * @return the month represented by this date object (zero based).
   * @deprecated Use Calendar instead of Date, and use get(Calendar.MONTH)
   * instead.
   * @see #setMonth(int)
   * @see Calendar
   */
  public int getMonth()
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    return cal.get(Calendar.MONTH);
  }

  /**
   * Sets the month to the given value.  The other
   * fields are only altered as necessary to match
   * the same date and time in the new month.  In most
   * cases, the other fields won't change at all.  However,
   * in the case of a shorter month or a leap second, values
   * may be adjusted.  For example, if the day of the month
   * is currently 31, and the month value is changed from
   * January (0) to September (8), the date will become
   * October the 1st, as September only has 30 days.  Similarly,
   * a seconds value of 60 or 61 (a leap second) may result
   * in the seconds value being reset to 0 and the minutes
   * value being incremented by 1, if the new time does
   * not include a leap second.
   * 
   * @param month the month, with a zero-based index
   *        from January.
   * @deprecated Use Calendar instead of Date, and use
   * set(Calendar.MONTH, month) instead.
   * @see #getMonth()
   * @see Calendar 
   */
  public void setMonth(int month)
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    cal.set(Calendar.MONTH, month);
    time = cal.getTimeInMillis();
  }

  /**
   * Returns the day of the month of this <code>Date</code>
   * object, as a value between 0 and 31.
   *
   * @return the day of month represented by this date object.
   * @deprecated Use Calendar instead of Date, and use get(Calendar.DATE)
   * instead.
   * @see Calendar
   * @see #setDate(int)
   */
  public int getDate()
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    return cal.get(Calendar.DATE);
  }

  /**
   * Sets the date to the given value. The other
   * fields are only altered as necessary to match
   * the same date and time on the new day of the month.  In most
   * cases, the other fields won't change at all.  However,
   * in the case of a leap second or the day being out of
   * the range of the current month, values
   * may be adjusted.  For example, if the day of the month
   * is currently 30 and the month is June, a new day of the
   * month value of 31 will cause the month to change to July,
   * as June only has 30 days .  Similarly,
   * a seconds value of 60 or 61 (a leap second) may result
   * in the seconds value being reset to 0 and the minutes
   * value being incremented by 1, if the new time does
   * not include a leap second.
   *
   * @param date the date.
   * @deprecated Use Calendar instead of Date, and use
   * set(Calendar.DATE, date) instead. 
   * @see Calendar
   * @see #getDate()
   */
  public void setDate(int date)
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    cal.set(Calendar.DATE, date);
    time = cal.getTimeInMillis();
  }

  /**
   * Returns the day represented by this <code>Date</code>
   * object as an integer between 0 (Sunday) and 6 (Saturday).
   *
   * @return the day represented by this date object.
   * @deprecated Use Calendar instead of Date, and use get(Calendar.DAY_OF_WEEK)
   * instead.
   * @see Calendar
   */
  public int getDay()
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    // For Calendar, Sunday is 1.  For Date, Sunday is 0.
    return cal.get(Calendar.DAY_OF_WEEK) - 1;
  }

  /**
   * Returns the hours represented by this <code>Date</code>
   * object as an integer between 0 and 23.
   *
   * @return the hours represented by this date object.
   * @deprecated Use Calendar instead of Date, and use get(Calendar.HOUR_OF_DAY)
   * instead.
   * @see Calendar
   * @see #setHours(int)
   */
  public int getHours()
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    return cal.get(Calendar.HOUR_OF_DAY);
  }

  /**
   * Sets the hours to the given value.  The other
   * fields are only altered as necessary to match
   * the same date and time in the new hour.  In most
   * cases, the other fields won't change at all.  However,
   * in the case of a leap second, values
   * may be adjusted.  For example,
   * a seconds value of 60 or 61 (a leap second) may result
   * in the seconds value being reset to 0 and the minutes
   * value being incremented by 1 if the new hour does
   * not contain a leap second.
   *
   * @param hours the hours.
   * @deprecated Use Calendar instead of Date, and use
   * set(Calendar.HOUR_OF_DAY, hours) instead.
   * @see Calendar
   * @see #getHours() 
   */
  public void setHours(int hours)
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    cal.set(Calendar.HOUR_OF_DAY, hours);
    time = cal.getTimeInMillis();
  }

  /**
   * Returns the number of minutes represented by the <code>Date</code>
   * object, as an integer between 0 and 59.
   *
   * @return the minutes represented by this date object.
   * @deprecated Use Calendar instead of Date, and use get(Calendar.MINUTE)
   * instead.
   * @see Calendar
   * @see #setMinutes(int)
   */
  public int getMinutes()
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    return cal.get(Calendar.MINUTE);
  }

  /**
   * Sets the minutes to the given value.  The other
   * fields are only altered as necessary to match
   * the same date and time in the new minute.  In most
   * cases, the other fields won't change at all.  However,
   * in the case of a leap second, values
   * may be adjusted.  For example,
   * a seconds value of 60 or 61 (a leap second) may result
   * in the seconds value being reset to 0 and the minutes
   * value being incremented by 1 if the new minute does
   * not contain a leap second.
   *
   * @param minutes the minutes.
   * @deprecated Use Calendar instead of Date, and use
   * set(Calendar.MINUTE, minutes) instead. 
   * @see Calendar
   * @see #getMinutes()
   */
  public void setMinutes(int minutes)
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    cal.set(Calendar.MINUTE, minutes);
    time = cal.getTimeInMillis();
  }

  /**
   * Returns the number of seconds represented by the <code>Date</code>
   * object, as an integer between 0 and 61 (60 and 61 being leap seconds).
   *
   * @return the seconds represented by this date object.
   * @deprecated Use Calendar instead of Date, and use get(Calendar.SECOND)
   * instead.
   * @see Calendar
   * @see #setSeconds(int)
   */
  public int getSeconds()
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    return cal.get(Calendar.SECOND);
  }

  /**
   * Sets the seconds to the given value.  The other
   * fields are only altered as necessary to match
   * the same date and time in the new minute.  In most
   * cases, the other fields won't change at all.  However,
   * in the case of a leap second, values
   * may be adjusted.  For example, setting the
   * seconds value to 60 or 61 (a leap second) may result
   * in the seconds value being reset to 0 and the minutes
   * value being incremented by 1, if the current time does
   * not contain a leap second.
   *
   * @param seconds the seconds.
   * @deprecated Use Calendar instead of Date, and use
   * set(Calendar.SECOND, seconds) instead.
   * @see Calendar
   * @see #getSeconds() 
   */
  public void setSeconds(int seconds)
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    cal.set(Calendar.SECOND, seconds);
    time = cal.getTimeInMillis();
  }

  /**
   * Deserializes a <code>Date</code> object from an
   * input stream, setting the time (in milliseconds
   * since the epoch) to the long value read from the
   * stream.
   *
   * @param input the input stream.
   * @throws IOException if an I/O error occurs in the stream.
   * @throws ClassNotFoundException if the class of the
   *         serialized object could not be found.
   */
  private void readObject(ObjectInputStream input)
    throws IOException, ClassNotFoundException
  {
    input.defaultReadObject();
    time = input.readLong();
  }

  /**
   * Serializes a <code>Date</code> object to an output stream,
   * storing the time (in milliseconds since the epoch) as a long
   * value in the stream.
   *
   * @serialdata A long value representing the offset from the epoch
   * in milliseconds.  This is the same value that is returned by the
   * method getTime().
   * @param output the output stream.
   * @throws IOException if an I/O error occurs in the stream.
   */
  private void writeObject(ObjectOutputStream output)
    throws IOException
  {
    output.defaultWriteObject();
    output.writeLong(time);
  }

}
