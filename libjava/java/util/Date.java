/* java.util.Date
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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

/**
 * This class represents a specific time in milliseconds since the epoch.
 * The epoch is 1970, January 1 00:00:00.0000 UTC.  
 *
 * Date is intended to reflect universal time coordinate (UTC), but doesn't
 * handle the leap seconds.
 *
 * Prior to jdk 1.1 this class was the sole Time class and had also 
 * calendar functionality.  But this can't be localized, so a new Calendar
 * class was created, that you should use instead.  The functions which
 * get or return a year, month, day etc. are all deprecated and shouldn't be
 * used.  Use Calendar instead.
 * 
 * @see Calendar
 * @see GregorianCalendar
 * @see java.text.DateFormat
 * @author Jochen Hoenicke
 * @author Per Bothner <bothner@cygnus.com>
 */
public class Date implements Cloneable, Comparable, java.io.Serializable
{
  /**
   * This is the serialization UID for this class
   */
  private static final long serialVersionUID = 7523967970034938905L;

  /**
   * The time in milliseconds since the epoch.
   */
  private transient long time;

  /**
   * Creates a new Date Object representing the current time.
   */
  public Date()
  {
    time = System.currentTimeMillis();
  }

  /**
   * Creates a new Date Object representing the given time.
   * @param time the time in milliseconds since the epoch.
   */
  public Date(long time)
  {
    this.time = time;
  }

  /**
   * Creates a new Date Object representing the given time.
   * @deprecated use <code>new GregorianCalendar(year+1900, month,
   * day)</code> instead.  
   */
  public Date(int year, int month, int day)
  {
    time = new GregorianCalendar(year + 1900, month, day).getTimeInMillis();
  }

  /**
   * Creates a new Date Object representing the given time.
   * @deprecated use <code>new GregorianCalendar(year+1900, month,
   * day, hour, min)</code> instead.  
   */
  public Date(int year, int month, int day, int hour, int min)
  {
    time =
      new GregorianCalendar(year + 1900, month, day, hour,
			    min).getTimeInMillis();
  }

  /*
   * Creates a new Date Object representing the given time.
   * @deprecated use <code>new GregorianCalendar(year+1900, month,
   * day)</code> instead.  
   */
  public Date(int year, int month, int day, int hour, int min, int sec)
  {
    time =
      new GregorianCalendar(year + 1900, month, day, hour, min,
			    sec).getTimeInMillis();
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
   * @deprecated Use Calendar with a UTC TimeZone instead.
   * @return the time in millis since the epoch.
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
   * Gets the time represented by this Object
   * @return the time in milliseconds since the epoch.
   */
  public long getTime()
  {
    return time;
  }

  /**
   * @deprecated use
   * Calendar.get(Calendar.ZONE_OFFSET)+Calendar.get(Calendar.DST_OFFSET)
   * instead.
   * @return The time zone offset in minutes of the local time zone
   * relative to UTC.  The time represented by this object is used to
   * determine if we should use daylight savings.
   */
  public int getTimezoneOffset()
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    return (cal.get(Calendar.ZONE_OFFSET)
	    + cal.get(Calendar.DST_OFFSET)) / (60 * 1000);
  }

  /**
   * Sets the time which this Object should represented.
   * @param time the time in milliseconds since the epoch.  */
  public void setTime(long time)
  {
    this.time = time;
  }

  /**
   * Tests if this date is after the specified date.
   * @param when the other date
   * @return true, if the date represented by this Object is
   * strictly later than the time represented by when.  
   */
  public boolean after(Date when)
  {
    return time > when.time;
  }

  /**
   * Tests if this date is before the specified date.
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
   * @param obj the object to compare.
   * @return true, if obj is a Date object and the date represented
   * by obj is exactly the same as the time represented by this
   * object.  
   */
  public boolean equals(Object obj)
  {
    return (obj instanceof Date && time == ((Date) obj).time);
  }

  /**
   * Compares two dates.
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
   * Compares this Date to another.  This behaves like
   * <code>compareTo(Date)</code>, but it may throw a
   * <code>ClassCastException</code>
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

  public int hashCode()
  {
    return (int) time ^ (int) (time >>> 32);
  }

  private static final String[] weekNames = { "Sun", "Mon", "Tue", "Wed",
					      "Thu", "Fri", "Sat" };

  private static final String[] monthNames = { "Jan", "Feb", "Mar", "Apr",
					       "May", "Jun", "Jul", "Aug",
					       "Sep", "Oct", "Nov", "Dec" };

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

  /** Format this object in a locale-specific way.
   * @deprecated Use DateFormat.format(Date)
   */
  public String toLocaleString()
  {
    return java.text.DateFormat.getInstance().format(this);
  }

  /** Format this object in a standard format in the GMT timezone.
   * @deprecated Use DateFormat.format(Date) with a GMT TimeZone.
   */
  public String toGMTString()
  {
    java.text.DateFormat format = java.text.DateFormat.getInstance();
    format.setTimeZone(TimeZone.getTimeZone("GMT"));
    return format.format(this);
  }

  private static int skipParens(String string, int offset)
  {
    int len = string.length();
    int p = 0;
    int i;

    for (i = offset; i < len; ++i)
      {
	if (string.charAt(i) == '(')
	  ++p;
	else if (string.charAt(i) == ')')
	  {
	    --p;
	    if (p == 0)
	      return i + 1;
	    // If we've encounted unbalanced parens, just return the
	    // leftover one as an ordinary character.  It will be
	    // caught later in parsing and cause an
	    // IllegalArgumentException.
      	    if (p < 0)
	      return i;
	  }
      }

    // Not sure what to do if `p != 0' here.
    return i;
  }

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

  /** Parse a String and return the time it represents.
   * @param s The String to parse.
   * @deprecated Use DateFormat.parse(String)
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
    int off = 0;
    int openParenOffset, tmpMonth;
    while ((openParenOffset = string.indexOf('(', off)) >= 0)
      {
	// Copy part of string leading up to open paren.
	buf.append(string.substring(off, openParenOffset));
	off = skipParens(string, openParenOffset);
      }
    buf.append(string.substring(off));

    // Make all chars upper case to simplify comparisons later.
    // Also ignore commas; treat them as delimiters.
    StringTokenizer strtok =
      new StringTokenizer(buf.toString().toUpperCase(), " \t\n\r,");

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
	        // A colon or slash may be valid in the number.
	        // Find the first of these before calling parseInt.
	        int colon = tok.indexOf(':');
	        int slash = tok.indexOf('/');
	        int hyphen = tok.indexOf('-');
		// We choose tok.length initially because it makes
		// processing simpler.
	        int punctOffset = tok.length();
		if (colon >= 0)
		  punctOffset = Math.min(punctOffset, colon);
	        if (slash >= 0)
	          punctOffset = Math.min(punctOffset, slash);
	        if (hyphen >= 0)
	          punctOffset = Math.min(punctOffset, hyphen);
		// Following code relies on -1 being the exceptional
		// case.
		if (punctOffset == tok.length())
		  punctOffset = -1;

	        int num;
	        try
	          {
		    num = Integer.parseInt(punctOffset < 0 ? tok :
					   tok.substring(0, punctOffset));
	          }
	        catch (NumberFormatException ex)
	          {
		    throw new IllegalArgumentException(tok);
	          }

		// TBD: Spec says year can be followed by a slash.  That might
		// make sense if using YY/MM/DD formats, but it would fail in
		// that format for years <= 70.  Also, what about 1900?  That
		// is interpreted as the year 3800; seems that the comparison
		// should be num >= 1900 rather than just > 1900.
		// What about a year of 62 - 70?  (61 or less could be a (leap)
		// second).  70/MM/DD cause an exception but 71/MM/DD is ok
		// even though there's no ambiguity in either case.
		// For the parse method, the spec as written seems too loose.
		// Until shown otherwise, we'll follow the spec as written.
	        if (num > 70 && (punctOffset < 0 || punctOffset == slash))
		  year = num > 1900 ? num - 1900 : num;
		else if (punctOffset > 0 && punctOffset == colon)
		  {
		    if (hour < 0)
		      hour = num;
		    else
		      minute = num;
		  }
		else if (punctOffset > 0 && punctOffset == slash)
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
		if (punctOffset < 0 || punctOffset + 1 >= tok.length())
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

    // Unspecified minutes and seconds should default to 0.
    if (minute < 0)
      minute = 0;
    if (second < 0)
      second = 0;

    // Throw exception if any other fields have not been recognized and set.
    if (year < 0 || month < 0 || day < 0 || hour < 0)
      throw new IllegalArgumentException("Missing field");

    // Return the time in either local time or relative to GMT as parsed.
    // If no time-zone was specified, get the local one (in minutes) and
    // convert to milliseconds before adding to the UTC.
    return UTC(year, month, day, hour, minute, second) + (localTimezone ?
		new Date(year, month, day).getTimezoneOffset() * 60 * 1000:
		-timezone * 60 * 1000);
  }

  /**
   * @return the year minus 1900 represented by this date object.
   * @deprecated Use Calendar instead of Date, and use get(Calendar.YEAR)
   * instead.  Note about the 1900 difference in year.
   */
  public int getYear()
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    return cal.get(Calendar.YEAR) - 1900;
  }

  /**
   * Sets the year to year minus 1900, not changing the other fields.
   * @param year the year minus 1900.
   * @deprecated Use Calendar instead of Date, and use
   * set(Calendar.YEAR, year) instead.  Note about the 1900
   * difference in year.  
   */
  public void setYear(int year)
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    cal.set(Calendar.YEAR, 1900 + year);
    time = cal.getTimeInMillis();
  }

  /**
   * @return the month represented by this date object (zero based).
   * @deprecated Use Calendar instead of Date, and use get(Calendar.MONTH)
   * instead.
   */
  public int getMonth()
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    return cal.get(Calendar.MONTH);
  }

  /**
   * Sets the month to the given value, not changing the other fields.
   * @param month the month, zero based.
   * @deprecated Use Calendar instead of Date, and use
   * set(Calendar.MONTH, month) instead. 
   */
  public void setMonth(int month)
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    cal.set(Calendar.MONTH, month);
    time = cal.getTimeInMillis();
  }

  /**
   * @return the day of month represented by this date object.
   * @deprecated Use Calendar instead of Date, and use get(Calendar.DATE)
   * instead.
   */
  public int getDate()
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    return cal.get(Calendar.DATE);
  }

  /**
   * Sets the date to the given value, not changing the other fields.
   * @param date the date.
   * @deprecated Use Calendar instead of Date, and use
   * set(Calendar.DATE, date) instead. 
   */
  public void setDate(int date)
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    cal.set(Calendar.DATE, date);
    time = cal.getTimeInMillis();
  }

  /**
   * @return the day represented by this date object.
   * @deprecated Use Calendar instead of Date, and use get(Calendar.DAY_OF_WEEK)
   * instead.
   */
  public int getDay()
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    // For Calendar, Sunday is 1.  For Date, Sunday is 0.
    return cal.get(Calendar.DAY_OF_WEEK) - 1;
  }

  /**
   * @return the hours represented by this date object.
   * @deprecated Use Calendar instead of Date, and use get(Calendar.HOUR_OF_DAY)
   * instead.
   */
  public int getHours()
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    return cal.get(Calendar.HOUR_OF_DAY);
  }

  /**
   * Sets the hours to the given value, not changing the other fields.
   * @param hours the hours.
   * @deprecated Use Calendar instead of Date, and use
   * set(Calendar.HOUR_OF_DAY, hours) instead. 
   */
  public void setHours(int hours)
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    cal.set(Calendar.HOUR_OF_DAY, hours);
    time = cal.getTimeInMillis();
  }

  /**
   * @return the minutes represented by this date object.
   * @deprecated Use Calendar instead of Date, and use get(Calendar.MINUTE)
   * instead.
   */
  public int getMinutes()
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    return cal.get(Calendar.MINUTE);
  }

  /**
   * Sets the minutes to the given value, not changing the other fields.
   * @param minutes the minutes.
   * @deprecated Use Calendar instead of Date, and use
   * set(Calendar.MINUTE, minutes) instead. 
   */
  public void setMinutes(int minutes)
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    cal.set(Calendar.MINUTE, minutes);
    time = cal.getTimeInMillis();
  }

  /**
   * @return the seconds represented by this date object.
   * @deprecated Use Calendar instead of Date, and use get(Calendar.SECOND)
   * instead.
   */
  public int getSeconds()
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    return cal.get(Calendar.SECOND);
  }

  /**
   * Sets the seconds to the given value, not changing the other fields.
   * @param seconds the seconds.
   * @deprecated Use Calendar instead of Date, and use
   * set(Calendar.SECOND, seconds) instead. 
   */
  public void setSeconds(int seconds)
  {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    cal.set(Calendar.SECOND, seconds);
    time = cal.getTimeInMillis();
  }

  /**
   * Reads an Object from the stream.
   */
  private void readObject(java.io.ObjectInputStream input)
    throws java.io.IOException, ClassNotFoundException
  {
    input.defaultReadObject();
    time = input.readLong();
  }

  /**
   * Writes an Object to the stream.
   * @serialdata A long value representing the offset from the epoch
   * in milliseconds.  This is the same value that is returned by the
   * method getTime().
   */
  private void writeObject(java.io.ObjectOutputStream output)
    throws java.io.IOException
  {
    output.defaultWriteObject();
    output.writeLong(time);
  }
}
