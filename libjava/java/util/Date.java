/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util;
import java.text.*;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date October 24, 1998.
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3,
 * "The Java Language Specification", ISBN 0-201-63451-1,
 * and O'Reilly's "Java in a Nutshell".
 * Status:  Need to re-write toString().
 *   Missing:  ToGMTString and toLocaleString.
 *   Serialization spec:  Specifies readObject/writeObject.
 */

public class Date implements java.io.Serializable, Cloneable
{
  private long millis;

  public Date() { millis = System.currentTimeMillis(); }

  public Date(long millis) { this.millis = millis; }

  public Date(int year, int month, int date, int hours,
	      int minutes, int seconds)
  {
    setTime(year, month, date, hours, minutes, seconds);
  }

  public Date(int year, int month, int date, int hours, int minutes)
  {
    setTime(year, month, date, hours, minutes, 0);
  }

  public Date(int year, int month, int date)
  {
    setTime(year, month, date, 0, 0, 0);
  }

  public Date (String s) { this(parse(s)); }

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

  public boolean after (Date when) { return this.millis > when.millis; }
  public boolean before (Date when) { return this.millis < when.millis; }

  public boolean equals(Object obj)
  {
    return (obj != null && obj instanceof Date
	    && ((Date)obj).millis == this.millis);
  }

  public long getTime() { return millis; }

  public int hashCode()
  {
    return (int)(millis^(millis>>>32));
  }

  private void setTime(int year, int month, int date,
		       int hours, int minutes, int seconds)
  {
    Calendar cal = new GregorianCalendar(year+1900, month, date,
					 hours, minutes, seconds);
    millis = cal.getTimeInMillis();
  }

  public void setTime(long millis) { this.millis = millis; }

  private int getField (int fld)
  {
    Calendar cal = new GregorianCalendar();
    cal.setTime(this);
    return cal.get(fld);
  }

  public int getYear ()
  {
    return getField(Calendar.YEAR) - 1900;
  }

  public int getMonth ()
  {
    return getField(Calendar.MONTH);
  }

  public int getDate ()
  {
    return getField(Calendar.DATE);
  }

  public int getDay ()
  {
    return getField(Calendar.DAY_OF_WEEK) - 1;
  }

  public int getHours ()
  {
    return getField(Calendar.HOUR_OF_DAY);
  }

  public int getMinutes ()
  {
    return getField(Calendar.MINUTE);
  }

  public int getSeconds ()
  {
    return getField(Calendar.SECOND);
  }

  private void setField (int fld, int value)
  {
    Calendar cal = new GregorianCalendar();
    cal.setTime(this);
    cal.set(fld, value);
    millis = cal.getTimeInMillis();
  }

  public void setYear (int year)
  {
    setField(Calendar.YEAR, 1900 + year);
  }

  public void setMonth (int month)
  {
    setField(Calendar.MONTH, month);
  }

  public void setDate (int date)
  {
    setField(Calendar.DATE, date);
  }

  public void setHours (int hours)
  {
    setField(Calendar.HOUR_OF_DAY, hours);
  }

  public void setMinutes (int minutes)
  {
    setField(Calendar.MINUTE, minutes);
  }

  public void setSeconds (int seconds)
  {
    setField(Calendar.SECOND, seconds);
  }

  public int getTimezoneOffset ()
  {
    Calendar cal = new GregorianCalendar(); 
    cal.setTime(this);
    return - (cal.get(Calendar.ZONE_OFFSET)
	      + cal.get(Calendar.DST_OFFSET)/(60*1000));
  }

  public String toString ()
  {
    // This is slow, but does it matter?  There is no particularly
    // fast way to do it, because we need the timezone offset, which
    // we don't store.  Unix ctime() doesn't provide this information.
    SimpleDateFormat fmt = new SimpleDateFormat ("E MMM dd HH:mm:ss z yyyy",
						 Locale.US);
    fmt.setTimeZone(TimeZone.getDefault());
    return fmt.format(this);
  }

  public String toGMTString ()
  {
    // This method is deprecated.  We don't care if it is very slow.
    SimpleDateFormat fmt = new SimpleDateFormat ("d MMM yyyy HH:mm:ss 'GMT'",
						 Locale.US);
    fmt.setTimeZone(TimeZone.zoneGMT);
    return fmt.format(this);
  }

  public String toLocaleString ()
  {
    // This method is deprecated.  We don't care if it is very slow.
    DateFormat fmt = DateFormat.getDateTimeInstance();
    fmt.setTimeZone(TimeZone.getDefault());
    return fmt.format(this);
  }

  public static long UTC (int year, int month, int date,
			  int hours, int minutes, int seconds)
  {
    GregorianCalendar cal = new GregorianCalendar (TimeZone.zoneGMT);
    cal.set(year+1900, month, date, hours, minutes, seconds);
    return cal.getTimeInMillis();
  }
}
