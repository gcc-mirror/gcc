/* DateFormat.java -- Class for formatting/parsing date/times
   Copyright (C) 1998, 1999, 2000, 2001, 2003, 2004, 2005
   Free Software Foundation, Inc.

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


package java.text;

import java.io.InvalidObjectException;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.TimeZone;

/**
 * @author Per Bothner (bothner@cygnus.com)
 * @date October 25, 1998.
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Mostly complete; search for FIXME to see omissions.
 */

public abstract class DateFormat extends Format implements Cloneable
{
  private static final long serialVersionUID = 7218322306649953788L;

  // Names fixed by serialization spec.
  protected Calendar calendar;
  protected NumberFormat numberFormat;

  // (Values determined using a test program.)
  public static final int FULL = 0;
  public static final int LONG = 1;
  public static final int MEDIUM = 2;
  public static final int SHORT = 3;
  public static final int DEFAULT = MEDIUM;

  /* These constants need to have these exact values.  They
   * correspond to index positions within the localPatternChars
   * string for a given locale.  Each locale may specify its
   * own character for a particular field, but the position
   * of these characters must correspond to an appropriate field
   * number (as listed below), in order for their meaning to
   * be determined.  For example, the US locale uses
   * the string "GyMdkHmsSEDFwWahKzYeugAZ", where 'G' is the character
   * for era, 'y' for year, and so on down to 'Z' for time zone.
   */
  /**
   * Represents the position of the era
   * pattern character in the array of
   * localized pattern characters. 
   * For example, 'AD' is an era used
   * in the Gregorian calendar system.
   * In the U.S. locale, this is 'G'.
   */  
  public static final int ERA_FIELD = 0;
  /**
   * Represents the position of the year
   * pattern character in the array of
   * localized pattern characters.
   * In the U.S. locale, this is 'y'.
   */
  public static final int YEAR_FIELD = 1;
  /**
   * Represents the position of the month
   * pattern character in the array of
   * localized pattern characters.
   * In the U.S. locale, this is 'M'.
   */
  public static final int MONTH_FIELD = 2;
  /**
   * Represents the position of the date
   * or day of the month pattern character
   * in the array of localized pattern
   * characters.  In the U.S. locale,
   * this is 'd'.
   */
  public static final int DATE_FIELD = 3;
  /**
   * Represents the position of the 24
   * hour pattern character in the array of
   * localized pattern characters.
   * In the U.S. locale, this is 'k'.
   * This field numbers hours from 1 to 24.
   */
  public static final int HOUR_OF_DAY1_FIELD = 4;
  /**
   * Represents the position of the 24
   * hour pattern character in the array of
   * localized pattern characters.
   * In the U.S. locale, this is 'H'.
   * This field numbers hours from 0 to 23.
   */
  public static final int HOUR_OF_DAY0_FIELD = 5;
  /**
   * Represents the position of the minute
   * pattern character in the array of
   * localized pattern characters.
   * In the U.S. locale, this is 'm'.
   */
  public static final int MINUTE_FIELD = 6;
  /**
   * Represents the position of the second
   * pattern character in the array of
   * localized pattern characters.
   * In the U.S. locale, this is 's'.
   */
  public static final int SECOND_FIELD = 7;
  /**
   * Represents the position of the millisecond
   * pattern character in the array of
   * localized pattern characters.
   * In the U.S. locale, this is 'S'.
   */
  public static final int MILLISECOND_FIELD = 8;
  /**
   * Represents the position of the day of the
   * week pattern character in the array of
   * localized pattern characters.
   * In the U.S. locale, this is 'E'.
   */
  public static final int DAY_OF_WEEK_FIELD = 9;
  /**
   * Represents the position of the day of the
   * year pattern character in the array of
   * localized pattern characters.
   * In the U.S. locale, this is 'D'.
   */
  public static final int DAY_OF_YEAR_FIELD = 10;
  /**
   * Represents the position of the day of the
   * week in the month pattern character in the
   * array of localized pattern characters.
   * In the U.S. locale, this is 'F'.
   */
  public static final int DAY_OF_WEEK_IN_MONTH_FIELD = 11;
  /**
   * Represents the position of the week of the
   * year pattern character in the array of
   * localized pattern characters.
   * In the U.S. locale, this is 'w'.
   */
  public static final int WEEK_OF_YEAR_FIELD = 12;
  /**
   * Represents the position of the week of the
   * month pattern character in the array of
   * localized pattern characters.
   * In the U.S. locale, this is 'W'.
   */
  public static final int WEEK_OF_MONTH_FIELD = 13;
  /**
   * Represents the position of the am/pm
   * pattern character in the array of
   * localized pattern characters.
   * In the U.S. locale, this is 'a'.
   */
  public static final int AM_PM_FIELD = 14;
  /**
   * Represents the position of the 12 
   * hour pattern character in the array of
   * localized pattern characters.
   * In the U.S. locale, this is 'h'.
   * This field numbers hours from 1 to 12.
   */
  public static final int HOUR1_FIELD = 15;
  /**
   * Represents the position of the 12 
   * hour pattern character in the array of
   * localized pattern characters.
   * In the U.S. locale, this is 'K'.
   * This field numbers hours from 0 to 11.
   */
  public static final int HOUR0_FIELD = 16;
  /**
   * Represents the position of the generic
   * timezone pattern character in the array of
   * localized pattern characters.
   * In the U.S. locale, this is 'z'.
   */
  public static final int TIMEZONE_FIELD = 17;
  /**
   * Represents the position of the ISO year
   * pattern character in the array of
   * localized pattern characters.
   * In the U.S. locale, this is 'Y'.
   * This is a GNU extension in accordance with
   * the CLDR data used.  This value may
   * differ from the normal year value.
   */
  public static final int ISO_YEAR_FIELD = 18;
  /**
   * Represents the position of the localized
   * day of the week pattern character in the
   * array of localized pattern characters.
   * In the U.S. locale, this is 'e'.
   * This is a GNU extension in accordance with
   * the CLDR data used.  This value only
   * differs from the day of the week with
   * numeric formatting, in which case the
   * locale's first day of the week is used.
   */
  public static final int LOCALIZED_DAY_OF_WEEK_FIELD = 19;
  /**
   * Represents the position of the extended year
   * pattern character in the array of
   * localized pattern characters.
   * In the U.S. locale, this is 'u'.
   * This is a GNU extension in accordance with
   * the CLDR data used.  This value modifies
   * the year value, so as to incorporate the era.
   * For example, in the Gregorian calendar system,
   * the extended year is negative instead of being
   * marked as BC.
   */
  public static final int EXTENDED_YEAR_FIELD = 20;
  /**
   * Represents the position of the modified Julian
   * day pattern character in the array of
   * localized pattern characters.
   * In the U.S. locale, this is 'g'.
   * This is a GNU extension in accordance with
   * the CLDR data used.  This value differs
   * from the standard Julian day in that days
   * are marked from midnight onwards rather than
   * noon, and the local time zone affects the value.
   * In simple terms, it can be thought of as all
   * the date fields represented as a single number.
   */
  public static final int MODIFIED_JULIAN_DAY_FIELD = 21;
  /**
   * Represents the position of the millisecond
   * in the day pattern character in the array of
   * localized pattern characters.
   * In the U.S. locale, this is 'A'.
   * This is a GNU extension in accordance with
   * the CLDR data used.  This value represents
   * all the time fields (excluding the time zone)
   * numerically, giving the number of milliseconds
   * into the day (e.g. 10 in the morning would
   * be 10 * 60 * 60 * 1000).  Any daylight savings
   * offset also affects this value.
   */
  public static final int MILLISECOND_IN_DAY_FIELD = 22;
  /**
   * Represents the position of the RFC822
   * timezone pattern character in the array of
   * localized pattern characters.
   * In the U.S. locale, this is 'Z'.
   * This is a GNU extension in accordance with
   * the CLDR data used.  The value is the offset
   * of the current time from GMT e.g. -0500 would
   * be five hours prior to GMT.
   */
  public static final int RFC822_TIMEZONE_FIELD = 23;

  public static class Field extends Format.Field
  {
    static final long serialVersionUID = 7441350119349544720L;
    
    private int calendarField;

    public static final DateFormat.Field ERA
	= new Field("era", Calendar.ERA);
    public static final DateFormat.Field YEAR
	= new Field("year", Calendar.YEAR);
    public static final DateFormat.Field MONTH
	= new Field("month", Calendar.MONTH);
    public static final DateFormat.Field DAY_OF_MONTH
	= new Field("day of month", Calendar.DAY_OF_MONTH);
    public static final DateFormat.Field HOUR_OF_DAY1
	= new Field("hour of day 1", Calendar.HOUR_OF_DAY);
    public static final DateFormat.Field HOUR_OF_DAY0
	= new Field("hour of day 0", Calendar.HOUR_OF_DAY);
    public static final DateFormat.Field MINUTE
	= new Field("minute", Calendar.MINUTE);
    public static final DateFormat.Field SECOND
	= new Field("second", Calendar.SECOND);
    public static final DateFormat.Field MILLISECOND
	= new Field("millisecond", Calendar.MILLISECOND);
    public static final DateFormat.Field DAY_OF_WEEK
	= new Field("day of week", Calendar.DAY_OF_WEEK);
    public static final DateFormat.Field DAY_OF_YEAR
	= new Field("day of year", Calendar.DAY_OF_YEAR);
    public static final DateFormat.Field DAY_OF_WEEK_IN_MONTH
	= new Field("day of week in month", Calendar.DAY_OF_WEEK_IN_MONTH);
    public static final DateFormat.Field WEEK_OF_YEAR
	= new Field("week of year", Calendar.WEEK_OF_YEAR);
    public static final DateFormat.Field WEEK_OF_MONTH
	= new Field("week of month", Calendar.WEEK_OF_MONTH);
    public static final DateFormat.Field AM_PM
	= new Field("am/pm", Calendar.AM_PM);
    public static final DateFormat.Field HOUR1
	= new Field("hour1", Calendar.HOUR);
    public static final DateFormat.Field HOUR0
	= new Field("hour0", Calendar.HOUR);
    public static final DateFormat.Field TIME_ZONE
	= new Field("timezone", Calendar.ZONE_OFFSET);
    public static final DateFormat.Field ISO_YEAR
	= new Field("iso year", Calendar.YEAR);
    public static final DateFormat.Field LOCALIZED_DAY_OF_WEEK
	= new Field("localized day of week", Calendar.DAY_OF_WEEK);
    public static final DateFormat.Field EXTENDED_YEAR
      = new Field("extended year", Calendar.YEAR);
    public static final DateFormat.Field MODIFIED_JULIAN_DAY
	= new Field("julian day", -1);
    public static final DateFormat.Field MILLISECOND_IN_DAY
	= new Field("millisecond in day", -1);
    public static final DateFormat.Field RFC822_TIME_ZONE
	= new Field("rfc822 timezone", Calendar.ZONE_OFFSET);

    static final DateFormat.Field[] allFields =
    {
      ERA, YEAR, MONTH, DAY_OF_MONTH, HOUR_OF_DAY1,
      HOUR_OF_DAY0, MINUTE, SECOND, MILLISECOND,
      DAY_OF_WEEK, DAY_OF_YEAR, DAY_OF_WEEK_IN_MONTH,
      WEEK_OF_YEAR, WEEK_OF_MONTH, AM_PM, HOUR1, HOUR0,
      TIME_ZONE, ISO_YEAR, LOCALIZED_DAY_OF_WEEK,
      EXTENDED_YEAR, MODIFIED_JULIAN_DAY, MILLISECOND_IN_DAY,
      RFC822_TIME_ZONE
    };

    // For deserialization
    private Field()
    {
      super("");
    }

    protected Field(String name, int calendarField)
    {
      super(name);
      this.calendarField = calendarField;
    }
    
    public int getCalendarField()
    {
      return calendarField;
    }

    public static Field ofCalendarField(int calendarField)
    {
      if (calendarField >= allFields.length || calendarField < 0)
	throw new IllegalArgumentException("no such calendar field ("
					   + calendarField + ")");
      
      return allFields[calendarField];
    }
    
    protected Object readResolve() throws InvalidObjectException
    {
      String s = getName();

      for (int i=0;i<allFields.length;i++)
	if (s.equals(allFields[i].getName()))
	  return allFields[i];
      
      throw new InvalidObjectException("no such DateFormat field called " + s);
    }
  }

  /**
   * This method initializes a new instance of <code>DateFormat</code>.
   */
  protected DateFormat ()
  {
  }

  /**
   * This method tests this object for equality against the specified object.
   * The two objects will be considered equal if an only if the specified
   * object:
   * <P>
   * <ul>
   * <li>Is not <code>null</code>.</li>
   * <li>Is an instance of <code>DateFormat</code>.</li>
   * <li>Has equal numberFormat field as this object.</li>
   * <li>Has equal (Calendar) TimeZone rules as this object.</li>
   * <li>Has equal (Calendar) isLenient results.</li> 
   * <li>Has equal Calendar first day of week and minimal days in week
   * values.</li>
   * </ul>
   * Note that not all properties of the Calendar are relevant for a
   * DateFormat. For formatting only the fact whether or not the
   * TimeZone has the same rules and whether the calendar is lenient
   * and has the same week rules is compared for this implementation
   * of equals. Other properties of the Calendar (such as the time)
   * are not taken into account.
   *
   * @param obj The object to test for equality against.
   * 
   * @return <code>true</code> if the specified object is equal to this object,
   * <code>false</code> otherwise.
   */
  public boolean equals (Object obj)
  {
    if (!(obj instanceof DateFormat))
      return false;

    DateFormat d = (DateFormat) obj;
    TimeZone tz = getTimeZone();
    TimeZone tzd = d.getTimeZone();
    if (tz.hasSameRules(tzd))
      if (isLenient() == d.isLenient())
	{
	  Calendar c = getCalendar();
	  Calendar cd = d.getCalendar();
	  if ((c == null && cd == null)
	      ||
	      (c.getFirstDayOfWeek() == cd.getFirstDayOfWeek()
	       &&
	       c.getMinimalDaysInFirstWeek()
	       == cd.getMinimalDaysInFirstWeek()))
	    return ((numberFormat == null && d.numberFormat == null)
		    || numberFormat.equals(d.numberFormat));
	}

    return false;
  }

  /**
   * This method returns a copy of this object.
   *
   * @return A copy of this object.
   */
  public Object clone ()
  {
    // We know the superclass just call's Object's generic cloner.
    return super.clone ();
  }

  /**
   * This method formats the specified <code>Object</code> into a date string
   * and appends it to the specified <code>StringBuffer</code>.
   * The specified object must be an instance of <code>Number</code> or
   * <code>Date</code> or an <code>IllegalArgumentException</code> will be
   * thrown.
   *
   * @param obj The <code>Object</code> to format.
   * @param buf The <code>StringBuffer</code> to append the resultant
   * <code>String</code> to.
   * @param pos Is updated to the start and end index of the
   * specified field.
   *
   * @return The <code>StringBuffer</code> supplied on input, with the
   * formatted date/time appended.
   */
  public final StringBuffer format (Object obj,
				    StringBuffer buf, FieldPosition pos)
  {
    if (obj instanceof Number)
      obj = new Date(((Number) obj).longValue());
    else if (! (obj instanceof Date))
      throw new IllegalArgumentException
	("Cannot format given Object as a Date");

    return format ((Date) obj, buf, pos);
  }

  /**  
    * Formats the date argument according to the pattern specified. 
    *
    * @param date The formatted date.
    */
  public final String format (Date date)
  {
    StringBuffer sb = new StringBuffer ();
    format (date, sb, new FieldPosition (MONTH_FIELD));
    return sb.toString();
  }

  /**
   * This method formats a <code>Date</code> into a string and appends it
   * to the specified <code>StringBuffer</code>.
   *
   * @param date The <code>Date</code> value to format.
   * @param buf The <code>StringBuffer</code> to append the resultant
   * <code>String</code> to.
   * @param pos Is updated to the start and end index of the
   * specified field.
   *
   * @return The <code>StringBuffer</code> supplied on input, with the
   * formatted date/time appended.
   */
  public abstract StringBuffer format (Date date,
				       StringBuffer buf, FieldPosition pos);

  /**
   * This method returns a list of available locales supported by this
   * class.
   */
  public static Locale[] getAvailableLocales()
  {
    return Locale.getAvailableLocales();
  }

  /**
    * This method returns the <code>Calendar</code> object being used by
    * this object to parse/format datetimes.
    *
    * @return The <code>Calendar</code> being used by this object.
    *
    * @see java.util.Calendar
    */
  public Calendar getCalendar ()
  {
    return calendar;
  }

  private static DateFormat computeInstance (int style, Locale loc,
                                             boolean use_date, boolean use_time)
  {
    return computeInstance (style, style, loc, use_date, use_time);
  }

  private static DateFormat computeInstance (int dateStyle, int timeStyle,
                                             Locale loc, boolean use_date,
                                             boolean use_time)
  {
    ResourceBundle res;
    try
      {
	res = ResourceBundle.getBundle("gnu.java.locale.LocaleInformation",
				       loc, ClassLoader.getSystemClassLoader());
      }
    catch (MissingResourceException x)
      {
	res = null;
      }

    String pattern = null;
    if (use_date)
      {
	String name, def;
	switch (dateStyle)
	  {
	  case FULL:
	    name = "fullDateFormat";
	    def = "EEEE MMMM d, yyyy G";
	    break;
	  case LONG:
	    name = "longDateFormat";
	    def = "MMMM d, yyyy";
	    break;
	  case MEDIUM:
	    name = "mediumDateFormat";
	    def = "d-MMM-yy";
	    break;
	  case SHORT:
	    name = "shortDateFormat";
	    def = "M/d/yy";
	    break;
	  default:
	    throw new IllegalArgumentException ();
	  }
	try
	  {
	    pattern = res == null ? def : res.getString(name);
	  }
	catch (MissingResourceException x)
	  {
	    pattern = def;
	  }
      }

    if (use_time)
      {
	if (pattern == null)
	  pattern = "";
	else
	  pattern += " ";

	String name, def;
	switch (timeStyle)
	  {
	  case FULL:
	    name = "fullTimeFormat";
	    def = "h:mm:ss;S 'o''clock' a z";
	    break;
	  case LONG:
	    name = "longTimeFormat";
	    def = "h:mm:ss a z";
	    break;
	  case MEDIUM:
	    name = "mediumTimeFormat";
	    def = "h:mm:ss a";
	    break;
	  case SHORT:
	    name = "shortTimeFormat";
	    def = "h:mm a";
	    break;
	  default:
	    throw new IllegalArgumentException ();
	  }

	String s;
	try
	  {
	    s = res == null ? def : res.getString(name);
	  }
	catch (MissingResourceException x)
	  {
	    s = def;
	  }
	pattern += s;
      }

    return new SimpleDateFormat (pattern, loc);
  }

 /**
   * This method returns an instance of <code>DateFormat</code> that will
   * format using the default formatting style for dates.
   *
   * @return A new <code>DateFormat</code> instance.
   */
  public static final DateFormat getDateInstance ()
  {
    return getDateInstance (DEFAULT, Locale.getDefault());
  }

  /**
   * This method returns an instance of <code>DateFormat</code> that will
   * format using the specified formatting style for dates.
   *
   * @param style The type of formatting to perform. 
   * 
   * @return A new <code>DateFormat</code> instance.
   */
  public static final DateFormat getDateInstance (int style)
  {
    return getDateInstance (style, Locale.getDefault());
  }

  /**
   * This method returns an instance of <code>DateFormat</code> that will
   * format using the specified formatting style for dates.  The specified
   * localed will be used in place of the default.
   *
   * @param style The type of formatting to perform. 
   * @param loc The desired locale.
   * 
   * @return A new <code>DateFormat</code> instance.
   */
  public static final DateFormat getDateInstance (int style, Locale loc)
  {
    return computeInstance (style, loc, true, false);
  }

  /**
   * This method returns a new instance of <code>DateFormat</code> that
   * formats both dates and times using the <code>SHORT</code> style.
   *
   * @return A new <code>DateFormat</code>instance.
   */
  public static final DateFormat getDateTimeInstance ()
  {
    return getDateTimeInstance (DEFAULT, DEFAULT, Locale.getDefault());
  }

  /**
   * This method returns a new instance of <code>DateFormat</code> that
   * formats both dates and times using the <code>DEFAULT</code> style.
   *
   * @return A new <code>DateFormat</code>instance.
   */
  public static final DateFormat getDateTimeInstance (int dateStyle, 
						      int timeStyle)
  {
    return getDateTimeInstance (dateStyle, timeStyle, Locale.getDefault());
  }

  /**
   * This method returns a new instance of <code>DateFormat</code> that
   * formats both dates and times using the specified styles.
   * 
   * @param dateStyle The desired style for date formatting.
   * @param timeStyle The desired style for time formatting
   *
   * @return A new <code>DateFormat</code>instance.
   */
  public static final DateFormat getDateTimeInstance (int dateStyle, 
						      int timeStyle, 
						      Locale loc)
  {
    return computeInstance (dateStyle, timeStyle, loc, true, true);
  }

  /**
   * This method returns a new instance of <code>DateFormat</code> that
   * formats both dates and times using the <code>SHORT</code> style.
   *
   * @return A new <code>DateFormat</code>instance.
   */
  public static final DateFormat getInstance ()
  {
    // JCL book says SHORT.
    return getDateTimeInstance (SHORT, SHORT, Locale.getDefault());
  }

  /**
   * This method returns the <code>NumberFormat</code> object being used
   * by this object to parse/format time values.
   *
   * @return The <code>NumberFormat</code> in use by this object.
   */
  public NumberFormat getNumberFormat ()
  {
    return numberFormat;
  }

 /**
   * This method returns an instance of <code>DateFormat</code> that will
   * format using the default formatting style for times.
   *
   * @return A new <code>DateFormat</code> instance.
   */
  public static final DateFormat getTimeInstance ()
  {
    return getTimeInstance (DEFAULT, Locale.getDefault());
  }

  /**
   * This method returns an instance of <code>DateFormat</code> that will
   * format using the specified formatting style for times.
   *
   * @param style The type of formatting to perform. 
   * 
   * @return A new <code>DateFormat</code> instance.
   */
  public static final DateFormat getTimeInstance (int style)
  {
    return getTimeInstance (style, Locale.getDefault());
  }

  /**
   * This method returns an instance of <code>DateFormat</code> that will
   * format using the specified formatting style for times.  The specified
   * localed will be used in place of the default.
   *
   * @param style The type of formatting to perform. 
   * @param loc The desired locale.
   * 
   * @return A new <code>DateFormat</code> instance.
   */
  public static final DateFormat getTimeInstance (int style, Locale loc)
  {
    return computeInstance (style, loc, false, true);
  }

  /**
   * This method returns the <code>TimeZone</code> object being used by
   * this instance.
   *
   * @return The time zone in use.
   */
  public TimeZone getTimeZone ()
  {
    return calendar.getTimeZone();
  }

  /**
   * This method returns a hash value for this object.
   * 
   * @return A hash value for this object.
   */
  public int hashCode ()
  {
    if (numberFormat != null)
      return numberFormat.hashCode();
    else
      return 0;
  }

  /**
   * This method indicates whether or not the parsing of date and time
   * values should be done in a lenient value.
   *
   * @return <code>true</code> if date/time parsing is lenient,
   * <code>false</code> otherwise.
   */
  public boolean isLenient ()
  {
    return calendar.isLenient();
  }

  /**
   * This method parses the specified date/time string.
   *
   * @param source The string to parse.
   * @return The resultant date.
   *
   * @exception ParseException If the specified string cannot be parsed.
   */
  public Date parse (String source) throws ParseException
  {
    ParsePosition pos = new ParsePosition(0);
    Date result = parse (source, pos);
    if (result == null)
      {
	int index = pos.getErrorIndex();
	if (index < 0)
	  index = pos.getIndex();
	throw new ParseException("invalid Date syntax in \""
				 + source + '\"', index);
      }
    return result;
  }

  /** 
   * This method parses the specified <code>String</code> into a 
   * <code>Date</code>.  The <code>pos</code> argument contains the
   * starting parse position on method entry and the ending parse
   * position on method exit.
   *
   * @param source The string to parse.
   * @param pos The starting parse position in entry, the ending parse
   * position on exit.
   *
   * @return The parsed date, or <code>null</code> if the string cannot
   * be parsed.
   */
  public abstract Date parse (String source, ParsePosition pos);

  /**
   * This method is identical to <code>parse(String, ParsePosition)</code>,
   * but returns its result as an <code>Object</code> instead of a
   * <code>Date</code>.
   * 
   * @param source The string to parse.
   * @param pos The starting parse position in entry, the ending parse
   * position on exit.
   *
   * @return The parsed date, or <code>null</code> if the string cannot
   * be parsed.
   */
  public Object parseObject (String source, ParsePosition pos)
  {
    return parse(source, pos);
  }

  /**
   * This method specified the <code>Calendar</code> that should be used 
   * by this object to parse/format datetimes.
   *
   * @param calendar The new <code>Calendar</code> for this object.
   *
   * @see java.util.Calendar
   */
  public void setCalendar (Calendar calendar)
  {
    this.calendar = calendar;
  }

  /**
   * This method specifies whether or not this object should be lenient in 
   * the syntax it accepts while parsing date/time values.
   *
   * @param lenient <code>true</code> if parsing should be lenient,
   * <code>false</code> otherwise.
   */
  public void setLenient (boolean lenient)
  {
    calendar.setLenient(lenient);
  }

  /**
   * This method specifies the <code>NumberFormat</code> object that should
   * be used by this object to parse/format times.
   *
   * @param numberFormat The <code>NumberFormat</code> in use by this object.
   */
  public void setNumberFormat (NumberFormat numberFormat)
  {
    this.numberFormat = numberFormat;
  }

  /**
   * This method sets the time zone that should be used by this object.
   *
   * @param timeZone The new time zone.
   */
  public void setTimeZone (TimeZone timeZone)
  {
    calendar.setTimeZone(timeZone);
  }
}
