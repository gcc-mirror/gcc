/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.text;

import java.util.*;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date October 25, 1998.
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Mostly complete; search for FIXME to see omissions.
 */

public abstract class DateFormat extends Format implements Cloneable
{
  protected Calendar calendar;
  protected NumberFormat numberFormat;

  // (Values determined using a test program.)
  public final static int FULL = 0;
  public final static int LONG = 1;
  public final static int MEDIUM = 2;
  public final static int SHORT = 3;
  public final static int DEFAULT = MEDIUM;

  public final static int ERA_FIELD = 0;
  public final static int YEAR_FIELD = 1;
  public final static int MONTH_FIELD = 2;
  public final static int DATE_FIELD = 3;
  public final static int HOUR_OF_DAY1_FIELD = 4;
  public final static int HOUR_OF_DAY0_FIELD = 5;
  public final static int MINUTE_FIELD = 6;
  public final static int SECOND_FIELD = 7;
  public final static int MILLISECOND_FIELD = 8;
  public final static int DAY_OF_WEEK_FIELD = 9;
  public final static int DAY_OF_YEAR_FIELD = 10;
  public final static int DAY_OF_WEEK_IN_MONTH_FIELD = 11;
  public final static int WEEK_OF_YEAR_FIELD = 12;
  public final static int WEEK_OF_MONTH_FIELD = 13;
  public final static int AM_PM_FIELD = 14;
  public final static int HOUR1_FIELD = 15;
  public final static int HOUR0_FIELD = 16;
  public final static int TIMEZONE_FIELD = 17;

  protected DateFormat ()
  {
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof DateFormat))
      return false;
    DateFormat d = (DateFormat) obj;
    return calendar.equals(d.calendar) && numberFormat.equals(d.numberFormat);
  }

  public Object clone ()
  {
    // We know the superclass just call's Object's generic cloner.
    return super.clone ();
  }

  public final StringBuffer format (Object obj,
				    StringBuffer buf, FieldPosition pos)
  {
    if (obj instanceof Number)
      return format (new Date(((Number) obj).longValue()), buf, pos);
    return format ((Date) obj, buf, pos);
  }

  public final String format (Date date)
  {
    StringBuffer sb = new StringBuffer ();
    format (date, sb, new FieldPosition (MONTH_FIELD));
    return sb.toString();
  }

  public abstract StringBuffer format (Date date,
				       StringBuffer buf, FieldPosition pos);

  public static Locale[] getAvailableLocales ()
  {
    return null;		// FIXME
  }

  public Calendar getCalendar ()
  {
    return calendar;
  }

  private static final DateFormat computeInstance (int style, Locale loc,
						   boolean use_date,
						   boolean use_time)
  {
    return computeInstance (style, style, loc, use_date, use_time);
  }

  private static final DateFormat computeInstance (int dateStyle, 
						   int timeStyle,
						   Locale loc,
						   boolean use_date,
						   boolean use_time)
  {
    ResourceBundle res;
    try
      {
	res = ResourceBundle.getBundle("gnu.gcj.text.LocaleData", loc);
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

  public static final DateFormat getDateInstance ()
  {
    return getDateInstance (DEFAULT, Locale.getDefault());
  }

  public static final DateFormat getDateInstance (int style)
  {
    return getDateInstance (style, Locale.getDefault());
  }

  public static final DateFormat getDateInstance (int style, Locale loc)
  {
    return computeInstance (style, loc, true, false);
  }

  public static final DateFormat getDateTimeInstance ()
  {
    return getDateTimeInstance (DEFAULT, DEFAULT, Locale.getDefault());
  }

  public static final DateFormat getDateTimeInstance (int style)
  {
    return getDateTimeInstance (style, style, Locale.getDefault());
  }

  public static final DateFormat getDateTimeInstance (int dateStyle, 
						      int timeStyle)
  {
    return getDateTimeInstance (dateStyle, timeStyle, Locale.getDefault());
  }

  public static final DateFormat getDateTimeInstance (int dateStyle, 
						      int timeStyle, 
						      Locale loc)
  {
    return computeInstance (dateStyle, timeStyle, loc, true, true);
  }

  public static final DateFormat getInstance ()
  {
    // JCL book says SHORT.
    return getDateTimeInstance (SHORT, SHORT, Locale.getDefault());
  }

  public NumberFormat getNumberFormat ()
  {
    return numberFormat;
  }

  public static final DateFormat getTimeInstance ()
  {
    return getTimeInstance (DEFAULT, Locale.getDefault());
  }

  public static final DateFormat getTimeInstance (int style)
  {
    return getTimeInstance (style, Locale.getDefault());
  }

  public static final DateFormat getTimeInstance (int style, Locale loc)
  {
    return computeInstance (style, loc, false, true);
  }

  public TimeZone getTimeZone ()
  {
    return calendar.getTimeZone();
  }

  public int hashCode ()
  {
    int hash = calendar.hashCode();
    if (numberFormat != null)
      hash ^= numberFormat.hashCode();
    return hash;
  }

  public boolean isLenient ()
  {
    return calendar.isLenient();
  }

  public Date parse (String source) throws ParseException
  {
    ParsePosition pos = new ParsePosition(0);
    Date result = parse (source, pos);
    if (result == null)
      {
	int index = pos.getErrorIndex();
	if (index < 0)
	  index = pos.getIndex();
	throw new ParseException("invalid Date syntax", index);
      }
    return result;
  }

  public abstract Date parse (String source, ParsePosition pos);

  public Object parseObject (String source, ParsePosition pos)
  {
    return parse(source, pos);
  }

  public void setCalendar (Calendar calendar)
  {
    this.calendar = calendar;
  }

  public void setLenient (boolean lenient)
  {
    calendar.setLenient(lenient);
  }

  public void setNumberFormat (NumberFormat numberFormat)
  {
    this.numberFormat = numberFormat;
  }

  public void setTimeZone (TimeZone timeZone)
  {
    calendar.setTimeZone(timeZone);
  }
}
