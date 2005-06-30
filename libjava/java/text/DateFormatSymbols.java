/* DateFormatSymbols.java -- Format over a range of numbers
   Copyright (C) 1998, 1999, 2000, 2001, 2003, 2005  Free Software Foundation, Inc.

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

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * This class acts as container for locale specific date/time formatting
 * information such as the days of the week and the months of the year.
 * @author Per Bothner (bothner@cygnus.com)
 * @date October 24, 1998.
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3.
 * Status:  Believed complete and correct.
 */
public class DateFormatSymbols implements java.io.Serializable, Cloneable
{
  String[] ampms;
  String[] eras;
  private String localPatternChars;
  String[] months;
  String[] shortMonths;
  String[] shortWeekdays;
  String[] weekdays;
  private String[][] zoneStrings;

  private static final long serialVersionUID = -5987973545549424702L;

  // The order of these prefixes must be the same as in DateFormat
  private static final String[] formatPrefixes =
  {
    "full", "long", "medium", "short"
  };

  // These are each arrays with a value for SHORT, MEDIUM, LONG, FULL,
  // and DEFAULT (constants defined in java.text.DateFormat).  While
  // not part of the official spec, we need a way to get at locale-specific
  // default formatting patterns.  They are declared package scope so
  // as to be easily accessible where needed (DateFormat, SimpleDateFormat).
  transient String[] dateFormats;
  transient String[] timeFormats;

  private String[] formatsForKey(ResourceBundle res, String key) 
  {
    String[] values = new String [formatPrefixes.length];
    for (int i = 0; i < formatPrefixes.length; i++)
      {
        values[i] = res.getString(formatPrefixes[i]+key);
      }
    return values;
  }

  /**
   * This method initializes a new instance of <code>DateFormatSymbols</code>
   * by loading the date format information for the specified locale.
   *
   * @param locale The locale for which date formatting symbols should
   *               be loaded. 
   */
  public DateFormatSymbols (Locale locale) throws MissingResourceException
  {
    ResourceBundle res
      = ResourceBundle.getBundle("gnu.java.locale.LocaleInformation", locale,
      				 ClassLoader.getSystemClassLoader());

    ampms = res.getStringArray ("ampms");
    eras = res.getStringArray ("eras");
    localPatternChars = res.getString ("localPatternChars");
    months = res.getStringArray ("months");
    shortMonths = res.getStringArray ("shortMonths");
    shortWeekdays = res.getStringArray ("shortWeekdays");
    weekdays = res.getStringArray ("weekdays");
    zoneStrings = (String[][]) res.getObject ("zoneStrings");

    dateFormats = formatsForKey(res, "DateFormat");
    timeFormats = formatsForKey(res, "TimeFormat");
  }

  /**
   * This method loads the format symbol information for the default
   * locale.
   */
  public DateFormatSymbols () throws MissingResourceException
  {
    this (Locale.getDefault());
  }

  /**
   * This method returns the list of strings used for displaying AM or PM.
   * This is a two element <code>String</code> array indexed by
   * <code>Calendar.AM</code> and <code>Calendar.PM</code>
   *
   * @return The list of AM/PM display strings.
   */
  public String[] getAmPmStrings()
  {
    return ampms;
  }

  /**
    * This method returns the list of strings used for displaying eras
    * (e.g., "BC" and "AD").  This is a two element <code>String</code>
    * array indexed by <code>Calendar.BC</code> and <code>Calendar.AD</code>.
    *
    * @return The list of era disply strings.
    */
  public String[] getEras()
  {
    return eras;
  }

  /**
    * This method returns the pattern character information for this
    * object.  This is an 18 character string that contains the characters
    * that are used in creating the date formatting strings in 
    * <code>SimpleDateFormat</code>.   The following are the character
    * positions in the string and which format character they correspond
    * to (the character in parentheses is the default value in the US English
    * locale):
    * <p>
    * <ul>
    * <li>0 - era (G)</li>
    * <li>1 - year (y)</li>
    * <li>2 - month (M)</li>
    * <li>3 - day of month (d)</li>
    * <li>4 - hour out of 12, from 1-12 (h)</li>
    * <li>5 - hour out of 24, from 0-23 (H)</li>
    * <li>6 - minute (m)</li>
    * <li>7 - second (s)</li>
    * <li>8 - millisecond (S)</li>
    * <li>9 - date of week (E)</li>
    * <li>10 - date of year (D)</li>
    * <li>11 - day of week in month, eg. "4th Thur in Nov" (F)</li>
    * <li>12 - week in year (w)</li>
    * <li>13 - week in month (W)</li>
    * <li>14 - am/pm (a)</li>
    * <li>15 - hour out of 24, from 1-24 (k)</li>
    * <li>16 - hour out of 12, from 0-11 (K)</li>
    * <li>17 - time zone (z)</li>
    * </ul>
    *
    * @return The format patter characters
    */
  public String getLocalPatternChars()
  {
    return localPatternChars;
  }

  /**
   * This method returns the list of strings used for displaying month
   * names (e.g., "January" and "February").  This is a thirteen element
   * string array indexed by <code>Calendar.JANUARY</code> through
   * <code>Calendar.UNDECEMBER</code>.  Note that there are thirteen
   * elements because some calendars have thriteen months.
   *
   * @return The list of month display strings.
   */
  public String[] getMonths ()
  {
    return months;
  }

  /**
   * This method returns the list of strings used for displaying abbreviated
   * month names (e.g., "Jan" and "Feb").  This is a thirteen element
   * <code>String</code> array indexed by <code>Calendar.JANUARY</code>
   * through <code>Calendar.UNDECEMBER</code>.  Note that there are thirteen
   * elements because some calendars have thirteen months.
   *
   * @return The list of abbreviated month display strings.
   */
  public String[] getShortMonths ()
  {
    return shortMonths;
  }

  /**
   * This method returns the list of strings used for displaying abbreviated 
   * weekday names (e.g., "Sun" and "Mon").  This is an eight element
   * <code>String</code> array indexed by <code>Calendar.SUNDAY</code>
   * through <code>Calendar.SATURDAY</code>.  Note that the first element
   * of this array is ignored.
   *
   * @return This list of abbreviated weekday display strings.
   */
  public String[] getShortWeekdays ()
  {
    return shortWeekdays;
  }

  /**
   * This method returns the list of strings used for displaying weekday
   * names (e.g., "Sunday" and "Monday").  This is an eight element
   * <code>String</code> array indexed by <code>Calendar.SUNDAY</code>
   * through <code>Calendar.SATURDAY</code>.  Note that the first element
   * of this array is ignored.
   *
   * @return This list of weekday display strings.
   */
  public String[] getWeekdays ()
  {
    return weekdays;
  }

  /**
   * This method returns this list of localized timezone display strings.
   * This is a two dimensional <code>String</code> array where each row in
   * the array contains five values:
   * <P>
   * <ul>
   * <li>0 - The non-localized time zone id string.</li>
   * <li>1 - The long name of the time zone (standard time).</li>
   * <li>2 - The short name of the time zone (standard time).</li>
   * <li>3 - The long name of the time zone (daylight savings time).</li>
   * <li>4 - the short name of the time zone (daylight savings time).</li>
   * </ul>
   *
   * @return The list of time zone display strings.
   */
  public String[] [] getZoneStrings ()
  {
    return zoneStrings;
  }

  /**
   * This method sets the list of strings used to display AM/PM values to
   * the specified list.
   * This is a two element <code>String</code> array indexed by
   * <code>Calendar.AM</code> and <code>Calendar.PM</code>
   *
   * @param ampms The new list of AM/PM display strings.
   */
  public void setAmPmStrings (String[] value)
  {
    ampms = value;
  }

  /**
   * This method sets the list of strings used to display time eras to
   * to the specified list.
   * This is a two element <code>String</code>
   * array indexed by <code>Calendar.BC</code> and <code>Calendar.AD</code>.
   *
   * @param eras The new list of era disply strings.
   */
  public void setEras (String[] value)
  {
    eras = value;
  }

  /**
    * This method sets the list of characters used to specific date/time
    * formatting strings.
    * This is an 18 character string that contains the characters
    * that are used in creating the date formatting strings in 
    * <code>SimpleDateFormat</code>.   The following are the character
    * positions in the string and which format character they correspond
    * to (the character in parentheses is the default value in the US English
    * locale):
    * <p>
    * <ul>
    * <li>0 - era (G)</li>
    * <li>1 - year (y)</li>
    * <li>2 - month (M)</li>
    * <li>3 - day of month (d)</li>
    * <li>4 - hour out of 12, from 1-12 (h)</li>
    * <li>5 - hour out of 24, from 0-23 (H)</li>
    * <li>6 - minute (m)</li>
    * <li>7 - second (s)</li>
    * <li>8 - millisecond (S)</li>
    * <li>9 - date of week (E)</li>
    * <li>10 - date of year (D)</li>
    * <li>11 - day of week in month, eg. "4th Thur in Nov" (F)</li>
    * <li>12 - week in year (w)</li>
    * <li>13 - week in month (W)</li>
    * <li>14 - am/pm (a)</li>
    * <li>15 - hour out of 24, from 1-24 (k)</li>
    * <li>16 - hour out of 12, from 0-11 (K)</li>
    * <li>17 - time zone (z)</li>
    * </ul>
    *
    * @param localPatternChars The new format patter characters
    */
  public void setLocalPatternChars (String value)
  {
    localPatternChars = value;
  }

  /**
    * This method sets the list of strings used to display month names.
    * This is a thirteen element
    * string array indexed by <code>Calendar.JANUARY</code> through
    * <code>Calendar.UNDECEMBER</code>.  Note that there are thirteen
    * elements because some calendars have thriteen months.
    *
    * @param months The list of month display strings.
    */
  public void setMonths (String[] value)
  {
    months = value;
  }

  /**
   * This method sets the list of strings used to display abbreviated month
   * names.
   * This is a thirteen element
   * <code>String</code> array indexed by <code>Calendar.JANUARY</code>
   * through <code>Calendar.UNDECEMBER</code>.  Note that there are thirteen
   * elements because some calendars have thirteen months.
   *
   * @param shortMonths The new list of abbreviated month display strings.
   */
  public void setShortMonths (String[] value)
  {
    shortMonths = value;
  }

  /**
   * This method sets the list of strings used to display abbreviated
   * weekday names.
   * This is an eight element
   * <code>String</code> array indexed by <code>Calendar.SUNDAY</code>
   * through <code>Calendar.SATURDAY</code>.  Note that the first element
   * of this array is ignored.
   *
   * @param shortWeekdays This list of abbreviated weekday display strings.
   */
  public void setShortWeekdays (String[] value)
  {
    shortWeekdays = value;
  }

  /**
   * This method sets the list of strings used to display weekday names.
   * This is an eight element
   * <code>String</code> array indexed by <code>Calendar.SUNDAY</code>
   * through <code>Calendar.SATURDAY</code>.  Note that the first element
   * of this array is ignored.
   *
   * @param weekdays This list of weekday display strings.
   */
  public void setWeekdays (String[] value)
  {
    weekdays = value;
  }

  /**
   * This method sets the list of display strings for time zones.
   * This is a two dimensional <code>String</code> array where each row in
   * the array contains five values:
   * <P>
   * <ul>
   * <li>0 - The non-localized time zone id string.</li>
   * <li>1 - The long name of the time zone (standard time).</li>
   * <li>2 - The short name of the time zone (standard time).</li>
   * <li>3 - The long name of the time zone (daylight savings time).</li>
   * <li>4 - the short name of the time zone (daylight savings time).</li>
   * </ul>
   *
   * @return The list of time zone display strings.
   */
  public void setZoneStrings (String[][] value)
  {
    zoneStrings = value;
  }

  /* Does a "deep" equality test - recurses into arrays. */
  private static boolean equals (Object x, Object y)
  {
    if (x == y)
      return true;
    if (x == null || y == null)
      return false;
    if (! (x instanceof Object[]) || ! (y instanceof Object[]))
      return x.equals(y);
    Object[] xa = (Object[]) x;
    Object[] ya = (Object[]) y;
    if (xa.length != ya.length)
      return false;
    for (int i = xa.length;  --i >= 0; )
      {
	if (! equals(xa[i], ya[i]))
	  return false;
      }
    return true;
  }

  private static int hashCode (Object x)
  {
    if (x == null)
      return 0;
    if (! (x instanceof Object[]))
      return x.hashCode();
    Object[] xa = (Object[]) x;
    int hash = 0;
    for (int i = 0;  i < xa.length;  i++)
      hash = 37 * hashCode(xa[i]);
    return hash;
  }

  /**
   * This method tests a specified object for equality against this object.
   * This will be true if and only if the specified object:
   * <p>
   * <ul>
   * <li> Is not <code>null</code>.</li>
   * <li> Is an instance of <code>DateFormatSymbols</code>.</li>
   * <li> Contains identical formatting symbols to this object.</li>
   * </ul>
   * 
   * @param obj The <code>Object</code> to test for equality against.
   *
   * @return <code>true</code> if the specified object is equal to this one,
   * <code>false</code> otherwise.
   */
  public boolean equals (Object obj)
  {
    if (! (obj instanceof DateFormatSymbols))
      return false;
    DateFormatSymbols other = (DateFormatSymbols) obj;
    return (equals(ampms, other.ampms)
	    && equals(eras, other.eras)
	    && equals(localPatternChars, other.localPatternChars)
	    && equals(months, other.months)
	    && equals(shortMonths, other.shortMonths)
	    && equals(shortWeekdays, other.shortWeekdays)
	    && equals(weekdays, other.weekdays)
	    && equals(zoneStrings, other.zoneStrings));
  }

  /**
   * Returns a new copy of this object.
   *
   * @param A copy of this object
   */
  public Object clone ()
  {
    try
      {
        return super.clone ();
      } 
    catch (CloneNotSupportedException e) 
      {
        return null;
      }
  }

  /**
   * This method returns a hash value for this object.
   *
   * @return A hash value for this object.
   */
  public int hashCode ()
  {
    return (hashCode(ampms)
	    ^ hashCode(eras)
	    ^ hashCode(localPatternChars)
	    ^ hashCode(months)
	    ^ hashCode(shortMonths)
	    ^ hashCode(shortWeekdays)
	    ^ hashCode(weekdays)
	    ^ hashCode(zoneStrings));
  }
}
