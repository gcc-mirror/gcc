/* Calendar.java -- Default Calendar locale data
   Copyright (C) 1999 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package gnu.java.locale;

import java.util.ListResourceBundle;
import java.util.Locale;
import java.util.Date;

/**
 * This class contains locale data for java.util.Calendar.<br>
 *
 * If you localize this to another language only set fields, that
 * you change.  See Calendar_de for an example.  Also add your
 * locale to this list of availableLocales below in this(!) file.
 *
 * @author Jochen Hoenicke
 */
public class Calendar extends ListResourceBundle
{
  /**
   * The locales for which Calendar is localized.
   */
  private static final Locale[] availableLocales = {
    Locale.GERMAN, Locale.ENGLISH, new Locale("nl", "")
  };

  /**
   * This is the default calendar class, that is returned on
   * java.util.Calendar.getInstance().
   * @see java.util.Calendar#getInstance()
   */
  private static final String calendarClass = "java.util.GregorianCalendar";
  
  /**
   * This is used by java.util.Calendar.
   * @see java.util.Calendar#getFirstDayOfWeek()
   */
  private static final Integer firstDayOfWeek
    = new Integer(java.util.Calendar.SUNDAY);
  /**
   * This is used by java.util.Calendar.
   * @see java.util.Calendar#getMinimalDaysInFirstWeek()
   */
  private static final Integer minimalDaysInFirstWeek = new Integer(1);

  /**
   * The point at which the Gregorian calendar rules were used.
   * The default for most catholic
   * countries is midnight (UTC) on October 5, 1582 (Julian),
   * or October 15, 1582 (Gregorian).
   * @see java.util.GregorianCalendar#getGregorianCutOver
   */
  /* If you change this date be aware, that this formular does only 
   * work for months from MARCH to DECEMBER and doesn't work in 
   * leap years (look in java.util.GregorianCalendar.getDayOfYear for
   * more info).
   */
  private static final Date gregorianCutOver = new Date
  ((24*60*60*1000L) *
   (((1582*(365*4+1))/4 + 
     (java.util.Calendar.OCTOBER*(31+30+31+30+31) - 9) / 5 + 5) -
    ((1970*(365*4+1))/4 + 1 - 13)));
  
  /**
   * This is the object array used to hold the keys and values
   * for this bundle
   */
  private static final Object[][] contents =
  {
    { "availableLocales", availableLocales },
    { "calendarClass", calendarClass },
    { "firstDayOfWeek", firstDayOfWeek },
    { "minimalDaysInFirstWeek", minimalDaysInFirstWeek },
    { "gregorianCutOver", gregorianCutOver }
  };

  /**
   * This method returns the object array of key, value pairs containing
   * the data for this bundle.
   *
   * @return The key, value information.
   */
  public Object[][] getContents()
  {
    return(contents);
  }
}
