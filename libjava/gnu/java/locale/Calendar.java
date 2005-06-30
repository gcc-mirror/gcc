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


package gnu.java.locale;

import java.util.Date;
import java.util.ListResourceBundle;
import java.util.Locale;

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
