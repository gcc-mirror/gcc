/* LocaleInformation_en.java -- US English locale data
   Copyright (C) 1998, 1999, 2001 Free Software Foundation, Inc.

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
import java.util.Calendar;
import java.util.Date;

/**
 * This class contains locale data for English in the US.
 */
public class LocaleInformation_en_US extends ListResourceBundle
{
  // These are for DateFormatSymbols.
  private static final String[][] zoneStrings =
  {
    { "GMT", "Greenwich Mean Time", "GMT",
      /**/   "Greenwich Mean Time", "GMT", "GMT" },
    { "PST", "Pacific Standard Time", "PST",
      /**/   "Pacific Daylight Time", "PDT", "San Francisco" },
    { "MST", "Mountain Standard Time", "MST",
      /**/   "Mountain Daylight Time", "MDT", "Denver" },
    { "PNT", "Mountain Standard Time", "MST",
      /**/   "Mountain Standard Time", "MST", "Phoenix" },
    { "CST", "Central Standard Time", "CST",
      /**/   "Central Daylight Time", "CDT", "Chicago" },
    { "EST", "Eastern Standard Time", "EST",
      /**/   "Eastern Daylight Time", "EDT", "Boston" },
    { "IET", "Eastern Standard Time", "EST",
      /**/   "Eastern Standard Time", "EST", "Indianapolis" },
    { "PRT", "Atlantic Standard Time", "AST",
      /**/   "Atlantic Daylight Time", "ADT", "Halifax" },
    { "CNT", "Newfoundland Standard Time", "NST",
      /**/   "Newfoundland Daylight Time", "NDT", "St. Johns" },
    { "ECT", "Central European Standard Time", "CET",
      /**/   "Central European Daylight Time", "CEST", "Paris" },
    { "CTT", "China Standard Time", "CST",
      /**/   "China Standard Time", "CST", "Shanghai" },
    { "JST", "Japan Standard Time", "JST",
      /**/   "Japan Standard Time", "JST", "Tokyo" },
    { "HST", "Hawaii Standard Time", "HST",
      /**/   "Hawaii Standard Time", "HST", "Honolulu" },
    { "AST", "Alaska Standard Time", "AKST",
      /**/   "Alaska Daylight Time", "AKDT", "Anchorage" }
  };

  /**
   * This is the object array used to hold the keys and values
   * for this bundle
   */

  private static final Object[][] contents =
  {
    // For SimpleDateFormat/DateFormatSymbols
    { "zoneStrings", zoneStrings },
    { "shortDateFormat", "M/d/yy" },	      // Java's Y2K bug.
    { "mediumDateFormat", "d-MMM-yy" },
    { "longDateFormat", "MMMM d, yyyy" },
    { "defaultDateFormat", "d-MMMM-yy" },
    { "fullDateFormat", "EEEE MMMM d, yyyy G" },
    { "shortTimeFormat", "h:mm a" },
    { "mediumTimeFormat", "h:mm:ss a" },
    { "longTimeFormat", "h:mm:ss a z" },
    { "fullTimeFormat", "h:mm:ss;S 'o''clock' a z" },
    { "defaultTimeFormat", "h:mm:ss a" },

    // For DecimalFormat/DecimalFormatSymbols
    { "currencySymbol", "$" },
    { "intlCurrencySymbol", "US$" },

    // For NumberFormat.
    { "currencyFormat", "$#,##0.00;($#,##0.00)" }
  };

  /*************************************************************************/

  /**
   * This method returns the object array of key, value pairs containing
   * the data for this bundle.
   *
   * @return The key, value information.
   */
  public Object[][] getContents ()
  {
    return contents;
  }
}
