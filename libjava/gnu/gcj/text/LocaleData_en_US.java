// US English locale data for java.text.

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.text;

import java.util.ListResourceBundle;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date March 4, 1999
 */

public final class LocaleData_en_US extends ListResourceBundle
{
  // These are for DateFormatSymbols.
  static String[][] zoneStringsDefault = {
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
    { "HST", "Hawaii Standard Time", "HST",
      /**/   "Hawaii Daylight Time", "HDT", "Honolulu" },
    { "AST", "Alaska Standard Time", "AST",
      /**/   "Alaska Daylight Time", "ADT", "Anchorage" }
  };

  private static final Object[][] contents =
  {
    // These are for DecimalFormatSymbols.
    { "currency", "$" },
    { "intlCurrencySymbol", "$" },	      // FIXME?

    // These are for NumberFormat.
    { "currencyFormat", "$#,##0.00;($#,##0.00)" },

    // These are for DateFormatSymbols.
    { "zoneStrings", zoneStringsDefault },

    // These are for DateFormat.
    { "shortDateFormat", "M/d/yy" },	      // Java's Y2K bug.
    { "mediumDateFormat", "d-MMM-yy" },
    { "longDateFormat", "MMMM d, yyyy" },
    { "fullDateFormat", "EEEE MMMM d, yyyy G" },
    { "shortTimeFormat", "h:mm a" },
    { "mediumTimeFormat", "h:mm:ss a" },
    { "longTimeFormat", "h:mm:ss a z" },
    { "fullTimeFormat", "h:mm:ss;S 'o''clock' a z" }
  };

  protected Object[][] getContents ()
    {
      return contents;
    }
}
