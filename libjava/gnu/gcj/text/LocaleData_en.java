// Generic English locale data for java.text.

/* Copyright (C) 1999  Cygnus Solutions

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

public final class LocaleData_en extends ListResourceBundle
{
  // These are for DateFormatSymbols.
  static final String[] ampmsDefault = {"AM", "PM" };
  static final String[] erasDefault = {"BC", "AD" };
  static final String localPatternCharsDefault = "GyMdkHmsSEDFwWahKz";
  static final String[] monthsDefault = {
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December", ""
  };
  static final String[] shortMonthsDefault = {
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", ""
  };
  static final String[] shortWeekdaysDefault = {
    "", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
  };
  static final String[] weekdaysDefault = {
    "", "Sunday", "Monday", "Tuesday",
    "Wednesday", "Thursday", "Friday", "Saturday"
  };

  private static final Object[][] contents =
  {
    // These are for DecimalFormatSymbols.
    { "decimalSeparator", "." },
    { "digit", "#" },
    { "exponential", "E" },
    { "groupingSeparator", "," },
    { "infinity", "\u221e" },
    { "minusSign", "-" },
    { "NaN", "\ufffd" },
    { "patternSeparator", ";" },
    { "percent", "%" },
    { "perMill", "\u2030" },
    { "zeroDigit", "0" },

    // These are for NumberFormat.
    { "numberFormat", "#,##0.###" },
    { "percentFormat", "#,##0%" },

    // These are for DateFormatSymbols.
    { "ampm", ampmsDefault },
    { "eras", erasDefault },
    { "datePatternChars", localPatternCharsDefault },
    { "months", monthsDefault },
    { "shortMonths", shortMonthsDefault },
    { "shortWeekdays", shortWeekdaysDefault },
    { "weekdays", weekdaysDefault }
  };

  protected Object[][] getContents ()
    {
      return contents;
    }
}
