// Generic English locale data for java.text.

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
    { "weekdays", weekdaysDefault },

    // For RuleBasedCollator.
    // FIXME: this is nowhere near complete.
    // In particular we must mark accents as ignorable,
    // and probably other things as well.
    { "collatorRule", "< 0 < 1 < 2 < 3 < 4 < 5 < 6 < 7 < 8 < 9 < a,A < b,B < c,C < d,D < e,E < f,F < g,G < h,H < i,I < j,J < k,K < l,L < m,M < n,N < o,O < p,P < q,Q < r,R < s,S < t,T < u,U < v,V < w,W < x,X < y,Y < z,Z" }
  };

  protected Object[][] getContents ()
    {
      return contents;
    }
}
