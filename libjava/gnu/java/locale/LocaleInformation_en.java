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
 * This class contains locale data for English.
 */
public class LocaleInformation_en extends ListResourceBundle
{

  /*
   * This area is used for defining object values
   */

  /**
   * This is the set of collation rules used by java.text.RuleBasedCollator 
   * to sort strings properly.  See the documentation of that class for the 
   * proper format.
   */
  // FIXME: this is nowhere near complete.
  // In particular we must mark accents as ignorable,
  // and probably other things as well.
  private static final String collation_rules = 
  "<0<1<2<3<4<5<6<7<8<9<A,a<b,B<c,C<d,D<e,E<f,F<g,G<h,H<i,I<j,J<k,K" +
  "<l,L<m,M<n,N<o,O<p,P<q,Q<r,R<s,S<t,T<u,U<v,V<w,W<x,X<y,Y,z<Z";

  /**
   * This is the list of months, fully spelled out
   */
  private static final String[] months =
  {
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December", null
  };

  /**
   * This is the list of abbreviated month names
   */
  private static final String[] shortMonths =
  {
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", null
  };

  /**
   * This is the list of weekdays, fully spelled out
   */
  private static final String[] weekdays =
  {
    null, "Sunday", "Monday", "Tuesday", "Wednesday",
    "Thursday", "Friday", "Saturday"
  };

  /**
   * This is the list of abbreviated weekdays
   */
  private static final String[] shortWeekdays =
  {
    null, "Sun", "Mon", "Tue", "Wed",
    "Thu", "Fri", "Sat"
  };

  /**
   * This is the list of AM/PM strings
   */
  private static final String[] ampms = { "AM", "PM" };

  /**
   * This is the list of era identifiers
   */
  private static final String[] eras = { "BC", "AD" };

  /*************************************************************************/

  /**
   * This is the object array used to hold the keys and values
   * for this bundle
   */

  private static final Object[][] contents =
  {
    // For RuleBasedCollator
    { "collation_rules", collation_rules },

    // For SimpleDateFormat/DateFormatSymbols
    { "months", months },
    { "shortMonths", shortMonths },
    { "weekdays", weekdays },
    { "shortWeekdays", shortWeekdays },
    { "ampms", ampms },
    { "eras", eras },
    { "localPatternChars", "GyMdkHmsSEDFwWahKz" },

    // For DecimalFormat/DecimalFormatSymbols
    { "decimalSeparator", "." },
    { "digit", "#" },
    { "exponential", "E" },
    { "groupingSeparator", "," },
    { "infinity", "\u221e" },
    { "NaN", "\ufffd" },
    { "minusSign", "-" },
    { "monetarySeparator", "." },
    { "patternSeparator", ";" },
    { "percent", "%" },
    { "perMill", "\u2030" },
    { "zeroDigit", "0" },

    // For NumberFormat.
    { "numberFormat", "#,##0.###" },
    { "percentFormat", "#,##0%" },
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
