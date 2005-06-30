/* LocaleInformation_en.java -- US English locale data
   Copyright (C) 1998, 1999, 2001, 2002 Free Software Foundation, Inc.

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

import java.util.ListResourceBundle;

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
  "<l,L<m,M<n,N<o,O<p,P<q,Q<r,R<s,S<t,T<u,U<v,V<w,W<x,X<y,Y<z,Z";

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
    { "localPatternChars", "GyMdkHmsSEDFwWahKzYeugAZ" },
    { "zoneStrings", zoneStrings },

    { "shortDateFormat", "M/d/yy" },         // Java's Y2K bug.
    { "mediumDateFormat", "d-MMM-yy" },
    { "longDateFormat", "MMMM d, yyyy" },
    { "fullDateFormat", "EEEE MMMM d, yyyy G" },
    { "defaultDateFormat", "d-MMMM-yy" },
    { "shortTimeFormat", "h:mm a" },
    { "mediumTimeFormat", "h:mm:ss a" },
    { "longTimeFormat", "h:mm:ss a z" },
    { "fullTimeFormat", "h:mm:ss;S 'o''clock' a z" },
    { "defaultTimeFormat", "h:mm:ss a" },

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
