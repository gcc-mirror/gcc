/* LocaleInformation_en.java -- US English locale data
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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
  * This class contains locale data for the US English locale
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
private static final String collation_rules = 
  "-<0,1<2<3<4<5<6<7<8<9A,a<b,B<c,C<d,D<e,E<f,F<g,G<h,H<i,I<j,J<j,K" +
  "<l,L<m,M<n,N<o,O<p,P<q,Q<r,R<s,S<t,T<u,U<v,V<w,W<x,X<y,Y,z<Z";

/*
 * For the followings lists, strings that are subsets of other break strigns
 * must be listed first.  For example, if "\r" and "\r\n" are sequences,
 * the "\r" must be first or it will never be used.
 */

/**
  * This is the list of word separator characters used by 
  * java.text.BreakIterator 
  */
private static final String[] word_breaks = { " ", "\t", "\r\n", "\n" }; 

/**
  * This is the list of sentence break sequences used by 
  * java.text.BreakIterator
  */
private static final String[] sentence_breaks = { ". " };

/**
  * This is the list of potential line break locations.
  */
private static final String[] line_breaks = { "\t", "-", "\r\n", 
  "\n", ".  ", ". ", ".",  "?  ", "? ", "?",  "!  ", "! ", "!", ", ", " " };

/**
  * This is the list of months, fully spelled out
  */
private static final String[] months = { "January", "February", "March", 
  "April", "May", "June", "July", "August", "September", "October",
  "November", "December", null };

/**
  * This is the list of abbreviated month names
  */
private static final String[] shortMonths = { "Jan", "Feb", "Mar", "Apr", "May",
  "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", null };

/**
  * This is the list of weekdays, fully spelled out
  */
private static final String[] weekdays = { null, "Sunday", "Monday", "Tuesday",
  "Wednesday", "Thursday", "Friday", "Saturday" };

/**
  * This is the list of abbreviated weekdays
  */
private static final String[] shortWeekdays = { null, "Sun", "Mon", "Tue", "Wed",
  "Thu", "Fri", "Sat" };

/**
  * This is the list of AM/PM strings
  */
private static final String[] ampms = { "AM", "PM" };

/**
  * This is the list of era identifiers
  */
private static final String[] eras = { "BC", "AD" };

/**
  * This is the list of timezone strings.  The JDK appears to include a
  * city name as the sixth element.
  */
private static final String[][] zoneStrings =
{
  { "EST6EDT", "Eastern Standard Time", "EST", "Eastern Daylight Time", "EDT",
    "New York" },
  { "EST6", "Eastern Standard Time", "EST", "Eastern Standard Time", "EST",
    "Indianapolis" },
  { "CST6CDT", "Central Standard Time", "CST", "Central Daylight Time", "CDT",
    "Chicago" },
  { "MST6MDT", "Mountain Standard Time", "MST", "Mountain Daylight Time", 
    "MDT", "Denver" },
  { "MST6", "Mountain Standard Time", "MST", "Mountain Standard Time", "MST",
    "Phoenix" },
  { "PST6PDT", "Pacific Standard Time", "PDT", "Pacific Daylight Time", "PDT",
    "San Francisco" },
  { "AST6ADT", "Alaska Standard Time", "AST", "Alaska Daylight Time", "ADT",
    "Anchorage" },
  { "HST6HDT", "Hawaii Standard Time", "HST", "Hawaii Daylight Time", "HDT",
    "Honolulu" },
  // European time zones.  The city names are a little bit random.
  { "WET", "Western European Time", "WET", "Western European Savings Time", "WEST", "London" },
  { "CET", "Central European Time", "CET", "Central European Savings Time", "CEST", "Berlin" },
  { "EET", "Eastern European Time", "EET", "Eastern European Savings Time", "EEST", "Istanbul" },
};

/**
  * This is the list of pattern characters for formatting dates
  */
private static final String localPatternChars = "GyMdkHmsSEDFwWahKz"; // Not a mistake!

/**
  * This is the DateFormat.SHORT date format
  */
private static final String shortDateFormat = "M/d/yy";

/**
  * This is the DateFormat.MEDIUM format
  */
private static final String mediumDateFormat = "dd-MMM-yy";

/**
  * This is the DateFormat.LONG format
  */
private static final String longDateFormat = "MMMM d, yyyy";

/**
  * This is the DateFormat.FULL format
  */
private static final String fullDateFormat = "EEEE, MMMM d, yyyy";

/**
  * This is the DateFormat.DEFAULT format
  */
private static final String defaultDateFormat = "dd-MMM-yy";

/**
  * This is the DateFormat.SHORT format
  */
private static final String shortTimeFormat = "h:mm a";

/**
  * This is the DateFormat.MEDIUM format
  */
private static final String mediumTimeFormat = "h:mm:ss a";

/**
  * This is the DateFormat.LONG format
  */
private static final String longTimeFormat = "h:mm:ss a z";

/**
  * This is the DateFormat.FULL format
  */
private static final String fullTimeFormat = "h:mm:ss 'o''clock' a z";

/**
  * This is the DateFormat.DEFAULT format
  */
private static final String defaultTimeFormat = "h:mm:ss a";

/**
  * This is the currency symbol
  */
private static final String currencySymbol = "$";

/**
  * This is the international currency symbol. 
  */
private static final String intlCurrencySymbol = "US$";

/**
  * This is the decimal point.
  */
private static final String decimalSeparator = ".";

/**
  * This is the exponential symbol
  */
private static final String exponential = "E";

/**
  * This is the char used for digits in format strings
  */
private static final String digit = "#";

/**
  * This is the grouping separator symbols
  */
private static final String groupingSeparator = ",";

/**
  * This is the symbols for infinity
  */
private static final String infinity = "\u221e";

/**
  * This is the symbol for the not a number value
  */
private static final String NaN = "\ufffd";

/**
  * This is the minus sign symbol.
  */
private static final String minusSign = "-";

/**
  * This is the decimal separator in monetary values.
  */
private static final String monetarySeparator = ".";

/**
  * This is the separator between positive and negative subpatterns.
  */
private static final String patternSeparator = ";";

/**
  * This is the percent sign
  */
private static final String percent = "%";

/**
  * This is the per mille sign
  */
private static final String perMill = "\u2030";

/**
  * This is the character for zero.
  */
private static final String zeroDigit = "0";

/*************************************************************************/

/**
  * This is the object array used to hold the keys and values
  * for this bundle
  */

private static final Object[][] contents =
{
  // For RuleBasedCollator
  { "collation_rules", collation_rules },
  // For BreakIterator
  { "word_breaks", word_breaks },
  { "sentence_breaks", sentence_breaks },
  { "line_breaks", line_breaks },
  // For SimpleDateFormat/DateFormatSymbols
  { "months", months },
  { "shortMonths", shortMonths },
  { "weekdays", weekdays },
  { "shortWeekdays", shortWeekdays },
  { "ampms", ampms },
  { "eras", eras },
  { "zoneStrings", zoneStrings },
  { "localPatternChars", localPatternChars },
  { "shortDateFormat", shortDateFormat },
  { "mediumDateFormat", mediumDateFormat },
  { "longDateFormat", longDateFormat },
  { "fullDateFormat", fullDateFormat },
  { "defaultDateFormat", defaultDateFormat },
  { "shortTimeFormat", shortTimeFormat },
  { "mediumTimeFormat", mediumTimeFormat },
  { "longTimeFormat", longTimeFormat },
  { "fullTimeFormat", fullTimeFormat },
  { "defaultTimeFormat", defaultTimeFormat },
  // For DecimalFormat/DecimalFormatSymbols
  { "currencySymbol", currencySymbol },
  { "intlCurrencySymbol", intlCurrencySymbol },
  { "decimalSeparator", decimalSeparator },
  { "digit", digit },
  { "exponential", exponential },
  { "groupingSeparator", groupingSeparator },
  { "infinity", infinity },
  { "NaN", NaN },
  { "minusSign", minusSign },
  { "monetarySeparator", monetarySeparator },
  { "patternSeparator", patternSeparator },
  { "percent", percent },
  { "perMill", perMill },
  { "zeroDigit", zeroDigit },
};

/*************************************************************************/

/**
  * This method returns the object array of key, value pairs containing
  * the data for this bundle.
  *
  * @return The key, value information.
  */
public Object[][]
getContents()
{
  return(contents);
}

} // class LocaleInformation_en

