/* LocaleInformation_nl.java -- Dutch locale data
   Copyright (C) 1999, 2001 Free Software Foundation, Inc.

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
  * This class contains locale data for the Dutch locale
  */

public class LocaleInformation_nl extends ListResourceBundle
{

/*
 * This area is used for defining object values
 */

/**
  * This is the set of collation rules used by java.text.RuleBasedCollator 
  * to sort strings properly.  See the documentation of that class for the 
  * proper format.
  * <p>
  * This is the same rule as used in the English locale.
  */
private static final String collation_rules = 
  "<0<1<2<3<4<5<6<7<8<9<A,a<b,B<c,C<d,D<e,E<f,F<g,G<h,H<i,I<j,J<k,K" +
  "<l,L<m,M<n,N<o,O<p,P<q,Q<r,R<s,S<t,T<u,U<v,V<w,W<x,X<y,Y<z,Z";

/**
  * This is the list of months, fully spelled out
  */
private static final String[] months = { "januari", "februari", "maart", 
  "april", "mei", "juni", "juli", "augustus", "september", "october",
  "november", "december", null };

/**
  * This is the list of abbreviated month names
  */
private static final String[] shortMonths = { "jan", "feb", "mrt", "apr", "mei",
  "jun", "jul", "aug", "sep", "oct", "nov", "dec", null };

/**
  * This is the list of weekdays, fully spelled out
  */
private static final String[] weekdays = { null, "zondag", "maandag", "dinsdag",
  "woensdag", "donderdag", "vrijdag", "zaterdag" };

/**
  * This is the list of abbreviated weekdays
  */
private static final String[] shortWeekdays = { null, "zo", "ma", "di", "wo",
  "do", "vr", "za" };

/**
  * This is the list of AM/PM strings
  * <p>
  * Is there a real equivalent in Dutch? "Voormiddag"/"Namiddag"?
  * Just using the Latin names for now.
  */
private static final String[] ampms = { "AM", "PM" };

/**
  * This is the list of era identifiers
  * <p>
  * Is there a real equivalent in Dutch? "voor Christus"/"na Christus"?
  * Just use the Latin/English names for now.
  */
private static final String[] eras = { "BC", "AD" };

/**
  * This is the list of timezone strings.  The JDK appears to include a
  * city name as the sixth element.
  * XXX - TODO - FIXME - Which timezones should be included here and how are
  * they called?
  */
private static final String[][] zoneStrings =
{
  // European time zones.  The city names are a little bit random.
  { "WET", "West Europese Tijd", "WET", "West Europese Zomertijd", "WEST", "London" },
  { "CET", "Centraal Europese Tijd", "CET", "Centraal Europese Zomertijd", "CEST", "Amsterdam" },
  { "EET", "Oost Europese Tijd", "EET", "Oost Europese Zomertijd", "EEST", "Istanbul" },
};

/**
  * This is the list of pattern characters for formatting dates
  * <p>
  * This is the same as the English locale uses: era (G), year (y), month (M),
  * month (d), hour from 1-12 (h), hour 0-23 (H), minute (m), second (s),
  * millisecond (S), date of week (E), date of year (D),
  * day of week in month (F), week in year (w), week in month (W), am/pm (a),
  * hour from 1-24 (k), hour from 0-11 (K), time zone (z).
  * Why would you use others?
  */
private static final String localPatternChars = "GyMdhHmsSEDFwWakKz"; // Not a mistake!

/**
  * This is the DateFormat.SHORT date format
  */
private static final String shortDateFormat = "dd-MM-yy";

/**
  * This is the DateFormat.MEDIUM format
  */
private static final String mediumDateFormat = "dd-MMM-yy";

/**
  * This is the DateFormat.LONG format
  */
private static final String longDateFormat = "dd MMMM yyyy";

/**
  * This is the DateFormat.FULL format
  */
private static final String fullDateFormat = "EEEE dd MMMM yyyy";

/**
  * This is the DateFormat.DEFAULT format
  */
private static final String defaultDateFormat = mediumDateFormat;

/**
  * This is the TimeFormat.SHORT format
  */
private static final String shortTimeFormat = "HH:mm";

/**
  * This is the TimeFormat.MEDIUM format
  */
private static final String mediumTimeFormat = "HH:mm:ss";

/**
  * This is the TimeFormat.LONG format
  */
private static final String longTimeFormat = "HH:mm:ss";

/**
  * This is the TimeFormat.FULL format
  */
private static final String fullTimeFormat = "HH:mm:ss z";

/**
  * This is the TimeFormat.DEFAULT format
  */
private static final String defaultTimeFormat = shortTimeFormat;

/**
  * This is the currency symbol
  */
private static final String currencySymbol = "fl";

/**
  * This is the international currency symbol. 
  */
private static final String intlCurrencySymbol = "NLG";

/**
  * This is the decimal point.
  */
private static final String decimalSeparator = ",";

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
private static final String monetarySeparator = ",";

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

} // class LocaleInformation_nl
