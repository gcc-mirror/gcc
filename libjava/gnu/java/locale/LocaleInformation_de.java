/* LocaleInformation_de.java -- German locale data
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
  * This class contains locale data for the German locale
  * @author Jochen Hoenicke
  */
public class LocaleInformation_de extends ListResourceBundle
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
  "<0<1<2<3<4<5<6<7<8<9<A,a<b,B<c,C<d,D<e,E<f,F<g,G<h,H<i,I<j,J<k,K" +
  "<l,L<m,M<n,N<o,O<p,P<q,Q<r,R<s,S<t,T<u,U<v,V<w,W<x,X<y,Y<z,Z" + 
  "&ae,\u00e4&Ae,\u00c4&oe,\u00f6&Oe,\u00d6&ue,\u00fc&Ue,\u00dc&ss,\u00df";

/**
  * This is the list of months, fully spelled out
  */
private static final String[] months = { "Januar", "Februar", "M\u00e4rz", 
  "April", "Mai", "Juni", "Juli", "August", "September", "Oktober",
  "November", "Dezember", null };

/**
  * This is the list of abbreviated month names
  */
private static final String[] shortMonths = { 
  "Jan", "Feb", "M\u00e4r", "Apr", "Mai",
  "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez", null 
};

/**
  * This is the list of weekdays, fully spelled out
  */
private static final String[] weekdays = { 
  null, "Sonntag", "Montag", "Dienstag",
  "Mittwoch", "Donnerstag", "Freitag", "Samstag" 
};

/**
  * This is the list of abbreviated weekdays
  */
private static final String[] shortWeekdays = { 
  null, "So", "Mo", "Di", "Mi", "Do", "Fr", "Sa" 
};

/**
  * This is the list of era identifiers
  */
private static final String[] eras = { "v. Chr.", "n. Chr." };

/**
  * This is the list of timezone strings.  The JDK appears to include a
  * city name as the sixth element.
  */
private static final String[][] zoneStrings =
{
  // European time zones.  The city names are a little bit random.
  { "WET", "Westeurop\u00e4ische Zeit", "WEZ", "Westeurop\u00e4ische Sommerzeit", "WESZ", "London" },
  { "CET", "Mitteleurop\u00e4ische Zeit", "MEZ", "Mitteleurop\u00e4ische Sommerzeit", "MESZ", "Berlin" },
  { "EET", "Osteurop\u00e4ische Zeit", "OEZ", "Mitteleurop\u00e4ische Sommerzeit", "OESZ", "Istanbul" },
};

/**
  * This is the DateFormat.SHORT date format
  */
private static final String shortDateFormat = "dd.MM.yy";

/**
  * This is the DateFormat.MEDIUM format
  */
private static final String mediumDateFormat = "d. MMM yy";

/**
  * This is the DateFormat.LONG format
  */
private static final String longDateFormat = "d. MMMM yyyy";

/**
  * This is the DateFormat.FULL format
  */
private static final String fullDateFormat = "EEEE, d. MMMM yyyy";

/**
  * This is the DateFormat.DEFAULT format
  */
private static final String defaultDateFormat = "dd.MM.yy";

/**
  * This is the DateFormat.SHORT format
  */
private static final String shortTimeFormat = "H:mm";

/**
  * This is the DateFormat.MEDIUM format
  */
private static final String mediumTimeFormat = "H:mm:ss";

/**
  * This is the DateFormat.LONG format
  */
private static final String longTimeFormat = "H:mm:ss z";

/**
  * This is the DateFormat.FULL format
  */
private static final String fullTimeFormat = "H:mm:ss 'Uhr' z";

/**
  * This is the DateFormat.DEFAULT format
  */
private static final String defaultTimeFormat = "H:mm:ss";

/**
  * This is the currency symbol
  */
private static final String currencySymbol = "DM";

/**
  * This is the international currency symbol. 
  */
private static final String intlCurrencySymbol = "DEM";

/**
  * This is the decimal point.
  */
private static final String decimalSeparator = ",";

/**
  * This is the decimal separator in monetary values.
  */
private static final String monetarySeparator = ",";

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
  { "eras", eras },
  { "zoneStrings", zoneStrings },
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
  { "monetarySeparator", monetarySeparator },
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

} // class LocaleInformation_de
