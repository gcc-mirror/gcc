/* NumberFormat.java -- Formats and parses numbers
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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


package java.text;

import java.util.Locale;
import java.util.ResourceBundle;
import java.util.MissingResourceException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.IOException;

/**
 * This is the abstract superclass of all classes which format and 
 * parse numeric values such as decimal numbers, integers, currency values,
 * and percentages.  These classes perform their parsing and formatting
 * in a locale specific manner, accounting for such items as differing
 * currency symbols and thousands separators.
 * <p>
 * To create an instance of a concrete subclass of <code>NumberFormat</code>,
 * do not call a class constructor directly.  Instead, use one of the
 * static factory methods in this class such as 
 * <code>getCurrencyInstance</code>.
 * 
 * @author Tom Tromey <tromey@cygnus.com>
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @date March 4, 1999
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 from http://www.javasoft.com.
 * Status:  Believed complete and correct to 1.2, except getAvailableLocales.
 */
public abstract class NumberFormat extends Format implements Cloneable
{
  /**
   * This is a constant used to create a <code>FieldPosition</code> object
   * that will return the integer portion of a formatted number.
   */
  public static final int INTEGER_FIELD = 0;

  /**
   * This is a constant used to create a <code>FieldPosition</code> object
   * that will return the fractional portion of a formatted number.
   */
  public static final int FRACTION_FIELD = 1;

  /**
   * This method is a specialization of the format method that performs
   * a simple formatting of the specified <code>long</code> number.
   *
   * @param number The <code>long</code> to format.
   *
   * @return The formatted number
   */
  public final String format (long number)
  {
    StringBuffer sbuf = new StringBuffer(50);
    format (number, sbuf, null);
    return sbuf.toString();
  }

  public final StringBuffer format (Object obj, StringBuffer sbuf,
				    FieldPosition pos)
  {
    if (obj instanceof Number)
      return format(((Number) obj).doubleValue(), sbuf, pos);
    else
      throw new IllegalArgumentException 
	("Cannot format given Object as a Number");
  }

  /**
   * This method formats the specified <code>double</code> and appends it to
   * a <code>StringBuffer</code>.
   * 
   * @param number The <code>double</code> to format.
   * @param sb The <code>StringBuffer</code> to append the formatted number to.
   * @param pos The desired <code>FieldPosition</code>.
   *
   * @return The <code>StringBuffer</code> with the appended number.
   */
  public abstract StringBuffer format (double number,
				       StringBuffer sbuf, FieldPosition pos);

  /**
   * This method formats the specified <code>long</code> and appends it to
   * a <code>StringBuffer</code>.
   * 
   * @param number The <code>long</code> to format.
   * @param sb The <code>StringBuffer</code> to append the formatted number to.
   * @param pos The desired <code>FieldPosition</code>.
   *
   * @return The <code>StringBuffer</code> with the appended number.
   */
  public abstract StringBuffer format (long number,
				       StringBuffer sbuf, FieldPosition pos);

  /**
   * This method tests the specified object for equality against this object.
   * This will be <code>true</code> if the following conditions are met:
   * <p>
   * <ul>
   * <li>The specified object is not <code>null</code>.
   * <li>The specified object is an instance of <code>NumberFormat</code>.
   * </ul>
   * <p>
   * Since this method does not test much, it is highly advised that 
   * concrete subclasses override this method.
   *
   * @param obj The <code>Object</code> to test against equality with
   *            this object. 
   * 
   * @return <code>true</code> if the specified object is equal to
   * this object, <code>false</code> otherwise. 
   */
  public boolean equals (Object obj)
  {
    if (! (obj instanceof NumberFormat))
      return false;
    NumberFormat nf = (NumberFormat) obj;
    return (groupingUsed == nf.groupingUsed
	    && maximumFractionDigits == nf.maximumFractionDigits
	    && maximumIntegerDigits == nf.maximumIntegerDigits
	    && minimumFractionDigits == nf.minimumFractionDigits
	    && minimumIntegerDigits == nf.minimumIntegerDigits
	    && parseIntegerOnly == nf.parseIntegerOnly);
  }

  /**
   * This method returns a list of locales for which concrete instances
   * of <code>NumberFormat</code> subclasses may be created.
   *
   * @return The list of available locales.
   */
  public static Locale[] getAvailableLocales ()
  {
    Locale[] list = new Locale[1];
    list[0] = Locale.US;
    return list;
  }

  private static final NumberFormat computeInstance (Locale loc,
						     String resource,
						     String def)
  {
    ResourceBundle res;
    try
      {
	res = ResourceBundle.getBundle("gnu.java.locale.LocaleInformation",
				       loc);
      }
    catch (MissingResourceException x)
      {
	res = null;
      }
    String fmt;
    try
      {
	fmt = res == null ? def : res.getString(resource);
      }
    catch (MissingResourceException x)
      {
	fmt = def;
      }
    DecimalFormatSymbols dfs = new DecimalFormatSymbols (loc);
    return new DecimalFormat (fmt, dfs);
  }

  /**
   * This method returns an instance of <code>NumberFormat</code> suitable
   * for formatting and parsing currency values in the default locale.
   *
   * @return An instance of <code>NumberFormat</code> for handling currencies.
   */
  public static final NumberFormat getCurrencyInstance ()
  {
    return getCurrencyInstance (Locale.getDefault());
  }

  /**
   * This method returns an instance of <code>NumberFormat</code> suitable
   * for formatting and parsing currency values in the specified locale.
   *
   * @return An instance of <code>NumberFormat</code> for handling currencies.
   */
  public static NumberFormat getCurrencyInstance (Locale loc)
  {
    return computeInstance (loc, "currencyFormat", "$#,##0.00;($#,##0.00)");
  }

  /**
   * This method returns a default instance for the default locale. This
   * will be a concrete subclass of <code>NumberFormat</code>, but the 
   * actual class returned is dependent on the locale.
   *
   * @return An instance of the default <code>NumberFormat</code> class.
   */
  public static final NumberFormat getInstance ()
  {
    return getInstance (Locale.getDefault());
  }

  /**
   * This method returns a default instance for the specified locale. This
   * will be a concrete subclass of <code>NumberFormat</code>, but the 
   * actual class returned is dependent on the locale.
   *
   * @param locale The desired locale.
   *
   * @return An instance of the default <code>NumberFormat</code> class.
   */
  public static NumberFormat getInstance (Locale loc)
  {
    // For now always return a number instance.
    return getNumberInstance (loc);
  }

  /**
   * This method returns the maximum number of digits allowed in the fraction
   * portion of a number.
   *
   * @return The maximum number of digits allowed in the fraction
   * portion of a number. 
   */
  public int getMaximumFractionDigits ()
  {
    return maximumFractionDigits;
  }

  /**
   * This method returns the maximum number of digits allowed in the integer
   * portion of a number.
   *
   * @return The maximum number of digits allowed in the integer
   * portion of a number. 
   */
  public int getMaximumIntegerDigits ()
  {
    return maximumIntegerDigits;
  }

  /**
   * This method returns the minimum number of digits allowed in the fraction
   * portion of a number.
   *
   * @return The minimum number of digits allowed in the fraction
   * portion of a number. 
   */
  public int getMinimumFractionDigits ()
  {
    return minimumFractionDigits;
  }

  /**
   * This method returns the minimum number of digits allowed in the integer
   * portion of a number.
   *
   * @return The minimum number of digits allowed in the integer
   * portion of a number. 
   */
  public int getMinimumIntegerDigits ()
  {
    return minimumIntegerDigits;
  }

  /**
   * This method returns a default instance for the specified locale. This
   * will be a concrete subclass of <code>NumberFormat</code>, but the 
   * actual class returned is dependent on the locale.
   *
   * @param locale The desired locale.
   *
   * @return An instance of the default <code>NumberFormat</code> class.
   */
  public static final NumberFormat getNumberInstance ()
  {
    return getNumberInstance (Locale.getDefault());
  }

  /**
   * This method returns a general purpose number formatting and parsing
   * class for the default locale.  This will be a concrete subclass of
   * <code>NumberFormat</code>, but the actual class returned is dependent
   * on the locale.
   *
   * @return An instance of a generic number formatter for the default locale.
   */
  public static NumberFormat getNumberInstance (Locale loc)
  {
    return computeInstance (loc, "numberFormat", "#,##0.###");
  }

  /**
   * This method returns an instance of <code>NumberFormat</code> suitable
   * for formatting and parsing percentage values in the default locale.
   *
   * @return An instance of <code>NumberFormat</code> for handling percentages.
   */
  public static final NumberFormat getPercentInstance ()
  {
    return getPercentInstance (Locale.getDefault());
  }

  /**
   * This method returns an instance of <code>NumberFormat</code> suitable
   * for formatting and parsing percentage values in the specified locale.
   *
   * @param locale The desired locale.
   *
   * @return An instance of <code>NumberFormat</code> for handling percentages.
   */
  public static NumberFormat getPercentInstance (Locale loc)
  {
    return computeInstance (loc, "percentFormat", "#,##0%");
  }

  /**
   * This method returns a hash value for this object.
   *
   * @return The hash code.
   */
  public int hashCode ()
  {
    int hash = super.hashCode();
    hash ^= (maximumFractionDigits + maximumIntegerDigits
	     + minimumFractionDigits + minimumIntegerDigits);
    if (groupingUsed)
      hash ^= 0xf0f0;
    if (parseIntegerOnly)
      hash ^= 0x0f0f;
    return hash;
  }

  /**
   * This method tests whether or not grouping is in use.  Grouping is
   * a method of marking separations in numbers, such as thousand separators
   * in the US English locale.  The grouping positions and symbols are all
   * locale specific.  As an example, with grouping disabled, the number one
   * million would appear as "1000000".  With grouping enabled, this number
   * might appear as "1,000,000".  (Both of these assume the US English
   * locale).
   *
   * @return <code>true</code> if grouping is enabled,
   * <code>false</code> otherwise. 
   */
  public boolean isGroupingUsed ()
  {
    return groupingUsed;
  }

  /**
   * This method tests whether or not only integer values should be parsed.
   * If this class is parsing only integers, parsing stops at the decimal
   * point.
   *
   * @return <code>true</code> if only integers are parsed,
   * <code>false</code> otherwise. 
   */
  public boolean isParseIntegerOnly ()
  {
    return parseIntegerOnly;
  }

  /**
   * This is a default constructor for use by subclasses.
   */
  public NumberFormat ()
  {
  }

  /**
   * This method parses the specified string into a <code>Number</code>.  This
   * will be a <code>Long</code> if possible, otherwise it will be a
   * <code>Double</code>.    If no number can be parsed, no exception is
   * thrown.  Instead, the parse position remains at its initial index.
   *
   * @param str The string to parse.
   * @param pp The desired <code>ParsePosition</code>.
   *
   * @return The parsed <code>Number</code>
   */
  public abstract Number parse (String sourceStr, ParsePosition pos);

  /**
   * This method parses the specified string into a <code>Number</code>.  This
   * will be a <code>Long</code> if possible, otherwise it will be a
   * <code>Double</code>.  If no number can be parsed, an exception will be
   * thrown.
   *
   * @param str The string to parse.
   *
   * @return The parsed <code>Number</code>
   *
   * @exception ParseException If no number can be parsed.
   */
  public Number parse (String sourceStr) throws ParseException
  {
    ParsePosition pp = new ParsePosition (0);
    Number r = parse (sourceStr, pp);
    if (r == null)
      {
	int index = pp.getErrorIndex();
	if (index < 0)
	  index = pp.getIndex();
	throw new ParseException ("couldn't parse number", index);
      }
    return r;
  }

  /**
   * This method parses the specified string into an <code>Object</code>.  This
   * will be a <code>Long</code> if possible, otherwise it will be a
   * <code>Double</code>.    If no number can be parsed, no exception is
   * thrown.  Instead, the parse position remains at its initial index.
   *
   * @param str The string to parse.
   * @param pp The desired <code>ParsePosition</code>.
  *
  * @return The parsed <code>Object</code>
  */
  public final Object parseObject (String sourceStr, ParsePosition pos)
  {
    return parse (sourceStr, pos);
  }

  /**
   * This method sets the grouping behavior of this formatter.  Grouping is
   * a method of marking separations in numbers, such as thousand separators
   * in the US English locale.  The grouping positions and symbols are all
   * locale specific.  As an example, with grouping disabled, the number one
   * million would appear as "1000000".  With grouping enabled, this number
   * might appear as "1,000,000".  (Both of these assume the US English
   * locale).
   *
   * @param groupingUsed <code>true</code> to enable grouping,
   *                     <code>false</code> to disable it. 
   */
  public void setGroupingUsed (boolean newValue)
  {
    groupingUsed = newValue;
  }

  /**
   * This method sets the maximum number of digits allowed in the fraction
   * portion of a number to the specified value.  If this is less than the
   * current minimum allowed digits, the minimum allowed digits value will
   * be lowered to be equal to the new maximum allowed digits value.
   *
   * @param maximumFractionDigits The new maximum fraction digits value.
   */
  public void setMaximumFractionDigits (int newValue)
  {
    maximumFractionDigits = newValue;
    if (getMinimumFractionDigits () > maximumFractionDigits)
      setMinimumFractionDigits (maximumFractionDigits);
  }

  /**
   * This method sets the maximum number of digits allowed in the integer
   * portion of a number to the specified value.  If this is less than the
   * current minimum allowed digits, the minimum allowed digits value will
   * be lowered to be equal to the new maximum allowed digits value.
   *
   * @param maximumIntegerDigits The new maximum integer digits value.
   */
  public void setMaximumIntegerDigits (int newValue)
  {
    maximumIntegerDigits = newValue;
    if (getMinimumIntegerDigits () > maximumIntegerDigits)
      setMinimumIntegerDigits (maximumIntegerDigits);
  }

  /**
   * This method sets the minimum number of digits allowed in the fraction
   * portion of a number to the specified value.  If this is greater than the
   * current maximum allowed digits, the maximum allowed digits value will
   * be raised to be equal to the new minimum allowed digits value.
   *
   * @param minimumFractionDigits The new minimum fraction digits value.
   */
  public void setMinimumFractionDigits (int newValue)
  {
    minimumFractionDigits = newValue;
    if (getMaximumFractionDigits () < minimumFractionDigits)
      setMaximumFractionDigits (minimumFractionDigits);
  }

  /**
   * This method sets the minimum number of digits allowed in the integer
   * portion of a number to the specified value.  If this is greater than the
   * current maximum allowed digits, the maximum allowed digits value will
   * be raised to be equal to the new minimum allowed digits value.
   *
   * @param minimumIntegerDigits The new minimum integer digits value.
   */
  public void setMinimumIntegerDigits (int newValue)
  {
    minimumIntegerDigits = newValue;
    if (getMaximumIntegerDigits () < minimumIntegerDigits)
      setMaximumIntegerDigits (minimumIntegerDigits);
  }

  /** 
   * This method sets the parsing behavior of this object to parse only 
   * integers or not.
   *
   * @param parseIntegerOnly <code>true</code> to parse only integers,
   *                         <code>false</code> otherwise. 
   */
  public void setParseIntegerOnly (boolean value)
  {
    parseIntegerOnly = value;
  }

  /**
   * This method is a specialization of the format method that performs
   * a simple formatting of the specified <code>double</code> number.
   *
   * @param number The <code>double</code> to format.
   *
   * @return The formatted number
   */
  public final String format (double number)
  {
    StringBuffer sbuf = new StringBuffer(50);
    format (number, sbuf, null);
    return sbuf.toString();
  }

  // These field names are fixed by the serialization spec.
  boolean groupingUsed;
  int maximumFractionDigits;
  private byte maxFractionDigits;
  int maximumIntegerDigits;
  private byte maxIntegerDigits;
  int minimumFractionDigits;
  private byte minFractionDigits;
  int minimumIntegerDigits;
  private byte minIntegerDigits;
  boolean parseIntegerOnly;
  private int serialVersionOnStream;
  private static final long serialVersionUID = -2308460125733713944L;

  private void readObject(ObjectInputStream stream)
    throws IOException, ClassNotFoundException
  {
    stream.defaultReadObject();
    if (serialVersionOnStream < 1)
      {
        maximumFractionDigits = maxFractionDigits;
	maximumIntegerDigits = maxIntegerDigits;
	minimumFractionDigits = minFractionDigits;
	minimumIntegerDigits = minIntegerDigits;
	serialVersionOnStream = 1;
      }
  }

  private void writeObject(ObjectOutputStream stream) throws IOException
  {
    maxFractionDigits = maximumFractionDigits < Byte.MAX_VALUE ?
      (byte) maximumFractionDigits : Byte.MAX_VALUE;
    maxIntegerDigits = maximumIntegerDigits < Byte.MAX_VALUE ?
      (byte) maximumIntegerDigits : Byte.MAX_VALUE;
    minFractionDigits = minimumFractionDigits < Byte.MAX_VALUE ?
      (byte) minimumFractionDigits : Byte.MAX_VALUE;
    minIntegerDigits = minimumIntegerDigits < Byte.MAX_VALUE ?
      (byte) minimumIntegerDigits : Byte.MAX_VALUE;
    serialVersionOnStream = 1;
    stream.defaultWriteObject();
  }
}
