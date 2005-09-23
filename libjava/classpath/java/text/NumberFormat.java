/* NumberFormat.java -- Formats and parses numbers
   Copyright (C) 1998, 1999, 2000, 2001, 2003, 2004 Free Software Foundation, Inc.

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


package java.text;

import java.io.IOException;
import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Currency;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

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
 * @author Tom Tromey (tromey@cygnus.com)
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

  public static class Field extends Format.Field
  {
    static final long serialVersionUID = 7494728892700160890L;

    /**
     * Attribute set to all characters containing digits of the integer
     * part.
     */
    public static final NumberFormat.Field INTEGER
      = new Field("integer");

    /**
     * Attribute set to all characters containing digits of the fractional
     * part.
     */
    public static final NumberFormat.Field FRACTION
      = new Field("fraction");

    /**
     * Attribute set to all characters containing digits of the exponential
     * part.
     */
    public static final NumberFormat.Field EXPONENT
      = new Field("exponent");

    /**
     * Attribute set to all characters containing a decimal separator.
     */
    public static final NumberFormat.Field DECIMAL_SEPARATOR
      = new Field("decimal separator");

    /**
     * Attribute set to all characters containing a sign (plus or minus).
     */
    public static final NumberFormat.Field SIGN
      = new Field("sign");

    /**
     * Attribute set to all characters containing a grouping separator (e.g.
     * a comma, a white space,...).
     */
    public static final NumberFormat.Field GROUPING_SEPARATOR
      = new Field("grouping separator");

    /**
     * Attribute set to all characters containing an exponential symbol (e.g.
     * 'E')
     */
    public static final NumberFormat.Field EXPONENT_SYMBOL
      = new Field("exponent symbol");

    /**
     * Attribute set to all characters containing a percent symbol (e.g. '%')
     */
    public static final NumberFormat.Field PERCENT
      = new Field("percent");

    /**
     * Attribute set to all characters containing a permille symbol.
     */
    public static final NumberFormat.Field PERMILLE
      = new Field("permille");

    /**
     * Attribute set to all characters containing the currency unit.
     */
    public static final NumberFormat.Field CURRENCY
      = new Field("currency");

    /**
     * Attribute set to all characters containing the exponent sign.
     */
    public static final NumberFormat.Field EXPONENT_SIGN
      = new Field("exponent sign");

    /**
     * Private fields to register all fields contained in this descriptor.
     */
    private static final NumberFormat.Field[] allFields =
    {
      INTEGER, FRACTION, EXPONENT, DECIMAL_SEPARATOR, SIGN,
      GROUPING_SEPARATOR, EXPONENT_SYMBOL, PERCENT,
      PERMILLE, CURRENCY, EXPONENT_SIGN
    };

    /**
     * This constructor is only used by the deserializer. Without it,
     * it would fail to construct a valid object.
     */
    private Field()
    {
      super("");
    }

    /**
     * Create a Field instance with the specified field name.
     *
     * @param field_name Field name for the new Field instance.
     */
    protected Field(String field_name)
    {
      super (field_name);
    }

    /**
     * This function is used by the deserializer to know which object
     * to use when it encounters an encoded NumberFormat.Field in a 
     * serialization stream. If the stream is valid it should return
     * one of the above field. In the other case we throw an exception.
     *
     * @return a valid official NumberFormat.Field instance.
     *
     * @throws InvalidObjectException if the field name is invalid.
     */
    protected Object readResolve() throws InvalidObjectException
    {
      String s = getName();
      for (int i = 0; i < allFields.length; i++)
	if (s.equals(allFields[i].getName()))
	  return allFields[i];

      throw new InvalidObjectException("no such NumberFormat field called "
				       + s);
    }
  }

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
   * @param sbuf The <code>StringBuffer</code> to append the formatted number 
   *             to.
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
   * @param sbuf The <code>StringBuffer</code> to append the formatted number 
   *             to.
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

  private static NumberFormat computeInstance(Locale loc, String resource,
                                              String def)
  {
    ResourceBundle res;
    try
      {
	res = ResourceBundle.getBundle("gnu.java.locale.LocaleInformation",
		loc, ClassLoader.getSystemClassLoader());
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
    NumberFormat format;
    
    format = computeInstance (loc, "currencyFormat", "$#,##0.00;($#,##0.00)");
    format.setMaximumFractionDigits(format.getCurrency().getDefaultFractionDigits());  
    return format;
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
   * @param loc The desired locale.
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
   * This method returns an integer formatting and parsing class for the
   * default locale. This will be a concrete subclass of <code>NumberFormat</code>,
   * but the actual class returned is dependent on the locale.
   *
   * @return An instance of an integer number formatter for the default locale.
   * @since 1.4 
   */
  public static final NumberFormat getIntegerInstance()
  {
    return getIntegerInstance (Locale.getDefault());
  }

  /**
   * This method returns an integer formatting and parsing class for the
   * default locale. This will be a concrete subclass of <code>NumberFormat</code>,
   * but the actual class returned is dependent on the locale.
   *
   * @param locale the desired locale.
   *
   * @return An instance of an integer number formatter for the desired locale.
   * @since 1.4 
   */
  public static NumberFormat getIntegerInstance(Locale locale)
  {
    NumberFormat format = computeInstance (locale, "numberFormat", "#,##0");
    format.setMaximumFractionDigits(0);
    format.setParseIntegerOnly (true);
    return format;
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
   * @param loc The desired locale.
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
   * @param sourceStr The string to parse.
   * @param pos The desired <code>ParsePosition</code>.
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
   * @param sourceStr The string to parse.
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
   * @param sourceStr The string to parse.
   * @param pos The desired <code>ParsePosition</code>.
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
   * @param newValue <code>true</code> to enable grouping,
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
   * @param digits The new maximum fraction digits value.
   */
  public void setMaximumFractionDigits (int digits)
  {
    maximumFractionDigits = digits;
    if (getMinimumFractionDigits () > maximumFractionDigits)
      setMinimumFractionDigits (maximumFractionDigits);
  }

  /**
   * This method sets the maximum number of digits allowed in the integer
   * portion of a number to the specified value.  If this is less than the
   * current minimum allowed digits, the minimum allowed digits value will
   * be lowered to be equal to the new maximum allowed digits value.
   *
   * @param digits The new maximum integer digits value.
   */
  public void setMaximumIntegerDigits (int digits)
  {
    maximumIntegerDigits = digits;
    if (getMinimumIntegerDigits () > maximumIntegerDigits)
      setMinimumIntegerDigits (maximumIntegerDigits);
  }

  /**
   * This method sets the minimum number of digits allowed in the fraction
   * portion of a number to the specified value.  If this is greater than the
   * current maximum allowed digits, the maximum allowed digits value will
   * be raised to be equal to the new minimum allowed digits value.
   *
   * @param digits The new minimum fraction digits value.
   */
  public void setMinimumFractionDigits (int digits)
  {
    minimumFractionDigits = digits;
    if (getMaximumFractionDigits () < minimumFractionDigits)
      setMaximumFractionDigits (minimumFractionDigits);
  }

  /**
   * This method sets the minimum number of digits allowed in the integer
   * portion of a number to the specified value.  If this is greater than the
   * current maximum allowed digits, the maximum allowed digits value will
   * be raised to be equal to the new minimum allowed digits value.
   *
   * @param digits The new minimum integer digits value.
   */
  public void setMinimumIntegerDigits (int digits)
  {
    minimumIntegerDigits = digits;
    if (getMaximumIntegerDigits () < minimumIntegerDigits)
      setMaximumIntegerDigits (minimumIntegerDigits);
  }

  /** 
   * This method sets the parsing behavior of this object to parse only 
   * integers or not.
   *
   * @param value <code>true</code> to parse only integers,
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

  /**
   * Returns the currency used by this number format when formatting currency
   * values.
   *
   * The default implementation throws UnsupportedOperationException.
   *
   * @return The used currency object, or null.
   *
   * @throws UnsupportedOperationException If the number format class doesn't
   * implement currency formatting.
   *
   * @since 1.4
   */
  public Currency getCurrency()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * Sets the currency used by this number format when formatting currency
   * values.
   *
   * The default implementation throws UnsupportedOperationException.
   *
   * @param currency The new currency to be used by this number format.
   *
   * @throws NullPointerException If currenc is null.
   * @throws UnsupportedOperationException If the number format class doesn't
   * implement currency formatting.
   *
   * @since 1.4
   */
  public void setCurrency(Currency currency)
  {
    if (currency == null)
      throw new NullPointerException("currency may not be null");
    
    throw new UnsupportedOperationException();
  }
}
