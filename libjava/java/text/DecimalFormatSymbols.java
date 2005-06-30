/* DecimalFormatSymbols.java -- Format symbols used by DecimalFormat
   Copyright (C) 1999, 2000, 2001, 2004 Free Software Foundation, Inc.

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
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.Currency;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * This class is a container for the symbols used by 
 * <code>DecimalFormat</code> to format numbers and currency.  These are
 * normally handled automatically, but an application can override
 * values as desired using this class.
 *
 * @author Tom Tromey (tromey@cygnus.com)
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @date February 24, 1999
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 from http://www.javasoft.com.
 * Status:  Believed complete and correct to 1.2.
 */
public final class DecimalFormatSymbols implements Cloneable, Serializable
{
  public Object clone ()
  {
    try
      {
	return super.clone ();
      }
    catch(CloneNotSupportedException e)
      {
	return null;
      }
  }

  /**
   * This method initializes a new instance of
   * <code>DecimalFormatSymbols</code> for the default locale.
   */
  public DecimalFormatSymbols ()
  {
    this (Locale.getDefault());
  }

  private String safeGetString(ResourceBundle bundle,
                               String name, String def)
  {
    if (bundle != null)
      {
	try
	  {
	    return bundle.getString(name);
	  }
	catch (MissingResourceException x)
	  {
	  }
      }
    return def;
  }

  private char safeGetChar(ResourceBundle bundle,
                           String name, char def)
  {
    String r = null;
    if (bundle != null)
      {
	try
	  {
	    r = bundle.getString(name);
	  }
	catch (MissingResourceException x)
	  {
	  }
      }
    if (r == null || r.length() < 1)
      return def;
    return r.charAt(0);
  }

  /**
   * This method initializes a new instance of
   * <code>DecimalFormatSymbols</code> for the specified locale.
   *
   * @param locale The local to load symbols for.
   */
  public DecimalFormatSymbols (Locale loc)
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
    currencySymbol = safeGetString (res, "currencySymbol", "$");
    decimalSeparator = safeGetChar (res, "decimalSeparator", '.');
    digit = safeGetChar (res, "digit", '#');
    exponential = safeGetChar (res, "exponential", 'E');
    groupingSeparator = safeGetChar (res, "groupingSeparator", ',');
    infinity = safeGetString (res, "infinity", "\u221e");
    // FIXME: default?
    intlCurrencySymbol = safeGetString (res, "intlCurrencySymbol", "$");
    try
      {
	monetarySeparator = safeGetChar (res, "monetarySeparator", '.');
      }
    catch (MissingResourceException x)
      {
	monetarySeparator = decimalSeparator;
      }
    minusSign = safeGetChar (res, "minusSign", '-');
    NaN = safeGetString (res, "NaN", "\ufffd");
    patternSeparator = safeGetChar (res, "patternSeparator", ';');
    percent = safeGetChar (res, "percent", '%');
    perMill = safeGetChar (res, "perMill", '\u2030');
    zeroDigit = safeGetChar (res, "zeroDigit", '0');
    locale = loc;
  }

  /**
   * This method this this object for equality against the specified object.
   * This will be true if and only if the following criteria are met with
   * regard to the specified object:
   * <p>
   * <ul>
   * <li>It is not <code>null</code>.</li>
   * <li>It is an instance of <code>DecimalFormatSymbols</code>.</li>
   * <li>All of its symbols are identical to the symbols in this object.</li>
   * </ul>
   *
   * @return <code>true</code> if the specified object is equal to this
   * object, <code>false</code> otherwise.
   */
  public boolean equals (Object obj)
  {
    if (! (obj instanceof DecimalFormatSymbols))
      return false;
    DecimalFormatSymbols dfs = (DecimalFormatSymbols) obj;
    return (currencySymbol.equals(dfs.currencySymbol)
	    && decimalSeparator == dfs.decimalSeparator
	    && digit == dfs.digit
	    && exponential == dfs.exponential
	    && groupingSeparator == dfs.groupingSeparator
	    && infinity.equals(dfs.infinity)
	    && intlCurrencySymbol.equals(dfs.intlCurrencySymbol)
	    && minusSign == dfs.minusSign
	    && monetarySeparator == dfs.monetarySeparator
	    && NaN.equals(dfs.NaN)
	    && patternSeparator == dfs.patternSeparator
	    && percent == dfs.percent
	    && perMill == dfs.perMill
	    && zeroDigit == dfs.zeroDigit);
  }

  /**
   * Returns the currency corresponding to the currency symbol stored
   * in the instance of <code>DecimalFormatSymbols</code>.
   *
   * @return A new instance of <code>Currency</code> if
   * the currency code matches a known one.
   */
  public Currency getCurrency ()
  {
    return Currency.getInstance (currencySymbol);
  }

  /**
   * This method returns the currency symbol in local format.  For example,
   * "$" for Canadian dollars.
   *
   * @return The currency symbol in local format.
   */
  public String getCurrencySymbol ()
  {
    return currencySymbol;
  }

  /**
   * This method returns the character used as the decimal point.
   *
   * @return The character used as the decimal point.
   */
  public char getDecimalSeparator ()
  {
    return decimalSeparator;
  }

  /**
   * This method returns the character used to represent a digit in a
   * format pattern string.
   *
   * @return The character used to represent a digit in a format
   * pattern string. 
   */
  public char getDigit ()
  {
    return digit;
  }

  // This is our own extension.
  char getExponential ()
  {
    return exponential;
  }

  /**
   * This method sets the character used to separate groups of digits.  For
   * example, the United States uses a comma (,) to separate thousands in
   * a number.
   *
   * @return The character used to separate groups of digits.
   */
  public char getGroupingSeparator ()
  {
    return groupingSeparator;
  }

  /**
   * This method returns the character used to represent infinity.
   *
   * @return The character used to represent infinity.
   */
  public String getInfinity ()
  {
    return infinity;
  }

  /**
   * This method returns the currency symbol in international format.  For
   * example, "C$" for Canadian dollars.
   *
   * @return The currency symbol in international format.
   */
  public String getInternationalCurrencySymbol ()
  {
    return intlCurrencySymbol;
  }

  /**
   * This method returns the character used to represent the minus sign.
   *
   * @return The character used to represent the minus sign.
   */
  public char getMinusSign ()
  {
    return minusSign;
  }

  /**
   * This method returns the character used to represent the decimal
   * point for currency values.
   *
   * @return The decimal point character used in currency values.
   */
  public char getMonetaryDecimalSeparator ()
  {
    return monetarySeparator;
  }

  /**
   * This method returns the string used to represent the NaN (not a number)
   * value.
   *
   * @return The string used to represent NaN
   */
  public String getNaN ()
  {
    return NaN;
  }

  /**
   * This method returns the character used to separate positive and negative
   * subpatterns in a format pattern.
   *
   * @return The character used to separate positive and negative subpatterns
   * in a format pattern.
   */
  public char getPatternSeparator ()
  {
    return patternSeparator;
  }

  /**
   * This method returns the character used as the percent sign.
   *
   * @return The character used as the percent sign.
   */
  public char getPercent ()
  {
    return percent;
  }

  /**
   * This method returns the character used as the per mille character.
   *
   * @return The per mille character.
   */
  public char getPerMill ()
  {
    return perMill;
  }

  /**
   * This method returns the character used to represent the digit zero.
   *
   * @return The character used to represent the digit zero.
   */
  public char getZeroDigit ()
  {
    return zeroDigit;
  }

  /**
   * This method returns a hash value for this object.
   *
   * @return A hash value for this object.
   */
  public int hashCode ()
  {
    // Compute based on zero digit, grouping separator, and decimal
    // separator -- JCL book.  This probably isn't a very good hash
    // code.
    return zeroDigit << 16 + groupingSeparator << 8 + decimalSeparator;
  }

  /**
   * This method sets the currency to the specified value.
   *
   * @param currency The new currency
   */
  public void setCurrency (Currency currency)
  {
    setCurrencySymbol (currency.getSymbol());
  }

  /**
   * This method sets the currency symbol to the specified value.
   *
   * @param currencySymbol The new currency symbol
   */
  public void setCurrencySymbol (String currency)
  {
    currencySymbol = currency;
  }

  /**
   * This method sets the decimal point character to the specified value.
   *
   * @param decimalSeparator The new decimal point character
   */
  public void setDecimalSeparator (char decimalSep)
  {
    decimalSeparator = decimalSep;
  }

  /**
   * This method sets the character used to represents a digit in a format
   * string to the specified value.
   *
   * @param digit The character used to represent a digit in a format pattern.
   */
  public void setDigit (char digit)
  {
    this.digit = digit;
  }

  // This is our own extension.
  void setExponential (char exp)
  {
    exponential = exp;
  }

  /**
   * This method sets the character used to separate groups of digits.
   *
   * @param groupingSeparator The character used to separate groups of digits.
   */
  public void setGroupingSeparator (char groupSep)
  {
    groupingSeparator = groupSep;
  }

  /**
   * This method sets the string used to represents infinity.
   *
   * @param infinity The string used to represent infinity.
   */
  public void setInfinity (String infinity)
  {
    this.infinity = infinity;
  }

  /**
   * This method sets the international currency symbols to the
   * specified value. 
   *
   * @param intlCurrencySymbol The new international currency symbol.
   */
  public void setInternationalCurrencySymbol (String currency)
  {
    intlCurrencySymbol = currency;
  }

  /**
   * This method sets the character used to represent the minus sign.
   *
   * @param minusSign The character used to represent the minus sign.
   */
  public void setMinusSign (char minusSign)
  {
    this.minusSign = minusSign;
  }

  /**
   * This method sets the character used for the decimal point in currency
   * values.
   *
   * @param monetarySeparator The decimal point character used in
   *                          currency values. 
   */
  public void setMonetaryDecimalSeparator (char decimalSep)
  {
    monetarySeparator = decimalSep;
  }

  /**
   * This method sets the string used to represent the NaN (not a
   * number) value. 
   *
   * @param NaN The string used to represent NaN
   */
  public void setNaN (String nan)
  {
    NaN = nan;
  }

  /**
   * This method sets the character used to separate positive and negative
   * subpatterns in a format pattern.
   *
   * @param patternSeparator The character used to separate positive and
   * negative subpatterns in a format pattern.
   */
  public void setPatternSeparator (char patternSep)
  {
    patternSeparator = patternSep;
  }

  /**
   * This method sets the character used as the percent sign.
   *
   * @param percent  The character used as the percent sign.
   */
  public void setPercent (char percent)
  {
    this.percent = percent;
  }

  /**
   * This method sets the character used as the per mille character.
   *
   * @param perMill The per mille character.
   */
  public void setPerMill (char perMill)
  {
    this.perMill = perMill;
  }

  /**
   * This method sets the character used to represent the digit zero.
   *
   * @param zeroDigit The character used to represent the digit zero.
   */
  public void setZeroDigit (char zeroDigit)
  {
    this.zeroDigit = zeroDigit;
  }

  /**
   * @serial A string used for the local currency
   */
  private String currencySymbol;
  /**
   * @serial The <code>char</code> used to separate decimals in a number.
   */
  private char decimalSeparator;
  /**
   * @serial This is the <code>char</code> used to represent a digit in
   * a format specification.
   */
  private char digit;
  /**
   * @serial This is the <code>char</code> used to represent the exponent
   * separator in exponential notation.
   */
  private char exponential;
  /**
   * @serial This separates groups of thousands in numbers.
   */
  private char groupingSeparator;
  /**
   * @serial This string represents infinity.
   */
  private String infinity;
  /**
   * @serial This string represents the local currency in an international
   * context, eg, "C$" for Canadian dollars.
   */
  private String intlCurrencySymbol;
  /**
   * @serial This is the character used to represent the minus sign.
   */
  private char minusSign;
  /**
   * @serial This character is used to separate decimals when formatting
   * currency values.
   */
  private char monetarySeparator;
  /**
   * @serial This string is used the represent the Java NaN value for
   * "not a number".
   */
  private String NaN;
  /**
   * @serial This is the character used to separate positive and negative
   * subpatterns in a format pattern.
   */
  private char patternSeparator;
  /**
   * @serial This is the percent symbols
   */
  private char percent;
  /**
   * @serial This character is used for the mille percent sign.
   */
  private char perMill;
  /**
   * @serial This value represents the type of object being de-serialized.
   * 0 indicates a pre-Java 1.1.6 version, 1 indicates 1.1.6 or later.
   * 0 indicates a pre-Java 1.1.6 version, 1 indicates 1.1.6 or later,
   * 2 indicates 1.4 or later
    */
  private int serialVersionOnStream = 2;
  /**
   * @serial This is the character used to represent 0.
   */
  private char zeroDigit;

  /**
   * @serial The locale of these currency symbols.
   */
  private Locale locale;
 
  private static final long serialVersionUID = 5772796243397350300L;

  private void readObject(ObjectInputStream stream)
    throws IOException, ClassNotFoundException
  {
    stream.defaultReadObject();
    if (serialVersionOnStream < 1)
      {
        monetarySeparator = decimalSeparator;
	exponential = 'E';
      }
    if (serialVersionOnStream < 2)
	locale = Locale.getDefault();

    serialVersionOnStream = 2;
  }
}
