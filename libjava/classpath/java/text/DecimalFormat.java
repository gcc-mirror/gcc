/* DecimalFormat.java -- Formats and parses numbers
   Copyright (C) 1999, 2000, 2001, 2003, 2004, 2005  Free Software Foundation, Inc.

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

/* 
 * This class contains few bits from ICU4J (http://icu.sourceforge.net/),
 * Copyright by IBM and others and distributed under the
 * distributed under MIT/X.
 */

package java.text;

import gnu.java.lang.CPStringBuilder;

import java.math.BigDecimal;
import java.math.BigInteger;

import java.util.ArrayList;
import java.util.Currency;
import java.util.Locale;

/*
 * This note is here for historical reasons and because I had not the courage
 * to remove it :)
 *  
 * @author Tom Tromey (tromey@cygnus.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @date March 4, 1999
 *
 * Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 from http://www.javasoft.com.
 * Status:  Believed complete and correct to 1.2.
 * Note however that the docs are very unclear about how format parsing
 * should work.  No doubt there are problems here.
 */

/**
 * This class is a concrete implementation of NumberFormat used to format
 * decimal numbers. The class can format numbers given a specific locale.
 * Generally, to get an instance of DecimalFormat you should call the factory
 * methods in the <code>NumberFormat</code> base class.
 * 
 * @author Mario Torre (neugens@limasoftware.net)
 * @author Tom Tromey (tromey@cygnus.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 */
public class DecimalFormat extends NumberFormat
{
  /** serialVersionUID for serializartion. */
  private static final long serialVersionUID = 864413376551465018L;
  
  /** Defines the default number of digits allowed while formatting integers. */
  private static final int DEFAULT_INTEGER_DIGITS = 309; 

  /**
   * Defines the default number of digits allowed while formatting
   * fractions.
   */
  private static final int DEFAULT_FRACTION_DIGITS = 340;
  
  /**
   * Locale-independent pattern symbols.
   */
  // Happen to be the same as the US symbols.
  private static final DecimalFormatSymbols nonLocalizedSymbols
    = new DecimalFormatSymbols (Locale.US);
  
  /**
   * Defines if parse should return a BigDecimal or not.
   */
  private boolean parseBigDecimal;
  
  /**
   * Defines if we have to use the monetary decimal separator or
   * the decimal separator while formatting numbers.
   */
  private boolean useCurrencySeparator;
    
  /** Defines if the decimal separator is always shown or not. */
  private boolean decimalSeparatorAlwaysShown;
  
  /**
   * Defines if the decimal separator has to be shown.
   * 
   * This is different then <code>decimalSeparatorAlwaysShown</code>,
   * as it defines if the format string contains a decimal separator or no.
   */
  private boolean showDecimalSeparator;
  
  /**
   * This field is used to determine if the grouping
   * separator is included in the format string or not.
   * This is only needed to match the behaviour of the RI.
   */
  private boolean groupingSeparatorInPattern;
  
  /** Defines the size of grouping groups when grouping is used. */
  private byte groupingSize;
  
  /**
   * This is an internal parameter used to keep track of the number
   * of digits the form the exponent, when exponential notation is used.
   * It is used with <code>exponentRound</code>
   */
  private byte minExponentDigits;
 
  /** This field is used to set the exponent in the engineering notation. */
  private int exponentRound;
  
  /** Multiplier used in percent style formats. */
  private int multiplier;
  
  /** Multiplier used in percent style formats. */
  private int negativePatternMultiplier;
  
  /** The negative prefix. */
  private String negativePrefix;
  
  /** The negative suffix. */
  private String negativeSuffix;
  
  /** The positive prefix. */
  private String positivePrefix;
  
  /** The positive suffix. */
  private String positiveSuffix;
  
  /** Decimal Format Symbols for the given locale. */
  private DecimalFormatSymbols symbols;
  
  /** Determine if we have to use exponential notation or not. */
  private boolean useExponentialNotation;
  
  /**
   * Defines the maximum number of integer digits to show when we use
   * the exponential notation.
   */
  private int maxIntegerDigitsExponent;
  
  /** Defines if the format string has a negative prefix or not. */
  private boolean hasNegativePrefix;
  
  /** Defines if the format string has a fractional pattern or not. */
  private boolean hasFractionalPattern;
 
  /** Stores a list of attributes for use by formatToCharacterIterator. */
  private ArrayList attributes = new ArrayList();
  
  /**
   * Constructs a <code>DecimalFormat</code> which uses the default
   * pattern and symbols.
   */
  public DecimalFormat()
  {
    this ("#,##0.###");
  }
  
  /**
   * Constructs a <code>DecimalFormat</code> which uses the given
   * pattern and the default symbols for formatting and parsing.
   *
   * @param pattern the non-localized pattern to use.
   * @throws NullPointerException if any argument is null.
   * @throws IllegalArgumentException if the pattern is invalid.
   */
  public DecimalFormat(String pattern)
  {
    this (pattern, new DecimalFormatSymbols());
  }

  /**
   * Constructs a <code>DecimalFormat</code> using the given pattern
   * and formatting symbols.  This construction method is used to give
   * complete control over the formatting process.  
   *
   * @param pattern the non-localized pattern to use.
   * @param symbols the set of symbols used for parsing and formatting.
   * @throws NullPointerException if any argument is null.
   * @throws IllegalArgumentException if the pattern is invalid.
   */
  public DecimalFormat(String pattern, DecimalFormatSymbols symbols)
  {
    this.symbols = (DecimalFormatSymbols) symbols.clone();
    applyPatternWithSymbols(pattern, nonLocalizedSymbols);
  }
  
  /**
   * Apply the given localized patern to the current DecimalFormat object.
   * 
   * @param pattern The localized pattern to apply.
   * @throws IllegalArgumentException if the given pattern is invalid.
   * @throws NullPointerException if the input pattern is null.
   */
  public void applyLocalizedPattern (String pattern)
  {
    applyPatternWithSymbols(pattern, this.symbols);
  }

  /**
   * Apply the given localized pattern to the current DecimalFormat object.
   * 
   * @param pattern The localized pattern to apply.
   * @throws IllegalArgumentException if the given pattern is invalid.
   * @throws NullPointerException if the input pattern is null.
   */
  public void applyPattern(String pattern)
  {
    applyPatternWithSymbols(pattern, nonLocalizedSymbols);
  }

  public Object clone()
  {
    DecimalFormat c = (DecimalFormat) super.clone();
    c.symbols = (DecimalFormatSymbols) symbols.clone();
    return c;
  }

  /**
   * Tests this instance for equality with an arbitrary object.  This method
   * returns <code>true</code> if:
   * <ul>
   * <li><code>obj</code> is not <code>null</code>;</li>
   * <li><code>obj</code> is an instance of <code>DecimalFormat</code>;</li>
   * <li>this instance and <code>obj</code> have the same attributes;</li>
   * </ul>
   * 
   * @param obj  the object (<code>null</code> permitted).
   * 
   * @return A boolean.
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof DecimalFormat))
      return false;
    DecimalFormat dup = (DecimalFormat) obj;
    return (decimalSeparatorAlwaysShown == dup.decimalSeparatorAlwaysShown 
           && groupingUsed == dup.groupingUsed
           && groupingSeparatorInPattern == dup.groupingSeparatorInPattern 
           && groupingSize == dup.groupingSize
           && multiplier == dup.multiplier
           && useExponentialNotation == dup.useExponentialNotation
           && minExponentDigits == dup.minExponentDigits
           && minimumIntegerDigits == dup.minimumIntegerDigits
           && maximumIntegerDigits == dup.maximumIntegerDigits
           && minimumFractionDigits == dup.minimumFractionDigits
           && maximumFractionDigits == dup.maximumFractionDigits
           && parseBigDecimal == dup.parseBigDecimal
           && useCurrencySeparator == dup.useCurrencySeparator
           && showDecimalSeparator == dup.showDecimalSeparator
           && exponentRound == dup.exponentRound
           && negativePatternMultiplier == dup.negativePatternMultiplier
           && maxIntegerDigitsExponent == dup.maxIntegerDigitsExponent
           // XXX: causes equivalent patterns to fail
           // && hasNegativePrefix == dup.hasNegativePrefix
           && equals(negativePrefix, dup.negativePrefix)
           && equals(negativeSuffix, dup.negativeSuffix)
           && equals(positivePrefix, dup.positivePrefix)
           && equals(positiveSuffix, dup.positiveSuffix)
           && symbols.equals(dup.symbols));
  }

  /**
   * Returns a hash code for this object.
   *
   * @return A hash code.
   */
  public int hashCode()
  {
    return toPattern().hashCode();
  }
  
  /**
   * Produce a formatted {@link String} representation of this object.
   * The passed object must be of type number. 
   * 
   * @param obj The {@link Number} to format.
   * @param sbuf The destination String; text will be appended to this String. 
   * @param pos If used on input can be used to define an alignment
   * field. If used on output defines the offsets of the alignment field.
   * @return The String representation of this long.
   */
  public final StringBuffer format(Object obj, StringBuffer sbuf, FieldPosition pos)
  {
    if (obj instanceof BigInteger)
      {
        BigDecimal decimal = new BigDecimal((BigInteger) obj);
        formatInternal(decimal, true, sbuf, pos);
        return sbuf;
      }
    else if (obj instanceof BigDecimal)
      {
        formatInternal((BigDecimal) obj, true, sbuf, pos);
        return sbuf;
      }
    
    return super.format(obj, sbuf, pos);
  }
  
  /**
   * Produce a formatted {@link String} representation of this double.
   * 
   * @param number The double to format.
   * @param dest The destination String; text will be appended to this String. 
   * @param fieldPos If used on input can be used to define an alignment
   * field. If used on output defines the offsets of the alignment field.
   * @return The String representation of this long.
   * @throws NullPointerException if <code>dest</code> or fieldPos are null
   */
  public StringBuffer format(double number, StringBuffer dest,
			     FieldPosition fieldPos)
  {
    // special cases for double: NaN and negative or positive infinity
    if (Double.isNaN(number))
      {
        // 1. NaN
        String nan = symbols.getNaN();
        dest.append(nan);
        
        // update field position if required
        if ((fieldPos.getField() == INTEGER_FIELD ||
             fieldPos.getFieldAttribute() == NumberFormat.Field.INTEGER))
          {
            int index = dest.length();
            fieldPos.setBeginIndex(index - nan.length());
            fieldPos.setEndIndex(index);
          }
      }
    else if (Double.isInfinite(number))
      {
        // 2. Infinity
        if (number < 0)
          dest.append(this.negativePrefix);
        else
          dest.append(this.positivePrefix);
        
        dest.append(symbols.getInfinity());
        
        if (number < 0)
          dest.append(this.negativeSuffix);
        else
          dest.append(this.positiveSuffix);
        
        if ((fieldPos.getField() == INTEGER_FIELD ||
            fieldPos.getFieldAttribute() == NumberFormat.Field.INTEGER))
         {
           fieldPos.setBeginIndex(dest.length());
           fieldPos.setEndIndex(0);
         }
      }
    else
      {
        // get the number as a BigDecimal
        BigDecimal bigDecimal = new BigDecimal(String.valueOf(number));
        formatInternal(bigDecimal, false, dest, fieldPos);
      }
    
    return dest;
  }

  /**
   * Produce a formatted {@link String} representation of this long.
   * 
   * @param number The long to format.
   * @param dest The destination String; text will be appended to this String. 
   * @param fieldPos If used on input can be used to define an alignment
   * field. If used on output defines the offsets of the alignment field.
   * @return The String representation of this long.
   */
  public StringBuffer format(long number, StringBuffer dest,
                             FieldPosition fieldPos)
  {
    BigDecimal bigDecimal = new BigDecimal(String.valueOf(number));
    formatInternal(bigDecimal, true, dest, fieldPos);
    return dest;
  }
  
  /**
   * Return an <code>AttributedCharacterIterator</code> as a result of
   * the formatting of the passed {@link Object}.
   * 
   * @return An {@link AttributedCharacterIterator}.
   * @throws NullPointerException if value is <code>null</code>. 
   * @throws IllegalArgumentException if value is not an instance of
   * {@link Number}.
   */
  public AttributedCharacterIterator formatToCharacterIterator(Object value)
  {
    /*
     * This method implementation derives directly from the
     * ICU4J (http://icu.sourceforge.net/) library, distributed under MIT/X.
     */
    
    if (value == null)
      throw new NullPointerException("Passed Object is null");
    
    if (!(value instanceof Number)) throw new
      IllegalArgumentException("Cannot format given Object as a Number");
    
    StringBuffer text = new StringBuffer();
    attributes.clear();
    super.format(value, text, new FieldPosition(0));

    AttributedString as = new AttributedString(text.toString());

    // add NumberFormat field attributes to the AttributedString
    for (int i = 0; i < attributes.size(); i++)
      {
        FieldPosition pos = (FieldPosition) attributes.get(i);
        Format.Field attribute = pos.getFieldAttribute();
        
        as.addAttribute(attribute, attribute, pos.getBeginIndex(),
                        pos.getEndIndex());
      }
    
    // return the CharacterIterator from AttributedString
    return as.getIterator();
  }
  
  /**
   * Returns the currency corresponding to the currency symbol stored
   * in the instance of <code>DecimalFormatSymbols</code> used by this
   * <code>DecimalFormat</code>.
   *
   * @return A new instance of <code>Currency</code> if
   * the currency code matches a known one, null otherwise.
   */
  public Currency getCurrency()
  {
    return symbols.getCurrency();
  }
  
  /**
   * Returns a copy of the symbols used by this instance.
   * 
   * @return A copy of the symbols.
   */
  public DecimalFormatSymbols getDecimalFormatSymbols()
  {
    return (DecimalFormatSymbols) symbols.clone();
  }
  
  /**
   * Gets the interval used between a grouping separator and the next.
   * For example, a grouping size of 3 means that the number 1234 is
   * formatted as 1,234.
   * 
   * The actual character used as grouping separator depends on the
   * locale and is defined by {@link DecimalFormatSymbols#getDecimalSeparator()}
   * 
   * @return The interval used between a grouping separator and the next.
   */
  public int getGroupingSize()
  {
    return groupingSize;
  }

  /**
   * Gets the multiplier used in percent and similar formats.
   * 
   * @return The multiplier used in percent and similar formats.
   */
  public int getMultiplier()
  {
    return multiplier;
  }
  
  /**
   * Gets the negative prefix.
   * 
   * @return The negative prefix.
   */
  public String getNegativePrefix()
  {
    return negativePrefix;
  }

  /**
   * Gets the negative suffix.
   * 
   * @return The negative suffix.
   */
  public String getNegativeSuffix()
  {
    return negativeSuffix;
  }
  
  /**
   * Gets the positive prefix.
   * 
   * @return The positive prefix.
   */
  public String getPositivePrefix()
  {
    return positivePrefix;
  }
  
  /**
   * Gets the positive suffix.
   *  
   * @return The positive suffix.
   */
  public String getPositiveSuffix()
  {
    return positiveSuffix;
  }
  
  public boolean isDecimalSeparatorAlwaysShown()
  {
    return decimalSeparatorAlwaysShown;
  }
  
  /**
   * Define if <code>parse(java.lang.String, java.text.ParsePosition)</code>
   * should return a {@link BigDecimal} or not. 
   * 
   * @param newValue
   */
  public void setParseBigDecimal(boolean newValue)
  {
    this.parseBigDecimal = newValue;
  }
  
  /**
   * Returns <code>true</code> if
   * <code>parse(java.lang.String, java.text.ParsePosition)</code> returns
   * a <code>BigDecimal</code>, <code>false</code> otherwise.
   * The default return value for this method is <code>false</code>.
   * 
   * @return <code>true</code> if the parse method returns a {@link BigDecimal},
   * <code>false</code> otherwise.
   * @since 1.5
   * @see #setParseBigDecimal(boolean)
   */
  public boolean isParseBigDecimal()
  {
    return this.parseBigDecimal;
  }
  
  /**
   * This method parses the specified string into a <code>Number</code>.
   * 
   * The parsing starts at <code>pos</code>, which is updated as the parser
   * consume characters in the passed string.
   * On error, the <code>Position</code> object index is not updated, while
   * error position is set appropriately, an <code>null</code> is returned.
   * 
   * @param str The string to parse.
   * @param pos The desired <code>ParsePosition</code>.
   *
   * @return The parsed <code>Number</code>
   */
  public Number parse(String str, ParsePosition pos)
  {
    // a special values before anything else
    // NaN
    if (str.contains(this.symbols.getNaN()))
      return Double.valueOf(Double.NaN);
   
    // this will be our final number
    CPStringBuilder number = new CPStringBuilder();
    
    // special character
    char minus = symbols.getMinusSign();
    
    // starting parsing position
    int start = pos.getIndex();
    
    // validate the string, it have to be in the 
    // same form as the format string or parsing will fail
    String _negativePrefix = (this.negativePrefix.compareTo("") == 0
                              ? minus + positivePrefix
                              : this.negativePrefix);
    
    // we check both prefixes, because one might be empty.
    // We want to pick the longest prefix that matches.
    int positiveLen = positivePrefix.length();
    int negativeLen = _negativePrefix.length();
    
    boolean isNegative = str.startsWith(_negativePrefix);
    boolean isPositive = str.startsWith(positivePrefix);
    
    if (isPositive && isNegative)
      {
        // By checking this way, we preserve ambiguity in the case
        // where the negative format differs only in suffix.
        if (negativeLen > positiveLen)
          {
            start += _negativePrefix.length();
            isNegative = true;
          }
        else
          {
            start += positivePrefix.length();
            isPositive = true;
            if (negativeLen < positiveLen)
              isNegative = false;
          }
      }
    else if (isNegative)
      {
        start += _negativePrefix.length();
        isPositive = false;
      }
    else if (isPositive)
      {
        start += positivePrefix.length();
        isNegative = false;
      }
    else
      {
        pos.setErrorIndex(start);
        return null;
      }
    
    // other special characters used by the parser
    char decimalSeparator = symbols.getDecimalSeparator();
    char zero = symbols.getZeroDigit();
    char exponent = symbols.getExponential(); 
    
    // stop parsing position in the string
    int stop = start + this.maximumIntegerDigits + maximumFractionDigits + 2;
    
    if (useExponentialNotation)
      stop += minExponentDigits + 1;
    
    boolean inExponent = false;

    // correct the size of the end parsing flag
    int len = str.length();
    if (len < stop) stop = len;
    char groupingSeparator = symbols.getGroupingSeparator();
    
    int i = start;
    while (i < stop)
      {
        char ch = str.charAt(i);
        i++;
       
        if (ch >= zero && ch <= (zero + 9))
          {
            number.append(ch);
          }
        else if (this.parseIntegerOnly)
          {
            i--;
            break;
          }
        else if (ch == decimalSeparator)
          {
            number.append('.');
          }
        else if (ch == exponent)
          {
            number.append(ch);
            inExponent = !inExponent;
          }
        else if ((ch == '+' || ch == '-' || ch == minus))
          {
            if (inExponent)
              number.append(ch);
            else
	      {
		i--;
        	break;
	      }
          }
	else
	  {
	    if (!groupingUsed || ch != groupingSeparator)
	      {
	        i--;
	        break;
	      }
	  }
      }

    // 2nd special case: infinity
    // XXX: need to be tested
    if (str.contains(symbols.getInfinity()))
      {
        int inf = str.indexOf(symbols.getInfinity()); 
        pos.setIndex(inf);
        
        // FIXME: ouch, this is really ugly and lazy code...
        if (this.parseBigDecimal)
          {
            if (isNegative)
              return BigDecimal.valueOf(Double.NEGATIVE_INFINITY);
            
            return BigDecimal.valueOf(Double.POSITIVE_INFINITY);
          }
        
        if (isNegative)
          return Double.valueOf(Double.NEGATIVE_INFINITY);

        return Double.valueOf(Double.POSITIVE_INFINITY);
      }
    
    // no number...
    if (i == start || number.length() == 0)
      {
        pos.setErrorIndex(i);
        return null;
      }

    // now we have to check the suffix, done here after number parsing
    // or the index will not be updated correctly...
    boolean hasNegativeSuffix = str.endsWith(this.negativeSuffix);
    boolean hasPositiveSuffix = str.endsWith(this.positiveSuffix);
    boolean positiveEqualsNegative = negativeSuffix.equals(positiveSuffix);

    positiveLen = positiveSuffix.length();
    negativeLen = negativeSuffix.length();
    
    if (isNegative && !hasNegativeSuffix)
      {
        pos.setErrorIndex(i);
        return null;
      }
    else if (hasNegativeSuffix &&
             !positiveEqualsNegative &&
             (negativeLen > positiveLen))
      {
        isNegative = true;
      }
    else if (!hasPositiveSuffix)
      {
        pos.setErrorIndex(i);
        return null;
      }
    
    if (isNegative) number.insert(0, '-');
   
    pos.setIndex(i);
    
    // now we handle the return type
    BigDecimal bigDecimal = new BigDecimal(number.toString());
    if (this.parseBigDecimal)
      return bigDecimal;
    
    // want integer?
    if (this.parseIntegerOnly)
      return Long.valueOf(bigDecimal.longValue());

    // 3th special case -0.0
    if (isNegative && (bigDecimal.compareTo(BigDecimal.ZERO) == 0))
      return Double.valueOf(-0.0);
    
    try
      {
        BigDecimal integer
          = bigDecimal.setScale(0, BigDecimal.ROUND_UNNECESSARY);
        return Long.valueOf(integer.longValue());
      }
    catch (ArithmeticException e)
      {
        return Double.valueOf(bigDecimal.doubleValue());
      }
  }

  /**
   * Sets the <code>Currency</code> on the
   * <code>DecimalFormatSymbols</code> used, which also sets the
   * currency symbols on those symbols.
   * 
   * @param currency The new <code>Currency</code> on the
   * <code>DecimalFormatSymbols</code>.
   */
  public void setCurrency(Currency currency)
  {
    Currency current = symbols.getCurrency();
    if (current != currency)
      {
	String oldSymbol = symbols.getCurrencySymbol();
	int len = oldSymbol.length();
	symbols.setCurrency(currency);
	String newSymbol = symbols.getCurrencySymbol();
	int posPre = positivePrefix.indexOf(oldSymbol);
	if (posPre != -1)
	  positivePrefix = positivePrefix.substring(0, posPre) +
	    newSymbol + positivePrefix.substring(posPre+len);
	int negPre = negativePrefix.indexOf(oldSymbol);
	if (negPre != -1)
	  negativePrefix = negativePrefix.substring(0, negPre) +
	    newSymbol + negativePrefix.substring(negPre+len);
	int posSuf = positiveSuffix.indexOf(oldSymbol);
	if (posSuf != -1)
	  positiveSuffix = positiveSuffix.substring(0, posSuf) +
	    newSymbol + positiveSuffix.substring(posSuf+len);
	int negSuf = negativeSuffix.indexOf(oldSymbol);
	if (negSuf != -1)
	  negativeSuffix = negativeSuffix.substring(0, negSuf) +
	    newSymbol + negativeSuffix.substring(negSuf+len);
      }
  }
  
  /**
   * Sets the symbols used by this instance.  This method makes a copy of 
   * the supplied symbols.
   * 
   * @param newSymbols  the symbols (<code>null</code> not permitted).
   */
  public void setDecimalFormatSymbols(DecimalFormatSymbols newSymbols)
  {
    symbols = (DecimalFormatSymbols) newSymbols.clone();
  }
  
  /**
   * Define if the decimal separator should be always visible or only
   * visible when needed. This method as effect only on integer values.
   * Pass <code>true</code> if you want the decimal separator to be
   * always shown, <code>false</code> otherwise.
   * 
   * @param newValue true</code> if you want the decimal separator to be
   * always shown, <code>false</code> otherwise.
   */
  public void setDecimalSeparatorAlwaysShown(boolean newValue)
  {
    decimalSeparatorAlwaysShown = newValue;
  }
  
  /**
   * Sets the number of digits used to group portions of the integer part of
   * the number. For example, the number <code>123456</code>, with a grouping
   * size of 3, is rendered <code>123,456</code>.
   * 
   * @param groupSize The number of digits used while grouping portions
   * of the integer part of a number.
   */
  public void setGroupingSize(int groupSize)
  {
    groupingSize = (byte) groupSize;
  }
  
  /**
   * Sets the maximum number of digits allowed in the integer
   * portion of a number to the specified value.
   * The new value will be the choosen as the minimum between
   * <code>newvalue</code> and 309. Any value below zero will be
   * replaced by zero.
   * 
   * @param newValue The new maximum integer digits value.
   */
  public void setMaximumIntegerDigits(int newValue)
  {
    newValue = (newValue > 0) ? newValue : 0;
    super.setMaximumIntegerDigits(Math.min(newValue, DEFAULT_INTEGER_DIGITS));
  }
  
  /**
   * Sets the minimum number of digits allowed in the integer
   * portion of a number to the specified value.
   * The new value will be the choosen as the minimum between
   * <code>newvalue</code> and 309. Any value below zero will be
   * replaced by zero.
   * 
   * @param newValue The new minimum integer digits value.
   */
  public void setMinimumIntegerDigits(int newValue)
  {
    newValue = (newValue > 0) ? newValue : 0;
    super.setMinimumIntegerDigits(Math.min(newValue,  DEFAULT_INTEGER_DIGITS));
  }
  
  /**
   * Sets the maximum number of digits allowed in the fraction
   * portion of a number to the specified value.
   * The new value will be the choosen as the minimum between
   * <code>newvalue</code> and 309. Any value below zero will be
   * replaced by zero.
   * 
   * @param newValue The new maximum fraction digits value.
   */
  public void setMaximumFractionDigits(int newValue)
  {
    newValue = (newValue > 0) ? newValue : 0;
    super.setMaximumFractionDigits(Math.min(newValue, DEFAULT_FRACTION_DIGITS));
  }
  
  /**
   * Sets the minimum number of digits allowed in the fraction
   * portion of a number to the specified value.
   * The new value will be the choosen as the minimum between
   * <code>newvalue</code> and 309. Any value below zero will be
   * replaced by zero.
   * 
   * @param newValue The new minimum fraction digits value.
   */
  public void setMinimumFractionDigits(int newValue)
  {
    newValue = (newValue > 0) ? newValue : 0;
    super.setMinimumFractionDigits(Math.min(newValue, DEFAULT_FRACTION_DIGITS));
  }
  
  /**
   * Sets the multiplier for use in percent and similar formats.
   * For example, for percent set the multiplier to 100, for permille, set the
   * miltiplier to 1000.
   * 
   * @param newValue the new value for multiplier.
   */
  public void setMultiplier(int newValue)
  {
    multiplier = newValue;
  }
  
  /**
   * Sets the negative prefix.
   * 
   * @param newValue The new negative prefix.
   */
  public void setNegativePrefix(String newValue)
  {
    negativePrefix = newValue;
  }

  /**
   * Sets the negative suffix.
   * 
   * @param newValue The new negative suffix.
   */
  public void setNegativeSuffix(String newValue)
  {
    negativeSuffix = newValue;
  }
  
  /**
   * Sets the positive prefix.
   * 
   * @param newValue The new positive prefix.
   */
  public void setPositivePrefix(String newValue)
  {
    positivePrefix = newValue;
  }
  
  /**
   * Sets the new positive suffix.
   * 
   * @param newValue The new positive suffix.
   */
  public void setPositiveSuffix(String newValue)
  {
    positiveSuffix = newValue;
  }
  
  /**
   * This method returns a string with the formatting pattern being used
   * by this object. The string is localized.
   * 
   * @return A localized <code>String</code> with the formatting pattern.
   * @see #toPattern()
   */
  public String toLocalizedPattern()
  {
    return computePattern(this.symbols);
  }
  
  /**
   * This method returns a string with the formatting pattern being used
   * by this object. The string is not localized.
   * 
   * @return A <code>String</code> with the formatting pattern.
   * @see #toLocalizedPattern()
   */
  public String toPattern()
  {
    return computePattern(nonLocalizedSymbols);
  }
  
  /* ***** private methods ***** */
  
  /**
   * This is an shortcut helper method used to test if two given strings are
   * equals.
   * 
   * @param s1 The first string to test for equality.
   * @param s2 The second string to test for equality.
   * @return <code>true</code> if the strings are both <code>null</code> or
   * equals.
   */
  private boolean equals(String s1, String s2)
  {
    if (s1 == null || s2 == null)
      return s1 == s2;
    return s1.equals(s2);
  }
  
  
  /* ****** PATTERN ****** */
  
  /**
   * This helper function creates a string consisting of all the
   * characters which can appear in a pattern and must be quoted.
   */
  private String patternChars (DecimalFormatSymbols syms)
  {
    CPStringBuilder buf = new CPStringBuilder ();
    
    buf.append(syms.getDecimalSeparator());
    buf.append(syms.getDigit());
    buf.append(syms.getExponential());
    buf.append(syms.getGroupingSeparator());
    buf.append(syms.getMinusSign());
    buf.append(syms.getPatternSeparator());
    buf.append(syms.getPercent());
    buf.append(syms.getPerMill());
    buf.append(syms.getZeroDigit());
    buf.append('\'');
    buf.append('\u00a4');
    
    return buf.toString();
  }

  /**
   * Quote special characters as defined by <code>patChars</code> in the
   * input string.
   * 
   * @param text
   * @param patChars
   * @return A StringBuffer with special characters quoted.
   */
  private CPStringBuilder quoteFix(String text, String patChars)
  {
    CPStringBuilder buf = new CPStringBuilder();
    
    int len = text.length();
    char ch;
    for (int index = 0; index < len; ++index)
      {
        ch = text.charAt(index);
        if (patChars.indexOf(ch) != -1)
          {
            buf.append('\'');
            buf.append(ch);
            if (ch != '\'') buf.append('\'');
          }
        else
          {
            buf.append(ch);
          }
      }
    
    return buf;
  }
  
  /**
   * Returns the format pattern, localized to follow the given
   * symbols.
   */
  private String computePattern(DecimalFormatSymbols symbols)
  {
    StringBuilder mainPattern = new StringBuilder();
    
    // We have to at least emit a zero for the minimum number of
    // digits. Past that we need hash marks up to the grouping
    // separator (and one beyond).
    int _groupingSize = groupingUsed ? groupingSize + 1: groupingSize;
    int totalDigits = Math.max(minimumIntegerDigits, _groupingSize);
    
    // if it is not in exponential notiation,
    // we always have a # prebended
    if (!useExponentialNotation) mainPattern.append(symbols.getDigit());
    
    for (int i = 1; i < totalDigits - minimumIntegerDigits; i++)
      mainPattern.append(symbols.getDigit());
    
    for (int i = totalDigits - minimumIntegerDigits; i < totalDigits; i++)
      mainPattern.append(symbols.getZeroDigit());
    
    if (groupingUsed)
      {
        mainPattern.insert(mainPattern.length() - groupingSize,
                           symbols.getGroupingSeparator());
      }

    // See if we need decimal info.
    if (minimumFractionDigits > 0 || maximumFractionDigits > 0 ||
        decimalSeparatorAlwaysShown)
      {
        mainPattern.append(symbols.getDecimalSeparator());
      }
    
    for (int i = 0; i < minimumFractionDigits; ++i)
      mainPattern.append(symbols.getZeroDigit());
    
    for (int i = minimumFractionDigits; i < maximumFractionDigits; ++i)
      mainPattern.append(symbols.getDigit());
    
    if (useExponentialNotation)
      {
        mainPattern.append(symbols.getExponential());
        
        for (int i = 0; i < minExponentDigits; ++i)
          mainPattern.append(symbols.getZeroDigit());
        
        if (minExponentDigits == 0)
          mainPattern.append(symbols.getDigit());
      }
    
    // save the pattern
    String pattern = mainPattern.toString();
    
    // so far we have the pattern itself, now we need to add
    // the positive and the optional negative prefixes and suffixes
    String patternChars = patternChars(symbols);
    mainPattern.insert(0, quoteFix(positivePrefix, patternChars));
    mainPattern.append(quoteFix(positiveSuffix, patternChars));
    
    if (hasNegativePrefix)
      {
        mainPattern.append(symbols.getPatternSeparator());
        mainPattern.append(quoteFix(negativePrefix, patternChars));
        mainPattern.append(pattern);
        mainPattern.append(quoteFix(negativeSuffix, patternChars));
      }
    
    // finally, return the pattern string
    return mainPattern.toString();
  }
  
  /* ****** FORMAT PARSING ****** */
  
  /**
   * Scan the input string and define a pattern suitable for use
   * with this decimal format.
   * 
   * @param pattern
   * @param symbols
   */
  private void applyPatternWithSymbols(String pattern,
                                       DecimalFormatSymbols symbols)
  {
    // The pattern string is described by a BNF diagram.
    // we could use a recursive parser to read and prepare
    // the string, but this would be too slow and resource
    // intensive, while this code is quite critical as it is
    // called always when the class is instantiated and every
    // time a new pattern is given.
    // Our strategy is to divide the string into section as given by
    // the BNF diagram, iterating through the string and setting up
    // the parameters we need for formatting (which is basicly what
    // a descendent recursive parser would do - but without recursion).
    // I'm sure that there are smarter methods to do this.
    
    // Restore default values. Most of these will be overwritten
    // but we want to be sure that nothing is left out.
    setDefaultValues();
    
    int len = pattern.length();
    if (len == 0)
      {
        // this is another special case...
        this.minimumIntegerDigits = 1;
        this.maximumIntegerDigits = DEFAULT_INTEGER_DIGITS;
        this.minimumFractionDigits = 0;
        this.maximumFractionDigits = DEFAULT_FRACTION_DIGITS;
        
        // FIXME: ...and these values may not be valid in all locales
        this.minExponentDigits = 0;
        this.showDecimalSeparator = true;
        this.groupingUsed = true;
        this.groupingSize = 3;
        
        return;
      }
    
    int start = scanFix(pattern, symbols, 0, true);
    if (start < len) start = scanNumberInteger(pattern, symbols, start);
    if (start < len)
      {
        start = scanFractionalPortion(pattern, symbols, start);
      }
    else
      {
        // special case, pattern that ends here does not have a fractional
        // portion
        this.minimumFractionDigits = 0;
        this.maximumFractionDigits = 0;
        //this.decimalSeparatorAlwaysShown = false;
        //this.showDecimalSeparator = false;
      }
    
    // XXX: this fixes a compatibility test with the RI.
    // If new uses cases fail, try removing this line first.
    //if (!this.hasIntegerPattern && !this.hasFractionalPattern)
    //  throw new IllegalArgumentException("No valid pattern found!");
    
    if (start < len) start = scanExponent(pattern, symbols, start);
    if (start < len) start = scanFix(pattern, symbols, start, false);
    if (start < len) scanNegativePattern(pattern, symbols, start);
    
    if (useExponentialNotation &&
        (maxIntegerDigitsExponent > minimumIntegerDigits) &&
        (maxIntegerDigitsExponent > 1))
      {
        minimumIntegerDigits = 1;
        exponentRound = maxIntegerDigitsExponent;
      }
    
    if (useExponentialNotation)
      maximumIntegerDigits = maxIntegerDigitsExponent;
    
    if (!this.hasFractionalPattern && this.showDecimalSeparator == true)
      {
        this.decimalSeparatorAlwaysShown = true;
      }
  }
  
  /**
   * Scans for the prefix or suffix portion of the pattern string.
   * This method handles the positive subpattern of the pattern string.
   *  
   * @param pattern The pattern string to parse.
   * @return The position in the pattern string where parsing ended.
   */
  private int scanFix(String pattern, DecimalFormatSymbols sourceSymbols,
                      int start, boolean prefix)
  {
    CPStringBuilder buffer = new CPStringBuilder();
    
    // the number portion is always delimited by one of those
    // characters
    char decimalSeparator = sourceSymbols.getDecimalSeparator();
    char patternSeparator = sourceSymbols.getPatternSeparator();
    char groupingSeparator = sourceSymbols.getGroupingSeparator();
    char digit = sourceSymbols.getDigit();
    char zero = sourceSymbols.getZeroDigit();
    char minus = sourceSymbols.getMinusSign();
    
    // other special characters, cached here to avoid method calls later
    char percent = sourceSymbols.getPercent();
    char permille = sourceSymbols.getPerMill();
    
    String currencySymbol = this.symbols.getCurrencySymbol();
    
    boolean quote = false;
    
    char ch = pattern.charAt(start);
    if (ch == patternSeparator)
      {
        // negative subpattern
        this.hasNegativePrefix = true;
        ++start;
        return start;
      }
    
    int len = pattern.length();
    int i;
    for (i = start; i < len; i++)
      {
        ch = pattern.charAt(i);

        // we are entering into the negative subpattern
        if (!quote && ch == patternSeparator)
          {
            if (this.hasNegativePrefix)
              {
                throw new IllegalArgumentException("Invalid pattern found: "
                                                   + start);
              }
            
            this.hasNegativePrefix = true;
            ++i;
            break;
          }
        
        // this means we are inside the number portion
        if (!quote &&
            (ch == minus || ch == digit || ch == zero ||
             ch == groupingSeparator))
          break;

        if (!quote && ch == decimalSeparator)
          {
            this.showDecimalSeparator = true;
            break;
          }
        else if (quote && ch != '\'')
          {
            buffer.append(ch);
            continue;
          }
        
        if (ch == '\u00A4')
          {
            // CURRENCY
            currencySymbol = this.symbols.getCurrencySymbol();

            // if \u00A4 is doubled, we use the international currency symbol
            if ((i + 1) < len && pattern.charAt(i + 1) == '\u00A4')
              {
                currencySymbol = this.symbols.getInternationalCurrencySymbol();
                i++;
              }

            this.useCurrencySeparator = true;
            buffer.append(currencySymbol);
          }
        else if (ch == percent)
          {
            // PERCENT
            this.multiplier = 100;
            buffer.append(this.symbols.getPercent());
          }
        else if (ch == permille)
          {
            // PERMILLE
            this.multiplier = 1000;
            buffer.append(this.symbols.getPerMill());
          }
        else if (ch == '\'')
          {
            // QUOTE
            if ((i + 1) < len && pattern.charAt(i + 1) == '\'')
              {
                // we need to add ' to the buffer 
                buffer.append(ch);
                i++;
              }
            else
              {
                quote = !quote;
                continue;
              }
          }
        else
          {
            buffer.append(ch);
          }
      }
    
    if (prefix)
      {
        this.positivePrefix = buffer.toString();
        this.negativePrefix = minus + "" + positivePrefix;
      }
    else
      {
        this.positiveSuffix = buffer.toString();
      }
    
    return i;
  }
  
  /**
   * Scan the given string for number patterns, starting
   * from <code>start</code>.
   * This method searches the integer part of the pattern only.
   * 
   * @param pattern The pattern string to parse.
   * @param start The starting parse position in the string.
   * @return The position in the pattern string where parsing ended,
   * counted from the beginning of the string (that is, 0).
   */
  private int scanNumberInteger(String pattern, DecimalFormatSymbols symbols,
                                int start)
  {
    char digit = symbols.getDigit();
    char zero = symbols.getZeroDigit();
    char groupingSeparator = symbols.getGroupingSeparator();
    char decimalSeparator = symbols.getDecimalSeparator();
    char exponent = symbols.getExponential();
    char patternSeparator = symbols.getPatternSeparator();
    
    // count the number of zeroes in the pattern
    // this number defines the minum digits in the integer portion
    int zeros = 0;
    
    // count the number of digits used in grouping
    int _groupingSize = 0;
    
    this.maxIntegerDigitsExponent = 0;
    
    boolean intPartTouched = false;
    
    char ch;
    int len = pattern.length();
    int i;
    for (i = start; i < len; i++)
      {
        ch = pattern.charAt(i);
 
        // break on decimal separator or exponent or pattern separator
        if (ch == decimalSeparator || ch == exponent)
          break;
        
        if (this.hasNegativePrefix && ch == patternSeparator)
          throw new IllegalArgumentException("Invalid pattern found: "
                                             + start);
        
        if (ch == digit)
          {
            // in our implementation we could relax this strict
            // requirement, but this is used to keep compatibility with
            // the RI
            if (zeros > 0) throw new
              IllegalArgumentException("digit mark following zero in " +
                        "positive subpattern, not allowed. Position: " + i);
            
            _groupingSize++;
            intPartTouched = true;
            this.maxIntegerDigitsExponent++;
          }
        else if (ch == zero)
          {
            zeros++;
            _groupingSize++;
            this.maxIntegerDigitsExponent++;
          }
        else if (ch == groupingSeparator)
          {
            this.groupingSeparatorInPattern = true;
            this.groupingUsed = true;
            _groupingSize = 0;
          }
        else
          {
            // any other character not listed above
            // means we are in the suffix portion
            break;
          }
      }
    
    if (groupingSeparatorInPattern) this.groupingSize = (byte) _groupingSize;
    this.minimumIntegerDigits = zeros;
    
    // XXX: compatibility code with the RI: the number of minimum integer
    // digits is at least one when maximumIntegerDigits is more than zero
    if (intPartTouched && this.maximumIntegerDigits > 0 &&
        this.minimumIntegerDigits == 0)
      this.minimumIntegerDigits = 1;

    return i;
  }
  
  /**
   * Scan the given string for number patterns, starting
   * from <code>start</code>.
   * This method searches the fractional part of the pattern only.
   * 
   * @param pattern The pattern string to parse.
   * @param start The starting parse position in the string.
   * @return The position in the pattern string where parsing ended,
   * counted from the beginning of the string (that is, 0).
   */
  private int scanFractionalPortion(String pattern,
                                    DecimalFormatSymbols symbols,
                                    int start)
  {
    char digit = symbols.getDigit();
    char zero = symbols.getZeroDigit();
    char groupingSeparator = symbols.getGroupingSeparator();
    char decimalSeparator = symbols.getDecimalSeparator();
    char exponent = symbols.getExponential();
    char patternSeparator = symbols.getPatternSeparator();
    
    // first character needs to be '.' otherwise we are not parsing the
    // fractional portion
    char ch = pattern.charAt(start);
    if (ch != decimalSeparator)
      {
        this.minimumFractionDigits = 0;
        this.maximumFractionDigits = 0;
        return start;
      }
    
    ++start;
    
    this.hasFractionalPattern = true;
    
    this.minimumFractionDigits = 0;
    int digits = 0;
    
    int len = pattern.length();
    int i;
    for (i = start; i < len; i++)
      {
        ch = pattern.charAt(i);
        
        // we hit the exponential or negative subpattern
        if (ch == exponent || ch == patternSeparator)
          break;
        
        // pattern error
        if (ch == groupingSeparator || ch == decimalSeparator) throw new
          IllegalArgumentException("unexpected character '" + ch + "' " +
                                   "in fractional subpattern. Position: " + i);
        
        if (ch == digit)
          {
            digits++;
          }
        else if (ch == zero)
          {
            if (digits > 0) throw new
            IllegalArgumentException("digit mark following zero in " +
                      "positive subpattern, not allowed. Position: " + i);
            
            this.minimumFractionDigits++;
          }
        else
          {
            // we are in the suffix section of pattern
            break;
          }
      }
    
    if (i == start) this.hasFractionalPattern = false;
    
    this.maximumFractionDigits = this.minimumFractionDigits + digits;
    this.showDecimalSeparator = true;
    
    return i;
  }
  
  /**
   * Scan the given string for number patterns, starting
   * from <code>start</code>.
   * This method searches the expoential part of the pattern only.
   * 
   * @param pattern The pattern string to parse.
   * @param start The starting parse position in the string.
   * @return The position in the pattern string where parsing ended,
   * counted from the beginning of the string (that is, 0).
   */
  private int scanExponent(String pattern, DecimalFormatSymbols symbols,
                           int start)
  {
    char digit = symbols.getDigit();
    char zero = symbols.getZeroDigit();
    char groupingSeparator = symbols.getGroupingSeparator();
    char decimalSeparator = symbols.getDecimalSeparator();
    char exponent = symbols.getExponential();
    
    char ch = pattern.charAt(start);
    
    if (ch == decimalSeparator)
      {
        // ignore dots
        ++start;
      }
    
    if (ch != exponent)
      {
        this.useExponentialNotation = false;
        return start;
      }
    
    ++start;
    
    this.minExponentDigits = 0;
    
    int len = pattern.length();
    int i;
    for (i = start; i < len; i++)
      {
        ch = pattern.charAt(i);
        
        if (ch == groupingSeparator || ch == decimalSeparator ||
            ch == digit || ch == exponent) throw new
        IllegalArgumentException("unexpected character '" + ch + "' " + 
                                 "in exponential subpattern. Position: " + i);
        
        if (ch == zero)
          {
            this.minExponentDigits++;
          }
        else
          {
            // any character other than zero is an exit point
            break;
          }
      }
    
    this.useExponentialNotation = true; 
    
    return i;
  }
  
  /**
   * Scan the given string for number patterns, starting
   * from <code>start</code>.
   * This method searches the negative part of the pattern only and scan
   * throught the end of the string.
   * 
   * @param pattern The pattern string to parse.
   * @param start The starting parse position in the string.
   */
  private void scanNegativePattern(String pattern,
                                   DecimalFormatSymbols sourceSymbols,
                                   int start)
  {
    StringBuilder buffer = new StringBuilder();
    
    // the number portion is always delimited by one of those
    // characters
    char decimalSeparator = sourceSymbols.getDecimalSeparator();
    char patternSeparator = sourceSymbols.getPatternSeparator();
    char groupingSeparator = sourceSymbols.getGroupingSeparator();
    char digit = sourceSymbols.getDigit();
    char zero = sourceSymbols.getZeroDigit();
    char minus = sourceSymbols.getMinusSign();
    
    // other special charcaters, cached here to avoid method calls later
    char percent = sourceSymbols.getPercent();
    char permille = sourceSymbols.getPerMill();
    
    String CURRENCY_SYMBOL = this.symbols.getCurrencySymbol();
    String currencySymbol = CURRENCY_SYMBOL;
    
    boolean quote = false;
    boolean prefixDone = false;
    
    int len = pattern.length();
    if (len > 0) this.hasNegativePrefix = true;
    
    char ch = pattern.charAt(start);
    if (ch == patternSeparator)
      {
        // no pattern separator in the negative pattern
        if ((start + 1) > len) throw new
          IllegalArgumentException("unexpected character '" + ch + "' " +
                                   "in negative subpattern.");    
        start++;
      }
    
    int i;
    for (i = start; i < len; i++)
      {
        ch = pattern.charAt(i);
        
        // this means we are inside the number portion
        if (!quote &&
            (ch == digit || ch == zero || ch == decimalSeparator ||
             ch == patternSeparator || ch == groupingSeparator))
          {
            if (!prefixDone)
              {
                this.negativePrefix = buffer.toString();
                buffer.delete(0, buffer.length());
                prefixDone = true;
              }
          }
        else if (ch == minus)
          {
            buffer.append(this.symbols.getMinusSign());
          }
        else if (quote && ch != '\'')
          {
            buffer.append(ch);
          }
        else if (ch == '\u00A4')
          {
            // CURRENCY
            currencySymbol = CURRENCY_SYMBOL;

            // if \u00A4 is doubled, we use the international currency symbol
            if ((i + 1) < len && pattern.charAt(i + 1) == '\u00A4')
              {
                currencySymbol = this.symbols.getInternationalCurrencySymbol();
                i = i + 2;
              }

            // FIXME: not sure about this, the specs says that we only have to
            // change prefix and suffix, so leave it as commented
            // unless in case of bug report/errors
            //this.useCurrencySeparator = true;
            
            buffer.append(currencySymbol);
          }
        else if (ch == percent)
          {
            // PERCENT
            this.negativePatternMultiplier = 100;
            buffer.append(this.symbols.getPercent());
          }
        else if (ch == permille)
          {
            // PERMILLE
            this.negativePatternMultiplier = 1000;
            buffer.append(this.symbols.getPerMill());
          }
        else if (ch == '\'')
          {
            // QUOTE
            if ((i + 1) < len && pattern.charAt(i + 1) == '\'')
              {
                // we need to add ' to the buffer 
                buffer.append(ch);
                i++;
              }
            else
              {
                quote = !quote;
              }
          }
        else if (ch == patternSeparator)
          {
            // no pattern separator in the negative pattern
            throw new IllegalArgumentException("unexpected character '" + ch +
                                               "' in negative subpattern.");
          }
        else
          {
            buffer.append(ch);
          }
      }
    
    if (prefixDone)
      this.negativeSuffix = buffer.toString();
    else
      this.negativePrefix = buffer.toString();
  }
  
  /* ****** FORMATTING ****** */
  
  /**
   * Handles the real formatting.
   * 
   * We use a BigDecimal to format the number without precision loss.
   * All the rounding is done by methods in BigDecimal.
   * The <code>isLong</code> parameter is used to determine if we are
   * formatting a long or BigInteger. In this case, we avoid to format
   * the fractional part of the number (unless specified otherwise in the
   * format string) that would consist only of a 0 digit.
   * 
   * @param number A BigDecimal representation fo the input number.
   * @param dest The destination buffer.
   * @param isLong A boolean that indicates if this BigDecimal is a real
   * decimal or an integer.
   * @param fieldPos Use to keep track of the formatting position.
   */
  private void formatInternal(BigDecimal number, boolean isLong,
                              StringBuffer dest, FieldPosition fieldPos)
  {  
    // The specs says that fieldPos should not be null, and that we
    // should throw a NPE, but it seems that in few classes that
    // reference this one, fieldPos is set to null.
    // This is even defined in the javadoc, see for example MessageFormat.
    // I think the best here is to check for fieldPos and build one if it is
    // null. If it cause harms or regressions, just remove this line and
    // fix the classes in the point of call, insted.
    if (fieldPos == null) fieldPos = new FieldPosition(0);
    
    int _multiplier = this.multiplier;
    
    // used to track attribute starting position for each attribute
    int attributeStart = -1;
    
    // now get the sign this will be used by the special case Inifinity
    // and by the normal cases.
    boolean isNegative = (number.signum() < 0) ? true : false;
    if (isNegative)
      {
        attributeStart = dest.length();
        
        // append the negative prefix to the string
        dest.append(negativePrefix);
        
        // once got the negative prefix, we can use
        // the absolute value.
        number = number.abs();
        
        _multiplier = negativePatternMultiplier;
        
        addAttribute(Field.SIGN, attributeStart, dest.length());
      }
    else
      {
        // not negative, use the positive prefix
        dest.append(positivePrefix);
      }
    
    // these are used ot update the field position
    int beginIndexInt = dest.length();
    int endIndexInt = 0;
    int beginIndexFract = 0;
    int endIndexFract = 0;
    
    // compute the multiplier to use with percent and similar
    number = number.multiply(BigDecimal.valueOf(_multiplier));
    
    // XXX: special case, not sure if it belongs here or if it is
    // correct at all. There may be other special cases as well
    // these should be handled in the format string parser.
    if (this.maximumIntegerDigits == 0 && this.maximumFractionDigits == 0)
      {
        number = BigDecimal.ZERO;
        this.maximumIntegerDigits = 1;
        this.minimumIntegerDigits = 1;
      }
    
    //  get the absolute number
    number = number.abs();

    // the scaling to use while formatting this number
    int scale = this.maximumFractionDigits;
    
    // this is the actual number we will use
    // it is corrected later on to handle exponential
    // notation, if needed
    long exponent = 0;
    
    // are we using exponential notation?
    if (this.useExponentialNotation)
      {
        exponent = getExponent(number);
        number = number.movePointLeft((int) exponent);
        
        // FIXME: this makes the test ##.###E0 to pass,
        // but all all the other tests to fail...
        // this should be really something like
        // min + max - what is already shown...
        //scale = this.minimumIntegerDigits + this.maximumFractionDigits;
      }
    
    // round the number to the nearest neighbor
    number = number.setScale(scale, BigDecimal.ROUND_HALF_EVEN);

    // now get the integer and fractional part of the string
    // that will be processed later
    String plain = number.toPlainString();
    
    String intPart = null;
    String fractPart = null;
    
    // remove - from the integer part, this is needed as
    // the Narrowing Primitive Conversions algorithm used may loose
    // information about the sign
    int minusIndex = plain.lastIndexOf('-', 0);
    if (minusIndex > -1) plain = plain.substring(minusIndex + 1);
    
    // strip the decimal portion
    int dot = plain.indexOf('.');
    if (dot > -1)
      {
        intPart = plain.substring(0, dot);
        dot++;
        
        if (useExponentialNotation)
          fractPart = plain.substring(dot, dot + scale);
        else
          fractPart = plain.substring(dot);
      }
    else
      {
        intPart = plain;
      }
    
    // used in various places later on
    int intPartLen = intPart.length();
    endIndexInt = intPartLen;
    
    // if the number of digits in our intPart is not greater than the
    // minimum we have to display, we append zero to the destination
    // buffer before adding the integer portion of the number.
    int zeroes = minimumIntegerDigits - intPartLen;
    if (zeroes > 0)
      {
        attributeStart = Math.max(dest.length() - 1, 0);
        appendZero(dest, zeroes, minimumIntegerDigits);
      }

    if (this.useExponentialNotation)
      {
        // For exponential numbers, the significant in mantissa are
        // the sum of the minimum integer and maximum fraction
        // digits, and does not take into account the maximun integer
        // digits to display.
        
        if (attributeStart < 0)
          attributeStart = Math.max(dest.length() - 1, 0);
        appendDigit(intPart, dest, this.groupingUsed);
      }
    else
      {
        // non exponential notation
        intPartLen = intPart.length();
        int canary = Math.min(intPartLen, this.maximumIntegerDigits);
        
        // remove from the string the number in excess
        // use only latest digits
        intPart = intPart.substring(intPartLen - canary);
        endIndexInt = intPart.length() + 1;
        
        // append it
        if (maximumIntegerDigits > 0 &&
            !(this.minimumIntegerDigits == 0 &&
             intPart.compareTo(String.valueOf(symbols.getZeroDigit())) == 0))
          {
            if (attributeStart < 0)
              attributeStart = Math.max(dest.length() - 1, 0);
            appendDigit(intPart, dest, this.groupingUsed);
          }
      }
    
    // add the INTEGER attribute
    addAttribute(Field.INTEGER, attributeStart, dest.length());
    
    // ...update field position, if needed, and return...
    if ((fieldPos.getField() == INTEGER_FIELD ||
        fieldPos.getFieldAttribute() == NumberFormat.Field.INTEGER))
      {
        fieldPos.setBeginIndex(beginIndexInt);
        fieldPos.setEndIndex(endIndexInt);
      }
    
    handleFractionalPart(dest, fractPart, fieldPos, isLong);
        
    // and the exponent
    if (this.useExponentialNotation)
      {
        attributeStart = dest.length();
        
        dest.append(symbols.getExponential());
        
        addAttribute(Field.EXPONENT_SYMBOL, attributeStart, dest.length());
        attributeStart = dest.length();
        
        if (exponent < 0)
          {
            dest.append(symbols.getMinusSign());
            exponent = -exponent;
            
            addAttribute(Field.EXPONENT_SIGN, attributeStart, dest.length());
          }
        
        attributeStart = dest.length();
        
        String exponentString = String.valueOf(exponent);
        int exponentLength = exponentString.length();
        
        for (int i = 0; i < minExponentDigits - exponentLength; i++)
          dest.append(symbols.getZeroDigit());
        
        for (int i = 0; i < exponentLength; ++i)
          dest.append(exponentString.charAt(i));
        
        addAttribute(Field.EXPONENT, attributeStart, dest.length());
      }
 
    // now include the suffixes...
    if (isNegative)
      {
        dest.append(negativeSuffix);
      }
    else
      {
        dest.append(positiveSuffix);
      }
  }

  /**
   * Add to the input buffer the result of formatting the fractional
   * portion of the number.
   * 
   * @param dest
   * @param fractPart
   * @param fieldPos
   * @param isLong
   */
  private void handleFractionalPart(StringBuffer dest, String fractPart,
                                    FieldPosition fieldPos, boolean isLong)
  {
    int dotStart = 0;
    int dotEnd = 0;
    boolean addDecimal = false;
    
    if (this.decimalSeparatorAlwaysShown  ||
         ((!isLong || this.useExponentialNotation) &&
           this.showDecimalSeparator && this.maximumFractionDigits > 0) ||
        this.minimumFractionDigits > 0)
      {
        dotStart = dest.length();
        
        if (this.useCurrencySeparator)
          dest.append(symbols.getMonetaryDecimalSeparator());
        else
          dest.append(symbols.getDecimalSeparator());
        
        dotEnd = dest.length();
        addDecimal = true;
      }
    
    // now handle the fraction portion of the number
    int fractStart = 0;
    int fractEnd = 0;
    boolean addFractional = false;
    
    if ((!isLong || this.useExponentialNotation)
        && this.maximumFractionDigits > 0
        || this.minimumFractionDigits > 0)
      {
        fractStart = dest.length();
        fractEnd = fractStart;
        
        int digits = this.minimumFractionDigits;
        
        if (this.useExponentialNotation)
          {
            digits = (this.minimumIntegerDigits + this.minimumFractionDigits)
              - dest.length();
            if (digits < 0) digits = 0;
          }
        
        fractPart = adjustTrailingZeros(fractPart, digits);
        
        // FIXME: this code must be improved
        // now check if the factional part is just 0, in this case
        // we need to remove the '.' unless requested
        boolean allZeros = true;
        char fracts[] = fractPart.toCharArray();
        for (int i = 0; i < fracts.length; i++)
          {
            if (fracts[i] != '0')
              allZeros = false;
          }
        
        if (!allZeros || (minimumFractionDigits > 0))
          {
            appendDigit(fractPart, dest, false);
            fractEnd = dest.length();
            
            addDecimal = true;
            addFractional = true;
          }
        else if (!this.decimalSeparatorAlwaysShown)
          {
            dest.deleteCharAt(dest.length() - 1);
            addDecimal = false;
          }
        else
          {
            fractEnd = dest.length();
            addFractional = true;
          }
      }
    
    if (addDecimal)
      addAttribute(Field.DECIMAL_SEPARATOR, dotStart, dotEnd);
    
    if (addFractional)
      addAttribute(Field.FRACTION, fractStart, fractEnd);
    
    if ((fieldPos.getField() == FRACTION_FIELD ||
        fieldPos.getFieldAttribute() == NumberFormat.Field.FRACTION))
      {
        fieldPos.setBeginIndex(fractStart);
        fieldPos.setEndIndex(fractEnd);
      }
  }
  
  /**
   * Append to <code>dest</code>the give number of zeros.
   * Grouping is added if needed.
   * The integer totalDigitCount defines the total number of digits
   * of the number to which we are appending zeroes.
   */
  private void appendZero(StringBuffer dest, int zeroes, int totalDigitCount)
  {
    char ch = symbols.getZeroDigit();
    char gSeparator = symbols.getGroupingSeparator();
    
    int i = 0;
    int gPos = totalDigitCount;
    for (i = 0; i < zeroes; i++, gPos--)
      {
        if (this.groupingSeparatorInPattern &&
            (this.groupingUsed && this.groupingSize != 0) &&
            (gPos % groupingSize == 0 && i > 0))
          dest.append(gSeparator);
        
        dest.append(ch);
      }
    
    // special case, that requires adding an additional separator
    if (this.groupingSeparatorInPattern &&
        (this.groupingUsed && this.groupingSize != 0) &&
        (gPos % groupingSize == 0))
      dest.append(gSeparator);
  }
  
  /**
   * Append src to <code>dest</code>.
   * 
   * Grouping is added if <code>groupingUsed</code> is set
   * to <code>true</code>.
   */
  private void appendDigit(String src, StringBuffer dest,
                             boolean groupingUsed)
  {
    int zero = symbols.getZeroDigit() - '0';
    
    int ch;
    char gSeparator = symbols.getGroupingSeparator();
        
    int len = src.length();
    for (int i = 0, gPos = len; i < len; i++, gPos--)
      {
        ch = src.charAt(i);
        if (groupingUsed && this.groupingSize != 0 &&
            gPos % groupingSize == 0 && i > 0)
          dest.append(gSeparator);

        dest.append((char) (zero + ch));
      }
  }
  
  /**
   * Calculate the exponent to use if eponential notation is used.
   * The exponent is calculated as a power of ten.
   * <code>number</code> should be positive, if is zero, or less than zero,
   * zero is returned.
   */
  private long getExponent(BigDecimal number)
  {
    long exponent = 0;
    
    if (number.signum() > 0)
      {
        double _number = number.doubleValue();
        exponent = (long) Math.floor (Math.log10(_number));
        
        // get the right value for the exponent
        exponent = exponent - (exponent % this.exponentRound);
        
        // if the minimumIntegerDigits is more than zero
        // we display minimumIntegerDigits of digits.
        // so, for example, if minimumIntegerDigits == 2
        // and the actual number is 0.123 it will be
        // formatted as 12.3E-2
        // this means that the exponent have to be shifted
        // to the correct value.
        if (minimumIntegerDigits > 0)
              exponent -= minimumIntegerDigits - 1;
      }
    
    return exponent;
  }
 
  /**
   * Remove contiguos zeros from the end of the <code>src</code> string,
   * if src contains more than <code>minimumDigits</code> digits.
   * if src contains less that <code>minimumDigits</code>,
   * then append zeros to the string.
   * 
   * Only the first block of zero digits is removed from the string
   * and only if they fall in the src.length - minimumDigits
   * portion of the string.
   * 
   * @param src The string with the correct number of zeros.
   */
  private String adjustTrailingZeros(String src, int minimumDigits)
  {
    int len = src.length();
    String result;
    
    // remove all trailing zero
    if (len > minimumDigits)
      {
        int zeros = 0;    
        for (int i = len - 1; i > minimumDigits; i--)
          {
            if (src.charAt(i) == '0')
              ++zeros;
            else
              break;
          }
        result =  src.substring(0, len - zeros);                
      }
    else
      {
        char zero = symbols.getZeroDigit();
        CPStringBuilder _result = new CPStringBuilder(src);
        for (int i = len; i < minimumDigits; i++)
          {
            _result.append(zero);
          }
        result = _result.toString();
      }
    
    return result;
  }
  
  /**
   * Adds an attribute to the attributes list.
   * 
   * @param field
   * @param begin
   * @param end
   */
  private void addAttribute(Field field, int begin, int end)
  {
    /*
     * This method and its implementation derives directly from the
     * ICU4J (http://icu.sourceforge.net/) library, distributed under MIT/X.
     */
    
    FieldPosition pos = new FieldPosition(field);
    pos.setBeginIndex(begin);
    pos.setEndIndex(end);
    attributes.add(pos);
  }
  
  /**
   * Sets the default values for the various properties in this DecimaFormat.
   */
  private void setDefaultValues()
  {
    // Maybe we should add these values to the message bundle and take
    // the most appropriate for them for any locale.
    // Anyway, these seem to be good values for a default in most languages.
    // Note that most of these will change based on the format string.
    
    this.negativePrefix = String.valueOf(symbols.getMinusSign());
    this.negativeSuffix = "";
    this.positivePrefix = "";
    this.positiveSuffix = "";
    
    this.multiplier = 1;
    this.negativePatternMultiplier = 1;
    this.exponentRound = 1;
    
    this.hasNegativePrefix = false;
    
    this.minimumIntegerDigits = 1;
    this.maximumIntegerDigits = DEFAULT_INTEGER_DIGITS;
    this.minimumFractionDigits = 0;
    this.maximumFractionDigits = DEFAULT_FRACTION_DIGITS;
    this.minExponentDigits = 0;
    
    this.groupingSize = 0;
    
    this.decimalSeparatorAlwaysShown = false;
    this.showDecimalSeparator = false;
    this.useExponentialNotation = false;
    this.groupingUsed = false;
    this.groupingSeparatorInPattern = false;
    
    this.useCurrencySeparator = false;
    
    this.hasFractionalPattern = false;
  }
}
