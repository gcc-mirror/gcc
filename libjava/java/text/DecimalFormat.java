/* DecimalFormat.java -- Formats and parses numbers
   Copyright (C) 1999, 2000, 2001, 2003, 2004  Free Software Foundation, Inc.

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

import gnu.java.text.AttributedFormatBuffer;
import gnu.java.text.FormatBuffer;
import gnu.java.text.FormatCharacterIterator;
import gnu.java.text.StringFormatBuffer;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.util.Currency;
import java.util.HashMap;
import java.util.Locale;

/**
 * @author Tom Tromey (tromey@cygnus.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @date March 4, 1999
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 from http://www.javasoft.com.
 * Status:  Believed complete and correct to 1.2.
 * Note however that the docs are very unclear about how format parsing
 * should work.  No doubt there are problems here.
 */
public class DecimalFormat extends NumberFormat
{
  // This is a helper for applyPatternWithSymbols.  It reads a prefix
  // or a suffix.  It can cause some side-effects.
  private int scanFix (String pattern, int index, FormatBuffer buf,
                       String patChars, DecimalFormatSymbols syms,
                       boolean is_suffix)
  {
    int len = pattern.length();
    boolean quoteStarted = false;
    buf.clear();
    
    boolean multiplierSet = false;
    while (index < len)
      {
	char c = pattern.charAt(index);

	if (quoteStarted)
	  {
	    if (c == '\'')
	      quoteStarted = false;
	    else
	      buf.append(c);
	    index++;
	    continue;
	  }

	if (c == '\'' && index + 1 < len
	    && pattern.charAt(index + 1) == '\'')
	  {
	    buf.append(c);
	    index++;
	  }
	else if (c == '\'')
	  {
	    quoteStarted = true;
	  }
	else if (c == '\u00a4')
	  {
	    if (index + 1 < len && pattern.charAt(index + 1) == '\u00a4')
	      {
		buf.append(syms.getInternationalCurrencySymbol(), NumberFormat.Field.CURRENCY);
		index++;
	      }
	    else
	      buf.append(syms.getCurrencySymbol(), NumberFormat.Field.CURRENCY);
	  }
	else if (c == syms.getPercent())
	  {
	    if (multiplierSet)
	      throw new IllegalArgumentException ("multiplier already set " +
						  "- index: " + index);
	    multiplierSet = true;
	    multiplier = 100;
	    buf.append(c, NumberFormat.Field.PERCENT);
	  }
	else if (c == syms.getPerMill())
	  {
	    if (multiplierSet)
	      throw new IllegalArgumentException ("multiplier already set " +
						  "- index: " + index);
	    multiplierSet = true;
	    multiplier = 1000;
	    buf.append(c, NumberFormat.Field.PERMILLE);
	  }
	else if (patChars.indexOf(c) != -1)
	  {
	    // This is a pattern character.
	    break;
	  }
	else
	  buf.append(c);
	index++;
      }

    if (quoteStarted)
      throw new IllegalArgumentException ("pattern is lacking a closing quote");

    return index;
  }

  // A helper which reads a number format.
  private int scanFormat (String pattern, int index, String patChars,
                          DecimalFormatSymbols syms, boolean is_positive)
  {
    int max = pattern.length();

    int countSinceGroup = 0;
    int zeroCount = 0;
    boolean saw_group = false;

    //
    // Scan integer part.
    //
    while (index < max)
      {
	char c = pattern.charAt(index);

	if (c == syms.getDigit())
	  {
	    if (zeroCount > 0)
	      throw new IllegalArgumentException ("digit mark following " +
						  "zero - index: " + index);
	    ++countSinceGroup;
	  }
	else if (c == syms.getZeroDigit())
	  {
	    ++zeroCount;
	    ++countSinceGroup;
	  }
	else if (c == syms.getGroupingSeparator())
	  {
	    countSinceGroup = 0;
	    saw_group = true;
	  }
	else
	  break;

	++index;
      }

    // We can only side-effect when parsing the positive format.
    if (is_positive)
      {
	groupingUsed = saw_group;
	groupingSize = (byte) countSinceGroup;
	minimumIntegerDigits = zeroCount;
      }

    // Early termination.
    if (index == max || pattern.charAt(index) == syms.getGroupingSeparator())
      {
	if (is_positive)
	  decimalSeparatorAlwaysShown = false;
	return index;
      }

    if (pattern.charAt(index) == syms.getDecimalSeparator())
      {
	++index;

	//
	// Scan fractional part.
	//
	int hashCount = 0;
	zeroCount = 0;
	while (index < max)
	  {
	    char c = pattern.charAt(index);
	    if (c == syms.getZeroDigit())
	      {
		if (hashCount > 0)
		  throw new IllegalArgumentException ("zero mark " +
						      "following digit - index: " + index);
		++zeroCount;
	      }
	    else if (c == syms.getDigit())
	      {
		++hashCount;
	      }
	    else if (c != syms.getExponential()
		     && c != syms.getPatternSeparator()
		     && c != syms.getPercent()
		     && c != syms.getPerMill()
		     && patChars.indexOf(c) != -1)
	      throw new IllegalArgumentException ("unexpected special " +
						  "character - index: " + index);
	    else
	      break;

	    ++index;
	  }

	if (is_positive)
	  {
	    maximumFractionDigits = hashCount + zeroCount;
	    minimumFractionDigits = zeroCount;
	  }

	if (index == max)
	  return index;
      }

    if (pattern.charAt(index) == syms.getExponential())
      {
	//
	// Scan exponential format.
	//
	zeroCount = 0;
	++index;
	while (index < max)
	  {
	    char c = pattern.charAt(index);
	    if (c == syms.getZeroDigit())
	      ++zeroCount;
	    else if (c == syms.getDigit())
	      {
		if (zeroCount > 0)
		  throw new
		    IllegalArgumentException ("digit mark following zero " +
					      "in exponent - index: " +
					      index);
	      }
	    else if (patChars.indexOf(c) != -1)
	      throw new IllegalArgumentException ("unexpected special " +
						  "character - index: " +
						  index);
	    else
	      break;

	    ++index;
	  }

	if (is_positive)
	  {
	    useExponentialNotation = true;
	    minExponentDigits = (byte) zeroCount;
	  }

	maximumIntegerDigits = groupingSize;
	groupingSize = 0;
	if (maximumIntegerDigits > minimumIntegerDigits && maximumIntegerDigits > 0)
	  {
	    minimumIntegerDigits = 1;
	    exponentRound = maximumIntegerDigits;
	  }
	else
	  exponentRound = 1;
      }

    return index;
  }

  // This helper function creates a string consisting of all the
  // characters which can appear in a pattern and must be quoted.
  private String patternChars (DecimalFormatSymbols syms)
  {
    StringBuffer buf = new StringBuffer ();
    buf.append(syms.getDecimalSeparator());
    buf.append(syms.getDigit());
    buf.append(syms.getExponential());
    buf.append(syms.getGroupingSeparator());
    // Adding this one causes pattern application to fail.
    // Of course, omitting is causes toPattern to fail.
    // ... but we already have bugs there.  FIXME.
    // buf.append(syms.getMinusSign());
    buf.append(syms.getPatternSeparator());
    buf.append(syms.getPercent());
    buf.append(syms.getPerMill());
    buf.append(syms.getZeroDigit());
    buf.append('\u00a4');
    return buf.toString();
  }

  private void applyPatternWithSymbols(String pattern, DecimalFormatSymbols syms)
  {
    // Initialize to the state the parser expects.
    negativePrefix = "";
    negativeSuffix = "";
    positivePrefix = "";
    positiveSuffix = "";
    decimalSeparatorAlwaysShown = false;
    groupingSize = 0;
    minExponentDigits = 0;
    multiplier = 1;
    useExponentialNotation = false;
    groupingUsed = false;
    maximumFractionDigits = 0;
    maximumIntegerDigits = MAXIMUM_INTEGER_DIGITS;
    minimumFractionDigits = 0;
    minimumIntegerDigits = 1;

    AttributedFormatBuffer buf = new AttributedFormatBuffer ();
    String patChars = patternChars (syms);

    int max = pattern.length();
    int index = scanFix (pattern, 0, buf, patChars, syms, false);
    buf.sync();
    positivePrefix = buf.getBuffer().toString();
    positivePrefixRanges = buf.getRanges();
    positivePrefixAttrs = buf.getAttributes();

    index = scanFormat (pattern, index, patChars, syms, true);

    index = scanFix (pattern, index, buf, patChars, syms, true);
    buf.sync();
    positiveSuffix = buf.getBuffer().toString();
    positiveSuffixRanges = buf.getRanges();
    positiveSuffixAttrs = buf.getAttributes();

    if (index == pattern.length())
      {
	// No negative info.
	negativePrefix = null;
	negativeSuffix = null;
      }
    else
      {
	if (pattern.charAt(index) != syms.getPatternSeparator())
	  throw new IllegalArgumentException ("separator character " +
					      "expected - index: " + index);

	index = scanFix (pattern, index + 1, buf, patChars, syms, false);
	buf.sync();
	negativePrefix = buf.getBuffer().toString();
	negativePrefixRanges = buf.getRanges();
	negativePrefixAttrs = buf.getAttributes();

	// We parse the negative format for errors but we don't let
	// it side-effect this object.
	index = scanFormat (pattern, index, patChars, syms, false);

	index = scanFix (pattern, index, buf, patChars, syms, true);
	buf.sync();
	negativeSuffix = buf.getBuffer().toString();
	negativeSuffixRanges = buf.getRanges();
	negativeSuffixAttrs = buf.getAttributes();

	if (index != pattern.length())
	  throw new IllegalArgumentException ("end of pattern expected " +
					      "- index: " + index);
      }
  }

  public void applyLocalizedPattern (String pattern)
  {
    // JCL p. 638 claims this throws a ParseException but p. 629
    // contradicts this.  Empirical tests with patterns of "0,###.0"
    // and "#.#.#" corroborate the p. 629 statement that an
    // IllegalArgumentException is thrown.
    applyPatternWithSymbols (pattern, symbols);
  }

  public void applyPattern (String pattern)
  {
    // JCL p. 638 claims this throws a ParseException but p. 629
    // contradicts this.  Empirical tests with patterns of "0,###.0"
    // and "#.#.#" corroborate the p. 629 statement that an
    // IllegalArgumentException is thrown.
    applyPatternWithSymbols (pattern, nonLocalizedSymbols);
  }

  public Object clone ()
  {
    DecimalFormat c = (DecimalFormat) super.clone ();
    c.symbols = (DecimalFormatSymbols) symbols.clone ();
    return c;
  }

  public DecimalFormat ()
  {
    this ("#,##0.###");
  }

  public DecimalFormat (String pattern)
  {
    this (pattern, new DecimalFormatSymbols ());
  }

  public DecimalFormat (String pattern, DecimalFormatSymbols symbols)
  {
    this.symbols = symbols;
    applyPattern (pattern);
  }

  private boolean equals(String s1, String s2)
  {
    if (s1 == null || s2 == null)
      return s1 == s2;
    return s1.equals(s2);
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof DecimalFormat))
      return false;
    DecimalFormat dup = (DecimalFormat) obj;
    return (decimalSeparatorAlwaysShown == dup.decimalSeparatorAlwaysShown
	    && groupingSize == dup.groupingSize
	    && minExponentDigits == dup.minExponentDigits
	    && multiplier == dup.multiplier
	    && equals(negativePrefix, dup.negativePrefix)
	    && equals(negativeSuffix, dup.negativeSuffix)
	    && equals(positivePrefix, dup.positivePrefix)
	    && equals(positiveSuffix, dup.positiveSuffix)
	    && symbols.equals(dup.symbols)
	    && useExponentialNotation == dup.useExponentialNotation);
  }

  private void formatInternal (double number, FormatBuffer dest,
			       FieldPosition fieldPos)
  {
    // A very special case.
    if (Double.isNaN(number))
      {
	dest.append(symbols.getNaN());
	if (fieldPos != null && 
	    (fieldPos.getField() == INTEGER_FIELD ||
	     fieldPos.getFieldAttribute() == NumberFormat.Field.INTEGER))
	  {
	    int index = dest.length();
	    fieldPos.setBeginIndex(index - symbols.getNaN().length());
	    fieldPos.setEndIndex(index);
	  }
	return;
      }

    boolean is_neg = number < 0;
    if (is_neg)
      {
	if (negativePrefix != null)
	  dest.append(negativePrefix, negativePrefixRanges, negativePrefixAttrs);
	else
	  {
	    dest.append(symbols.getMinusSign(), NumberFormat.Field.SIGN);
	    dest.append(positivePrefix, positivePrefixRanges, positivePrefixAttrs);
	  }
	number = - number;
      }
    else
      dest.append(positivePrefix, positivePrefixRanges, positivePrefixAttrs);

    int integerBeginIndex = dest.length();
    int integerEndIndex = 0;
    int zeroStart = symbols.getZeroDigit() - '0';

    if (Double.isInfinite (number))
      {
	dest.append(symbols.getInfinity());
	integerEndIndex = dest.length();
      }
    else
      {
	number *= multiplier;

	// Compute exponent.
	long exponent = 0;
	double baseNumber;
	if (useExponentialNotation)
	  {
	    exponent = (long) Math.floor (Math.log(number) / Math.log(10));
	    exponent = exponent - (exponent % exponentRound);
	    if (minimumIntegerDigits > 0)
	      exponent -= minimumIntegerDigits - 1;
	    baseNumber = (number / Math.pow(10.0, exponent));
	  }
	else
	  baseNumber = number;

	// Round to the correct number of digits.
	baseNumber += 5 * Math.pow(10.0, - maximumFractionDigits - 1);

	int index = dest.length();
	//double intPart = Math.floor(baseNumber);
	String intPart = Long.toString((long)Math.floor(baseNumber));
	int count, groupPosition = intPart.length();

	dest.setDefaultAttribute(NumberFormat.Field.INTEGER);

	for (count = 0; count < minimumIntegerDigits-intPart.length(); count++)
	  dest.append(symbols.getZeroDigit());

	for (count = 0;
	     count < maximumIntegerDigits && count < intPart.length();
	     count++)
	  {
	    int dig = intPart.charAt(count);

	    // Append group separator if required.
	    if (groupingUsed && count > 0 && groupingSize != 0 && groupPosition % groupingSize == 0)
	      {
		dest.append(symbols.getGroupingSeparator(), NumberFormat.Field.GROUPING_SEPARATOR);
		dest.setDefaultAttribute(NumberFormat.Field.INTEGER);
	      }
	    dest.append((char) (zeroStart + dig));

	    groupPosition--;
	  }
	dest.setDefaultAttribute(null);

	integerEndIndex = dest.length();
	   
	int decimal_index = integerEndIndex;
	int consecutive_zeros = 0;
	int total_digits = 0;

	int localMaximumFractionDigits = maximumFractionDigits;

	if (useExponentialNotation)
	  localMaximumFractionDigits += minimumIntegerDigits - count;

	// Strip integer part from NUMBER.
	double fracPart = baseNumber - Math.floor(baseNumber);
	
	if ( ((fracPart != 0 || minimumFractionDigits > 0) && localMaximumFractionDigits > 0)
	     || decimalSeparatorAlwaysShown)
	  {
	    dest.append (symbols.getDecimalSeparator(), NumberFormat.Field.DECIMAL_SEPARATOR);
	  }

	int fraction_begin = dest.length();
	dest.setDefaultAttribute(NumberFormat.Field.FRACTION);
	for (count = 0;
	     count < localMaximumFractionDigits
	       && (fracPart != 0 || count < minimumFractionDigits);
	     ++count)
	  {
	    ++total_digits;
	    fracPart *= 10;
	    long dig = (long) fracPart;
	    if (dig == 0)
	      ++consecutive_zeros;
	    else
	      consecutive_zeros = 0;
	    dest.append((char) (symbols.getZeroDigit() + dig));

	    // Strip integer part from FRACPART.
	    fracPart = fracPart - Math.floor (fracPart);
	  }

	// Strip extraneous trailing `0's.  We can't always detect
	// these in the loop.
	int extra_zeros = Math.min (consecutive_zeros,
				    total_digits - minimumFractionDigits);
	if (extra_zeros > 0)
	  {
	    dest.cutTail(extra_zeros);
	    total_digits -= extra_zeros;
	    if (total_digits == 0 && !decimalSeparatorAlwaysShown)
	      dest.cutTail(1);
	  }

	if (fieldPos != null && fieldPos.getField() == FRACTION_FIELD)
	  {
	    fieldPos.setBeginIndex(fraction_begin);
	    fieldPos.setEndIndex(dest.length());
	  }

	// Finally, print the exponent.
	if (useExponentialNotation)
	  {
	    dest.append(symbols.getExponential(), NumberFormat.Field.EXPONENT_SYMBOL);	    
	    if (exponent < 0)
	      {
		dest.append (symbols.getMinusSign (), NumberFormat.Field.EXPONENT_SIGN);
		exponent = - exponent;
	      }
	    index = dest.length();
	    dest.setDefaultAttribute(NumberFormat.Field.EXPONENT);
	    String exponentString = Long.toString ((long) exponent);
	    
	    for (count = 0; count < minExponentDigits-exponentString.length();
		 count++)
	      dest.append((char) symbols.getZeroDigit());

	    for (count = 0;
		 count < exponentString.length();
		 ++count)
	      {
		int dig = exponentString.charAt(count);
		dest.append((char) (zeroStart + dig));
	      }
	  }
      }

    if (fieldPos != null && 
	(fieldPos.getField() == INTEGER_FIELD ||
	 fieldPos.getFieldAttribute() == NumberFormat.Field.INTEGER))
      {
	fieldPos.setBeginIndex(integerBeginIndex);
	fieldPos.setEndIndex(integerEndIndex);
      }

    if (is_neg && negativeSuffix != null)
      dest.append(negativeSuffix, negativeSuffixRanges, negativeSuffixAttrs);
    else
      dest.append(positiveSuffix, positiveSuffixRanges, positiveSuffixAttrs);
  }

  public StringBuffer format (double number, StringBuffer dest,
			      FieldPosition fieldPos)
  {
    formatInternal (number, new StringFormatBuffer(dest), fieldPos);
    return dest;
  }

  public AttributedCharacterIterator formatToCharacterIterator (Object value)
  {
    AttributedFormatBuffer sbuf = new AttributedFormatBuffer();

    if (value instanceof Number)
      formatInternal(((Number) value).doubleValue(), sbuf, null);
    else
      throw new IllegalArgumentException 
	("Cannot format given Object as a Number");
    
    sbuf.sync();
    return new FormatCharacterIterator(sbuf.getBuffer().toString(), 
				       sbuf.getRanges(), 
				       sbuf.getAttributes());
  }

  public StringBuffer format (long number, StringBuffer dest,
			      FieldPosition fieldPos)
  {
    // If using exponential notation, we just format as a double.
    if (useExponentialNotation)
       return format ((double) number, dest, fieldPos);

    boolean is_neg = number < 0;
    if (is_neg)
      {
	if (negativePrefix != null)
	  dest.append(negativePrefix);
	else
	  {
	    dest.append(symbols.getMinusSign());
	    dest.append(positivePrefix);
	  }
	number = - number;
      }
    else
      dest.append(positivePrefix);

    int integerBeginIndex = dest.length();
    int index = dest.length();
    int count = 0;
    while (count < maximumIntegerDigits
	   && (number > 0 || count < minimumIntegerDigits))
      {
	long dig = number % 10;
	number /= 10;
	// NUMBER and DIG will be less than 0 if the original number
	// was the most negative long.
	if (dig < 0)
	  {
	    dig = - dig;
	    number = - number;
	  }

	// Append group separator if required.
	if (groupingUsed && count > 0 && groupingSize != 0 && count % groupingSize == 0)
	  dest.insert(index, symbols.getGroupingSeparator());

	dest.insert(index, (char) (symbols.getZeroDigit() + dig));

	++count;
      }

    if (fieldPos != null && fieldPos.getField() == INTEGER_FIELD)
      {
	fieldPos.setBeginIndex(integerBeginIndex);
	fieldPos.setEndIndex(dest.length());
      }

    if (decimalSeparatorAlwaysShown || minimumFractionDigits > 0)
      {
	dest.append(symbols.getDecimalSeparator());
	if (fieldPos != null && fieldPos.getField() == FRACTION_FIELD)
	  {
	    fieldPos.setBeginIndex(dest.length());
	    fieldPos.setEndIndex(dest.length() + minimumFractionDigits);
	  }
      }

    for (count = 0; count < minimumFractionDigits; ++count)
      dest.append(symbols.getZeroDigit());

    dest.append((is_neg && negativeSuffix != null)
		? negativeSuffix
		: positiveSuffix);
    return dest;
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

  public DecimalFormatSymbols getDecimalFormatSymbols ()
  {
    return symbols;
  }

  public int getGroupingSize ()
  {
    return groupingSize;
  }

  public int getMultiplier ()
  {
    return multiplier;
  }

  public String getNegativePrefix ()
  {
    return negativePrefix;
  }

  public String getNegativeSuffix ()
  {
    return negativeSuffix;
  }

  public String getPositivePrefix ()
  {
    return positivePrefix;
  }

  public String getPositiveSuffix ()
  {
    return positiveSuffix;
  }

  public int hashCode ()
  {
    int hash = (negativeSuffix.hashCode() ^ negativePrefix.hashCode()
		^positivePrefix.hashCode() ^ positiveSuffix.hashCode());
    // FIXME.
    return hash;
  }

  public boolean isDecimalSeparatorAlwaysShown ()
  {
    return decimalSeparatorAlwaysShown;
  }

  public Number parse (String str, ParsePosition pos)
  {
    /*
     * Our strategy is simple: copy the text into separate buffers: one for the int part,
     * one for the fraction part and for the exponential part.
     * We translate or omit locale-specific information.  
     * If exponential is sufficiently big we merge the fraction and int part and
     * remove the '.' and then we use Long to convert the number. In the other
     * case, we use Double to convert the full number.
     */

    boolean is_neg = false;
    int index = pos.getIndex();
    StringBuffer int_buf = new StringBuffer ();
        
    // We have to check both prefixes, because one might be empty.  We
    // want to pick the longest prefix that matches.
    boolean got_pos = str.startsWith(positivePrefix, index);
    String np = (negativePrefix != null
		 ? negativePrefix
		 : positivePrefix + symbols.getMinusSign());
    boolean got_neg = str.startsWith(np, index);

    if (got_pos && got_neg)
      {
	// By checking this way, we preserve ambiguity in the case
	// where the negative format differs only in suffix.  We
	// check this again later.
	if (np.length() > positivePrefix.length())
	  {
	    is_neg = true;
	    index += np.length();
	  }
	else
	  index += positivePrefix.length();
      }
    else if (got_neg)
      {
	is_neg = true;
	index += np.length();
      }
    else if (got_pos)
      index += positivePrefix.length();
    else
      {
	pos.setErrorIndex (index);
	return null;
      }

    // FIXME: handle Inf and NaN.

    // FIXME: do we have to respect minimum digits?
    // What about multiplier?

    StringBuffer buf = int_buf;
    StringBuffer frac_buf = null;
    StringBuffer exp_buf = null;
    int start_index = index;
    int max = str.length();
    int exp_index = -1;
    int last = index + maximumIntegerDigits; 

    if (maximumFractionDigits > 0)
      last += maximumFractionDigits + 1;
    
    if (useExponentialNotation)
      last += minExponentDigits + 1;

    if (last > 0 && max > last)
      max = last;

    char zero = symbols.getZeroDigit();
    int last_group = -1;
    boolean int_part = true;
    boolean exp_part = false;
    for (; index < max; ++index)
      {
	char c = str.charAt(index);

	// FIXME: what about grouping size?
	if (groupingUsed && c == symbols.getGroupingSeparator())
	  {
	    if (last_group != -1 
		&& groupingSize != 0  
		&& (index - last_group) % groupingSize != 0)
	      {
		pos.setErrorIndex(index);
		return null;
	      }
	    last_group = index+1;
	  }
	else if (c >= zero && c <= zero + 9)
	  {
	    buf.append((char) (c - zero + '0'));
	  }
	else if (parseIntegerOnly)
	  break;
	else if (c == symbols.getDecimalSeparator())
	  {
	    if (last_group != -1 
		&& groupingSize != 0 
		&& (index - last_group) % groupingSize != 0)
	      {
		pos.setErrorIndex(index);
		return null;
	      }
	    buf = frac_buf = new StringBuffer();
	    frac_buf.append('.');
	    int_part = false;
	  }
	else if (c == symbols.getExponential())
	  {
	    buf = exp_buf = new StringBuffer();
	    int_part = false;
	    exp_part = true;
	    exp_index = index+1;
	  }
	else if (exp_part
		 && (c == '+' || c == '-' || c == symbols.getMinusSign()))
	  {
	    // For exponential notation.
	    buf.append(c);
	  }
	else
	  break;
      }

    if (index == start_index)
      {
	// Didn't see any digits.
	pos.setErrorIndex(index);
	return null;
      }

    // Check the suffix.  We must do this before converting the
    // buffer to a number to handle the case of a number which is
    // the most negative Long.
    boolean got_pos_suf = str.startsWith(positiveSuffix, index);
    String ns = (negativePrefix == null ? positiveSuffix : negativeSuffix);
    boolean got_neg_suf = str.startsWith(ns, index);
    if (is_neg)
      {
	if (! got_neg_suf)
	  {
	    pos.setErrorIndex(index);
	    return null;
	  }
      }
    else if (got_pos && got_neg && got_neg_suf)
      {
	is_neg = true;
      }
    else if (got_pos != got_pos_suf && got_neg != got_neg_suf)
      {
	pos.setErrorIndex(index);
	return null;
      }

    String suffix = is_neg ? ns : positiveSuffix;
    long multiplier = 1;
    boolean use_long;

    if (is_neg)
      int_buf.insert(0, '-');

    // Now handle the exponential part if there is one.
    if (exp_buf != null)
      {
	int exponent_value;

	try
	  {
	    exponent_value = Integer.parseInt(exp_buf.toString());
	  }
	catch (NumberFormatException x1)
	  {
	    pos.setErrorIndex(exp_index);
	    return null;
	  }

	if (frac_buf == null)
	  {
	    // We only have to add some zeros to the int part.
	    // Build a multiplier.
	    for (int i = 0; i < exponent_value; i++)
	      int_buf.append('0');
	    
	    use_long = true;
	  }
	else
	  {
	    boolean long_sufficient;

	    if (exponent_value < frac_buf.length()-1)
	      {
		int lastNonNull = -1;
		/* We have to check the fraction buffer: it may only be full of '0'
		 * or be sufficiently filled with it to convert the number into Long.
		 */
		for (int i = 1; i < frac_buf.length(); i++)
		  if (frac_buf.charAt(i) != '0')
		    lastNonNull = i;

		long_sufficient = (lastNonNull < 0 || lastNonNull <= exponent_value);
	      }
	    else
	      long_sufficient = true;
	    
	    if (long_sufficient)
	      {
		for (int i = 1; i < frac_buf.length() && i < exponent_value; i++)
		  int_buf.append(frac_buf.charAt(i));
		for (int i = frac_buf.length()-1; i < exponent_value; i++)
		  int_buf.append('0');
		use_long = true;
	      }
	    else
	      {
		/*
		 * A long type is not sufficient, we build the full buffer to
		 * be parsed by Double.
		 */
		int_buf.append(frac_buf);
		int_buf.append('E');
		int_buf.append(exp_buf);
		use_long = false;
	      }
	  }
      }
    else
      {
	if (frac_buf != null)
	  {
	    /* Check whether the fraction buffer contains only '0' */
	    int i;
	    for (i = 1; i < frac_buf.length(); i++)
	      if (frac_buf.charAt(i) != '0')
		break;
	   
	    if (i != frac_buf.length())
	      {
		use_long = false;
		int_buf.append(frac_buf);
	      }
	    else
	      use_long = true;
	  }
	else
	  use_long = true;
      }

    String t = int_buf.toString();
    Number result = null;
    if (use_long)
      {
	try
	  {
	    result = new Long (t);
	  }
	catch (NumberFormatException x1)
	  {
	  }
      }
    else
      {
	try
	  {
	    result = new Double (t);
	  }
	catch (NumberFormatException x2)
	  {
	  }
      }
    if (result == null)
      {
	pos.setErrorIndex(index);
	return null;
      }

    pos.setIndex(index + suffix.length());

    return result;
  }

  /**
   * Sets the <code>Currency</code> on the
   * <code>DecimalFormatSymbols</code> used, which also sets the
   * currency symbols on those symbols.
   */
  public void setCurrency(Currency currency)
  {
    symbols.setCurrency(currency);
  }

  public void setDecimalFormatSymbols (DecimalFormatSymbols newSymbols)
  {
    symbols = newSymbols;
  }

  public void setDecimalSeparatorAlwaysShown (boolean newValue)
  {
    decimalSeparatorAlwaysShown = newValue;
  }

  public void setGroupingSize (int groupSize)
  {
    groupingSize = (byte) groupSize;
  }

  public void setMaximumFractionDigits (int newValue)
  {
    super.setMaximumFractionDigits(Math.min(newValue, 340));
  }

  public void setMaximumIntegerDigits (int newValue)
  {
    super.setMaximumIntegerDigits(Math.min(newValue, 309));
  }

  public void setMinimumFractionDigits (int newValue)
  {
    super.setMinimumFractionDigits(Math.min(newValue, 340));
  }

  public void setMinimumIntegerDigits (int newValue)
  {
    super.setMinimumIntegerDigits(Math.min(newValue, 309));
  }

  public void setMultiplier (int newValue)
  {
    multiplier = newValue;
  }

  public void setNegativePrefix (String newValue)
  {
    negativePrefix = newValue;
  }

  public void setNegativeSuffix (String newValue)
  {
    negativeSuffix = newValue;
  }

  public void setPositivePrefix (String newValue)
  {
    positivePrefix = newValue;
  }

  public void setPositiveSuffix (String newValue)
  {
    positiveSuffix = newValue;
  }

  private void quoteFix(StringBuffer buf, String text, String patChars)
  {
    int len = text.length();
    for (int index = 0; index < len; ++index)
      {
	char c = text.charAt(index);
	if (patChars.indexOf(c) != -1)
	  {
	    buf.append('\'');
	    buf.append(c);
	    buf.append('\'');
	  }
	else
	  buf.append(c);
      }
  }

  private String computePattern(DecimalFormatSymbols syms)
  {
    StringBuffer mainPattern = new StringBuffer ();
    // We have to at least emit a zero for the minimum number of
    // digits.  Past that we need hash marks up to the grouping
    // separator (and one beyond).
    int total_digits = Math.max(minimumIntegerDigits,
				groupingUsed ? groupingSize + 1: groupingSize);
    for (int i = 0; i < total_digits - minimumIntegerDigits; ++i)
      mainPattern.append(syms.getDigit());
    for (int i = total_digits - minimumIntegerDigits; i < total_digits; ++i)
      mainPattern.append(syms.getZeroDigit());
    // Inserting the gropuing operator afterwards is easier.
    if (groupingUsed)
      mainPattern.insert(mainPattern.length() - groupingSize,
			 syms.getGroupingSeparator());
    // See if we need decimal info.
    if (minimumFractionDigits > 0 || maximumFractionDigits > 0
	|| decimalSeparatorAlwaysShown)
      mainPattern.append(syms.getDecimalSeparator());
    for (int i = 0; i < minimumFractionDigits; ++i)
      mainPattern.append(syms.getZeroDigit());
    for (int i = minimumFractionDigits; i < maximumFractionDigits; ++i)
      mainPattern.append(syms.getDigit());
    if (useExponentialNotation)
      {
	mainPattern.append(syms.getExponential());
	for (int i = 0; i < minExponentDigits; ++i)
	  mainPattern.append(syms.getZeroDigit());
	if (minExponentDigits == 0)
	  mainPattern.append(syms.getDigit());
      }

    String main = mainPattern.toString();
    String patChars = patternChars (syms);
    mainPattern.setLength(0);

    quoteFix (mainPattern, positivePrefix, patChars);
    mainPattern.append(main);
    quoteFix (mainPattern, positiveSuffix, patChars);

    if (negativePrefix != null)
      {
	quoteFix (mainPattern, negativePrefix, patChars);
	mainPattern.append(main);
	quoteFix (mainPattern, negativeSuffix, patChars);
      }

    return mainPattern.toString();
  }

  public String toLocalizedPattern ()
  {
    return computePattern (symbols);
  }

  public String toPattern ()
  {
    return computePattern (nonLocalizedSymbols);
  }

  private static final int MAXIMUM_INTEGER_DIGITS = 309; 

  // These names are fixed by the serialization spec.
  private boolean decimalSeparatorAlwaysShown;
  private byte groupingSize;
  private byte minExponentDigits;
  private int exponentRound;
  private int multiplier;
  private String negativePrefix;
  private String negativeSuffix;
  private String positivePrefix;
  private String positiveSuffix;
  private int[] negativePrefixRanges, positivePrefixRanges;
  private HashMap[] negativePrefixAttrs, positivePrefixAttrs;
  private int[] negativeSuffixRanges, positiveSuffixRanges;
  private HashMap[] negativeSuffixAttrs, positiveSuffixAttrs;
  private int serialVersionOnStream = 1;
  private DecimalFormatSymbols symbols;
  private boolean useExponentialNotation;
  private static final long serialVersionUID = 864413376551465018L;

  private void readObject(ObjectInputStream stream)
    throws IOException, ClassNotFoundException
  {
    stream.defaultReadObject();
    if (serialVersionOnStream < 1)
      {
        useExponentialNotation = false;
	serialVersionOnStream = 1;
      }
  }

  // The locale-independent pattern symbols happen to be the same as
  // the US symbols.
  private static final DecimalFormatSymbols nonLocalizedSymbols
    = new DecimalFormatSymbols (Locale.US);
}
