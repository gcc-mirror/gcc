// DecimalFormat.java - Localized number formatting.

/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.text;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date March 4, 1999
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 from http://www.javasoft.com.
 * Status:  Believed complete and correct to 1.2, except serialization.
 * Note however that the docs are very unclear about how format parsing
 * should work.  No doubt there are problems here.
 */

public class DecimalFormat extends NumberFormat
{
  // This is a helper for applyPatternWithSymbols.  It reads a prefix
  // or a suffix.  It can cause some side-effects.
  private final int scanFix (String pattern, int index, StringBuffer buf,
			     String patChars, DecimalFormatSymbols syms,
			     boolean is_suffix)
    {
      int len = pattern.length();
      buf.setLength(0);
      boolean multiplierSet = false;
      while (index < len)
	{
	  char c = pattern.charAt(index);
	  if (c == '\'' && index + 1 < len
	      && pattern.charAt(index + 1) == '\'')
	    {
	      buf.append(c);
	      ++index;
	    }
	  else if (c == '\'' && index + 2 < len
		   && pattern.charAt(index + 2) == '\'')
	    {
	      buf.append(pattern.charAt(index + 1));
	      index += 2;
	    }
	  else if (c == '\u00a4')
	    {
	      if (index + 1 < len && pattern.charAt(index + 1) == '\u00a4')
		{
		  buf.append(syms.getInternationalCurrencySymbol());
		  ++index;
		}
	      else
		buf.append(syms.getCurrencySymbol());
	    }
	  else if (is_suffix && c == syms.getPercent())
	    {
	      if (multiplierSet)
		throw new IllegalArgumentException ("multiplier already set " +
						    "- index: " + index);
	      multiplierSet = true;
	      multiplier = 100;
	      buf.append(c);
	    }
	  else if (is_suffix && c == syms.getPerMill())
	    {
	      if (multiplierSet)
		throw new IllegalArgumentException ("multiplier already set " +
						    "- index: " + index);
	      multiplierSet = true;
	      multiplier = 1000;
	      buf.append(c);
	    }
	  else if (patChars.indexOf(c) != -1)
	    {
	      // This is a pattern character.
	      break;
	    }
	  else
	    buf.append(c);
	  ++index;
	}

      return index;
    }

  // A helper which reads a number format.
  private final int scanFormat (String pattern, int index,
				String patChars, DecimalFormatSymbols syms,
				boolean is_positive)
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
	}

      return index;
    }

  // This helper function creates a string consisting of all the
  // characters which can appear in a pattern and must be quoted.
  private final String patternChars (DecimalFormatSymbols syms)
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

  private final void applyPatternWithSymbols (String pattern,
					      DecimalFormatSymbols syms)
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
      maximumIntegerDigits = 309;
      minimumFractionDigits = 0;
      minimumIntegerDigits = 1;

      StringBuffer buf = new StringBuffer ();
      String patChars = patternChars (syms);

      int max = pattern.length();
      int index = scanFix (pattern, 0, buf, patChars, syms, false);
      positivePrefix = buf.toString();

      index = scanFormat (pattern, index, patChars, syms, true);

      index = scanFix (pattern, index, buf, patChars, syms, true);
      positiveSuffix = buf.toString();

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
	  negativePrefix = buf.toString();

	  // We parse the negative format for errors but we don't let
	  // it side-effect this object.
	  index = scanFormat (pattern, index, patChars, syms, false);

	  index = scanFix (pattern, index, buf, patChars, syms, true);
	  negativeSuffix = buf.toString();

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
      return new DecimalFormat (this);
    }

  private DecimalFormat (DecimalFormat dup)
    {
      decimalSeparatorAlwaysShown = dup.decimalSeparatorAlwaysShown;
      groupingSize = dup.groupingSize;
      minExponentDigits = dup.minExponentDigits;
      multiplier = dup.multiplier;
      negativePrefix = dup.negativePrefix;
      negativeSuffix = dup.negativeSuffix;
      positivePrefix = dup.positivePrefix;
      positiveSuffix = dup.positiveSuffix;
      symbols = (DecimalFormatSymbols) dup.symbols.clone();
      useExponentialNotation = dup.useExponentialNotation;
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

  private final boolean equals (String s1, String s2)
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

  public StringBuffer format (double number, StringBuffer dest,
			      FieldPosition fieldPos)
    {
      // A very special case.
      if (Double.isNaN(number))
	{
	  dest.append(symbols.getNaN());
	  if (fieldPos != null && fieldPos.getField() == INTEGER_FIELD)
	    {
	      int index = dest.length();
	      fieldPos.setBeginIndex(index - symbols.getNaN().length());
	      fieldPos.setEndIndex(index);
	    }
	  return dest;
	}

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
      int integerEndIndex = 0;
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
	      exponent = (long) (Math.log(number) / Math.log(10));
	      if (minimumIntegerDigits > 0)
		exponent -= minimumIntegerDigits - 1;
	      baseNumber = (long) (number / Math.pow(10.0, exponent));
	    }
	  else
	    baseNumber = number;

	  // Round to the correct number of digits.
	  baseNumber += 5 * Math.pow(10.0, - maximumFractionDigits - 1);

	  int index = dest.length();
	  double intPart = Math.floor(baseNumber);
	  int count = 0;
	  while (count < maximumIntegerDigits
		 && (intPart > 0 || count < minimumIntegerDigits))
	    {
	      long dig = (long) (intPart % 10);
	      intPart = Math.floor(intPart / 10);

	      // Append group separator if required.
	      if (groupingUsed && count > 0 && count % groupingSize == 0)
		dest.insert(index, symbols.getGroupingSeparator());

	      dest.insert(index, (char) (symbols.getZeroDigit() + dig));

	      ++count;
	    }

	  integerEndIndex = dest.length();

	  int decimal_index = integerEndIndex;
	  int consecutive_zeros = 0;
	  int total_digits = 0;

	  // Strip integer part from NUMBER.
	  double fracPart = baseNumber - Math.floor(baseNumber);
	  for (count = 0;
	       count < maximumFractionDigits
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
	      dest.setLength(dest.length() - extra_zeros);
	      total_digits -= extra_zeros;
	    }

	  // If required, add the decimal symbol.
	  if (decimalSeparatorAlwaysShown
	      || total_digits > 0)
	    {
	      dest.insert(decimal_index, symbols.getDecimalSeparator());
	      if (fieldPos != null && fieldPos.getField() == FRACTION_FIELD)
		{
		  fieldPos.setBeginIndex(decimal_index + 1);
		  fieldPos.setEndIndex(dest.length());
		}
	    }

	  // Finally, print the exponent.
	  if (useExponentialNotation)
	    {
	      dest.append(symbols.getExponential());
	      dest.append(exponent < 0 ? '-' : '+');
	      index = dest.length();
	      for (count = 0;
		   exponent > 0 || count < minExponentDigits;
		   ++count)
		{
		  long dig = exponent % 10;
		  exponent /= 10;
		  dest.insert(index, (char) (symbols.getZeroDigit() + dig));
		}
	    }
	}

      if (fieldPos != null && fieldPos.getField() == INTEGER_FIELD)
	{
	  fieldPos.setBeginIndex(integerBeginIndex);
	  fieldPos.setEndIndex(integerEndIndex);
	}

      dest.append((is_neg && negativeSuffix != null)
		  ? negativeSuffix
		  : positiveSuffix);
      return dest;
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
	  if (groupingUsed && count > 0 && count % groupingSize == 0)
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
      // Our strategy is simple: copy the text into a buffer,
      // translating or omitting locale-specific information.  Then
      // let Double or Long convert the number for us.

      boolean is_neg = false;
      int index = pos.getIndex();
      StringBuffer buf = new StringBuffer ();

      // We have to check both prefixes, because one might be empty.
      // We want to pick the longest prefix that matches.
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

      // FIXME: do we have to respect minimum/maxmimum digit stuff?
      // What about leading zeros?  What about multiplier?

      int start_index = index;
      int max = str.length();
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
		  && (index - last_group) % groupingSize != 0)
		{
		  pos.setErrorIndex(index);
		  return null;
		}
	      last_group = index;
	    }
	  else if (c >= zero && c <= zero + 9)
	    {
	      buf.append((char) (c - zero + '0'));
	      exp_part = false;
	    }
	  else if (parseIntegerOnly)
	    break;
	  else if (c == symbols.getDecimalSeparator())
	    {
	      if (last_group != -1
		  && (index - last_group) % groupingSize != 0)
		{
		  pos.setErrorIndex(index);
		  return null;
		}
	      buf.append('.');
	      int_part = false;
	    }
	  else if (c == symbols.getExponential())
	    {
	      buf.append('E');
	      int_part = false;
	      exp_part = true;
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
      if (is_neg)
	buf.insert(0, '-');

      String t = buf.toString();
      Number result = null;
      try
	{
	  result = new Long (t);
	}
      catch (NumberFormatException x1)
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
      maximumFractionDigits = Math.min(newValue, 340);
    }

  public void setMaximumIntegerDigits (int newValue)
    {
      maximumIntegerDigits = Math.min(newValue, 309);
    }

  public void setMinimumFractionDigits (int newValue)
    {
      minimumFractionDigits = Math.min(newValue, 340);
    }

  public void setMinimumIntegerDigits (int newValue)
    {
      minimumIntegerDigits = Math.min(newValue, 309);
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

  private final void quoteFix (StringBuffer buf, String text, String patChars)
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

  private final String computePattern (DecimalFormatSymbols syms)
    {
      StringBuffer mainPattern = new StringBuffer ();
      // We have to at least emit a zero for the minimum number of
      // digits.  Past that we need hash marks up to the grouping
      // separator (and one beyond).
      int total_digits = Math.max(minimumIntegerDigits,
				  groupingUsed ? groupingSize + 1: 0);
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

  // These names are fixed by the serialization spec.
  private boolean decimalSeparatorAlwaysShown;
  private byte groupingSize;
  private byte minExponentDigits;
  private int multiplier;
  private String negativePrefix;
  private String negativeSuffix;
  private String positivePrefix;
  private String positiveSuffix;
  private DecimalFormatSymbols symbols;
  private boolean useExponentialNotation;

  // The locale-independent pattern symbols happen to be the same as
  // the US symbols.
  private static final DecimalFormatSymbols nonLocalizedSymbols
    = new DecimalFormatSymbols (Locale.US);
}
