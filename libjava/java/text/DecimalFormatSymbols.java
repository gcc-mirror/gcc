// DecimalFormatSymbols.java - Symbols used to format numbers.

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.text;

import java.io.Serializable;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date February 24, 1999
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 from http://www.javasoft.com.
 * Status:  Believed complete and correct to 1.2, except serialization.
 */

public final class DecimalFormatSymbols implements Cloneable, Serializable
{
  public Object clone ()
    {
      return new DecimalFormatSymbols (this);
    }

  private DecimalFormatSymbols (DecimalFormatSymbols orig)
    {
      this.currencySymbol = orig.currencySymbol;
      this.decimalSeparator = orig.decimalSeparator;
      this.digit = orig.digit;
      this.exponential = orig.exponential;
      this.groupingSeparator = orig.groupingSeparator;
      this.infinity = orig.infinity;
      this.intlCurrencySymbol = orig.intlCurrencySymbol;
      this.minusSign = orig.minusSign;
      this.NaN = orig.NaN;
      this.patternSeparator = orig.patternSeparator;
      this.percent = orig.percent;
      this.perMill = orig.perMill;
      this.zeroDigit = orig.zeroDigit;
    }

  public DecimalFormatSymbols ()
    {
      this (Locale.getDefault());
    }

  private final String safeGetString (ResourceBundle bundle,
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

  public final char safeGetChar (ResourceBundle bundle,
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

  public DecimalFormatSymbols (Locale loc)
    {
      ResourceBundle res;
      try
	{
	  res = ResourceBundle.getBundle("gnu.gcj.text.LocaleData", loc);
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
      minusSign = safeGetChar (res, "minusSign", '-');
      NaN = safeGetString (res, "NaN", "\ufffd");
      patternSeparator = safeGetChar (res, "patternSeparator", ';');
      percent = safeGetChar (res, "percent", '%');
      perMill = safeGetChar (res, "perMill", '\u2030');
      zeroDigit = safeGetChar (res, "zeroDigit", '0');
    }

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
	      && NaN.equals(dfs.NaN)
	      && patternSeparator == dfs.patternSeparator
	      && percent == dfs.percent
	      && perMill == dfs.perMill
	      && zeroDigit == dfs.zeroDigit);
    }

  public String getCurrencySymbol ()
    {
      return currencySymbol;
    }

  public char getDecimalSeparator ()
    {
      return decimalSeparator;
    }

  public char getDigit ()
    {
      return digit;
    }

  // This is our own extension.
  char getExponential ()
    {
      return exponential;
    }

  public char getGroupingSeparator ()
    {
      return groupingSeparator;
    }

  public String getInfinity ()
    {
      return infinity;
    }

  public String getInternationalCurrencySymbol ()
    {
      return intlCurrencySymbol;
    }

  public char getMinusSign ()
    {
      return minusSign;
    }

  public String getNaN ()
    {
      return NaN;
    }

  public char getPatternSeparator ()
    {
      return patternSeparator;
    }

  public char getPercent ()
    {
      return percent;
    }

  public char getPerMill ()
    {
      return perMill;
    }

  public char getZeroDigit ()
    {
      return zeroDigit;
    }

  public int hashCode ()
    {
      // Compute based on zero digit, grouping separator, and decimal
      // separator -- JCL book.  This probably isn't a very good hash
      // code.
      return zeroDigit << 16 + groupingSeparator << 8 + decimalSeparator;
    }

  public void setCurrenySymbol (String currency)
    {
      currencySymbol = currency;
    }

  public void setDecimalSeparator (char decimalSep)
    {
      decimalSeparator = decimalSep;
    }

  public void setDigit (char digit)
    {
      this.digit = digit;
    }

  // This is our own extension.
  void setExponential (char exp)
    {
      exponential = exp;
    }

  public void setGroupingSeparator (char groupSep)
    {
      groupingSeparator = groupSep;
    }

  public void setInfinity (String infinity)
    {
      this.infinity = infinity;
    }

  public void setInternationalCurrencySymbol (String currency)
    {
      intlCurrencySymbol = currency;
    }

  public void setMinusSign (char minusSign)
    {
      this.minusSign = minusSign;
    }

  public void setNaN (String nan)
    {
      NaN = nan;
    }

  public void setPatternSeparator (char patternSep)
    {
      patternSeparator = patternSep;
    }

  public void setPercent (char percent)
    {
      this.percent = percent;
    }

  public void setPerMill (char perMill)
    {
      this.perMill = perMill;
    }

  public void setZeroDigit (char zeroDigit)
    {
      this.zeroDigit = zeroDigit;
    }

  // The names of the instance variables are fixed by the
  // serialization spec.
  private String currencySymbol;
  private char decimalSeparator;
  private char digit;
  private char exponential;
  private char groupingSeparator;
  private String infinity;
  private String intlCurrencySymbol;
  private char minusSign;
  private String NaN;
  private char patternSeparator;
  private char percent;
  private char perMill;
  private char zeroDigit;
}
