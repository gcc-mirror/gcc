/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.text;

import java.util.Locale;
import java.util.ResourceBundle;
import java.util.MissingResourceException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.IOException;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date March 4, 1999
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 from http://www.javasoft.com.
 * Status:  Believed complete and correct to 1.2, except getAvailableLocales.
 */

public abstract class NumberFormat extends Format implements Cloneable
{
  public static final int INTEGER_FIELD = 0;
  public static final int FRACTION_FIELD = 1;

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

  public abstract StringBuffer format (double number,
				       StringBuffer sbuf, FieldPosition pos);

  public abstract StringBuffer format (long number,
				       StringBuffer sbuf, FieldPosition pos);

  public Object clone ()
  {
    // We know the superclass just uses Object's generic cloner.
    // Why not just inherit?  Because the online docs specify that
    // this method exists for this class.
    return super.clone ();
  }

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

  public static Locale[] getAvailableLocales ()
    {
      // FIXME.
      return null;
    }

  private static final NumberFormat computeInstance (Locale loc,
						     String resource,
						     String def)
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

  public static final NumberFormat getCurrencyInstance ()
    {
      return getCurrencyInstance (Locale.getDefault());
    }

  public static NumberFormat getCurrencyInstance (Locale loc)
    {
      return computeInstance (loc, "currencyFormat", "$#,##0.00;($#,##0.00)");
    }

  public static final NumberFormat getInstance ()
    {
      return getInstance (Locale.getDefault());
    }

  public static NumberFormat getInstance (Locale loc)
    {
      // For now always return a number instance.
      return getNumberInstance (loc);
    }

  public int getMaximumFractionDigits ()
    {
      return maximumFractionDigits;
    }

  public int getMaximumIntegerDigits ()
    {
      return maximumIntegerDigits;
    }

  public int getMinimumFractionDigits ()
    {
      return minimumFractionDigits;
    }

  public int getMinimumIntegerDigits ()
    {
      return minimumIntegerDigits;
    }

  public static final NumberFormat getNumberInstance ()
    {
      return getNumberInstance (Locale.getDefault());
    }

  public static NumberFormat getNumberInstance (Locale loc)
    {
      return computeInstance (loc, "numberFormat", "#,##0.###");
    }

  public static final NumberFormat getPercentInstance ()
    {
      return getPercentInstance (Locale.getDefault());
    }

  public static NumberFormat getPercentInstance (Locale loc)
    {
      return computeInstance (loc, "percentFormat", "#,##0%");
    }

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

  public boolean isGroupingUsed ()
    {
      return groupingUsed;
    }

  public boolean isParseIntegerOnly ()
    {
      return parseIntegerOnly;
    }

  public NumberFormat ()
    {
    }

  public abstract Number parse (String sourceStr, ParsePosition pos);

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

  public final Object parseObject (String sourceStr, ParsePosition pos)
    {
      return parse (sourceStr, pos);
    }

  public void setGroupingUsed (boolean newValue)
    {
      groupingUsed = newValue;
    }

  public void setMaximumFractionDigits (int newValue)
    {
      maximumFractionDigits = newValue;
    }

  public void setMaximumIntegerDigits (int newValue)
    {
      maximumIntegerDigits = newValue;
    }

  public void setMinimumFractionDigits (int newValue)
    {
      minimumFractionDigits = newValue;
    }

  public void setMinimumIntegerDigits (int newValue)
    {
      minimumIntegerDigits = newValue;
    }

  public void setParseIntegerOnly (boolean value)
    {
      parseIntegerOnly = value;
    }

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
