// ChoiceFormat.java - Formatter for `switch'-like string substitution.

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.text;

import java.util.Vector;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date March 9, 1999
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 from http://www.javasoft.com.
 * Status:  Believed complete and correct to 1.1.
 */

public class ChoiceFormat extends NumberFormat
{
  // Note: we assume the same kind of quoting rules apply here.
  // This isn't explicitly documented.  But for instance we accept
  // '#' as a literal hash in a format string.
  public void applyPattern (String newPattern)
    {
      int index = 0, max = newPattern.length();
      Vector stringVec = new Vector ();
      Vector limitVec = new Vector ();
      StringBuffer buf = new StringBuffer ();

      while (true)
	{
	  // Find end of double.
	  int dstart = index;
	  while (index < max)
	    {
	      char c = newPattern.charAt(index);
	      if (c == '#' || c == '\u2064' || c == '<')
		break;
	      ++index;
	    }

	  if (index == max)
	    throw new IllegalArgumentException ("unexpected end of text");
	  Double d = new Double (newPattern.substring(dstart, index));

	  if (newPattern.charAt(index) == '<')
	    d = new Double (nextDouble (d.doubleValue()));

	  limitVec.addElement(d);

	  // Scan text.
	  ++index;
	  buf.setLength(0);
	  while (index < max)
	    {
	      char c = newPattern.charAt(index);
	      if (c == '\'' && index < max + 1
		  && newPattern.charAt(index + 1) == '\'')
		{
		  buf.append(c);
		  ++index;
		}
	      else if (c == '\'' && index < max + 2)
		{
		  buf.append(newPattern.charAt(index + 1));
		  index += 2;
		}
	      else if (c == '|')
		break;
	      else
		buf.append(c);
	      ++index;
	    }

	  stringVec.addElement(buf.toString());
	  if (index == max)
	    break;
	  ++index;
	}

      strings = new String[stringVec.size()];
      stringVec.copyInto(strings);

      limits = new double[limitVec.size()];
      for (int i = 0; i < limits.length; ++i)
	{
	  Double d = (Double) limitVec.elementAt(i);
	  limits[i] = d.doubleValue();
	}
    }

  public ChoiceFormat (String newPattern)
    {
      super ();
      applyPattern (newPattern);
    }

  public ChoiceFormat (double[] limits, String[] strings)
    {
      super ();
      setChoices (limits, strings);
    }

  public Object clone ()
    {
      return new ChoiceFormat (limits, strings);
    }

  public boolean equals (Object obj)
    {
      if (! (obj instanceof ChoiceFormat))
	return false;
      ChoiceFormat cf = (ChoiceFormat) obj;
      if (limits.length != cf.limits.length)
	return false;
      for (int i = limits.length - 1; i >= 0; --i)
	{
	  if (limits[i] != cf.limits[i]
	      || !strings[i].equals(cf.strings[i]))
	    return false;
	}
      return true;
    }

  public StringBuffer format (long num, StringBuffer appendBuf,
			      FieldPosition pos)
    {
      return format ((double) num, appendBuf, pos);
    }

  public StringBuffer format (double num, StringBuffer appendBuf,
			      FieldPosition pos)
    {
      if (limits.length == 0)
	return appendBuf;

      int index =  0;
      if (! Double.isNaN(num) && num >= limits[0])
	{
	  for (; index < limits.length - 1; ++index)
	    {
	      if (limits[index] <= num
		  && index != limits.length - 2
		  && num < limits[index + 1])
		break;
	    }
	}

      return appendBuf.append(strings[index]);
    }

  public Object[] getFormats ()
    {
      return (Object[]) strings.clone();
    }

  public double[] getLimits ()
    {
      return (double[]) limits.clone();
    }

  public int hashCode ()
    {
      int hash = 0;
      for (int i = 0; i < limits.length; ++i)
	{
	  long v = Double.doubleToLongBits(limits[i]);
	  hash ^= (v ^ (v >>> 32));
	  hash ^= strings[i].hashCode();
	}
      return hash;
    }

  public static final double nextDouble (double d)
    {
      return nextDouble (d, true);
    }

  public static double nextDouble (double d, boolean next)
    {
      if (Double.isInfinite(d) || Double.isNaN(d))
	return d;

      long bits = Double.doubleToLongBits(d);

      long mantMask = (1L << mantissaBits) - 1;
      long mantissa = bits & mantMask;

      long expMask = (1L << exponentBits) - 1;
      long exponent = (bits >>> mantissaBits) & expMask;

      if (next ^ (bits < 0)) // Increment magnitude
	{
	  if (mantissa == (1L << mantissaBits) - 1)
	    {
	      mantissa = 0L;
	      exponent++;
	     
	      // Check for absolute overflow.
	      if (exponent >= (1L << mantissaBits))
		return (bits > 0) ? Double.POSITIVE_INFINITY 
		  : Double.NEGATIVE_INFINITY;		      
	    }
	  else
	    mantissa++;
	}
      else // Decrement magnitude
	{
	  if (exponent == 0L && mantissa == 0L)
	    {
	      // The only case where there is a change of sign
	      return next ? Double.MIN_VALUE : -Double.MIN_VALUE;
	    }
	  else
	    {
	      if (mantissa == 0L)
		{
		  mantissa = (1L << mantissaBits) - 1;
		  exponent--;
		}
	      else
		mantissa--;
	    }
	}

      long result = bits < 0 ? 1 : 0;
      result = (result << exponentBits) | exponent;
      result = (result << mantissaBits) | mantissa;
      return Double.longBitsToDouble(result);
    }

  public Number parse (String sourceStr, ParsePosition pos)
    {
      int index = pos.getIndex();
      for (int i = 0; i < limits.length; ++i)
	{
	  if (sourceStr.startsWith(strings[i], index))
	    {
	      pos.setIndex(index + strings[i].length());
	      return new Double (limits[i]);
	    }
	}
      pos.setErrorIndex(index);
      return new Double (Double.NaN);
    }

  public static final double previousDouble (double d)
    {
      return nextDouble (d, false);
    }

  public void setChoices (double[] limits, String[] strings)
    {
      if (limits == null || strings == null)
	throw new NullPointerException ();
      if (limits.length != strings.length)
	throw new IllegalArgumentException ();
      this.strings = (String[]) strings.clone();
      this.limits = (double[]) limits.clone();
    }

  private final void quoteString (StringBuffer dest, String text)
    {
      int max = text.length();
      for (int i = 0; i < max; ++i)
	{
	  char c = text.charAt(i);
	  if (c == '\'')
	    {
	      dest.append(c);
	      dest.append(c);
	    }
	  else if (c == '#' || c == '|' || c == '\u2064' || c == '<')
	    {
	      dest.append('\'');
	      dest.append(c);
	      dest.append('\'');
	    }
	  else
	    dest.append(c);
	}
    }

  public String toPattern ()
    {
      StringBuffer result = new StringBuffer ();
      for (int i = 0; i < limits.length; ++i)
	{
	  result.append(limits[i]);
	  result.append('#');
	  quoteString (result, strings[i]);
	}
      return result.toString();
    }

  // Formats and limits.
  private String[] strings;
  private double[] limits;

  // Number of mantissa bits in double.
  private static final int mantissaBits = 52;
  // Number of exponent bits in a double.
  private static final int exponentBits = 11;
}
