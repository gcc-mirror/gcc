// MessageFormat.java - Localized message formatting.

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.text;

import java.util.Date;
import java.util.Locale;
import java.util.Vector;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date March 3, 1999
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 from http://www.javasoft.com.
 * Status:  Believed complete and correct to 1.2, except serialization.
 *          and parsing.
 */

final class MessageFormatElement
{
  // Argument number.
  int argNumber;
  // Formatter to be used.  This is the format set by setFormat.
  Format setFormat;
  // Formatter to be used based on the type.
  Format format;

  // Argument will be checked to make sure it is an instance of this
  // class.
  Class formatClass;

  // Formatter type.
  String type;
  // Formatter style.
  String style;

  // Text to follow this element.
  String trailer;

  // FIXME: shouldn't need this.
  Class forName (String name)
    {
      try
	{
	  return Class.forName (name);
	}
      catch (ClassNotFoundException x)
	{
	}
      return null;
    }

  // Recompute the locale-based formatter.
  void setLocale (Locale loc)
    {
      if (type == null)
	;
      else if (type.equals("number"))
	{
	  // FIXME: named class literal.
	  // formatClass = Number.class;
	  formatClass = forName ("java.lang.Number");

	  if (style == null)
	    format = NumberFormat.getInstance(loc);
	  else if (style.equals("currency"))
	    format = NumberFormat.getCurrencyInstance(loc);
	  else if (style.equals("percent"))
	    format = NumberFormat.getPercentInstance(loc);
	  else if (style.equals("integer"))
	    {
	      NumberFormat nf = NumberFormat.getNumberInstance(loc);
	      nf.setMaximumFractionDigits(0);
	      nf.setGroupingUsed(false);
	      format = nf;
	    }
	  else
	    {
	      format = NumberFormat.getNumberInstance(loc);
	      DecimalFormat df = (DecimalFormat) format;
	      try
		{
		  df.applyPattern(style);
		}
	      catch (ParseException x)
		{
		  throw new IllegalArgumentException (x.getMessage());
		}
	    }
	}
      else if (type.equals("time") || type.equals("date"))
	{
	  // FIXME: named class literal.
	  // formatClass = Date.class;
	  formatClass = forName ("java.util.Date");

	  int val = DateFormat.DEFAULT;
	  if (style == null)
	    ;
	  if (style.equals("short"))
	    val = DateFormat.SHORT;
	  else if (style.equals("medium"))
	    val = DateFormat.MEDIUM;
	  else if (style.equals("long"))
	    val = DateFormat.LONG;
	  else if (style.equals("full"))
	    val = DateFormat.FULL;

	  if (type.equals("time"))
	    format = DateFormat.getTimeInstance(val, loc);
	  else
	    format = DateFormat.getDateInstance(val, loc);

	  if (style != null && val == DateFormat.DEFAULT)
	    {
	      SimpleDateFormat sdf = (SimpleDateFormat) format;
	      sdf.applyPattern(style);
	    }
	}
      else if (type.equals("choice"))
	{
	  // FIXME: named class literal.
	  // formatClass = Number.class;
	  formatClass = forName ("java.lang.Number");

	  if (style == null)
	    throw new
	      IllegalArgumentException ("style required for choice format");
	  format = new ChoiceFormat (style);
	}
    }
}

public class MessageFormat extends Format
{
  // Helper that returns the text up to the next format opener.  The
  // text is put into BUFFER.  Returns index of character after end of
  // string.  Throws IllegalArgumentException on error.
  private static final int scanString (String pat, int index,
				       StringBuffer buffer)
    {
      int max = pat.length();
      buffer.setLength(0);
      for (; index < max; ++index)
	{
	  char c = pat.charAt(index);
	  if (c == '\'' && index + 2 < max && pat.charAt(index + 2) == '\'')
	    {
	      buffer.append(pat.charAt(index + 1));
	      index += 2;
	    }
	  else if (c == '\'' && index + 1 < max
		   && pat.charAt(index + 1) == '\'')
	    {
	      buffer.append(c);
	      ++index;
	    }
	  else if (c == '{')
	    break;
	  else if (c == '}')
	    throw new IllegalArgumentException ();
	  else
	    buffer.append(c);
	}
      return index;
    }

  // This helper retrieves a single part of a format element.  Returns
  // the index of the terminating character.
  private static final int scanFormatElement (String pat, int index,
					      StringBuffer buffer,
					      char term)
    {
      int max = pat.length();
      buffer.setLength(0);
      int brace_depth = 1;

      for (; index < max; ++index)
	{
	  char c = pat.charAt(index);
	  if (c == '\'' && index + 2 < max && pat.charAt(index + 2) == '\'')
	    {
	      buffer.append(c);
	      buffer.append(pat.charAt(index + 1));
	      buffer.append(c);
	      index += 2;
	    }
	  else if (c == '\'' && index + 1 < max
		   && pat.charAt(index + 1) == '\'')
	    {
	      buffer.append(c);
	      ++index;
	    }
	  else if (c == '{')
	    {
	      buffer.append(c);
	      ++brace_depth;
	    }
	  else if (c == '}')
	    {
	      if (--brace_depth == 0)
		break;
	      buffer.append(c);
	    }
	  // Check for TERM after braces, because TERM might be `}'.
	  else if (c == term)
	    break;
	  else
	    buffer.append(c);
	}
      return index;
    }

  // This is used to parse a format element and whatever non-format
  // text might trail it.
  private static final int scanFormat (String pat, int index,
				       StringBuffer buffer, Vector elts,
				       Locale locale)
    {
      MessageFormatElement mfe = new MessageFormatElement ();
      elts.addElement(mfe);

      int max = pat.length();

      // Skip the opening `{'.
      ++index;

      // Fetch the argument number.
      index = scanFormatElement (pat, index, buffer, ',');
      try
	{
	  mfe.argNumber = Integer.parseInt(buffer.toString());
	}
      catch (NumberFormatException nfx)
	{
	  throw new IllegalArgumentException ();
	}

      // Extract the element format.
      if (index < max && pat.charAt(index) == ',')
	{
	  index = scanFormatElement (pat, index + 1, buffer, ',');
	  mfe.type = buffer.toString();

	  // Extract the style.
	  if (index < max && pat.charAt(index) == ',')
	    {
	      index = scanFormatElement (pat, index + 1, buffer, '}');
	      mfe.style = buffer.toString ();
	    }
	}

      // Advance past the last terminator.
      if (index >= max || pat.charAt(index) != '}')
	throw new IllegalArgumentException ();
      ++index;

      // Now fetch trailing string.
      index = scanString (pat, index, buffer);
      mfe.trailer = buffer.toString ();

      mfe.setLocale(locale);

      return index;
    }

  public void applyPattern (String newPattern)
    {
      pattern = newPattern;

      StringBuffer tempBuffer = new StringBuffer ();

      int index = scanString (newPattern, 0, tempBuffer);
      leader = tempBuffer.toString();

      Vector elts = new Vector ();
      while (index < newPattern.length())
	index = scanFormat (newPattern, index, tempBuffer, elts, locale);

      elements = new MessageFormatElement[elts.size()];
      elts.copyInto(elements);
    }

  public Object clone ()
    {
      MessageFormat c = new MessageFormat ();
      c.setLocale(locale);
      c.applyPattern(pattern);
      return (Object) c;
    }

  public boolean equals (Object obj)
    {
      if (! (obj instanceof MessageFormat))
	return false;
      MessageFormat mf = (MessageFormat) obj;
      return (pattern.equals(mf.pattern)
	      && locale.equals(mf.locale));
    }

  public static String format (String pattern, Object arguments[])
    {
      MessageFormat mf = new MessageFormat (pattern);
      StringBuffer sb = new StringBuffer ();
      FieldPosition fp = new FieldPosition (NumberFormat.INTEGER_FIELD);
      return mf.format(arguments, sb, fp).toString();
    }

  public final StringBuffer format (Object arguments[], StringBuffer appendBuf,
				    FieldPosition ignore)
    {
      appendBuf.append(leader);

      for (int i = 0; i < elements.length; ++i)
	{
	  if (elements[i].argNumber >= arguments.length)
	    throw new IllegalArgumentException ();
	  Object thisArg = arguments[elements[i].argNumber];

	  Format formatter = null;
	  if (elements[i].setFormat != null)
	    formatter = elements[i].setFormat;
	  else if (elements[i].format != null)
	    {
	      if (elements[i].formatClass != null
		  && ! elements[i].formatClass.isInstance(thisArg))
		throw new IllegalArgumentException ();
	      formatter = elements[i].format;
	    }
	  else if (thisArg instanceof Number)
	    formatter = NumberFormat.getInstance(locale);
	  else if (thisArg instanceof Date)
	    formatter = DateFormat.getTimeInstance(DateFormat.DEFAULT, locale);
	  else
	    appendBuf.append(thisArg);

	  if (formatter != null)
	    {
	      // Special-case ChoiceFormat.
	      if (formatter instanceof ChoiceFormat)
		{
		  StringBuffer buf = new StringBuffer ();
		  // FIXME: don't actually know what is correct here.
		  // Can a sub-format refer to any argument, or just
		  // the single argument passed to it?  Must test
		  // against JDK.
		  formatter.format(thisArg, buf, ignore);
		  MessageFormat mf = new MessageFormat ();
		  mf.setLocale(locale);
		  mf.applyPattern(buf.toString());
		  formatter = mf;
		}
	      formatter.format(thisArg, appendBuf, ignore);
	    }

	  appendBuf.append(elements[i].trailer);
	}

      return appendBuf;
    }

  public final StringBuffer format (Object singleArg, StringBuffer appendBuf,
				    FieldPosition ignore)
    {
      Object[] args = new Object[1];
      args[0] = singleArg;
      return format (args, appendBuf, ignore);
    }

  public Format[] getFormats ()
    {
      Format[] f = new Format[elements.length];
      for (int i = elements.length - 1; i >= 0; --i)
	f[i] = elements[i].setFormat;
      return f;
    }

  public Locale getLocale ()
    {
      return locale;
    }

  public int hashCode ()
    {
      // FIXME: not a very good hash.
      return pattern.hashCode() + locale.hashCode();
    }

  private MessageFormat ()
    {
    }

  public MessageFormat (String pattern)
    {
      locale = Locale.getDefault();
      applyPattern (pattern);
    }

  public Object[] parse (String sourceStr, ParsePosition pos)
    {
      // Check initial text.
      int index = pos.getIndex();
      if (! sourceStr.startsWith(leader, index))
	{
	  pos.setErrorIndex(index);
	  return null;
	}
      index += leader.length();

      Vector results = new Vector (elements.length, 1);
      // Now check each format.
      for (int i = 0; i < elements.length; ++i)
	{
	  Format formatter = null;
	  if (elements[i].setFormat != null)
	    formatter = elements[i].setFormat;
	  else if (elements[i].format != null)
	    formatter = elements[i].format;

	  Object value = null;
	  if (formatter instanceof ChoiceFormat)
	    {
	      // We must special-case a ChoiceFormat because it might
	      // have recursive formatting.
	      ChoiceFormat cf = (ChoiceFormat) formatter;
	      String[] formats = (String[]) cf.getFormats();
	      double[] limits = (double[]) cf.getLimits();
	      MessageFormat subfmt = new MessageFormat ();
	      subfmt.setLocale(locale);
	      ParsePosition subpos = new ParsePosition (index);

	      int j;
	      for (j = 0; value == null && j < limits.length; ++j)
		{
		  subfmt.applyPattern(formats[j]);
		  subpos.setIndex(index);
		  value = subfmt.parse(sourceStr, subpos);
		}
	      if (value != null)
		{
		  index = subpos.getIndex();
		  value = new Double (limits[j]);
		}
	    }
	  else if (formatter != null)
	    {
	      pos.setIndex(index);
	      value = formatter.parseObject(sourceStr, pos);
	      if (value != null)
		index = pos.getIndex();
	    }
	  else
	    {
	      // We have a String format.  This can lose in a number
	      // of ways, but we give it a shot.
	      int next_index = sourceStr.indexOf(elements[i].trailer, index);
	      if (next_index == -1)
		{
		  pos.setErrorIndex(index);
		  return null;
		}
	      value = sourceStr.substring(index, next_index);
	      index = next_index;
	    }

	  if (value == null
	      || ! sourceStr.startsWith(elements[i].trailer, index))
	    {
	      pos.setErrorIndex(index);
	      return null;
	    }

	  if (elements[i].argNumber >= results.size())
	    results.setSize(elements[i].argNumber + 1);
	  results.setElementAt(value, elements[i].argNumber);

	  index += elements[i].trailer.length();
	}

      Object[] r = new Object[results.size()];
      results.copyInto(r);
      return r;
    }

  public Object[] parse (String sourceStr) throws ParseException
    {
      ParsePosition pp = new ParsePosition (0);
      Object[] r = parse (sourceStr, pp);
      if (r == null)
	throw new ParseException ("couldn't parse string", pp.getErrorIndex());
      return r;
    }

  public Object parseObject (String sourceStr, ParsePosition pos)
    {
      return parse (sourceStr, pos);
    }

  public void setFormat (int variableNum, Format newFormat)
    {
      elements[variableNum].setFormat = newFormat;
    }

  public void setFormats (Format[] newFormats)
    {
      if (newFormats.length < elements.length)
	throw new IllegalArgumentException ();
      int len = Math.min(newFormats.length, elements.length);
      for (int i = 0; i < len; ++i)
	elements[i].setFormat = newFormats[i];
    }

  public void setLocale (Locale loc)
    {
      locale = loc;
      if (elements != null)
	{
	  for (int i = 0; i < elements.length; ++i)
	    elements[i].setLocale(loc);
	}
    }

  public String toPattern ()
    {
      return pattern;
    }

  // The pattern string.
  private String pattern;
  // The locale.
  private Locale locale;
  // Variables.
  private MessageFormatElement[] elements;
  // Leader text.
  private String leader;
}
