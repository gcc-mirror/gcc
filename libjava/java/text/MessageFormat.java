/* MessageFormat.java - Localized message formatting.
   Copyright (C) 1999, 2001, 2002 Free Software Foundation, Inc.

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

import java.util.Date;
import java.util.Locale;
import java.util.Vector;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @author Jorge Aliss <jaliss@hotmail.com>
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

  // Recompute the locale-based formatter.
  void setLocale (Locale loc)
  {
    if (type == null)
      ;
    else if (type.equals("number"))
      {
	formatClass = java.lang.Number.class;

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
	    df.applyPattern(style);
	  }
      }
    else if (type.equals("time") || type.equals("date"))
      {
	formatClass = java.util.Date.class;

	int val = DateFormat.DEFAULT;
	if (style == null)
	  ;
	else if (style.equals("short"))
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
	formatClass = java.lang.Number.class;

	if (style == null)
	  throw new
	    IllegalArgumentException ("style required for choice format");
	format = new ChoiceFormat (style);
      }
  }
}

public class MessageFormat extends Format
{
  private static final long serialVersionUID = 6479157306784022952L;

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
	  throw new IllegalArgumentException("Found '}' without '{'");
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
	throw new IllegalArgumentException("Failed to parse integer string");
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
      throw new IllegalArgumentException("Missing '}' at end of message format");
    ++index;

    // Now fetch trailing string.
    index = scanString (pat, index, buffer);
    mfe.trailer = buffer.toString ();

    mfe.setLocale(locale);

    return index;
  }

  /**
   * Applies the specified pattern to this MessageFormat.
   *
   * @param aPattern The Pattern
   */
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

  /**
   * Overrides Format.clone()
   */
  public Object clone ()
  {
    MessageFormat c = (MessageFormat) super.clone ();
    c.elements = (MessageFormatElement[]) elements.clone ();
    return c;
  }

  /**
   * Overrides Format.equals(Object obj)
   */
  public boolean equals (Object obj)
  {
    if (! (obj instanceof MessageFormat))
      return false;
    MessageFormat mf = (MessageFormat) obj;
    return (pattern.equals(mf.pattern)
	    && locale.equals(mf.locale));
  }

  /**
   * A convinience method to format patterns.
   *
   * @param aPattern The pattern used when formatting.
   * @param arguments The array containing the objects to be formatted.
   */
  public static String format (String pattern, Object arguments[])
  {
    MessageFormat mf = new MessageFormat (pattern);
    StringBuffer sb = new StringBuffer ();
    FieldPosition fp = new FieldPosition (NumberFormat.INTEGER_FIELD);
    return mf.format(arguments, sb, fp).toString();
  }

  /**
   * Returns the pattern with the formatted objects.
   *
   * @param source The array containing the objects to be formatted.
   * @param result The StringBuffer where the text is appened.
   * @param fp A FieldPosition object (it is ignored).
   */
  public final StringBuffer format (Object arguments[], StringBuffer appendBuf,
				    FieldPosition ignore)
  {
    appendBuf.append(leader);

    for (int i = 0; i < elements.length; ++i)
      {
	if (elements[i].argNumber >= arguments.length)
	  throw new IllegalArgumentException("Not enough arguments given");

	Object thisArg = arguments[elements[i].argNumber];

	Format formatter = null;
	if (elements[i].setFormat != null)
	  formatter = elements[i].setFormat;
	else if (elements[i].format != null)
	  {
	    if (elements[i].formatClass != null
		&& ! elements[i].formatClass.isInstance(thisArg))
	      throw new IllegalArgumentException("Wrong format class");
	    
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
		formatter.format(thisArg, buf, ignore);
		MessageFormat mf = new MessageFormat ();
		mf.setLocale(locale);
		mf.applyPattern(buf.toString());
		mf.format(arguments, appendBuf, ignore);
	      }
	    else
	      formatter.format(thisArg, appendBuf, ignore);
	  }

	appendBuf.append(elements[i].trailer);
      }

    return appendBuf;
  }

  /**
   * Returns the pattern with the formatted objects.
   *
   * @param source The object to be formatted.
   * @param result The StringBuffer where the text is appened.
   * @param fp A FieldPosition object (it is ignored).
   */
  public final StringBuffer format (Object singleArg, StringBuffer appendBuf,
				    FieldPosition ignore)
  {
    Object[] args;

    if (singleArg instanceof Object[])
      {
	// This isn't specified in any manual, but it follows the
	// JDK implementation.
	args = (Object[]) singleArg;
      }
    else
      {
	args = new Object[1];
	args[0] = singleArg;
      }
    return format (args, appendBuf, ignore);
  }

  /**
   * Returns an array with the Formats for
   * the arguments.
   */
  public Format[] getFormats ()
  {
    Format[] f = new Format[elements.length];
    for (int i = elements.length - 1; i >= 0; --i)
      f[i] = elements[i].setFormat;
    return f;
  }

  /**
   * Returns the locale.
   */
  public Locale getLocale ()
  {
    return locale;
  }

  /**
   * Overrides Format.hashCode()
   */
  public int hashCode ()
  {
    // FIXME: not a very good hash.
    return pattern.hashCode() + locale.hashCode();
  }

  private MessageFormat ()
  {
  }

  /**
   * Creates a new MessageFormat object with
   * the specified pattern
   *
   * @param pattern The Pattern
   */
  public MessageFormat(String pattern)
  {
    this(pattern, Locale.getDefault());
  }

  /**
   * Creates a new MessageFormat object with
   * the specified pattern
   *
   * @param pattern The Pattern
   * @param locale The Locale to use
   *
   * @since 1.4
   */
  public MessageFormat(String pattern, Locale locale)
  {
    this.locale = locale;
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

  /**
   * Sets the format for the argument at an specified
   * index.
   *
   * @param index The index.
   * @format The Format object.
   */
  public void setFormat (int variableNum, Format newFormat)
  {
    elements[variableNum].setFormat = newFormat;
  }

  /**
   * Sets the formats for the arguments.
   *
   * @param formats An array of Format objects.
   */
  public void setFormats (Format[] newFormats)
  {
    if (newFormats.length < elements.length)
      throw new IllegalArgumentException("Not enough format objects");

    int len = Math.min(newFormats.length, elements.length);
    for (int i = 0; i < len; ++i)
      elements[i].setFormat = newFormats[i];
  }

  /**
   * Sets the locale.
   *
   * @param locale A Locale
   */
  public void setLocale (Locale loc)
  {
    locale = loc;
    if (elements != null)
      {
	for (int i = 0; i < elements.length; ++i)
	  elements[i].setLocale(loc);
      }
  }

  /**
   * Returns the pattern.
   */
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
