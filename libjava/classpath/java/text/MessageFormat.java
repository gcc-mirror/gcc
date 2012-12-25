/* MessageFormat.java - Localized message formatting.
   Copyright (C) 1999, 2001, 2002, 2004, 2005, 2012 Free Software Foundation, Inc.

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

import gnu.java.lang.CPStringBuilder;

import gnu.java.text.FormatCharacterIterator;

import java.io.InvalidObjectException;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;

public class MessageFormat extends Format
{
  /**
   * @author Tom Tromey (tromey@cygnus.com)
   * @author Jorge Aliss (jaliss@hotmail.com)
   * @date March 3, 1999
   */
  /* Written using "Java Class Libraries", 2nd edition, plus online
   * API docs for JDK 1.2 from http://www.javasoft.com.
   * Status:  Believed complete and correct to 1.2, except serialization.
   *          and parsing.
   */
  private static final class MessageFormatElement
  {
    // Argument number.
    int argNumber;
    // Formatter to be used.  This is the format set by setFormat.
    Format setFormat;
    // Formatter to be used based on the type.
    Format format;

    // Argument will be checked to make sure it is an instance of this
    // class.
    Class<?> formatClass;

    // Formatter type.
    String type;
    // Formatter style.
    String style;

    // Text to follow this element.
    String trailer;

    // Recompute the locale-based formatter.
    void setLocale (Locale loc)
    {
      if (type != null)
        {
          if (type.equals("number"))
            {
              formatClass = java.lang.Number.class;

              if (style == null)
                format = NumberFormat.getInstance(loc);
              else if (style.equals("currency"))
                format = NumberFormat.getCurrencyInstance(loc);
              else if (style.equals("percent"))
                format = NumberFormat.getPercentInstance(loc);
              else if (style.equals("integer"))
                format = NumberFormat.getIntegerInstance(loc);
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
              boolean styleIsPattern = false;
              if (style != null)
                {
                  if (style.equals("short"))
                    val = DateFormat.SHORT;
                  else if (style.equals("medium"))
                    val = DateFormat.MEDIUM;
                  else if (style.equals("long"))
                    val = DateFormat.LONG;
                  else if (style.equals("full"))
                    val = DateFormat.FULL;
                  else
                    styleIsPattern = true;
                }

              if (type.equals("time"))
                format = DateFormat.getTimeInstance(val, loc);
              else
                format = DateFormat.getDateInstance(val, loc);

              if (styleIsPattern)
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
  }

  private static final long serialVersionUID = 6479157306784022952L;

  public static class Field extends Format.Field
  {
    static final long serialVersionUID = 7899943957617360810L;

    /**
     * This is the attribute set for all characters produced
     * by MessageFormat during a formatting.
     */
    public static final MessageFormat.Field ARGUMENT = new MessageFormat.Field("argument");

    // For deserialization
    private Field()
    {
      super("");
    }

    protected Field(String s)
    {
      super(s);
    }

    /**
     * invoked to resolve the true static constant by
     * comparing the deserialized object to know name.
     *
     * @return object constant
     */
    protected Object readResolve() throws InvalidObjectException
    {
      if (getName().equals(ARGUMENT.getName()))
        return ARGUMENT;

      throw new InvalidObjectException("no such MessageFormat field called " + getName());
    }

  }

  // Helper that returns the text up to the next format opener.  The
  // text is put into BUFFER.  Returns index of character after end of
  // string.  Throws IllegalArgumentException on error.
  private static int scanString(String pat, int index, CPStringBuilder buffer)
  {
    int max = pat.length();
    buffer.setLength(0);
    boolean quoted = false;
    for (; index < max; ++index)
      {
        char c = pat.charAt(index);
        if (quoted)
          {
            // In a quoted context, a single quote ends the quoting.
            if (c == '\'')
              quoted = false;
            else
              buffer.append(c);
          }
        // Check for '', which is a single quote.
        else if (c == '\'' && index + 1 < max && pat.charAt(index + 1) == '\'')
          {
            buffer.append(c);
            ++index;
          }
        else if (c == '\'')
          {
            // Start quoting.
            quoted = true;
          }
        else if (c == '{')
          break;
        else
          buffer.append(c);
      }
    // Note that we explicitly allow an unterminated quote.  This is
    // done for compatibility.
    return index;
  }

  // This helper retrieves a single part of a format element.  Returns
  // the index of the terminating character.
  private static int scanFormatElement(String pat, int index,
                                       CPStringBuilder buffer, char term)
  {
    int max = pat.length();
    buffer.setLength(0);
    int brace_depth = 1;
    boolean quoted = false;

    for (; index < max; ++index)
      {
        char c = pat.charAt(index);
        // First see if we should turn off quoting.
        if (quoted)
          {
            if (c == '\'')
              quoted = false;
            // In both cases we fall through to inserting the
            // character here.
          }
        // See if we have just a plain quote to insert.
        else if (c == '\'' && index + 1 < max
                 && pat.charAt(index + 1) == '\'')
          {
            buffer.append(c);
            ++index;
          }
        // See if quoting should turn on.
        else if (c == '\'')
          quoted = true;
        else if (c == '{')
          ++brace_depth;
        else if (c == '}')
          {
            if (--brace_depth == 0)
              break;
          }
        // Check for TERM after braces, because TERM might be `}'.
        else if (c == term)
          break;
        // All characters, including opening and closing quotes, are
        // inserted here.
        buffer.append(c);
      }
    return index;
  }

  // This is used to parse a format element and whatever non-format
  // text might trail it.
  private static int scanFormat(String pat, int index, CPStringBuilder buffer,
                                List<MessageFormatElement> elts, Locale locale)
  {
    MessageFormatElement mfe = new MessageFormatElement ();
    elts.add(mfe);

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
        IllegalArgumentException iae = new IllegalArgumentException(pat);
        iae.initCause(nfx);
        throw iae;
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
   * @param newPattern The Pattern
   */
  public void applyPattern (String newPattern)
  {
    pattern = newPattern;

    CPStringBuilder tempBuffer = new CPStringBuilder ();

    int index = scanString (newPattern, 0, tempBuffer);
    leader = tempBuffer.toString();

    List<MessageFormatElement> elts = new ArrayList<MessageFormatElement>();
    while (index < newPattern.length())
      index = scanFormat (newPattern, index, tempBuffer, elts, locale);

    elements = elts.toArray(new MessageFormatElement[elts.size()]);
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
   * @param arguments The array containing the objects to be formatted.
   */
  public AttributedCharacterIterator formatToCharacterIterator (Object arguments)
  {
    Object[] arguments_array = (Object[])arguments;
    FormatCharacterIterator iterator = new FormatCharacterIterator();

    formatInternal(arguments_array, new StringBuffer(), null, iterator);

    return iterator;
  }

  /**
   * A convinience method to format patterns.
   *
   * @param pattern The pattern used when formatting.
   * @param arguments The array containing the objects to be formatted.
   */
  public static String format (String pattern, Object... arguments)
  {
    MessageFormat mf = new MessageFormat (pattern);
    StringBuffer sb = new StringBuffer ();
    FieldPosition fp = new FieldPosition (NumberFormat.INTEGER_FIELD);
    return mf.formatInternal(arguments, sb, fp, null).toString();
  }

  /**
   * Returns the pattern with the formatted objects.
   *
   * @param arguments The array containing the objects to be formatted.
   * @param appendBuf The StringBuffer where the text is appened.
   * @param fp A FieldPosition object (it is ignored).
   */
  public final StringBuffer format (Object arguments[], StringBuffer appendBuf,
                                    FieldPosition fp)
  {
    return formatInternal(arguments, appendBuf, fp, null);
  }

  private StringBuffer formatInternal (Object arguments[],
                                       StringBuffer appendBuf,
                                       FieldPosition fp,
                                       FormatCharacterIterator output_iterator)
  {
    appendBuf.append(leader);
    if (output_iterator != null)
      output_iterator.append(leader);

    for (int i = 0; i < elements.length; ++i)
      {
        Object thisArg = null;
        boolean unavailable = false;
        if (arguments == null || elements[i].argNumber >= arguments.length)
          unavailable = true;
        else
          thisArg = arguments[elements[i].argNumber];

        AttributedCharacterIterator iterator = null;

        Format formatter = null;

        if (fp != null && i == fp.getField() && fp.getFieldAttribute() == Field.ARGUMENT)
          fp.setBeginIndex(appendBuf.length());

        if (unavailable)
          appendBuf.append("{" + elements[i].argNumber + "}");
        else
          {
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
          }

        if (fp != null && fp.getField() == i && fp.getFieldAttribute() == Field.ARGUMENT)
          fp.setEndIndex(appendBuf.length());

        if (formatter != null)
          {
            // Special-case ChoiceFormat.
            if (formatter instanceof ChoiceFormat)
              {
                StringBuffer buf = new StringBuffer ();
                formatter.format(thisArg, buf, fp);
                MessageFormat mf = new MessageFormat ();
                mf.setLocale(locale);
                mf.applyPattern(buf.toString());
                mf.format(arguments, appendBuf, fp);
              }
            else
              {
                if (output_iterator != null)
                  iterator = formatter.formatToCharacterIterator(thisArg);
                else
                  formatter.format(thisArg, appendBuf, fp);
              }

            elements[i].format = formatter;
          }

        if (output_iterator != null)
          {
            HashMap<MessageFormat.Field, Integer> hash_argument =
              new HashMap<MessageFormat.Field, Integer>();
            int position = output_iterator.getEndIndex();

            hash_argument.put (MessageFormat.Field.ARGUMENT,
                               Integer.valueOf(elements[i].argNumber));


            if (iterator != null)
              {
                output_iterator.append(iterator);
                output_iterator.addAttributes(hash_argument, position,
                                              output_iterator.getEndIndex());
              }
            else
              output_iterator.append(thisArg.toString(), hash_argument);

            output_iterator.append(elements[i].trailer);
          }

        appendBuf.append(elements[i].trailer);
      }

    return appendBuf;
  }

  /**
   * Returns the pattern with the formatted objects.  The first argument
   * must be a array of Objects.
   * This is equivalent to format((Object[]) objectArray, appendBuf, fpos)
   *
   * @param objectArray The object array to be formatted.
   * @param appendBuf The StringBuffer where the text is appened.
   * @param fpos A FieldPosition object (it is ignored).
   */
  public final StringBuffer format (Object objectArray, StringBuffer appendBuf,
                                    FieldPosition fpos)
  {
    return format ((Object[])objectArray, appendBuf, fpos);
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

  /**
   * Parse a string <code>sourceStr</code> against the pattern specified
   * to the MessageFormat constructor.
   *
   * @param sourceStr the string to be parsed.
   * @param pos the current parse position (and eventually the error position).
   * @return the array of parsed objects sorted according to their argument number
   * in the pattern.
   */
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

    ArrayList<Object> results = new ArrayList<Object>(elements.length);
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
            double[] limits = cf.getLimits();
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
            int next_index;
            if (elements[i].trailer.length() > 0)
              next_index = sourceStr.indexOf(elements[i].trailer, index);
            else
              next_index = sourceStr.length();
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
          {
            // Emulate padding behaviour of Vector.setSize() with ArrayList
            results.ensureCapacity(elements[i].argNumber + 1);
            for (int a = results.size(); a <= elements[i].argNumber; ++a)
              results.add(a, null);
          }
        results.set(elements[i].argNumber, value);

        index += elements[i].trailer.length();
      }

    return results.toArray(new Object[results.size()]);
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
   * @param variableNum The index.
   * @param newFormat The Format object.
   */
  public void setFormat (int variableNum, Format newFormat)
  {
    elements[variableNum].setFormat = newFormat;
  }

  /**
   * Sets the formats for the arguments.
   *
   * @param newFormats An array of Format objects.
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
   * @param loc A Locale
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

  /**
   * Return the formatters used sorted by argument index. It uses the
   * internal table to fill in this array: if a format has been
   * set using <code>setFormat</code> or <code>setFormatByArgumentIndex</code>
   * then it returns it at the right index. If not it uses the detected
   * formatters during a <code>format</code> call. If nothing is known
   * about that argument index it just puts null at that position.
   * To get useful informations you may have to call <code>format</code>
   * at least once.
   *
   * @return an array of formatters sorted by argument index.
   */
  public Format[] getFormatsByArgumentIndex()
  {
    int argNumMax = 0;
    // First, find the greatest argument number.
    for (int i=0;i<elements.length;i++)
      if (elements[i].argNumber > argNumMax)
        argNumMax = elements[i].argNumber;

    Format[] formats = new Format[argNumMax];
    for (int i=0;i<elements.length;i++)
      {
        if (elements[i].setFormat != null)
          formats[elements[i].argNumber] = elements[i].setFormat;
        else if (elements[i].format != null)
          formats[elements[i].argNumber] = elements[i].format;
      }
    return formats;
  }

  /**
   * Set the format to used using the argument index number.
   *
   * @param argumentIndex the argument index.
   * @param newFormat the format to use for this argument.
   */
  public void setFormatByArgumentIndex(int argumentIndex,
                                       Format newFormat)
  {
    for (int i=0;i<elements.length;i++)
      {
        if (elements[i].argNumber == argumentIndex)
          elements[i].setFormat = newFormat;
      }
  }

  /**
   * Set the format for argument using a specified array of formatters
   * which is sorted according to the argument index. If the number of
   * elements in the array is fewer than the number of arguments only
   * the arguments specified by the array are touched.
   *
   * @param newFormats array containing the new formats to set.
   *
   * @throws NullPointerException if newFormats is null
   */
  public void setFormatsByArgumentIndex(Format[] newFormats)
  {
    for (int i=0;i<newFormats.length;i++)
      {
        // Nothing better than that can exist here.
        setFormatByArgumentIndex(i, newFormats[i]);
      }
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
