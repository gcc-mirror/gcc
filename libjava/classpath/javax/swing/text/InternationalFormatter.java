/* InternationalFormatter.java --
Copyright (C) 2005  Free Software Foundation, Inc.

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

package javax.swing.text;

import java.text.AttributedCharacterIterator;
import java.text.Format;
import java.text.ParseException;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.swing.Action;
import javax.swing.JFormattedTextField;

/**
 * This extends {@link DefaultFormatter} so that the value to string
 * conversion is done via a {@link Format} object. This allows
 * various additional formats to be handled by JFormattedField.
 *
 * @author Roman Kennke (roman@kennke.org)
 */
public class InternationalFormatter
  extends DefaultFormatter
{

  /** The serialVersoinUID. */
  private static final long serialVersionUID = 6941977820906408656L;

  /** The format that handles value to string conversion. */
  Format format;

  /** The minimal permissable value. */
  Comparable minimum;

  /** The maximal permissable value. */
  Comparable maximum;

  /**
   * Creates a new InternationalFormatter with no Format specified.
   */
  public InternationalFormatter()
  {
    super();
    minimum = null;
    maximum = null;
    format = null;
  }

  /**
   * Creates a new InternationalFormatter that uses the specified
   * Format object for value to string conversion.
   *
   * @param format the Format object to use for value to string conversion
   */
  public InternationalFormatter(Format format)
  {
    this();
    setFormat(format);
  }

  /**
   * Sets the Format object that is used to convert values to strings.
   *
   * @param format the Format to use for value to string conversion
   *
   * @see Format
   */
  public void setFormat(Format format)
  {
    this.format = format;
  }

  /**
   * Returns the currently used Format object that is used to format
   * the JFormattedField.
   *
   * @return the current Format
   */
  public Format getFormat()
  {
    return format;
  }

  /**
   * Sets the minimum value that is allowed by this Formatter. The minimum
   * value is given as an object that implements the {@link Comparable}
   * interface.
   *
   * If <code>minValue</code> is null, then the Formatter has no restrictions
   * at the lower end.
   *
   * If value class is not yet specified and <code>minValue</code> is not
   * null, then <code>valueClass</code> is set to the class of the minimum
   * value.
   *
   * @param minValue the minimum permissable value
   *
   * @see Comparable
   */
  public void setMinimum(Comparable minValue)
  {
    minimum = minValue;
    if (valueClass == null && minValue != null)
      valueClass = minValue.getClass();
  }

  /**
   * Returns the minimal value that is allowed by this Formatter.
   *
   * A <code>null</code> value means that there is no restriction.
   *
   * @return the minimal value that is allowed by this Formatter or
   *     <code>null</code> if there is no restriction
   */
  public Comparable getMinimum()
  {
    return minimum;
  }

  /**
   * Sets the maximum value that is allowed by this Formatter. The maximum
   * value is given as an object that implements the {@link Comparable}
   * interface.
   *
   * If <code>maxValue</code> is null, then the Formatter has no restrictions
   * at the upper end.
   *
   * If value class is not yet specified and <code>maxValue</code> is not
   * null, then <code>valueClass</code> is set to the class of the maximum
   * value.
   *
   * @param maxValue the maximum permissable value
   *
   * @see Comparable
   */
  public void setMaximum(Comparable maxValue)
  {
    maximum = maxValue;
    if (valueClass == null && maxValue != null)
      valueClass = maxValue.getClass();
  }

  /**
   * Returns the maximal value that is allowed by this Formatter.
   *
   * A <code>null</code> value means that there is no restriction.
   *
   * @return the maximal value that is allowed by this Formatter or
   *     <code>null</code> if there is no restriction
   */
  public Comparable getMaximum()
  {
    return maximum;
  }

  /**
   * Installs the formatter on the specified {@link JFormattedTextField}.
   *
   * This method does the following things:
   * <ul>
   * <li>Display the value of #valueToString in the
   *  <code>JFormattedTextField</code></li>
   * <li>Install the Actions from #getActions on the <code>JTextField</code>
   * </li>
   * <li>Install the DocumentFilter returned by #getDocumentFilter</li>
   * <li>Install the NavigationFilter returned by #getNavigationFilter</li>
   * </ul>
   *
   * This method is typically not overridden by subclasses. Instead override
   * one of the mentioned methods in order to customize behaviour.
   *
   * @param ftf the {@link JFormattedTextField} in which this formatter
   *     is installed 
   */
  public void install(JFormattedTextField ftf)
  {
    super.install(ftf);
  }

  /**
   * Converts a value object into a String. This is done by invoking
   * {@link Format#format(Object)} on the specified <code>Format</code> object.
   * If no format is set, then {@link DefaultFormatter#valueToString(Object)}
   * is called as a fallback.
   *
   * @param value the value to be converted
   *
   * @return the string representation of the value
   *
   * @throws ParseException if the value cannot be converted
   */
  public String valueToString(Object value)
    throws ParseException
  {
    if (format != null)
      return format.format(value);
    else
      return super.valueToString(value);
  }

  /**
   * Converts a String (from the JFormattedTextField input) to a value.
   * This is achieved by invoking {@link Format#parseObject(String)} on
   * the specified <code>Format</code> object.
   *
   * This implementation differs slightly from {@link DefaultFormatter},
   * it does:
   * <ol>
   * <li>Convert the string to an <code>Object</code> using the
   *   <code>Formatter</code>.</li>
   * <li>If a <code>valueClass</code> has been set, this object is passed to
   *   {@link DefaultFormatter#stringToValue(String)} so that the value
   *   has the correct type. This may or may not work correctly, depending on
   *   the implementation of toString() in the value class and if the class
   *   implements a constructor that takes one String as argument.</li>
   * <li>If no {@link ParseException} has been thrown so far, we check if the
   *   value exceeds either <code>minimum</code> or <code>maximum</code> if
   *   one of those has been specified and throw a <code>ParseException</code>
   *   if it does.</li>
   * <li>Return the value.</li>
   * </ol>
   *
   * If no format has been specified, then
   * {@link DefaultFormatter#stringToValue(String)} is invoked as fallback.
   *
   * @param string the string to convert
   *
   * @return the value for the string
   *
   * @throws ParseException if the string cannot be converted into
   *     a value object (e.g. invalid input)
   */
  public Object stringToValue(String string)
    throws ParseException
  {
    if (format != null)
      {
        Object o = format.parseObject(string);

        // If a value class has been set, call super in order to get
        // the class right. That is what the JDK API docs suggest, so we do
        // it that way.
        if (valueClass != null)
          o = super.stringToValue(o.toString());

        // Check for minimum and maximum bounds
        if (minimum != null && minimum.compareTo(o) > 0)
          throw new ParseException("The value may not be less than the"
                                    + " specified minimum", 0);
        if (maximum != null && minimum.compareTo(o) < 0)
          throw new ParseException("The value may not be greater than the"
                                    + " specified maximum", 0);
        return o;
      }
    else
      return super.stringToValue(string);
  }

  /**
   * Returns the {@link Format.Field} constants that are associated with
   * the specified position in the text.
   *
   * If <code>offset</code> is not a valid location in the input field,
   * an empty array of fields is returned.
   *
   * @param offset the position in the text from which we want to fetch
   *     the fields constants
   *
   * @return the field values associated with the specified position in
   *     the text
   */
  public Format.Field[] getFields(int offset)
  {
    // TODO: don't know if this is correct
    AttributedCharacterIterator aci = format.formatToCharacterIterator
                                         (getFormattedTextField().getValue());
    aci.setIndex(offset);
    Map atts = aci.getAttributes();
    Set keys = atts.keySet();
    Format.Field[] fields = new Format.Field[keys.size()];
    int index = 0;
    for (Iterator i = keys.iterator(); i.hasNext(); index++)
      fields[index] = (Format.Field) i.next();
    return fields;
  }

  /**
   * This creates and returns a clone of this Formatter.
   *
   * @return a clone of this formatter
   *
   * @throws CloneNotSupportedException not thrown here, since cloning is
   *     supported
   * XXX - FIXME - Whole method disabled as workaround for gcj bug #22060.
  public Object clone()
    throws CloneNotSupportedException
  {
    // TODO: it has to be considered, if we should return a deep or shallow
    // clone here. for now we return a shallow clone
    Object clone = super.clone();
    return clone;
  }
   */

  /**
   * Returns the Actions that are supported by this Formatter.
   *
   * @specnote the JDK API docs say here: <cite>If
   *     <code>getSupportsIncrement</code> returns true, this returns two
   *     Actions suitable for incrementing/decrementing the value.</cite>
   *     The questsion is, which method <code>getSupportsIncrement</code>?
   *     There is no such method in the whole API! So we just call
   *     super.getActions here.
   */
  public Action[] getActions()
  {
    return super.getActions();
  }
}
