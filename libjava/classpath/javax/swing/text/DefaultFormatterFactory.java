/* DefaultFormatterFactory.java -- FIXME: briefly describe file purpose
   Copyright (C) 2005 Free Software Foundation, Inc.

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

import java.io.Serializable;

import javax.swing.JFormattedTextField;
import javax.swing.JFormattedTextField.AbstractFormatter;
import javax.swing.JFormattedTextField.AbstractFormatterFactory;

/**
 * This class is Swing's only concrete implementation of 
 * JFormattedTextField.AbstractFormatterFactory.  It holds several
 * formatters and determines the best one to be used based on the 
 * passed-in value from the text field.
 * 
 * @author Anthony Balkissoon abalkiss at redhat dot com
 * @since 1.4
 */
public class DefaultFormatterFactory extends AbstractFormatterFactory implements
    Serializable
{
  /** 
   * The default formatter. 
   **/
  AbstractFormatter defaultFormatter;

  /**
   * The formatter to use when the JFormattedTextField has focus and either the
   * value isn't null or the value is null but no <code>nullFormatter</code>
   * has been specified.
   */
  AbstractFormatter editFormatter;

  /**
   * The formatter to use when the JFormattedTextField doesn't havefocus and 
   * either the value isn't null or the value is null but no 
   * <code>nullFormatter</code> has been specified.
   */
  AbstractFormatter displayFormatter;

  /**
   * The formatter to use when the value of the JFormattedTextField is null.   
   */
  AbstractFormatter nullFormatter;

  /**
   * Creates a DefaultFormatterFactory with no formatters
   */
  public DefaultFormatterFactory()
  {
    // Nothing to be done here.
  }

  /**
   * Creates a new DefaultFormatterFactory with the specified formatters.
   * @param defaultFormat the formatter to use if no other appropriate non-null
   * formatted can be found.
   */
  public DefaultFormatterFactory(AbstractFormatter defaultFormat)
  {
    defaultFormatter = defaultFormat;
  }

  /**
   * Creates a new DefaultFormatterFactory with the specified formatters.
   * @param defaultFormat the formatter to use if no other appropriate non-null
   * formatted can be found.
   * @param displayFormat the formatter to use if the JFormattedTextField 
   * doesn't have focus and either the value is not null or the value is null
   * but no <code>nullFormatter</code> has been specified.
   */
  public DefaultFormatterFactory(AbstractFormatter defaultFormat,
                                 AbstractFormatter displayFormat)
  {
    defaultFormatter = defaultFormat;
    displayFormatter = displayFormat;
  }

  /**
   * Creates a new DefaultFormatterFactory with the specified formatters.
   * @param defaultFormat the formatter to use if no other appropriate non-null
   * formatted can be found.
   * @param displayFormat the formatter to use if the JFormattedTextField 
   * doesn't have focus and either the value is not null or the value is null
   * but no <code>nullFormatter</code> has been specified.
   * @param editFormat the formatter to use if the JFormattedTextField has
   * focus and either the value is not null or the value is null but not
   * <code>nullFormatter</code> has been specified.
   */
  public DefaultFormatterFactory(AbstractFormatter defaultFormat,
                                 AbstractFormatter displayFormat,
                                 AbstractFormatter editFormat)
  {
    defaultFormatter = defaultFormat;
    displayFormatter = displayFormat;
    editFormatter = editFormat;
  }

  /**
   * Creates a new DefaultFormatterFactory with the specified formatters.
   * @param defaultFormat the formatter to use if no other appropriate non-null
   * formatted can be found.
   * @param displayFormat the formatter to use if the JFormattedTextField 
   * doesn't have focus and either the value is not null or the value is null
   * but no <code>nullFormatter</code> has been specified.
   * @param editFormat the formatter to use if the JFormattedTextField has
   * focus and either the value is not null or the value is null but not
   * <code>nullFormatter</code> has been specified.
   * @param nullFormat the formatter to use when the value of the 
   * JFormattedTextField is null.
   */
  public DefaultFormatterFactory(AbstractFormatter defaultFormat,
                                 AbstractFormatter displayFormat,
                                 AbstractFormatter editFormat,
                                 AbstractFormatter nullFormat)
  {
    defaultFormatter = defaultFormat;
    displayFormatter = displayFormat;
    editFormatter = editFormat;
    nullFormatter = nullFormat;
  }

  /**
   * Returns the formatted to be used if no other appropriate non-null 
   * formatter can be found.
   * @return the formatted to be used if no other appropriate non-null 
   * formatter can be found.
   */
  public AbstractFormatter getDefaultFormatter()
  {
    return defaultFormatter;
  }

  /**
   * Sets the formatted to be used if no other appropriate non-null formatter 
   * can be found.
   * @param defaultFormatter the formatted to be used if no other appropriate
   * non-null formatter can be found.
   */
  public void setDefaultFormatter(AbstractFormatter defaultFormatter)
  {
    this.defaultFormatter = defaultFormatter;
  }

  /**
   * Gets the <code>displayFormatter</code>.  This is the formatter to use if 
   * the JFormattedTextField is not being edited and either the value is not 
   * null or the value is null and no <code>nullFormatter<code> has been 
   * specified.
   * @return the formatter to use if 
   * the JFormattedTextField is not being edited and either the value is not 
   * null or the value is null and no <code>nullFormatter<code> has been 
   * specified.
   */
  public AbstractFormatter getDisplayFormatter()
  {
    return displayFormatter;
  }

  /**
   * Sets the <code>displayFormatter</code>.  This is the formatter to use if 
   * the JFormattedTextField is not being edited and either the value is not 
   * null or the value is null and no <code>nullFormatter<code> has been 
   * specified.
   * @param displayFormatter the formatter to use.
   */
  public void setDisplayFormatter(AbstractFormatter displayFormatter)
  {
    this.displayFormatter = displayFormatter;
  }

  /**
   * Gets the <code>editFormatter</code>.  This is the formatter to use if the
   * JFormattedTextField is being edited and either the value is not null or 
   * the value is null and no <code>nullFormatter<code> has been specified.
   * @return the formatter to use if the JFormattedTextField is being edited
   * and the value is not null or the value is null but no nullFormatted has 
   * been specified.
   */
  public AbstractFormatter getEditFormatter()
  {
    return editFormatter;
  }

  /**
   * Sets the <code>editFormatter</code>.  This is the formatter to use if the
   * JFormattedTextField is being edited and either the value is not null or 
   * the value is null and no <code>nullFormatter<code> has been specified.
   * @param editFormatter the formatter to use.
   */
  public void setEditFormatter(AbstractFormatter editFormatter)
  {
    this.editFormatter = editFormatter;
  }

  /**
   * Gets the formatter to use if the value of the JFormattedTextField is null.
   * @return the formatter to use for null values.
   */
  public AbstractFormatter getNullFormatter()
  {
    return nullFormatter;
  }

  /**
   * Sets the <code>nullFormatter</code>.  This is the formatter to use if the
   * value of the JFormattedTextField is null.
   * @param nullFormatter the formatter to use for null values.
   */
  public void setNullFormatter(AbstractFormatter nullFormatter)
  {
    this.nullFormatter = nullFormatter;
  }

  /**
   * Returns the appropriate formatter based on the state of 
   * <code>tf</code>. If <code>tf<code> is null we return null, otherwise
   * we return one of the following:
   * 1. Returns <code>nullFormatter</code> if <code>tf.getValue()</code> is 
   * null and <code>nullFormatter</code> is not.  
   * 2. Returns <code>editFormatter</code> if <code>tf.hasFocus()</code> is
   * true and <code>editFormatter</code> is not null.
   * 3. Returns <code>displayFormatter</code> if <code>tf.hasFocus()</code> is
   * false and <code>displayFormatter</code> is not null.
   * 4. Otherwise returns <code>defaultFormatter</code>.
   */
  public AbstractFormatter getFormatter(JFormattedTextField tf)
  {
    if (tf == null)
      return null;
    
    if (tf.getValue() == null && nullFormatter != null)
      return nullFormatter;

    if (tf.hasFocus() && editFormatter != null)
      return editFormatter;

    if (!tf.hasFocus() && displayFormatter != null)
      return displayFormatter;

    return defaultFormatter;
  }
}
