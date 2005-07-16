/* DefaultFormatter.java --
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

import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.text.ParseException;

import javax.swing.JFormattedTextField;

/**
 * The <code>DefaultFormatter</code> is a concrete formatter for use in
 * {@link JFormattedTextField}s.
 *
 * It can format arbitrary values by invoking
 * their {@link Object#toString} method.
 *
 * In order to convert a String back to
 * a value, the value class must provide a single argument constructor that
 * takes a String object as argument value. If no such constructor is found,
 * the String itself is passed back by #stringToValue.
 *  
 * @author Roman Kennke (roman@kennke.org)
 */
public class DefaultFormatter
  extends JFormattedTextField.AbstractFormatter
  implements Cloneable, Serializable
{

  /**
   * A {@link DocumentFilter} that intercepts modification of the
   * JFormattedTextField's Document and commits the value depending
   * on the value of the <code>commitsOnValidEdit</code> property.
   *
   */
  // FIXME: Handle allowsInvalid and overwriteMode properties
  private class FormatterDocumentFilter
    extends DocumentFilter
  {
    /**
     * Invoked when text is removed from a text component.
     *
     * @param bypass the FilterBypass to use to mutate the document
     * @param offset the start position of the modification
     * @param length the length of the removed text
     *
     * @throws BadLocationException if offset or lenght are invalid in
     *     the Document
     */
    public void remove(DocumentFilter.FilterBypass bypass, int offset,
                        int length)
      throws BadLocationException
    {
      super.remove(bypass, offset, length);
      checkValidInput();
      commitIfAllowed();
    }
    
    /**
     * Invoked when text is inserted into a text component.
     *
     * @param bypass the FilterBypass to use to mutate the document
     * @param offset the start position of the modification
     * @param text the inserted text
     * @param attributes the attributes of the inserted text
     *
     * @throws BadLocationException if offset or lenght are invalid in
     *     the Document
     */
    public void insertString(DocumentFilter.FilterBypass bypass, int offset,
                              String text, AttributeSet attributes)
      throws BadLocationException
    {
      if (overwriteMode == true)
        replace(bypass, offset, text.length(), text, attributes);
      else
        super.insertString(bypass, offset, text, attributes);
      checkValidInput();
      commitIfAllowed();
    }

    /**
     * Invoked when text is replaced in a text component.
     * 
     * @param bypass the FilterBypass to use to mutate the document
     * @param offset the start position of the modification
     * @param length the length of the removed text
     * @param text the inserted text
     * @param attributes the attributes of the inserted text
     *
     * @throws BadLocationException if offset or lenght are invalid in
     *     the Document
     */
    public void replace(DocumentFilter.FilterBypass bypass, int offset,
                         int length, String text, AttributeSet attributes)
      throws BadLocationException
    {
      super.replace(bypass, offset, length, text, attributes);
      checkValidInput();
      commitIfAllowed();
    }

    /**
     * Commits the value to the JTextTextField if the property
     * <code>commitsOnValidEdit</code> is set to <code>true</code>.
     */
    private void commitIfAllowed()
    {
      if (commitsOnValidEdit == true)
        try
          {
            getFormattedTextField().commitEdit();
          }
        catch (ParseException ex)
          {
            // ignore invalid edits
          }
    }

    /**
     * Checks if the value in the input field is valid. If the
     * property allowsInvalid is set to <code>false</code>, then
     * the string in the input field is not allowed to be entered.
     *
     * @param doc the document of the input field
     * @param value the current (old) value of the input field
     */
    private void checkValidInput()
    {
      JFormattedTextField ftf = getFormattedTextField();
      try
        {
          Object newval = stringToValue(ftf.getText());
        }
      catch (ParseException ex)
        {
          if (!allowsInvalid)
            {
              // roll back the input if invalid edits are not allowed
              try
                {
                  ftf.setText(valueToString(ftf.getValue()));
                }
              catch (ParseException pe)
                {
                  // if that happens, something serious must be wrong
                  throw new AssertionError("values must be parseable");
                }
            }
        }
    }
  }

  /** The serialVersoinUID. */
  private static final long serialVersionUID = -7369196326612908900L;

  /**
   * Indicates if the value should be committed after every
   * valid modification of the Document.
   */
  boolean commitsOnValidEdit;

  /**
   * If <code>true</code> newly inserted characters overwrite existing
   * values, otherwise insertion is done the normal way.
   */
  boolean overwriteMode;

  /**
   * If <code>true</code> invalid edits are allowed for a limited
   * time.
   */
  boolean allowsInvalid;

  /**
   * The class that is used for values.
   */
  Class valueClass;

  /**
   * Creates a new instance of <code>DefaultFormatter</code>.
   */
  public DefaultFormatter()
  {
    commitsOnValidEdit = true;
    overwriteMode = true;
    allowsInvalid = true;
    valueClass = Object.class;
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
   * Returns <code>true</code> if the value should be committed after
   * each valid modification of the input field, <code>false</code> if
   * it should never be committed by this formatter.
   *
   * @return the state of the <code>commitsOnValidEdit</code> property
   *
   * @see #setCommitsOnValidEdit
   */
  public boolean getCommitsOnValidEdit()
  {
    return commitsOnValidEdit;
  }

  /**
   * Sets the value of the <code>commitsOnValidEdit</code> property.
   *
   * @param commitsOnValidEdit the new state of the
   *     <code>commitsOnValidEdit</code> property
   *
   * @see #getCommitsOnValidEdit
   */
  public void setCommitsOnValidEdit(boolean commitsOnValidEdit)
  {
    this.commitsOnValidEdit = commitsOnValidEdit;
  }

  /**
   * Returns the value of the <code>overwriteMode</code> property.
   * If that is set to <code>true</code> then newly inserted characters
   * overwrite existing values, otherwise the characters are inserted like
   * normal. The default is <code>true</code>.
   *
   * @return the value of the <code>overwriteMode</code> property
   */
  public boolean getOverwriteMode()
  {
    return overwriteMode;
  }

  /**
   * Sets the value of the <code>overwriteMode</code> property.
   * 
   * If that is set to <code>true</code> then newly inserted characters
   * overwrite existing values, otherwise the characters are inserted like
   * normal. The default is <code>true</code>.
   *
   * @param overwriteMode the new value for the <code>overwriteMode</code>
   *     property
   */
  public void setOverwriteMode(boolean overwriteMode)
  {
    this.overwriteMode = overwriteMode;
  }

  /**
   * Returns whether or not invalid edits are allowed or not. If invalid
   * edits are allowed, the JFormattedTextField may temporarily contain invalid
   * characters.
   *
   * @return the value of the allowsInvalid property
   */
  public boolean getAllowsInvalid()
  {
    return allowsInvalid;
  }

  /**
   * Sets the value of the <code>allowsInvalid</code> property.
   *
   * @param allowsInvalid the new value for the property
   *
   * @see #getAllowsInvalid()
   */
  public void setAllowsInvalid(boolean allowsInvalid)
  {
    this.allowsInvalid = allowsInvalid;
  }

  /**
   * Returns the class that is used for values. When Strings are converted
   * back to values, this class is used to create new value objects.
   *
   * @return the class that is used for values
   */
  public Class getValueClass()
  {
    return valueClass;
  }

  /**
   * Sets the class that is used for values.
   *
   * @param valueClass the class that is used for values
   *
   * @see #getValueClass()
   */
  public void setValueClass(Class valueClass)
  {
    this.valueClass = valueClass;
  }

  /**
   * Converts a String (from the JFormattedTextField input) to a value.
   * In order to achieve this, the formatter tries to instantiate an object
   * of the class returned by #getValueClass() using a single argument
   * constructor that takes a String argument. If such a constructor cannot
   * be found, the String itself is returned.
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
    Object value = string;
    Class valueClass = getValueClass();
    if (valueClass == null)
      valueClass = getFormattedTextField().getValue().getClass();
    if (valueClass != null)
      try
        {
          Constructor constr = valueClass.getConstructor
                                             (new Class[]{String.class});
          value = constr.newInstance(new Object[]{ string });
        }
      catch (NoSuchMethodException ex)
        {
          // leave value as string
        }
      catch (Exception ex)
        {
          throw new ParseException(string, 0);
        }
    return value;
  }

  /**
   * Converts a value object into a String. This is done by invoking the
   * {@link Object#toString()} method on the value.
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
    return value.toString();
  }

  /**
   * Creates and returns a clone of this DefaultFormatter.
   *
   * @return a clone of this object
   *
   * @throws CloneNotSupportedException not thrown here
   */
  public Object clone()
    throws CloneNotSupportedException
  {
    return super.clone();
  }

  /**
   * Returns the DocumentFilter that is used to restrict input.
   *
   * @return the DocumentFilter that is used to restrict input
   */
  protected DocumentFilter getDocumentFilter()
  {
    return new FormatterDocumentFilter();
  }
}
