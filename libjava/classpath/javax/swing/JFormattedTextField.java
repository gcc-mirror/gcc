/* JFormattedTextField.java --
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.

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


package javax.swing;

import java.awt.event.FocusEvent;
import java.io.Serializable;
import java.text.DateFormat;
import java.text.Format;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.Date;

import javax.swing.text.AbstractDocument;
import javax.swing.text.DateFormatter;
import javax.swing.text.DefaultFormatter;
import javax.swing.text.DefaultFormatterFactory;
import javax.swing.text.Document;
import javax.swing.text.DocumentFilter;
import javax.swing.text.InternationalFormatter;
import javax.swing.text.NavigationFilter;
import javax.swing.text.NumberFormatter;

/**
 * A text field that makes use of a formatter to display and edit a specific
 * type of data. The value that is displayed can be an arbitrary object. The
 * formatter is responsible for displaying the value in a textual form and
 * it may allow editing of the value.
 *
 * Formatters are usually obtained using an instance of
 * {@link AbstractFormatterFactory}. This factory is responsible for providing
 * an instance of {@link AbstractFormatter} that is able to handle the
 * formatting of the value of the JFormattedTextField.
 *
 * @author Michael Koch
 * @author Anthony Balkissoon abalkiss at redhat dot com
 *
 * @since 1.4
 */
public class JFormattedTextField extends JTextField
{
  private static final long serialVersionUID = 5464657870110180632L;

  /**
   * An abstract base implementation for a formatter that can be used by
   * a JTextField. A formatter can display a specific type of object and
   * may provide a way to edit this value.
   */
  public abstract static class AbstractFormatter implements Serializable
  {
    private static final long serialVersionUID = -5193212041738979680L;
    
    private JFormattedTextField textField;
    
    public AbstractFormatter ()
    {
      //Do nothing here.
    }

    /**
     * Clones the AbstractFormatter and removes the association to any 
     * particular JFormattedTextField.
     * 
     * @return a clone of this formatter with no association to any particular
     * JFormattedTextField
     * @throws CloneNotSupportedException if the Object's class doesn't support
     * the {@link Cloneable} interface
     */
    protected Object clone ()
      throws CloneNotSupportedException
    {
      // Clone this formatter.
      AbstractFormatter newFormatter = (AbstractFormatter)super.clone();
      
      // And remove the association to the JFormattedTextField.
      newFormatter.textField = null;
      return newFormatter;
    }

    /**
     * Returns a custom set of Actions that this formatter supports.  Should
     * be subclassed by formatters that have a custom set of Actions.
     * 
     * @return <code>null</code>.  Should be subclassed by formatters that want
     * to install custom Actions on the JFormattedTextField.
     */
    protected Action[] getActions ()
    {
      return null;
    }

    /**
     * Gets the DocumentFilter for this formatter.  Should be subclassed
     * by formatters wishing to install a filter that oversees Document
     * mutations.
     * 
     * @return <code>null</code>.  Should be subclassed by formatters
     * that want to restrict Document mutations.
     */
    protected DocumentFilter getDocumentFilter ()
    {
      // Subclasses should override this if they want to install a 
      // DocumentFilter.
      return null;
    }

    /**
     * Returns the JFormattedTextField on which this formatter is
     * currently installed.
     * 
     * @return the JFormattedTextField on which this formatter is currently
     * installed
     */
    protected JFormattedTextField getFormattedTextField ()
    {
      return textField;
    }

    /**
     * Gets the NavigationFilter for this formatter.  Should be subclassed
     * by formatters (such as {@link DefaultFormatter}) that wish to 
     * restrict where the cursor can be placed within the text field.
     * 
     * @return <code>null</code>.  Subclassed by formatters that want to restrict
     * cursor location within the JFormattedTextField.
     */
    protected NavigationFilter getNavigationFilter ()
    {
      // This should be subclassed if the formatter wants to install 
      // a NavigationFilter on the JFormattedTextField.
      return null;
    }

    /**
     * Installs this formatter on the specified JFormattedTextField.  This 
     * converts the current value to a displayable String and displays it, 
     * and installs formatter specific Actions from <code>getActions</code>.
     * It also installs a DocumentFilter and NavigationFilter on the 
     * JFormattedTextField.  
     * <p>
     * If there is a <code>ParseException</code> this sets the text to an 
     * empty String and marks the text field in an invalid state.
     * 
     * @param textField the JFormattedTextField on which to install this
     * formatter
     */
    public void install(JFormattedTextField textField)
    {
      // Uninstall the current textfield.
      if (this.textField != null)
        uninstall();
      
      this.textField = textField;
      
      // Install some state on the text field, including display text, 
      // DocumentFilter, NavigationFilter, and formatter specific Actions.
      if (textField != null)
        {
          try
          {
            // Set the text of the field.
            textField.setText(valueToString(textField.getValue()));
            Document doc = textField.getDocument();
            
            // Set the DocumentFilter for the field's Document.
            if (doc instanceof AbstractDocument)
              ((AbstractDocument)doc).setDocumentFilter(getDocumentFilter());
            
            // Set the NavigationFilter.
            textField.setNavigationFilter(getNavigationFilter());
            
            // Set the Formatter Actions
            // FIXME: Have to add the actions from getActions()            
          }
          catch (ParseException pe)
          {
            // Set the text to an empty String and mark the field as invalid.
            textField.setText("");
            setEditValid(false);
          }
        }
    }

    /**
     * Clears the state installed on the JFormattedTextField by the formatter.
     * This resets the DocumentFilter, NavigationFilter, and any additional 
     * Actions (returned by <code>getActions()</code>).     
     */
    public void uninstall ()
    {
      // Set the DocumentFilter for the field's Document.
      Document doc = textField.getDocument();
      if (doc instanceof AbstractDocument)
        ((AbstractDocument)doc).setDocumentFilter(null);
      textField.setNavigationFilter(null);
      // FIXME: Have to remove the Actions from getActions()
      this.textField = null;
    }

    /**
     * Invoke this method when invalid values are entered.  This forwards the
     * call to the JFormattedTextField.     
     */
    protected void invalidEdit ()
    {
      textField.invalidEdit();
    }

    /**
     * This method updates the <code>editValid</code> property of 
     * JFormattedTextField.
     * 
     * @param valid the new state for the <code>editValid</code> property
     */
    protected void setEditValid (boolean valid)
    {
      textField.editValid = valid;
    }

    /**
     * Parses <code>text</code> to return a corresponding Object.
     * 
     * @param text the String to parse
     * @return an Object that <code>text</code> represented
     * @throws ParseException if there is an error in the conversion
     */
    public abstract Object stringToValue (String text)
      throws ParseException;

    /**
     * Returns a String to be displayed, based on the Object
     * <code>value</code>.
     * 
     * @param value the Object from which to generate a String
     * @return a String to be displayed
     * @throws ParseException if there is an error in the conversion
     */
    public abstract String valueToString (Object value)
      throws ParseException;
  }

  /**
   * Delivers instances of an {@link AbstractFormatter} for
   * a specific value type for a JFormattedTextField. 
   */
  public abstract static class AbstractFormatterFactory
  {
    public AbstractFormatterFactory ()
    {
      // Do nothing here.
    }

    public abstract AbstractFormatter getFormatter (JFormattedTextField tf);
  }

  /** The possible focusLostBehavior options **/
  public static final int COMMIT = 0;
  public static final int COMMIT_OR_REVERT = 1;
  public static final int REVERT = 2;
  public static final int PERSIST = 3;

  /** The most recent valid and committed value **/
  private Object value;
  
  /** The behaviour for when this text field loses focus **/
  private int focusLostBehavior = COMMIT_OR_REVERT;
  
  /** The formatter factory currently being used **/
  private AbstractFormatterFactory formatterFactory;
  
  /** The formatter currently being used **/
  private AbstractFormatter formatter;
  
  // Package-private to avoid an accessor method.
  boolean editValid = true;
  
  /**
   * Creates a JFormattedTextField with no formatter factory.  
   * <code>setValue</code> or <code>setFormatterFactory</code> will 
   * properly configure this text field to edit a particular type
   * of value.
   */
  public JFormattedTextField ()
  {
    this((AbstractFormatterFactory) null, null);
  }

  /**
   * Creates a JFormattedTextField that can handle the specified Format.  
   * An appopriate AbstractFormatter and AbstractFormatterFactory will 
   * be created for the specified Format.
   * 
   * @param format the Format that this JFormattedTextField should be able
   * to handle
   */
  public JFormattedTextField (Format format)
  {
    this ();
    setFormatterFactory(getAppropriateFormatterFactory(format));
  }

  /**
   * Creates a JFormattedTextField with the specified formatter.  This will 
   * create a {@link DefaultFormatterFactory} with this formatter as the default
   * formatter.
   * 
   * @param formatter the formatter to use for this JFormattedTextField
   */
  public JFormattedTextField (AbstractFormatter formatter)
  {
    this(new DefaultFormatterFactory (formatter));
  }

  /**
   * Creates a JFormattedTextField with the specified formatter factory.
   * 
   * @param factory the formatter factory to use for this JFormattedTextField
   */
  public JFormattedTextField (AbstractFormatterFactory factory)
  {
    setFormatterFactory(factory);
  }

  /**
   * Creates a JFormattedTextField with the specified formatter factory and
   * initial value.
   * 
   * @param factory the initial formatter factory for this JFormattedTextField
   * @param value the initial value for the text field
   */
  public JFormattedTextField (AbstractFormatterFactory factory, Object value)
  {    
    setFormatterFactory(factory);
    setValue(value);
  }

  /**
   * Creates a JFormattedTextField with the specified value.  This creates a
   * formatter and formatterFactory that are appropriate for the value.
   * 
   * @param value the initial value for this JFormattedTextField
   */
  public JFormattedTextField (Object value)
  {
    setValue(value);
  }
  
  /**
   * Returns an AbstractFormatterFactory that will give an appropriate
   * AbstractFormatter for the given Format.
   * @param format the Format to match with an AbstractFormatter.
   * @return a DefaultFormatterFactory whose defaultFormatter is appropriate
   * for the given Format.
   */
  private AbstractFormatterFactory getAppropriateFormatterFactory (Format format)
  {
    AbstractFormatter newFormatter;
    if (format instanceof DateFormat)
      newFormatter = new DateFormatter((DateFormat)format);
    else if (format instanceof NumberFormat)
      newFormatter = new NumberFormatter ((NumberFormat)format);
    else
      newFormatter = new InternationalFormatter(format);
    
    return new DefaultFormatterFactory(newFormatter);
  }

  /**
   * Forces the current value from the editor to be set as the current
   * value.  If there is no current formatted this has no effect.
   * 
   * @throws ParseException if the formatter cannot format the current value
   */
  public void commitEdit ()
    throws ParseException
  {
    if (formatter == null)
      return;
    // Note: this code is a lot like setValue except that we don't want
    // to create a new formatter.
    Object oldValue = this.value;
    
    this.value = formatter.stringToValue(getText());;
    editValid = true;
    
    firePropertyChange("value", oldValue, this.value); 
  }

  /**
   * Gets the command list supplied by the UI augmented by the specific
   * Actions for JFormattedTextField.
   * 
   * @return an array of Actions that this text field supports
   */
  public Action[] getActions ()
  {
    // FIXME: Add JFormattedTextField specific actions
    // These are related to committing or cancelling edits.
    return super.getActions();
  }

  /**
   * Returns the behaviour of this JFormattedTextField upon losing focus.  This
   * is one of <code>COMMIT</code>, <code>COMMIT_OR_REVERT</code>, 
   * <code>PERSIST</code>, or <code>REVERT</code>.  
   * @return the behaviour upon losing focus
   */
  public int getFocusLostBehavior()
  {
    return focusLostBehavior;
  }

  /**
   * Returns the current formatter used for this JFormattedTextField.
   * @return the current formatter used for this JFormattedTextField
   */
  public AbstractFormatter getFormatter ()
  {
    return formatter;
  }
  
  /**
   * Returns the factory currently used to generate formatters for this
   * JFormattedTextField.
   * @return the factory currently used to generate formatters
   */
  public AbstractFormatterFactory getFormatterFactory ()
  {
    return formatterFactory;
  }

  public String getUIClassID ()
  {
    return "FormattedTextFieldUI";
  }

  /**
   * Returns the last valid value.  This may not be the value currently shown 
   * in the text field depending on whether or not the formatter commits on 
   * valid edits and allows invalid input to be temporarily displayed.  
   * @return the last committed valid value
   */
  public Object getValue ()
  {
    return value;
  }

  /**
   * This method is used to provide feedback to the user when an invalid value
   * is input during editing.   
   */
  protected void invalidEdit ()
  {
    UIManager.getLookAndFeel().provideErrorFeedback(this);
  }

  /**
   * Returns true if the current value being edited is valid.  This property is
   * managed by the current formatted.
   * @return true if the value being edited is valid.
   */
  public boolean isEditValid ()
  {
    return editValid;
  }

  /**
   * Processes focus events.  This is overridden because we may want to 
   * change the formatted depending on whether or not this field has 
   * focus.
   * 
   * @param evt the FocusEvent
   */
  protected void processFocusEvent (FocusEvent evt)
  {
    super.processFocusEvent(evt);
    // Let the formatterFactory change the formatter for this text field
    // based on whether or not it has focus.
    setFormatter (formatterFactory.getFormatter(this));
  }
  
  /**
   * Associates this JFormattedTextField with a Document and propagates
   * a PropertyChange event to each listener.
   * 
   * @param newDocument the Document to associate with this text field
   */
  public void setDocument(Document newDocument)
  {
    // FIXME: This method should do more than this.  Must do some handling
    // of the DocumentListeners.
    Document oldDocument = getDocument();

    if (oldDocument == newDocument)
      return;
    
    super.setDocument(newDocument);
  }

  /**
   * Sets the behaviour of this JFormattedTextField upon losing focus.
   * This must be <code>COMMIT</code>, <code>COMMIT_OR_REVERT</code>, 
   * <code>PERSIST</code>, or <code>REVERT</code> or an 
   * IllegalArgumentException will be thrown.
   * 
   * @param behavior
   * @throws IllegalArgumentException if <code>behaviour</code> is not 
   * one of the above
   */
  public void setFocusLostBehavior(int behavior)
  {
    if (behavior != COMMIT
	&& behavior != COMMIT_OR_REVERT
	&& behavior != PERSIST
	&& behavior != REVERT)
      throw new IllegalArgumentException("invalid behavior");

    this.focusLostBehavior = behavior;
  }

  /**
   * Sets the formatter for this JFormattedTextField.  Normally the formatter
   * factory will take care of this, or calls to setValue will also make sure
   * that the formatter is set appropriately.  
   * 
   * @param formatter the AbstractFormatter to use for formatting the value for
   * this JFormattedTextField
   */
  protected void setFormatter (AbstractFormatter formatter)
  {
    AbstractFormatter oldFormatter = null;
    
    oldFormatter = this.formatter;

    if (oldFormatter != null)
      oldFormatter.uninstall();
    
    this.formatter = formatter;
    
    if (formatter != null)
      formatter.install(this);

    firePropertyChange("formatter", oldFormatter, formatter);
  }

  /**
   * Sets the factory from which this JFormattedTextField should obtain 
   * its formatters.  
   * 
   * @param factory the AbstractFormatterFactory that will be used to generate
   * formatters for this JFormattedTextField
   */
  public void setFormatterFactory (AbstractFormatterFactory factory)
  {
    if (formatterFactory == factory)
      return;
    
    AbstractFormatterFactory oldFactory = formatterFactory;
    formatterFactory = factory;
    firePropertyChange("formatterFactory", oldFactory, factory);
    
    // Now set the formatter according to our new factory.
    if (formatterFactory != null)
      setFormatter(formatterFactory.getFormatter(this));
    else
      setFormatter(null);
  }

  /**
   * Sets the value that will be formatted and displayed.
   *   
   * @param newValue the value to be formatted and displayed
   */
  public void setValue (Object newValue)
  {
    if (value == newValue)
      return;

    Object oldValue = value;
    value = newValue;
    
    // If there is no formatterFactory then make one.
    if (formatterFactory == null)
      setFormatterFactory(createFormatterFactory(newValue));
    
    // Set the formatter appropriately.  This is because there may be a new
    // formatterFactory from the line above, or we may want a new formatter
    // depending on the type of newValue (or if newValue is null).
    setFormatter (formatterFactory.getFormatter(this));
    firePropertyChange("value", oldValue, newValue);
  }

  /**
   * A helper method that attempts to create a formatter factory that is 
   * suitable to format objects of the type like <code>value</code>.
   *
   * @param value an object which should be formatted by the formatter factory.
   *
   * @return a formatter factory able to format objects of the class of
   *     <code>value</code>
   */
  AbstractFormatterFactory createFormatterFactory(Object value)
  {
    AbstractFormatter formatter = null;
    if (value instanceof Date)
      formatter = new DateFormatter();
    else if (value instanceof Number)
      formatter = new NumberFormatter();
    else
      formatter = new DefaultFormatter();        
    return new DefaultFormatterFactory(formatter);
  }
}
