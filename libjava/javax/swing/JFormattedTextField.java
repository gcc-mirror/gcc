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


package javax.swing;

import java.awt.event.FocusEvent;
import java.io.Serializable;
import java.text.Format;
import java.text.ParseException;

import javax.swing.text.Document;
import javax.swing.text.DocumentFilter;
import javax.swing.text.NavigationFilter;

/**
 * @author Michael Koch
 * @since 1.4
 */
public class JFormattedTextField extends JTextField
{
  private static final long serialVersionUID = 5464657870110180632L;

  public abstract static class AbstractFormatter implements Serializable
  {
    private static final long serialVersionUID = -5193212041738979680L;
    
    private JFormattedTextField textField;
    
    public AbstractFormatter ()
    {
      //Do nothing here.
    }

    protected Object clone ()
      throws CloneNotSupportedException
    {
      throw new InternalError ("not implemented");
    }

    protected Action[] getActions ()
    {
      return textField.getActions();
    }

    protected DocumentFilter getDocumentFilter ()
    {
      throw new InternalError ("not implemented");
    }

    protected JFormattedTextField getFormattedTextField ()
    {
      return textField;
    }

    protected NavigationFilter getNavigationFilter ()
    {
      return textField.getNavigationFilter();
    }

    public void install(JFormattedTextField textField)
    {
      if (this.textField != null)
	uninstall();
      
      this.textField = textField;
    }

    public void uninstall ()
    {
      this.textField = null;
    }

    protected void invalidEdit ()
    {
      textField.invalidEdit();
    }

    protected void setEditValid (boolean valid)
    {
      textField.editValid = valid;
    }

    public abstract Object stringToValue (String text)
      throws ParseException;

    public abstract String valueToString (Object value)
      throws ParseException;
  }
  
  public abstract static class AbstractFormatterFactory
  {
    public AbstractFormatterFactory ()
    {
      // Do nothing here.
    }

    public abstract AbstractFormatter getFormatter (JFormattedTextField tf);
  }

  static class FormatterFactoryWrapper extends AbstractFormatterFactory
  {
    AbstractFormatter formatter;

    public FormatterFactoryWrapper(AbstractFormatter formatter)
    {
      this.formatter = formatter;
    }

    public AbstractFormatter getFormatter(JFormattedTextField tf)
    {
      return formatter;
    }
  }

  public static final int COMMIT = 0;
  public static final int COMMIT_OR_REVERT = 1;
  public static final int REVERT = 2;
  public static final int PERSIST = 3;

  private Object value;
  private int focusLostBehavior = COMMIT_OR_REVERT;
  private AbstractFormatterFactory formatterFactory;
  private boolean editValid = true;
  
  public JFormattedTextField ()
  {
    this((AbstractFormatterFactory) null, null);
  }

  public JFormattedTextField (Format format)
  {
    throw new InternalError ("not implemented");
  }

  public JFormattedTextField (AbstractFormatter formatter)
  {
    this(new FormatterFactoryWrapper(formatter), null);
  }

  public JFormattedTextField (AbstractFormatterFactory factory)
  {
    this(factory, null);
  }

  public JFormattedTextField (AbstractFormatterFactory factory, Object value)
  {
    this.formatterFactory = factory;
    this.value = value;
  }

  public JFormattedTextField (Object value)
  {
    this.value = value;
  }

  public void commitEdit ()
    throws ParseException
  {
    throw new InternalError ("not implemented");
  }

  public Action[] getActions ()
  {
    throw new InternalError ("not implemented");
  }

  public int getFocusLostBehavior()
  {
    return focusLostBehavior;
  }

  public AbstractFormatter getFormatter ()
  {
    if (formatterFactory == null)
      return null;
    
    return formatterFactory.getFormatter(this);
  }

  public AbstractFormatterFactory getFormatterFactory ()
  {
    return formatterFactory;
  }

  public String getUIClassID ()
  {
    return "FormattedTextFieldUI";
  }

  public Object getValue ()
  {
    return value;
  }

  protected void invalidEdit ()
  {
    UIManager.getLookAndFeel().provideErrorFeedback(this);
  }

  public boolean isEditValid ()
  {
    return editValid;
  }

  protected void processFocusEvent (FocusEvent evt)
  {
    throw new InternalError ("not implemented");
  }

  public void setDocument(Document newDocument)
  {
    Document oldDocument = getDocument();

    if (oldDocument == newDocument)
      return;
    
    super.setDocument(newDocument);
  }

  public void setFocusLostBehavior(int behavior)
  {
    if (behavior != COMMIT
	&& behavior != COMMIT_OR_REVERT
	&& behavior != PERSIST
	&& behavior != REVERT)
      throw new IllegalArgumentException("invalid behavior");

    this.focusLostBehavior = behavior;
  }

  protected void setFormatter (AbstractFormatter formatter)
  {
    AbstractFormatter oldFormatter = null;
    
    if (formatterFactory != null)
      oldFormatter = formatterFactory.getFormatter(this);

    if (oldFormatter == formatter)
      return;

    setFormatterFactory(new FormatterFactoryWrapper(formatter));
    firePropertyChange("formatter", oldFormatter, formatter);
  }

  public void setFormatterFactory (AbstractFormatterFactory factory)
  {
    if (formatterFactory == factory)
      return;
    
    AbstractFormatterFactory oldFactory = formatterFactory;
    formatterFactory = factory;
    firePropertyChange("formatterFactory", oldFactory, factory);
  }

  public void setValue (Object newValue)
  {
    if (value == newValue)
      return;
    
    Object oldValue = value;
    value = newValue;
    firePropertyChange("value", oldValue, newValue);
  }
}
