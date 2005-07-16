/* JTextField.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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

import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.accessibility.AccessibleStateSet;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;
import javax.swing.text.PlainDocument;
import javax.swing.text.TextAction;

public class JTextField extends JTextComponent
  implements SwingConstants
{
  /**
   * AccessibleJTextField
   */
  protected class AccessibleJTextField extends AccessibleJTextComponent
  {
    private static final long serialVersionUID = 8255147276740453036L;

    /**
     * Constructor AccessibleJTextField
     */
    protected AccessibleJTextField()
    {
    }

    /**
     * getAccessibleStateSet
     * @return AccessibleStateSet
     */
    public AccessibleStateSet getAccessibleStateSet()
    {
      return null;
    }
  }

  private static final long serialVersionUID = 353853209832607592L;

  private static final Action[] actions;

  /**
   * Name of the action that gets sent when the content of the text field
   * gets accepted.
   */
  public static final String notifyAction = "notify-field-accept";
  
  static
  {
    actions = new Action[1];
    actions[0] = new TextAction(notifyAction)
      {
	public void actionPerformed(ActionEvent event)
	{
	  JTextField textField = (JTextField) event.getSource();
	  textField.fireActionPerformed();
	}
      };
  }
  
  private int columns;
  private int align;
  private int scrollOffset;

  /** @since 1.3 */
  private Action action;

  /** @since 1.3 */
  private String actionCommand;
  
  private PropertyChangeListener actionPropertyChangeListener;

  /**
   * Creates a new instance of <code>JTextField</code>.
   */
  public JTextField()
  {
    this(null, null, 0);
  }

  /**
   * Creates a new instance of <code>JTextField</code>.
   *
   * @param text the initial text
   */
  public JTextField(String text)
  {
    this(null, text, 0);
  }
  
  /**
   * Creates a new instance of <code>JTextField</code>.
   *
   * @param columns the number of columns
   *
   * @exception IllegalArgumentException if columns %lt; 0
   */
  public JTextField(int columns)
  {
    this(null, null, columns);
  }

  /**
   * Creates a new instance of <code>JTextField</code>.
   *
   * @param text the initial text
   * @param columns the number of columns
   *
   * @exception IllegalArgumentException if columns %lt; 0
   */
  public JTextField(String text, int columns)
  {
    this(null, text, columns);
  }

  /**
   * Creates a new instance of <code>JTextField</code>.
   *
   * @param doc the document to use
   * @param text the initial text
   * @param columns the number of columns
   *
   * @exception IllegalArgumentException if columns %lt; 0
   */
  public JTextField(Document doc, String text, int columns)
  {
    if (columns < 0)
      throw new IllegalArgumentException();
    
    this.columns = columns;
    
    setDocument(doc == null ? createDefaultModel() : doc);

    if (text != null)
      setText(text);

    // default value for alignment
    align = LEADING;
  }

  /**
   * Creates the default model for this text field.
   * This implementation returns an instance of <code>PlainDocument</code>.
   *
   * @return a new instance of the default model
   */
  protected Document createDefaultModel()
  {
    // subclassed to swallow newlines
    return new PlainDocument() {
        public void insertString(int offset, String str, AttributeSet a)
          throws BadLocationException
        {
          if (str.indexOf('\n') == -1)
            super.insertString(offset, str, a);
        }
      };
  }

  /**
   * Returns the class ID for the UI.
   *
   * @return "TextFieldUI";
   */
  public String getUIClassID()
  {
    return "TextFieldUI";
  }

  /**
   * Adds a new listener object to this text field.
   *
   * @param listener the listener to add
   */
  public void addActionListener(ActionListener listener)
  {
    listenerList.add(ActionListener.class, listener);
  }

  /**
   * Removes a listener object from this text field.
   *
   * @param listener the listener to remove
   */
  public void removeActionListener(ActionListener listener)
  {
    listenerList.remove(ActionListener.class, listener);
  }

  /**
   * Returns all registered <code>ActionListener</code> objects.
   *
   * @return an array of listeners
   *
   * @since 1.4
   */
  public ActionListener[] getActionListeners()
  {
    return (ActionListener[]) getListeners(ActionListener.class);
  }

  /**
   * Sends an action event to all registered
   * <code>ActionListener</code> objects.
   */
  protected void fireActionPerformed()
  {
    ActionEvent event = new ActionEvent(this, 0, notifyAction);
    ActionListener[] listeners = getActionListeners();

    for (int index = 0; index < listeners.length; ++index)
      listeners[index].actionPerformed(event);
  }

  /**
   * Returns the number of columns of this text field.
   *
   * @return the number of columns
   */
  public int getColumns()
  {
    return columns;
  }

  public void setColumns(int columns)
  {
    if (columns < 0)
      throw new IllegalArgumentException();

    this.columns = columns;
    invalidate();
    repaint();
  }

  public int getHorizontalAlignment()
  {
    return align;
  }

  public void setHorizontalAlignment(int newAlign)
  {
    if (align == newAlign)
      return;

    int oldAlign = align;
    align = newAlign;
    firePropertyChange("horizontalAlignment", oldAlign, newAlign);
    invalidate();
    repaint();
  }

  public void setFont(Font newFont)
  {
    super.setFont(newFont);
    revalidate();
  }

  public Dimension getPreferredSize()
  {
    Dimension size = super.getPreferredSize();

    if (columns != 0)
      size.width = columns * getColumnWidth();

    return size;
  }

  /**
   * Returns the scroll offset in pixels.
   *
   * @return the scroll offset
   */
  public int getScrollOffset()
  {
    return scrollOffset;
  }

  /**
   * Sets the scroll offset in pixels.
   * 
   * @param offset the scroll offset
   */
  public void setScrollOffset(int offset)
  {
    scrollOffset = offset;
  }

  public Action[] getActions()
  {
    return TextAction.augmentList(super.getActions(), actions);
  }

  public void postActionEvent()
  {
    String command = actionCommand != null ? actionCommand : getText();
    ActionEvent event = new ActionEvent(this, 0, command);
    ActionListener[] listeners = getActionListeners();

    for (int index = 0; index < listeners.length; ++index)
      listeners[index].actionPerformed(event);
  }
  
  /**
   * @since 1.3
   */
  public Action getAction()
  {
    return action;
  }

  /**
   * @since 1.3
   */
  public void setAction(Action newAction)
  {
    if (action == newAction)
      return;

    if (action != null)
      {
	removeActionListener(action);
	action.removePropertyChangeListener(actionPropertyChangeListener);
	actionPropertyChangeListener = null;
      }
    
    Action oldAction = action;
    action = newAction;

    if (action != null)
      {
	addActionListener(action);
	actionPropertyChangeListener =
	  createActionPropertyChangeListener(action);
	action.addPropertyChangeListener(actionPropertyChangeListener);
      }
    
    firePropertyChange("horizontalAlignment", oldAction, newAction);
  }

  /**
   * @since 1.3
   */
  public void setActionCommand(String command)
  {
    actionCommand = command;
  }

  /**
   * @since 1.3
   */
  protected PropertyChangeListener createActionPropertyChangeListener(Action action)
  {
    return new PropertyChangeListener()
      {
	public void propertyChange(PropertyChangeEvent event)
	{
	  // Update properties "action" and "horizontalAlignment".
	  String name = event.getPropertyName();

	  if (name.equals("enabled"))
	    {
	      boolean enabled = ((Boolean) event.getNewValue()).booleanValue();
	      JTextField.this.setEnabled(enabled);
	    }
	  else if (name.equals(Action.SHORT_DESCRIPTION))
	    {
	      JTextField.this.setToolTipText((String) event.getNewValue());
	    }
	}
      };
  }

  /**
   * @since 1.3
   */
  protected void configurePropertiesFromAction(Action action)
  {
    if (action != null)
      {
	setEnabled(action.isEnabled());
	setToolTipText((String) action.getValue(Action.SHORT_DESCRIPTION));
      }
    else
      {
	setEnabled(true);      
	setToolTipText(null);
      }
  }

  protected int getColumnWidth()
  {
    FontMetrics metrics = getToolkit().getFontMetrics(getFont());
    return metrics.charWidth('m');
  }
}
