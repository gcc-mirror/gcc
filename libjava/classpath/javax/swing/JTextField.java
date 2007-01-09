/* JTextField.java --
   Copyright (C) 2002, 2004, 2005, 2006  Free Software Foundation, Inc.

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
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleStateSet;
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
      super();
    }

    /**
     * Returns the accessible state of this <code>AccessibleJTextField</code>.
     *
     * @return the accessible state of this <code>AccessibleJTextField</code>
     */
    public AccessibleStateSet getAccessibleStateSet()
    {
      AccessibleStateSet state = super.getAccessibleStateSet();
      // TODO: Figure out what state must be added here to the super's state.
      return state;
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
  
  /** @since 1.3 */
  private Action action;

  /** @since 1.3 */
  private String actionCommand;
  
  private PropertyChangeListener actionPropertyChangeListener;

  /**
   * The horizontal visibility of the textfield.
   */
  private BoundedRangeModel horizontalVisibility;

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

    // Initialize the horizontal visibility model.
    horizontalVisibility = new DefaultBoundedRangeModel();

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
    return new PlainDocument();
  }

  /**
   * Sets the document to be used for this JTextField.
   *
   * This sets the document property <code>filterNewlines</code> to
   * <code>true</code> and then calls the super behaviour to setup a view and
   * revalidate the text field.
   *
   * @param doc the document to set
   */
  public void setDocument(Document doc)
  {
    doc.putProperty("filterNewlines", Boolean.TRUE);
    super.setDocument(doc);
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
    ActionEvent event = new ActionEvent(this, 0, 
                          actionCommand == null ? getText() : actionCommand);
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

  /**
   * Sets the number of columns and then invalidates the layout.
   * @param columns the number of columns
   * @throws IllegalArgumentException if columns < 0
   */
  public void setColumns(int columns)
  {
    if (columns < 0)
      throw new IllegalArgumentException();

    this.columns = columns;
    invalidate();
    //FIXME: do we need this repaint call?
    repaint();
  }

  /**
   * Returns the horizontal alignment, which is one of: JTextField.LEFT, 
   * JTextField.CENTER, JTextField.RIGHT, JTextField.LEADING, 
   * JTextField.TRAILING.
   * @return the horizontal alignment
   */
  public int getHorizontalAlignment()
  {
    return align;
  }

  /**
   * Sets the horizontal alignment of the text.  Calls invalidate and repaint
   * and fires a property change event.
   * @param newAlign must be one of: JTextField.LEFT, JTextField.CENTER,
   * JTextField.RIGHT, JTextField.LEADING, JTextField.TRAILING.
   * @throws IllegalArgumentException if newAlign is not one of the above.
   */
  public void setHorizontalAlignment(int newAlign)
  {
    //FIXME: should throw an IllegalArgumentException if newAlign is invalid
    if (align == newAlign)
      return;

    int oldAlign = align;
    align = newAlign;
    firePropertyChange("horizontalAlignment", oldAlign, newAlign);
    invalidate();
    repaint();
  }

  /**
   * Sets the current font and revalidates so the font will take effect.
   */
  public void setFont(Font newFont)
  {
    super.setFont(newFont);
    revalidate();
  }

  /**
   * Returns the preferred size.  If there is a non-zero number of columns, 
   * this is the number of columns multiplied by the column width, otherwise
   * it returns super.getPreferredSize().
   */
  public Dimension getPreferredSize()
  {
    Dimension size = super.getPreferredSize();

    if (columns != 0)
      {
        Insets i = getInsets();
        size.width = columns * getColumnWidth() + i.left + i.right;
      }

    return size;
  }

  /**
   * Returns the scroll offset in pixels.
   *
   * @return the scroll offset
   */
  public int getScrollOffset()
  {
    return horizontalVisibility.getValue();
  }

  /**
   * Sets the scroll offset in pixels.
   * 
   * @param offset the scroll offset
   */
  public void setScrollOffset(int offset)
  {
    // Automatically sets to the highest possible value if
    // offset is bigger than that.
    horizontalVisibility.setValue(
                                  Math.min(horizontalVisibility.getMaximum()
                                           - horizontalVisibility.getExtent(),
                                           offset));
    
  }

  /**
   * Returns the set of Actions that are commands for the editor.
   * This is the actions supported by this editor plus the actions
   * of the UI (returned by JTextComponent.getActions()).
   */
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
        actionPropertyChangeListener = createActionPropertyChangeListener(action);
        action.addPropertyChangeListener(actionPropertyChangeListener);
      }

    //FIXME: is this a hack?  The horizontal alignment hasn't changed
    firePropertyChange("horizontalAlignment", oldAction, newAction);
  }

  /**
   * Sets the command string used in action events.
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
   * 
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

  /**
   * Returns the column width, which is the width of the character m
   * for the font in use.
   * @return the width of the character m for the font in use.
   */
  protected int getColumnWidth()
  {
    FontMetrics metrics = getToolkit().getFontMetrics(getFont());
    return metrics.charWidth('m');
  }

  /**
   * Returns the accessible context associated with the <code>JTextField</code>.
   *
   * @return the accessible context associated with the <code>JTextField</code>
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJTextField();
    return accessibleContext;
  }

  /**
   * Returns the bounded range model that describes the horizontal visibility
   * of the text field in the case when the text does not fit into the
   * available space. The actual values of this model are managed by the look
   * and feel implementation.
   *
   * @return the bounded range model that describes the horizontal visibility
   */
  public BoundedRangeModel getHorizontalVisibility()
  {
    return horizontalVisibility;
  }

  /**
   * Returns <code>true</code>, unless this is embedded in a
   * <code>JViewport</code> in which case the viewport takes responsibility of
   * validating.
   *
   * @return <code>true</code>, unless this is embedded in a
   *         <code>JViewport</code> in which case the viewport takes
   *         responsibility of validating
   */
  public boolean isValidateRoot()
  {
    return ! (getParent() instanceof JViewport);
  }
  
  public void scrollRectToVisible(Rectangle r)
  {
    int v = horizontalVisibility.getValue();
    
    // The extent value is the inner width of the text field.
    int e = horizontalVisibility.getExtent();
    Insets i = getInsets();
    
    // The x value in the rectangle (usually) denotes the new location
    // of the caret. We check whether the location lies inside the left or
    // right border and scroll into the appropriate direction.
    // The calculation has to be shifted by the BoundedRangeModel's value
    // because that value was already used to calculate r.x (this happens
    // as part of a modelToView() call in FieldView).
    if (r.x < i.left)
      setScrollOffset(v + r.x - i.left);
    else if (r.x > e + i.left)
      setScrollOffset(r.x + v - e - i.left);
  }
  
}
