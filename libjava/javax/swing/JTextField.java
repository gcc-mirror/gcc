/* JTextField.java --
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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

import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.accessibility.AccessibleStateSet;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;
import javax.swing.text.PlainDocument;


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

  public static final String notifyAction = "notify-field-accept";
  
  private int columns;

  private int align;

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
    int oldAlign = align;
    align = newAlign;
    invalidate();
    repaint();
    firePropertyChange("horizontalAlignment", oldAlign, newAlign);
  }

  public void setFont(Font newFont)
  {
    super.setFont(newFont);
    revalidate();
  }

  public Dimension getPreferredSize()
  {
    Dimension size;
    FontMetrics fm = getFontMetrics(getFont());
    int fontHeight = fm.getMaxAscent() + fm.getMaxDescent();
    int columnWidth = fm.charWidth('m');
    
    if (columns != 0)
      {
	size = new Dimension(columns * columnWidth + 4, fontHeight + 4);
      }
    else
      {
	size = new Dimension(10, 10);
      }

    return size;
  }
}
