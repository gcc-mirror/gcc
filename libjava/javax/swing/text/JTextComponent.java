/* JTextComponent.java --
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

package javax.swing.text;

import java.awt.AWTEvent;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.InputMethodListener;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleStateSet;
import javax.accessibility.AccessibleText;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JViewport;
import javax.swing.KeyStroke;
import javax.swing.Scrollable;
import javax.swing.UIManager;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.plaf.TextUI;


public abstract class JTextComponent extends JComponent
  implements Scrollable, Accessible
{
//    public class AccessibleJTextComponent extends AccessibleJComponent
//      implements AccessibleText, CaretListener, DocumentListener,
//                 AccessibleAction, AccessibleEditableText
//    {
//    }

  /**
   * AccessibleJTextComponent
   */
  public class AccessibleJTextComponent extends AccessibleJComponent
    implements AccessibleText, CaretListener, DocumentListener
  {
    private static final long serialVersionUID = 7664188944091413696L;

    /**
     * caretPos
     */
    int caretPos;

    /**
     * Constructor AccessibleJTextComponent
     * @param component TODO
     */
    public AccessibleJTextComponent()
    {
    }

    /**
     * getCaretPosition
     * @return int
     */
    public int getCaretPosition()
    {
      return 0; // TODO
    }

    /**
     * getSelectedText
     * @return String
     */
    public String getSelectedText()
    {
      return null; // TODO
    }

    /**
     * getSelectionStart
     * @return int
     */
    public int getSelectionStart()
    {
      return 0; // TODO
    }

    /**
     * getSelectionEnd
     * @return int
     */
    public int getSelectionEnd()
    {
      return 0; // TODO
    }

    /**
     * caretUpdate
     * @param value0 TODO
     */
    public void caretUpdate(CaretEvent value0)
    {
      // TODO
    }

    /**
     * getAccessibleStateSet
     * @return AccessibleStateSet
     */
    public AccessibleStateSet getAccessibleStateSet()
    {
      return null; // TODO
    }

    /**
     * getAccessibleRole
     * @return AccessibleRole
     */
    public AccessibleRole getAccessibleRole()
    {
      return null; // TODO
    }

    /**
     * getAccessibleText
     * @return AccessibleText
     */
    public AccessibleText getAccessibleText()
    {
      return null; // TODO
    }

    /**
     * insertUpdate
     * @param value0 TODO
     */
    public void insertUpdate(DocumentEvent value0)
    {
      // TODO
    }

    /**
     * removeUpdate
     * @param value0 TODO
     */
    public void removeUpdate(DocumentEvent value0)
    {
      // TODO
    }

    /**
     * changedUpdate
     * @param value0 TODO
     */
    public void changedUpdate(DocumentEvent value0)
    {
      // TODO
    }

    /**
     * getIndexAtPoint
     * @param value0 TODO
     * @return int
     */
    public int getIndexAtPoint(Point value0)
    {
      return 0; // TODO
    }

    /**
     * getRootEditorRect
     * @return Rectangle
     */
    Rectangle getRootEditorRect()
    {
      return null;
    }

    /**
     * getCharacterBounds
     * @param value0 TODO
     * @return Rectangle
     */
    public Rectangle getCharacterBounds(int value0)
    {
      return null; // TODO
    }

    /**
     * getCharCount
     * @return int
     */
    public int getCharCount()
    {
      return 0; // TODO
    }

    /**
     * getCharacterAttribute
     * @param value0 TODO
     * @return AttributeSet
     */
    public AttributeSet getCharacterAttribute(int value0)
    {
      return null; // TODO
    }

    /**
     * getAtIndex
     * @param value0 TODO
     * @param value1 TODO
     * @return String
     */
    public String getAtIndex(int value0, int value1)
    {
      return null; // TODO
    }

    /**
     * getAfterIndex
     * @param value0 TODO
     * @param value1 TODO
     * @return String
     */
    public String getAfterIndex(int value0, int value1)
    {
      return null; // TODO
    }

    /**
     * getBeforeIndex
     * @param value0 TODO
     * @param value1 TODO
     * @return String
     */
    public String getBeforeIndex(int value0, int value1)
    {
      return null; // TODO
    }
  }

  public static class KeyBinding
  {
    public KeyStroke key;
    public String actionName;

    public KeyBinding(KeyStroke key, String actionName)
    {
      this.key = key;
      this.actionName = actionName;
    }
  }

  private static final long serialVersionUID = -8796518220218978795L;
  
  public static final String DEFAULT_KEYMAP = "default";
  public static final String FOCUS_ACCELERATOR_KEY = "focusAcceleratorKey";

  private Document doc;
  private Caret caret;
  private boolean editable;

  public JTextComponent()
  {
    enableEvents(AWTEvent.KEY_EVENT_MASK);
    updateUI();
  }

  public void setDocument(Document s)
  {
    doc = s;
    revalidate();
    repaint();
  }

  public Document getDocument()
  {
    if (doc == null)
      System.out.println("doc == null !!!");
    return doc;
  }

  /**
   * Get the AccessibleContext of this object
   */
  public AccessibleContext getAccessibleContext()
  {
    return null;
  }

  public Insets getMargin()
  {
    // FIXME: Not implemented.
    return null;
  }

  public void setText(String text)
  {
    try
      {
	getDocument().remove(0, doc.getLength());
	getDocument().insertString(0, text, null);
      }
    catch (BadLocationException e)
      {
      }
  }

  /**
   * Retrieves the current text in this text document.
   *
   * @return the text
   *
   * @exception NullPointerException if the underlaying document is null
   */
  public String getText()
  {
    return getDocument().getText(0, getDocument().getLength());
  }

  /**
   * Retrieves a part of the current text in this document.
   *
   * @param offset the postion of the first character
   * @param length the length of the text to retrieve
   *
   * @return the text
   *
   * @exception BadLocationException if arguments do not hold pre-conditions
   */
  public String getText(int offset, int length)
    throws BadLocationException
  {
    return getDocument().getText(offset, length);
  }

  /**
   * Returns a string that specifies the name of the l&amp;f class
   * that renders this component.
   *
   * @return the string "TextComponentUI"
   */
  public String getUIClassID()
  {
    return "TextComponentUI";
  }

  /**
   * Returns a string representation of this JTextComponent.
   */
  protected String paramString()
  {
    return "JTextComponent";
  }

  public TextUI getUI()
  {
    return (TextUI) UIManager.getUI(this);
  }

  public void updateUI()
  {
    setUI(getUI());
  }

  public Dimension getPreferredScrollableViewportSize()
  {
    return null;
  }

  public int getScrollableUnitIncrement(Rectangle visible, int orientation,
                                        int direction)
  {
    return 0;
  }

  public int getScrollableBlockIncrement(Rectangle visible, int orientation,
                                         int direction)
  {
    return 0;
  }

  /**
   * Checks whether this text component it editable.
   *
   * @return true if editable, false otherwise
   */
  public boolean isEditable()
  {
    return editable;
  }

  /**
   * Enables/disabled this text component's editability.
   *
   * @param editable true to make it editable, false otherwise.
   */
  public void setEditable(boolean editable)
  {
    firePropertyChange("editable", this.editable, editable);
    this.editable = editable;
  }

  /**
   * The <code>Caret</code> object used in this text component.
   *
   * @return the caret object
   */
  public Caret getCaret()
  {
    return caret;
  }

  /**
   * Retrisves the current caret position.
   *
   * @return the current position
   */
  public int getCaretPosition()
  {
    return caret.getDot();
  }

  /**
   * Sets the caret to a new position.
   *
   * @param position the new position
   */
  public void setCaretPosition(int position)
  {
    if (doc == null)
      return;

    if (position < 0 || position > doc.getLength())
      throw new IllegalArgumentException();

    caret.setDot(position);
  }

  /**
   * Moves the caret to a given position. This selects the text between
   * the old and the new position of the caret.
   */
  public void moveCaretPosition(int position)
  {
    if (doc == null)
      return;

    if (position < 0 || position > doc.getLength())
      throw new IllegalArgumentException();

    caret.moveDot(position);
  }

  /**
   * Returns the start postion of the currently selected text.
   *
   * @return the start postion
   */
  public int getSelectionStart()
  {
    return Math.min(caret.getDot(), caret.getMark());
  }

  /**
   * Selects the text from the given postion to the selection end position.
   *
   * @param end the start positon of the selected text.
   */
  public void setSelectionStart(int start)
  {
    select(start, getSelectionEnd());
  }

  /**
   * Returns the end postion of the currently selected text.
   *
   * @return the end postion
   */
  public int getSelectionEnd()
  {
    return Math.max(caret.getDot(), caret.getMark());
  }

  /**
   * Selects the text from the selection start postion to the given position.
   *
   * @param end the end positon of the selected text.
   */
  public void setSelectionEnd(int end)
  {
    select(getSelectionStart(), end);
  }

  /**
   * Selects a part of the content of the text component.
   *
   * @param start the start position of the selected text
   * @param ent the end position of the selected text
   */
  public void select(int start, int end)
  {
    int length = doc.getLength();
    
    start = Math.max(start, 0);
    start = Math.min(start, length);

    end = Math.max(end, 0);
    end = Math.min(end, length);

    setCaretPosition(start);
    moveCaretPosition(end);
  }

  /**
   * Selects the whole content of the text component.
   */
  public void selectAll()
  {
    select(0, doc.getLength());
  }

  public boolean getScrollableTracksViewportHeight()
  {
    if (getParent() instanceof JViewport)
      return ((JViewport) getParent()).getHeight() > getPreferredSize().height;

    return false;
  }

  public boolean getScrollableTracksViewportWidth()
  {
    if (getParent() instanceof JViewport)
      return ((JViewport) getParent()).getWidth() > getPreferredSize().width;

    return false;
  }

  /**
   * Adds a <code>CaretListener</code> object to this text component.
   *
   * @param listener the listener to add
   */
  public void addCaretListener(CaretListener listener)
  {
    listenerList.add(CaretListener.class, listener);
  }

  /**
   * Removed a <code>CaretListener</code> object from this text component.
   *
   * @param listener the listener to remove
   */
  public void removeCaretListener(CaretListener listener)
  {
    listenerList.remove(CaretListener.class, listener);
  }

  /**
   * Returns all added <code>CaretListener</code> objects.
   *
   * @return an array of listeners
   */
  public CaretListener[] getCaretListeners()
  {
    return (CaretListener[]) getListeners(CaretListener.class);
  }

  /**
   * Notifies all registered <code>CaretListener</code> objects that the caret
   * was updated.
   *
   * @param event the event to send
   */
  protected void fireCaretUpdate(CaretEvent event)
  {
    CaretListener[] listeners = getCaretListeners();

    for (int index = 0; index < listeners.length; ++index)
      listeners[index].caretUpdate(event);
  }

  /**
   * Adds an <code>InputListener</code> object to this text component.
   *
   * @param listener the listener to add
   */
  public void addInputMethodListener(InputMethodListener listener)
  {
    listenerList.add(InputMethodListener.class, listener);
  }

  /**
   * Removes an <code>InputListener</code> object from this text component.
   *
   * @param listener the listener to remove
   */
  public void removeInputMethodListener(InputMethodListener listener)
  {
    listenerList.remove(InputMethodListener.class, listener);
  }

  /**
   * Returns all added <code>InputMethodListener</code> objects.
   *
   * @return an array of listeners
   */
  public InputMethodListener[] getInputMethodListeners()
  {
    return (InputMethodListener[]) getListeners(InputMethodListener.class);
  }
}
