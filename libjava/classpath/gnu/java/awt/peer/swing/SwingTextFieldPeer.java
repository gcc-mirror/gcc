/* SwingTextFieldPeer.java -- A Swing based peer for AWT textfields
   Copyright (C)  2006, 2007  Free Software Foundation, Inc.

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
package gnu.java.awt.peer.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.TextField;
import java.awt.event.FocusEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.im.InputMethodRequests;
import java.awt.peer.TextFieldPeer;

import javax.swing.JComponent;
import javax.swing.JTextField;

/**
 * A TextFieldPeer based on Swing JTextField.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class SwingTextFieldPeer
  extends SwingComponentPeer
  implements TextFieldPeer
{

  /**
   * A specialized Swing textfield for use in the peer.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  private class SwingTextField
    extends JTextField
    implements SwingComponent
  {

    TextField textField;

    SwingTextField(TextField textField)
    {
      this.textField = textField; 
    }

    /**
     * Overridden to provide normal behaviour even without a real peer
     * attached.
     *
     * @return the location of the textfield on screen
     */
    public Point getLocationOnScreen()
    {
      return SwingTextFieldPeer.this.getLocationOnScreen();
    }

    /**
     * Overridden so that the isShowing method returns the correct value
     * for the swing button, even if it has no peer on its own.
     *
     * @return <code>true</code> if the button is currently showing,
     *         <code>false</code> otherwise
     */
    public boolean isShowing()
    {
      boolean retVal = false;
      if (textField != null)
        retVal = textField.isShowing();
      return retVal;
    }

    /**
     * Overridden, so that the Swing button can create an Image without its
     * own peer.
     *
     * @param w the width of the image
     * @param h the height of the image
     *
     * @return an image
     */
    public Image createImage(int w, int h)
    {
      return SwingTextFieldPeer.this.createImage(w, h);
    }

    /**
     * Returns this textfield.
     *
     * @return <code>this</code>
     */
    public JComponent getJComponent()
    {
      return this;
    }

    /**
     * Handles mouse events by forwarding it to the swing textfield.
     *
     * @param ev the mouse event
     */
    public void handleMouseEvent(MouseEvent ev)
    {
      ev.setSource(this);
      processMouseEvent(ev);
    }

    /**
     * Handles mouse motion events by forwarding it to the swing textfield.
     *
     * @param ev the mouse motion event
     */
    public void handleMouseMotionEvent(MouseEvent ev)
    {
      ev.setSource(this);
      processMouseMotionEvent(ev);
    }

    /**
     * Handles key events by forwarding it to the swing textfield.
     *
     * @param ev the key event
     */
    public void handleKeyEvent(KeyEvent ev)
    {
      ev.setSource(this);
      processKeyEvent(ev);
    }

    /**
     * Handles focus events by forwarding it to
     * <code>processFocusEvent()</code>.
     *
     * @param ev the Focus event
     */
    public void handleFocusEvent(FocusEvent ev)
    {
      processFocusEvent(ev);
    }

    
    public Container getParent()
    {
      Container par = null;
      if (textField != null)
        par = textField.getParent();
      return par;
    }

    public Graphics getGraphics()
    {
      return SwingTextFieldPeer.this.getGraphics();
    }
    
    public void requestFocus() {
        SwingTextFieldPeer.this.requestFocus(awtComponent, false, true, 0);
    }

    public boolean requestFocus(boolean temporary) {
        return SwingTextFieldPeer.this.requestFocus(awtComponent, temporary,
                                                    true, 0);
    }

  }

  /**
   * Creates a new <code>SwingTextFieldPeer</code> instance for the specified
   * AWT textfield.
   *
   * @param textField the AWT textfield
   */
  public SwingTextFieldPeer(TextField textField)
  {
    SwingTextField swingTextField = new SwingTextField(textField);
    swingTextField.setText(textField.getText());
    init(textField, swingTextField);
  }

  /**
   * Returns the minimum size of the textfield.
   *
   * @param len not used here
   *
   * @return the minimum size of the textfield
   */
  public Dimension minimumSize(int len)
  {
    return swingComponent.getJComponent().getMinimumSize();
  }

  /**
   * Returns the preferred size of the textfield.
   *
   * @param len not used here
   *
   * @return the preferred size of the textfield
   */
  public Dimension preferredSize(int len)
  {
    return swingComponent.getJComponent().getPreferredSize();
  }

  /**
   * Returns the minimum size of the textfield.
   *
   * @param len not used here
   *
   * @return the minimum size of the textfield
   */
  public Dimension getMinimumSize(int len)
  {
    return swingComponent.getJComponent().getMinimumSize();
  }

  /**
   * Returns the preferred size of the textfield.
   *
   * @param len not used here
   *
   * @return the preferred size of the textfield
   */
  public Dimension getPreferredSize(int len)
  {
    return swingComponent.getJComponent().getPreferredSize();
  }

  /**
   * Sets the echo character.
   *
   * @param echoChar the echo character to be set
   */
  public void setEchoChar(char echoChar)
  {
    // TODO: Must be implemented.
  }

  /**
   * Sets the echo character.
   *
   * @param echoChar the echo character to be set
   */
  public void setEchoCharacter(char echoChar)
  {
    // TODO: Must be implemented.
  }

  /**
   * Returns the end index of the current selection.
   *
   * @return the end index of the current selection
   */
  public int getSelectionEnd()
  {
    // TODO: Must be implemented.
    return 0;
  }

  /**
   * Returns the start index of the current selection.
   *
   * @return the start index of the current selection
   */
  public int getSelectionStart()
  {
    // TODO: Must be implemented.
    return 0;
  }

  /**
   * Returns the current content of the textfield.
   *
   * @return the current content of the textfield
   */
  public String getText()
  {
    return ((JTextField) swingComponent.getJComponent()).getText();
  }

  /**
   * Sets the content of the textfield.
   *
   * @param text the text to set
   */
  public void setText(String text)
  {
    ((JTextField) swingComponent.getJComponent()).setText(text);
  }

  /**
   * Sets the current selection.
   *
   * @param startPos the start index of the selection
   * @param endPos the start index of the selection
   */
  public void select(int start_pos, int endPos)
  {
    // TODO: Must be implemented.
  }

  /**
   * Sets the editable flag of the text field.
   *
   * @param editable <code>true</code> to make the textfield editable,
   *        <code>false</code> to make it uneditable
   */
  public void setEditable(boolean editable)
  {
    ((JTextField) swingComponent.getJComponent()).setEditable(editable);
  }

  /**
   * Returns the current caret position.
   *
   * @return the current caret position
   */
  public int getCaretPosition()
  {
    return ((JTextField) swingComponent.getJComponent()).getCaret().getDot();
  }

  /**
   * Sets the current caret position.
   *
   * @param pos the caret position to set
   */
  public void setCaretPosition(int pos)
  {
    ((JTextField) swingComponent.getJComponent()).getCaret().setDot(pos);
  }

  /**
   * Returns the index of the character at the specified location.
   *
   * @param x the X coordinate of the point to query
   * @param y the Y coordinate of the point to query
   *
   * @return the index of the character at the specified location
   */
  public int getIndexAtPoint(int x, int y)
  {
    // TODO: Must be implemented.
    return 0;
  }

  /**
   * Returns the bounds of the character at the specified index.
   *
   * @param pos the index of the character
   *
   * @return the bounds of the character at the specified index
   */
  public Rectangle getCharacterBounds(int pos)
  {
    // TODO: Must be implemented.
    return null;
  }

  /**
   * Not used.
   */
  public long filterEvents(long filter)
  {
    // TODO: Must be implemented.
    return 0;
  }

  /**
   * Not used.
   */
  public InputMethodRequests getInputMethodRequests()
  {
    // TODO: Must be implemented.
    return null;
  }

}
