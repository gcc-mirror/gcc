/* SwingCheckboxPeer.java -- A Swing based peer for AWT checkboxes
   Copyright (C)  2007  Free Software Foundation, Inc.

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

import java.awt.Button;
import java.awt.Checkbox;
import java.awt.CheckboxGroup;
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Label;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.peer.CheckboxPeer;

import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JToggleButton;

/**
 * A CheckboxPeer implementation that is backed by the Swing JCheckBox.
 */
public class SwingCheckboxPeer extends SwingComponentPeer implements
        CheckboxPeer {

  /**
   * A spezialized Swing checkbox used to paint the checkbox for the
   * AWT checkbox. 
   */
  private class SwingCheckbox
    extends JCheckBox
    implements SwingComponent
  {
    Checkbox checkbox;

    SwingCheckbox(Checkbox checkbox)
    {
      this.checkbox = checkbox;
    }

    /**
     * Returns this checkbox.
     *
     * @return <code>this</code>
     */
    public JComponent getJComponent()
    {
      return this;
    }

    /**
     * Handles mouse events by forwarding it to
     * <code>processMouseEvent()</code>.
     *
     * @param ev the mouse event
     */
    public void handleMouseEvent(MouseEvent ev)
    {
      ev.setSource(this);
      processMouseEvent(ev);
    }

    /**
     * Handles mouse motion events by forwarding it to
     * <code>processMouseMotionEvent()</code>.
     *
     * @param ev the mouse motion event
     */
    public void handleMouseMotionEvent(MouseEvent ev)
    {
      ev.setSource(this);
      processMouseMotionEvent(ev);
    }

    /**
     * Handles key events by forwarding it to <code>processKeyEvent()</code>.
     *
     * @param ev the mouse event
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

    /**
     * Overridden so that this method returns the correct value even without a
     * peer.
     *
     * @return the screen location of the button
     */
    public Point getLocationOnScreen()
    {
      return SwingCheckboxPeer.this.getLocationOnScreen();
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
      if (checkbox != null)
        retVal = checkbox.isShowing();
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
      return SwingCheckboxPeer.this.createImage(w, h);
    }

    public Graphics getGraphics()
    {
      return SwingCheckboxPeer.this.getGraphics();
    }

    public Container getParent()
    {
      Container par = null;
      if (checkbox != null)
        par = checkbox.getParent();
      return par;
    }

    public void requestFocus() {
      SwingCheckboxPeer.this.requestFocus(awtComponent, false, true, 0);
    }

    public boolean requestFocus(boolean temporary) {
      return SwingCheckboxPeer.this.requestFocus(awtComponent, temporary,
                                                 true, 0);
    }
  }

  /**
   * Listens for ActionEvents on the Swing button and triggers corresponding
   * ActionEvents on the AWT button.
   */
  class SwingCheckboxListener implements ItemListener
  {
    Checkbox awtCheckbox;

    SwingCheckboxListener(Checkbox checkbox)
    {
      awtCheckbox = checkbox;
    }

    /**
     * Receives notification when an action was performend on the button.
     *
     * @param event the action event
     */ 
    public void itemStateChanged(ItemEvent event)
    {
      awtCheckbox.setState(event.getStateChange()==ItemEvent.SELECTED);
      ItemListener[] l = awtCheckbox.getItemListeners();
      if (l.length == 0)
        return;
      ItemEvent ev = new ItemEvent(awtCheckbox, ItemEvent.ITEM_STATE_CHANGED,
                                   awtCheckbox, event.getStateChange());
      for (int i = 0; i < l.length; ++i)
        l[i].itemStateChanged(ev);
    }
  }
    
  /**
   * Creates a new SwingCheckboxPeer instance.
   */
  public SwingCheckboxPeer(Checkbox checkbox)
  {
    SwingCheckbox swingCheckbox = new SwingCheckbox(checkbox);
    swingCheckbox.addItemListener(new SwingCheckboxListener(checkbox));

    init(checkbox, swingCheckbox);
    setLabel(checkbox.getLabel());
    setState(checkbox.getState());
  }

  public void setCheckboxGroup(CheckboxGroup group)
  {
    // TODO: Implement this.
  }

  public void setLabel(String label)
  {
    ((JToggleButton) swingComponent).setText(label);
  }

  public void setState(boolean state)
  {
    ((JToggleButton) swingComponent).setSelected(state);
  }

}
