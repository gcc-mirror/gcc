/* SwingButtonPeer.java -- A Swing based peer for AWT buttons
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

import java.awt.Button;
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.peer.ButtonPeer;

import javax.swing.JButton;
import javax.swing.JComponent;

/**
 * A Swing based peer for the AWT button.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class SwingButtonPeer
  extends SwingComponentPeer
  implements ButtonPeer
{

  /**
   * A specialized Swing button to be used as AWT button.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  class SwingButton
    extends JButton
    implements SwingComponent
  {
    Button button;

    SwingButton(Button button)
    {
      this.button = button;
    }

    /**
     * Overridden so that this method returns the correct value even without a
     * peer.
     *
     * @return the screen location of the button
     */
    public Point getLocationOnScreen()
    {
      return SwingButtonPeer.this.getLocationOnScreen();
    }

    /**
     * Overridden so that the isShowing method returns the correct value for the
     * swing button, even if it has no peer on its own.
     *
     * @return <code>true</code> if the button is currently showing,
     *         <code>false</code> otherwise
     */
    public boolean isShowing()
    {
      boolean retVal = false;
      if (button != null)
        retVal = button.isShowing();
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
      return SwingButtonPeer.this.createImage(w, h);
    }

    /**
     * Overridden, so that the Swing button can create a Graphics without its
     * own peer.
     *
     * @return a graphics instance for the button
     */
    public Graphics getGraphics()
    {
      return SwingButtonPeer.this.getGraphics();
    }

    /**
     * Returns this button.
     *
     * @return this button
     */
    public JComponent getJComponent()
    {
      return this;
    }

    /**
     * Handles mouse events by forwarding it to
     * <code>processMouseEvent()</code> after having retargetted it to this
     * button.
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
     * <code>processMouseMotionEvent()</code> after having retargetted it to
     * this button.
     *
     * @param ev the mouse motion event
     */
    public void handleMouseMotionEvent(MouseEvent ev)
    {
      ev.setSource(this);
      processMouseMotionEvent(ev);
    }

    /**
     * Handles key events by forwarding it to
     * <code>processKeyEvent()</code> after having retargetted it to this
     * button.
     *
     * @param ev the mouse event
     */
    public void handleKeyEvent(KeyEvent ev)
    {
      ev.setSource(this);
      processKeyEvent(ev);
    }

    public Container getParent()
    {
      Container par = null;
      if (button != null)
        par = button.getParent();
      return par;
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

    public void requestFocus() {
        SwingButtonPeer.this.requestFocus(awtComponent, false, true, 0);
    }

    public boolean requestFocus(boolean temporary) {
        return SwingButtonPeer.this.requestFocus(awtComponent, temporary,
                                                 true, 0);
    }
  }

  /**
   * Listens for ActionEvents on the Swing button and triggers corresponding
   * ActionEvents on the AWT button.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  class SwingButtonListener implements ActionListener
  {

    /**
     * Receives notification when an action was performend on the button.
     *
     * @param event the action event
     */
    public void actionPerformed(ActionEvent event)
    {
      Button b = (Button) SwingButtonPeer.this.awtComponent;
      ActionListener[] l = b.getActionListeners();
      if (l.length == 0)
        return;
      ActionEvent ev = new ActionEvent(b, ActionEvent.ACTION_PERFORMED,
                                       b.getActionCommand());
      for (int i = 0; i < l.length; ++i)
        l[i].actionPerformed(ev);
    }

  }

  /**
   * Constructs a new SwingButtonPeer.
   *
   * @param theButton the AWT button for this peer
   */
  public SwingButtonPeer(Button theButton)
  {
    SwingButton button = new SwingButton(theButton);
    button.setText(theButton.getLabel());
    button.addActionListener(new SwingButtonListener());
    init(theButton, button);
  }

  /**
   * Sets the label of the button. This call is forwarded to the setText method
   * of the managed Swing button.
   *
   * @param label the label to set
   */
  public void setLabel(String label)
  {
    ((SwingButton) swingComponent).setText(label);
  }
}
