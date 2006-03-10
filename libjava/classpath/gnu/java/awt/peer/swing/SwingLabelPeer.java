/* SwingLabelPeer.java -- A Swing based peer for AWT labels
   Copyright (C)  2006  Free Software Foundation, Inc.

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

import java.awt.Image;
import java.awt.Label;
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.peer.LabelPeer;

import javax.swing.JComponent;
import javax.swing.JLabel;


/**
 * A Label peer based on {@link JLabel}.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class SwingLabelPeer
  extends SwingComponentPeer
  implements LabelPeer
{

  /**
   * A spezialized Swing label used to paint the label for the AWT Label. 
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  private class SwingLabel
    extends JLabel
    implements SwingComponent
  {

    /**
     * Returns this label.
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
      processMouseMotionEvent(ev);
    }

    /**
     * Handles key events by forwarding it to <code>processKeyEvent()</code>.
     *
     * @param ev the mouse event
     */
    public void handleKeyEvent(KeyEvent ev)
    {
      processKeyEvent(ev);
    }

    /**
     * Overridden so that this method returns the correct value even without a
     * peer.
     *
     * @return the screen location of the button
     */
    public Point getLocationOnScreen()
    {
      return SwingLabelPeer.this.getLocationOnScreen();
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
      if (SwingLabelPeer.this.awtComponent != null)
        retVal = SwingLabelPeer.this.awtComponent.isShowing();
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
      return SwingLabelPeer.this.createImage(w, h);
    }
    
  }

  /**
   * Creates a new <code>SwingLabelPeer</code> for the specified AWT label.
   *
   * @param label the AWT label
   */
  public SwingLabelPeer(Label label)
  {
    super();
    SwingLabel swingLabel = new SwingLabel();
    swingLabel.setText(label.getText());
    swingLabel.setHorizontalAlignment(label.getAlignment());
    swingLabel.setOpaque(true);
    init(label, swingLabel);
  }

  /**
   * Sets the text of the label. This is implemented to set the text on the
   * Swing label.
   *
   * @param text the text to be set
   */
  public void setText(String text)
  {
    ((JLabel) swingComponent.getJComponent()).setText(text);
  }

  /**
   * Sets the horizontal alignment of the label. This is implemented to
   * set the alignment on the Swing label.
   *
   * @param alignment the horizontal alignment
   *
   * @see Label#LEFT
   * @see Label#RIGHT
   * @see Label#CENTER
   */
  public void setAlignment(int alignment)
  {
    ((JLabel) swingComponent.getJComponent()).setHorizontalAlignment(alignment);
  }

}
