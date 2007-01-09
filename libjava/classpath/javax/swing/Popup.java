/* Popup.java --
   Copyright (C) 2003 Free Software Foundation, Inc.

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

import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Point;
import java.awt.Rectangle;


/**
 * Manages a popup window that displays a Component on top of
 * everything else.
 *
 * <p>To obtain an instance of <code>Popup</code>, use the
 * {@link javax.swing.PopupFactory}.
 *
 * @since 1.4
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class Popup
{
  /**
   * Constructs a new <code>Popup</code> given its owner,
   * contents and the screen position where the popup
   * will appear.
   *
   * @param owner the Component to which <code>x</code> and
   *        <code>y</code> are relative, or <code>null</code> for
   *        placing the popup relative to the origin of the screen.
   *
   * @param contents the contents that will be displayed inside
   *        the <code>Popup</code>.
   *
   * @param x the horizontal position where the Popup will appear.
   *
   * @param y the vertical position where the Popup will appear.
   *
   * @throws IllegalArgumentException if <code>contents</code>
   *         is <code>null</code>.
   */
  protected Popup(Component owner, Component contents,
                  int x, int y)
  {
    if (contents == null)
      throw new IllegalArgumentException();

    // The real stuff happens in the implementation of subclasses,
    // for instance JWindowPopup.
  }
  
  
  /**
   * Constructs a new <code>Popup</code>.
   */
  protected Popup()
  {
    // Nothing to do here.
  }


  /**
   * Displays the <code>Popup</code> on the screen.  Nothing happens
   * if it is currently shown.
   */
  public void show()
  {
    // Implemented by subclasses, for instance JWindowPopup.
  }


  /**
   * Removes the <code>Popup</code> from the screen.  Nothing happens
   * if it is currently hidden.
   */
  public void hide()
  {
    // Implemented by subclasses, for instance JWindowPopup.
  }


  /**
   * A <code>Popup</code> that uses a <code>JWindow</code> for
   * displaying its contents.
   *
   * @see PopupFactory#getPopup
   *
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  static class JWindowPopup
    extends Popup
  {
    /**
     * The <code>JWindow</code> used for displaying the contents
     * of the popup.
     */
    JWindow window;

    private Component contents;

    /**
     * Constructs a new <code>JWindowPopup</code> given its owner,
     * contents and the screen position where the popup
     * will appear.
     *
     * @param owner the Component to which <code>x</code> and
     *        <code>y</code> are relative, or <code>null</code> for
     *        placing the popup relative to the origin of the screen.
     *
     * @param contents the contents that will be displayed inside
     *        the <code>Popup</code>.
     *
     * @param x the horizontal position where the Popup will appear.
     *
     * @param y the vertical position where the Popup will appear.
     *
     * @throws IllegalArgumentException if <code>contents</code>
     *         is <code>null</code>.
     */
    public JWindowPopup(Component owner, Component contents,
                        int x, int y)
    {
      /* Checks whether contents is null. */
      super(owner, contents, x, y);

      this.contents = contents;
      window = new JWindow(SwingUtilities.getWindowAncestor(owner));
      window.getContentPane().add(contents);
      window.setLocation(x, y);
      window.setFocusableWindowState(false);
    }


    /**
     * Displays the popup's <code>JWindow</code> on the screen.
     * Nothing happens if it is already visible.
     */
    public void show()
    {
      window.setSize(contents.getSize());
      window.show();
    }
    
    
    /**
     * Removes the popup's <code>JWindow</code> from the
     * screen.  Nothing happens if it is currently not visible.
     */
    public void hide()
    {
      /* Calling dispose() instead of hide() will conserve native
       * system resources, for example memory in an X11 server.
       * They will automatically be re-allocated by a call to
       * show().
       */
      window.dispose();
    }
  }

  /**
   * A popup that displays itself within the JLayeredPane of a JRootPane of
   * the containment hierarchy of the owner component.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  static class LightweightPopup extends Popup
  {
    /**
     * The owner component for this popup.
     */
    Component owner;

    /**
     * The contents that should be shown.
     */
    Component contents;

    /**
     * The X location in screen coordinates.
     */
    int x;

    /**
     * The Y location in screen coordinates.
     */
    int y;

    /**
     * The panel that holds the content.
     */
    private JPanel panel;
    
    /**
     * The layered pane of the owner.
     */
    private JLayeredPane layeredPane;
    
    /**
     * Constructs a new <code>LightweightPopup</code> given its owner,
     * contents and the screen position where the popup
     * will appear.
     *
     * @param owner the component that should own the popup window; this
     *        provides the JRootPane in which we place the popup window
     *
     * @param contents the contents that will be displayed inside
     *        the <code>Popup</code>.
     *
     * @param x the horizontal position where the Popup will appear in screen
     *        coordinates
     *
     * @param y the vertical position where the Popup will appear in screen
     *        coordinates
     *
     * @throws IllegalArgumentException if <code>contents</code>
     *         is <code>null</code>.
     */
    public LightweightPopup(Component owner, Component  contents, int x, int y)
    {
      super(owner, contents, x, y);
      this.owner = owner;
      this.contents = contents;
      this.x = x;
      this.y = y;
      
      JRootPane rootPane = SwingUtilities.getRootPane(owner);
      JLayeredPane layeredPane = rootPane.getLayeredPane();
      this.layeredPane = layeredPane;
    }

    /**
     * Places the popup within the JLayeredPane of the owner component and
     * makes it visible.
     */
    public void show()
    {
      // We insert a JPanel between the layered pane and the contents so we
      // can fiddle with the setLocation() method without disturbing a
      // JPopupMenu (which overrides setLocation in an unusual manner).
      if (panel == null)
        {
          panel = new JPanel();
          panel.setLayout(new FlowLayout(0, 0, 0));
        }
      
      panel.add(contents);
      panel.setSize(contents.getSize());
      Point layeredPaneLoc = layeredPane.getLocationOnScreen();
      panel.setLocation(x - layeredPaneLoc.x, y - layeredPaneLoc.y);
      layeredPane.add(panel, JLayeredPane.POPUP_LAYER, 0);
      panel.repaint();
    }

    /**
     * Removes the popup from the JLayeredPane thus making it invisible.
     */
    public void hide()
    {
      Rectangle bounds = panel.getBounds();
      layeredPane.remove(panel);
      layeredPane.repaint(bounds.x, bounds.y, bounds.width, bounds.height);
    }
  }
}
