/* BasicDesktopIconUI.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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

package javax.swing.plaf.basic;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDesktopPane;
import javax.swing.JInternalFrame;
import javax.swing.JInternalFrame.JDesktopIcon;
import javax.swing.SwingConstants;
import javax.swing.border.Border;
import javax.swing.event.MouseInputAdapter;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.DesktopIconUI;
import javax.swing.plaf.DesktopPaneUI;


/**
 * This class acts as the UI delegate for JDesktopIcons for the Basic look and feel.
 */
public class BasicDesktopIconUI extends DesktopIconUI
{
  /**
   * This helper class handles mouse events that occur on the JDesktopIcon.
   */
  public class MouseInputHandler extends MouseInputAdapter
  {
    /** The x offset from the MouseEvent coordinates to the top left corner. */
    private transient int xOffset;

    /** The y offset fromt he MouseEvent coordinates to the top left corner. */
    private transient int yOffset;

    /** A cached value of the JDesktopPane that parents this JDesktopIcon. */
    private transient JDesktopPane pane;

    /**
     * This method is called when the mouse is dragged in the JDesktopIcon.
     *
     * @param e The MouseEvent.
     */
    public void mouseDragged(MouseEvent e)
    {
      Rectangle b = desktopIcon.getBounds();

      moveAndRepaint(desktopIcon, b.x + e.getX() - xOffset,
                     b.y + e.getY() - yOffset, b.width, b.height);
    }

    /**
     * This method is called when the mouse is moved in the JDesktopIcon.
     *
     * @param e The MouseEvent.
     */
    public void mouseMoved(MouseEvent e)
    {
      // Nothing to do.
    }

    /**
     * This method is called when the mouse is pressed in the JDesktopIcon.
     *
     * @param e The MouseEvent.
     */
    public void mousePressed(MouseEvent e)
    {
      xOffset = e.getX();
      yOffset = e.getY();
      pane = frame.getDesktopPane();
      if (pane != null)
	pane.getDesktopManager().beginDraggingFrame(desktopIcon);
    }

    /**
     * This method is called when the mouse is released in the JDesktopIcon.
     *
     * @param e The MouseEvent.
     */
    public void mouseReleased(MouseEvent e)
    {
      if (pane != null)
	pane.getDesktopManager().endDraggingFrame(desktopIcon);
      xOffset = 0;
      yOffset = 0;
    }

    /**
     * This method moves and repaints the JDesktopIcon to the given bounds.
     *
     * @param f The JComponent to move and repaint.
     * @param newX The new x coordinate.
     * @param newY The new y coordinate.
     * @param newWidth The new width.
     * @param newHeight The new height.
     */
    public void moveAndRepaint(JComponent f, int newX, int newY, int newWidth,
                               int newHeight)
    {
      if (pane != null)
	pane.getDesktopManager().dragFrame(f, newX, newY);
      else
	desktopIcon.setBounds(newX, newY, newWidth, newHeight);
    }
  }

  /**
   * This class acts as the border for the JDesktopIcon.
   */
  private class DesktopIconBorder implements Border
  {
    /** The left inset value. */
    int left = 10;

    /** The top inset value. */
    int top = 4;

    /** The right inset value. */
    int right = top;

    /** The bottom inset value. */
    int bottom = top;

    /**
     * This method returns the insets of the border.
     *
     * @param c The Component to find border insets for.
     *
     * @return The border insets.
     */
    public Insets getBorderInsets(Component c)
    {
      return new Insets(top, left, bottom, right);
    }

    /**
     * This method returns whether the border is opaque.
     *
     * @return Whether the border is opaque.
     */
    public boolean isBorderOpaque()
    {
      return true;
    }

    /**
     * This method paints the border.
     *
     * @param c The Component the border is in.
     * @param g The Graphics object to paint with.
     * @param x The x coordinate of the Component.
     * @param y The y coordinate of the Component.
     * @param width The width of the Component.
     * @param height The height of the Component.
     */
    public void paintBorder(Component c, Graphics g, int x, int y, int width,
                            int height)
    {
      g.translate(x, y);
      Color saved = g.getColor();

      g.setColor(Color.LIGHT_GRAY);

      g.fillRect(0, 0, left, height);
      g.fillRect(0, 0, width, top);
      g.fillRect(0, height - bottom, width, bottom);
      g.fillRect(width - right, 0, right, height);

      g.setColor(Color.BLACK);
      g.drawRect(0, 0, width - 1, height - 1);

      int fHeight = height / 4;
      int hLeft = left / 2;

      g.setColor(Color.BLACK);
      g.fillRect(hLeft, fHeight, 2, 2);
      g.fillRect(hLeft, fHeight * 2, 2, 2);
      g.fillRect(hLeft, fHeight * 3, 2, 2);

      g.setColor(saved);
      g.translate(-x, -y);
    }
  }

  /** The static width and height of the iconSize. */
  private static final int iconSize = 16;

  /**
   * This class represents the default frame icon when none
   * is supplied by the JInternalFrame.
   */
  static class InternalFrameDefaultMenuIcon implements Icon
  {
    /**
     * This returns the icon height.
     *
     * @return The icon height.
     */
    public int getIconHeight()
    {
      return iconSize;
    }

    /**
     * This returns the icon width.
     *
     * @return The icon width.
     */
    public int getIconWidth()
    {
      return iconSize;
    }

    /**
     * This method paints the icon.
     *
     * @param c The Component this icon belongs to.
     * @param g The Graphics object to paint with.
     * @param x The x coordinate to paint at.
     * @param y The y coordinate to paint at.
     */
    public void paintIcon(Component c, Graphics g, int x, int y)
    {
      g.translate(x, y);
      Color saved = g.getColor();

      g.setColor(Color.BLUE);
      g.fillRect(0, 0, iconSize, (int) ((double) iconSize / 3) + 1);

      g.setColor(Color.WHITE);
      g.fillRect(0, (int) ((double) iconSize / 3), iconSize, iconSize * 5 / 6);

      g.setColor(Color.GRAY);
      g.drawRect(0, 0, iconSize, iconSize);

      g.setColor(saved);
      g.translate(-x, -y);
    }
  }

  /** The default JDesktopIcon width. */
  private static final int iconWidth = 160;

  /** The default JDesktopIcon height */
  private static final int iconHeight = 35;

  /** The JDesktopIcon this UI delegate represents. */
  protected JDesktopIcon desktopIcon;

  /** The JInternalFrame associated with the JDesktopIcon. */
  protected JInternalFrame frame;

  /** The MouseListener responsible for reacting to MouseEvents on the JDesktopIcon. */
  private transient MouseInputListener mouseHandler;

  /** The Button in the JDesktopIcon responsible for deiconifying it. */
  private transient BoundButton button;

  /** The PropertyChangeListener listening to the JDesktopIcon. */
  private transient PropertyChangeListener propertyHandler;
  
  /** The default icon used when no frame icon is given by the JInternalFrame. */
  static Icon defaultIcon = new InternalFrameDefaultMenuIcon();

  /**
   * This is a helper class that is used in JDesktopIcon and gives the Button a predetermined size.
   */
  private class BoundButton extends JButton
  {
    /**
     * Creates a new BoundButton object.
     *
     * @param title The title of the button.
     */
    public BoundButton(String title)
    {
      super(title);
    }

    /**
     * This method returns a standard size (based on the defaults of the JDesktopIcon) and the insets.
     *
     * @return The preferred size of the JDesktopIcon.
     */
    public Dimension getPreferredSize()
    {
      Insets insets = desktopIcon.getInsets();
      return new Dimension(iconWidth - insets.left - insets.right,
                           iconHeight - insets.top - insets.bottom);
    }

    /**
     * This method returns the minimum size of the button.
     *
     * @return The minimum size of the button.
     */
    public Dimension getMinimumSize()
    {
      return getPreferredSize();
    }

    /**
     * This method returns the maximum size of the button.
     *
     * @return The maximum size of the button.
     */
    public Dimension getMaximumSize()
    {
      return getPreferredSize();
    }
  }

  /**
   * Creates a new BasicDesktopIconUI object.
   */
  public BasicDesktopIconUI()
  {
  }

  /**
   * This method creates a new BasicDesktopIconUI for the given JComponent.
   *
   * @param c The JComponent to create a UI for.
   *
   * @return A new BasicDesktopIconUI.
   */
  public static ComponentUI createUI(JComponent c)
  {
    return new BasicDesktopIconUI();
  }

  /**
   * This method installs the UI for the given JComponent.
   *
   * @param c The JComponent to install this UI for.
   */
  public void installUI(JComponent c)
  {
    if (c instanceof JDesktopIcon)
      {
	desktopIcon = (JDesktopIcon) c;
	desktopIcon.setLayout(new BorderLayout());
	frame = desktopIcon.getInternalFrame();

	installDefaults();
	installComponents();
	installListeners();

	desktopIcon.setOpaque(true);
      }
  }

  /**
   * This method uninstalls the UI for the given JComponent.
   *
   * @param c The JComponent to uninstall this UI for.
   */
  public void uninstallUI(JComponent c)
  {
    desktopIcon.setOpaque(false);
    
    uninstallListeners();
    uninstallComponents();
    uninstallDefaults();
    
    frame = null;
    desktopIcon.setLayout(null);
    desktopIcon = null;
  }

  /**
   * This method installs the necessary sub components for the JDesktopIcon.
   */
  protected void installComponents()
  {
    // Try to create a button based on what the frame's
    // state is currently
    button = new BoundButton(frame.getTitle());
    button.setHorizontalAlignment(SwingConstants.LEFT);
    button.setHorizontalTextPosition(SwingConstants.TRAILING);

    Icon use = frame.getFrameIcon();
    if (use == null)
      use = defaultIcon;
    button.setIcon(use);

    desktopIcon.add(button, SwingConstants.CENTER);
  }

  /**
   * This method uninstalls the sub components for the JDesktopIcon.
   */
  protected void uninstallComponents()
  {
    desktopIcon.remove(button);
    
    button = null;
  }

  /**
   * This method installs the listeners needed by this UI.
   */
  protected void installListeners()
  {
    mouseHandler = createMouseInputListener();

    desktopIcon.addMouseMotionListener(mouseHandler);
    desktopIcon.addMouseListener(mouseHandler);

    propertyHandler = new PropertyChangeListener()
        {
	  public void propertyChange(PropertyChangeEvent e)
	  {
	    if (e.getPropertyName().equals(JInternalFrame.TITLE_PROPERTY))
	      button.setText(desktopIcon.getInternalFrame().getTitle());
	    else if (e.getPropertyName().equals(JInternalFrame.FRAME_ICON_PROPERTY))
	      {
		Icon use = desktopIcon.getInternalFrame().getFrameIcon();
		if (use == null)
		  use = defaultIcon;
		button.setIcon(use);
	      }
	    desktopIcon.revalidate();
	    desktopIcon.repaint();
	  }
        };
    frame.addPropertyChangeListener(propertyHandler);

    button.addActionListener(new ActionListener()
        {
	  public void actionPerformed(ActionEvent e)
	  {
            deiconize();
	  }
        });
  }

  /**
   * This method uninstalls the listeners needed by the UI.
   */
  protected void uninstallListeners()
  {
    // button is nulled so no need to remove it.
    
    frame.removePropertyChangeListener(propertyHandler);
    propertyHandler = null;
    
    desktopIcon.removeMouseMotionListener(mouseHandler);
    desktopIcon.removeMouseListener(mouseHandler);
  }

  /**
   * This method installs the defaults for the JDesktopIcon.
   */
  protected void installDefaults()
  {
    // FIXME: Move border to defaults.
    desktopIcon.setBorder(new DesktopIconBorder());  
  }

  /**
   * This method uninstalls the defaults for the JDesktopIcon.
   */
  protected void uninstallDefaults()
  {
    desktopIcon.setBorder(null);
  }

  /**
   * This method creates a new MouseInputListener for the JDesktopIcon.
   *
   * @return A new MouseInputListener.
   */
  protected MouseInputListener createMouseInputListener()
  {
    return new MouseInputHandler();
  }

  /**
   * This method returns the preferred size for the given JComponent.
   *
   * @param c The JComponent to find a preferred size for.
   *
   * @return The preferred size.
   */
  public Dimension getPreferredSize(JComponent c)
  {
    return new Dimension(iconWidth, iconHeight);
  }

  /**
   * This method returns the minimum size for the given JComponent.
   *
   * @param c The JComponent to find a minimum size for.
   *
   * @return The minimum size.
   */
  public Dimension getMinimumSize(JComponent c)
  {
    return getPreferredSize(c);
  }

  /**
   * This method returns the maximum size for the given JComponent.
   *
   * @param c The JComponent to find a maximum size for.
   *
   * @return The maximum size.
   */
  public Dimension getMaximumSize(JComponent c)
  {
    return getPreferredSize(c);
  }

  /**
   * This method returns the insets of the given JComponent.
   *
   * @param c The JComponent to find insets for.
   *
   * @return The insets of the given JComponent.
   */
  public Insets getInsets(JComponent c)
  {
    return c.getInsets();
  }

  /**
   * This method deiconizes the JInternalFrame associated with the JDesktopIcon.
   */
  public void deiconize() 
  {
    try
    {
      frame.setIcon(false);
    }
    catch (PropertyVetoException pve)
    {
    }
  }
}
