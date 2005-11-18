/* BasicToolBarUI.java --
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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


package javax.swing.plaf.basic;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Window;
import java.awt.event.ContainerEvent;
import java.awt.event.ContainerListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Hashtable;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;
import javax.swing.LookAndFeel;
import javax.swing.RootPaneContainer;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.BorderUIResource.EtchedBorderUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.ToolBarUI;
import javax.swing.plaf.UIResource;

/**
 * This is the Basic Look and Feel UI class for JToolBar.
 */
public class BasicToolBarUI extends ToolBarUI implements SwingConstants
{
  /** Static owner of all DragWindows.
   * This is package-private to avoid an accessor method.  */
  static JFrame owner = new JFrame();

  /** The border used when the JToolBar is in nonrollover mode. */
  private static Border nonRolloverBorder;

  /** The border used when the JToolBar is in rollover mode. */
  private static Border rolloverBorder;

  /** The last known BorderLayout constraint before floating. */
  protected String constraintBeforeFloating;

  /** The last known orientation of the JToolBar before floating.
   * This is package-private to avoid an accessor method.  */
  int lastGoodOrientation;

  /** The color of the border when it is dockable. */
  protected Color dockingBorderColor;

  /** The background color of the JToolBar when it is dockable. */
  protected Color dockingColor;

  /** The docking listener responsible for mouse events on the JToolBar. */
  protected MouseInputListener dockingListener;

  /** The window used for dragging the JToolBar. */
  protected BasicToolBarUI.DragWindow dragWindow;

  /** The color of the border when it is not dockable. */
  protected Color floatingBorderColor;

  /** The background color of the JToolBar when it is not dockable. */
  protected Color floatingColor;

  /** The index of the focused component. */
  protected int focusedCompIndex;

  /** The PropertyChangeListener for the JToolBar. */
  protected PropertyChangeListener propertyListener;

  /** The JToolBar this UI delegate is responsible for. */
  protected JToolBar toolBar;

  /** The Container listener for the JToolBar. */
  protected ContainerListener toolBarContListener;

  /** The Focus listener for the JToolBar. */
  protected FocusListener toolBarFocusListener;

  /**
   * @deprecated since JDK1.3.
   */
  protected KeyStroke leftKey;

  /**
   * @deprecated since JDK1.3.
   */
  protected KeyStroke rightKey;

  /**
   * @deprecated since JDK1.3.
   */
  protected KeyStroke upKey;

  /**
   * @deprecated since JDK1.3.
   */
  protected KeyStroke downKey;

  /**
   * The floating window that is responsible for holding the JToolBar when it
   * is dragged outside of its original parent.
   */
  private transient Window floatFrame;

  /** The original parent of the JToolBar.
   * This is package-private to avoid an accessor method.  */
  transient Container origParent;

  /** A hashtable of components and their original borders.
   * This is package-private to avoid an accessor method.  */
  transient Hashtable borders;

  /** A window listener for the floatable frame. */
  private transient WindowListener windowListener;

  /** A set of cached bounds of the JToolBar.
   * This is package-private to avoid an accessor method.  */
  transient Dimension cachedBounds;

  /** The cached orientation of the JToolBar.
   * This is package-private to avoid an accessor method.  */
  transient int cachedOrientation;

  /**
   * This method creates a new <code>BasicToolBarUI</code> object for the given JToolBar.
   */
  public BasicToolBarUI()
  {
    // Do nothing here.
  }

  /**
   * This method returns whether the JToolBar can dock at the given position.
   *
   * @param c The component to try to dock in.
   * @param p The position of the mouse cursor relative to the given
   *        component.
   *
   * @return Whether the JToolBar can dock.
   */
  public boolean canDock(Component c, Point p)
  {
    return areaOfClick(c, p) != -1;
  }

  /**
   * This helper method returns the position of the JToolBar if it can dock.
   *
   * @param c The component to try to dock in.
   * @param p The position of the mouse cursor relative to the given
   *        component.
   *
   * @return One of the SwingConstants directions or -1 if the JToolBar can't
   *         dock.
   */
  private int areaOfClick(Component c, Point p)
  {
    // Has to dock in immediate parent, not eventual root container.
    Rectangle pBounds = c.getBounds();

    // XXX: In Sun's implementation, the space the toolbar has to dock is dependent on the size it had last.
    Dimension d = toolBar.getSize();
    int limit = Math.min(d.width, d.height);

    // The order of checking is 1. top 2. bottom 3. left 4. right
    if (! pBounds.contains(p))
      return -1;

    if (p.y < limit)
      return SwingConstants.NORTH;

    if (p.y > (pBounds.height - limit))
      return SwingConstants.SOUTH;

    if (p.x < limit)
      return SwingConstants.WEST;

    if (p.x > (pBounds.width - limit))
      return SwingConstants.EAST;

    return -1;
  }

  /**
   * This method creates a new DockingListener for the JToolBar.
   *
   * @return A new DockingListener for the JToolBar.
   */
  protected MouseInputListener createDockingListener()
  {
    return new DockingListener(toolBar);
  }

  /**
   * This method creates a new DragWindow for the given JToolBar.
   *
   * @param toolbar The JToolBar to create a DragWindow for.
   *
   * @return A new DragWindow.
   */
  protected BasicToolBarUI.DragWindow createDragWindow(JToolBar toolbar)
  {
    return new DragWindow();
  }

  /**
   * This method creates a new floating frame for the JToolBar. By default,
   * this UI uses createFloatingWindow instead. This method of creating a
   * floating frame is deprecated.
   *
   * @param toolbar The JToolBar to create a floating frame for.
   *
   * @return A new floating frame.
   */
  protected JFrame createFloatingFrame(JToolBar toolbar)
  {
    // FIXME: Though deprecated, this should still work.
    return null;
  }

  /**
   * This method creates a new floating window for the JToolBar. This is the
   * method used by default to create a floating container for the JToolBar.
   *
   * @param toolbar The JToolBar to create a floating window for.
   *
   * @return A new floating window.
   */
  protected RootPaneContainer createFloatingWindow(JToolBar toolbar)
  {
    // This one is used by default though.
    return new ToolBarDialog();
  }

  /**
   * This method creates a new WindowListener for the JToolBar.
   *
   * @return A new WindowListener.
   */
  protected WindowListener createFrameListener()
  {
    return new FrameListener();
  }

  /**
   * This method creates a new nonRolloverBorder for JButtons when the
   * JToolBar's rollover property is set to false.
   *
   * @return A new NonRolloverBorder.
   */
  protected Border createNonRolloverBorder()
  {
    return new EtchedBorderUIResource();
  }

  /**
   * This method creates a new PropertyChangeListener for the JToolBar.
   *
   * @return A new PropertyChangeListener.
   */
  protected PropertyChangeListener createPropertyListener()
  {
    return new PropertyListener();
  }

  /**
   * This method creates a new rollover border for JButtons when the
   * JToolBar's rollover property is set to true.
   *
   * @return A new rollover border.
   */
  protected Border createRolloverBorder()
  {
    return new EtchedBorderUIResource()
      {
	public void paintBorder(Component c, Graphics g, int x, int y,
	                        int width, int height)
	{
	  if (c instanceof JButton)
	    {
	      if (((JButton) c).getModel().isRollover())
		super.paintBorder(c, g, x, y, width, height);
	    }
	}
      };
  }

  /**
   * This method creates a new Container listener for the JToolBar.
   *
   * @return A new Container listener.
   */
  protected ContainerListener createToolBarContListener()
  {
    return new ToolBarContListener();
  }

  /**
   * This method creates a new FocusListener for the JToolBar.
   *
   * @return A new FocusListener for the JToolBar.
   */
  protected FocusListener createToolBarFocusListener()
  {
    return new ToolBarFocusListener();
  }

  /**
   * This method creates a new UI delegate for the given JComponent.
   *
   * @param c The JComponent to create a UI delegate for.
   *
   * @return A new UI delegate.
   */
  public static ComponentUI createUI(JComponent c)
  {
    return new BasicToolBarUI();
  }

  /**
   * This method is called to drag the DragWindow around when the JToolBar is
   * being dragged around.
   *
   * @param position The mouse cursor coordinates relative to the JToolBar.
   * @param origin The screen position of the JToolBar.
   */
  protected void dragTo(Point position, Point origin)
  {
    int loc = areaOfClick(origParent,
                          SwingUtilities.convertPoint(toolBar, position,
                                                      origParent));

    if (loc != -1)
      {
	dragWindow.setBorderColor(dockingBorderColor);
	dragWindow.setBackground(dockingColor);
      }
    else
      {
	dragWindow.setBorderColor(floatingBorderColor);
	dragWindow.setBackground(floatingColor);
      }

    int w = 0;
    int h = 0;

    boolean tmp = ((loc == SwingConstants.NORTH)
                  || (loc == SwingConstants.SOUTH) || (loc == -1));

    cachedOrientation = toolBar.getOrientation();
    cachedBounds = toolBar.getSize();
    if (((cachedOrientation == SwingConstants.HORIZONTAL) && tmp)
        || ((cachedOrientation == VERTICAL) && ! tmp))
      {
	w = cachedBounds.width;
	h = cachedBounds.height;
      }
    else
      {
	w = cachedBounds.height;
	h = cachedBounds.width;
      }

    Point p = dragWindow.getOffset();
    Insets insets = toolBar.getInsets();

    dragWindow.setBounds((origin.x + position.x) - p.x
                         - ((insets.left + insets.right) / 2),
                         (origin.y + position.y) - p.y
                         - ((insets.top + insets.bottom) / 2), w, h);

    if (! dragWindow.isVisible())
      dragWindow.show();
  }

  /**
   * This method is used at the end of a drag session to place the frame in
   * either its original parent as a docked JToolBar or in its floating
   * frame.
   *
   * @param position The position of the mouse cursor relative to the
   *        JToolBar.
   * @param origin The screen position of the JToolBar before the drag session
   *        started.
   */
  protected void floatAt(Point position, Point origin)
  {
    Point p = new Point(position);
    int aoc = areaOfClick(origParent,
                          SwingUtilities.convertPoint(toolBar, p, origParent));

    Container oldParent = toolBar.getParent();

    oldParent.remove(toolBar);
    oldParent.doLayout();
    oldParent.repaint();

    Container newParent;

    if (aoc == -1)
      newParent = ((RootPaneContainer) floatFrame).getContentPane();
    else
      {
	floatFrame.hide();
	newParent = origParent;
      }

    String constraint;
    switch (aoc)
      {
      case SwingConstants.EAST:
	constraint = BorderLayout.EAST;
	break;
      case SwingConstants.NORTH:
	constraint = BorderLayout.NORTH;
	break;
      case SwingConstants.SOUTH:
	constraint = BorderLayout.SOUTH;
	break;
      case SwingConstants.WEST:
	constraint = BorderLayout.WEST;
	break;
      default:
	constraint = BorderLayout.CENTER;
	break;
      }

    int newOrientation = SwingConstants.HORIZONTAL;
    if ((aoc != -1)
        && ((aoc == SwingConstants.EAST) || (aoc == SwingConstants.WEST)))
      newOrientation = SwingConstants.VERTICAL;

    if (aoc != -1)
      {
	constraintBeforeFloating = constraint;
	lastGoodOrientation = newOrientation;
      }

    newParent.add(toolBar, constraint);

    setFloating(aoc == -1, null);
    toolBar.setOrientation(newOrientation);

    Insets insets = floatFrame.getInsets();
    Dimension dims = toolBar.getPreferredSize();
    p = dragWindow.getOffset();
    setFloatingLocation((position.x + origin.x) - p.x
                        - ((insets.left + insets.right) / 2),
                        (position.y + origin.y) - p.y
                        - ((insets.top + insets.bottom) / 2));

    if (aoc == -1)
      {
	floatFrame.pack();
	floatFrame.setSize(dims.width + insets.left + insets.right,
	                   dims.height + insets.top + insets.bottom);
	floatFrame.show();
      }

    newParent.invalidate();
    newParent.validate();
    newParent.repaint();
  }

  /**
   * This method returns the docking color.
   *
   * @return The docking color.
   */
  public Color getDockingColor()
  {
    return dockingColor;
  }

  /**
   * This method returns the Color which is displayed when over a floating
   * area.
   *
   * @return The color which is displayed when over a floating area.
   */
  public Color getFloatingColor()
  {
    return floatingColor;
  }

  /**
   * This method returns the maximum size of the given JComponent for this UI.
   *
   * @param c The JComponent to find the maximum size for.
   *
   * @return The maximum size for this UI.
   */
  public Dimension getMaximumSize(JComponent c)
  {
    return getPreferredSize(c);
  }

  /**
   * This method returns the minimum size of the given JComponent for this UI.
   *
   * @param c The JComponent to find a minimum size for.
   *
   * @return The minimum size for this UI.
   */
  public Dimension getMinimumSize(JComponent c)
  {
    return getPreferredSize(c);
  }

  /**
   * This method installs the needed components for the JToolBar.
   */
  protected void installComponents()
  {
    floatFrame = (Window) createFloatingWindow(toolBar);

    dragWindow = createDragWindow(toolBar);

    nonRolloverBorder = createNonRolloverBorder();
    rolloverBorder = createRolloverBorder();

    borders = new Hashtable();

    fillHashtable();
  }

  /**
   * This method installs the defaults as specified by the look and feel.
   */
  protected void installDefaults()
  {
    LookAndFeel.installBorder(toolBar, "ToolBar.border");
    LookAndFeel.installColorsAndFont(toolBar, "ToolBar.background",
                                     "ToolBar.foreground", "ToolBar.font");

    dockingBorderColor = UIManager.getColor("ToolBar.dockingForeground");
    dockingColor = UIManager.getColor("ToolBar.dockingBackground");

    floatingBorderColor = UIManager.getColor("ToolBar.floatingForeground");
    floatingColor = UIManager.getColor("ToolBar.floatingBackground");
    setRolloverBorders(toolBar.isRollover());
  }

  /**
   * This method installs the keyboard actions for the JToolBar as specified
   * by the look and feel.
   */
  protected void installKeyboardActions()
  {
    // FIXME: implement.
  }

  /**
   * This method installs listeners for the JToolBar.
   */
  protected void installListeners()
  {
    dockingListener = createDockingListener();
    toolBar.addMouseListener(dockingListener);
    toolBar.addMouseMotionListener(dockingListener);

    propertyListener = createPropertyListener();
    toolBar.addPropertyChangeListener(propertyListener);

    toolBarContListener = createToolBarContListener();
    toolBar.addContainerListener(toolBarContListener);

    windowListener = createFrameListener();
    floatFrame.addWindowListener(windowListener);

    toolBarFocusListener = createToolBarFocusListener();
    toolBar.addFocusListener(toolBarFocusListener);
  }

  /**
   * This method installs non rollover borders for each component inside the
   * given JComponent.
   *
   * @param c The JComponent whose children need to have non rollover borders
   *        installed.
   */
  protected void installNonRolloverBorders(JComponent c)
  {
    Component[] components = toolBar.getComponents();

    for (int i = 0; i < components.length; i++)
      setBorderToNonRollover(components[i]);
  }

  /**
   * This method installs normal (or their original) borders for each
   * component inside the given JComponent.
   *
   * @param c The JComponent whose children need to have their original
   *        borders installed.
   */
  protected void installNormalBorders(JComponent c)
  {
    Component[] components = toolBar.getComponents();

    for (int i = 0; i < components.length; i++)
      setBorderToNormal(components[i]);
  }

  /**
   * This method install rollover borders for each component inside the given
   * JComponent.
   *
   * @param c The JComponent whose children need to have rollover borders
   *        installed.
   */
  protected void installRolloverBorders(JComponent c)
  {
    Component[] components = toolBar.getComponents();

    for (int i = 0; i < components.length; i++)
      setBorderToRollover(components[i]);
  }

  /**
   * This method fills the borders hashtable with a list of components that
   * are JButtons and their borders.
   */
  private void fillHashtable()
  {
    Component[] c = toolBar.getComponents();

    for (int i = 0; i < c.length; i++)
      {
	if (c[i] instanceof JButton)
	  {
	    // Don't really care about anything other than JButtons
	    JButton b = (JButton) c[i];

	    if (b.getBorder() != null)
	      borders.put(b, b.getBorder());
	  }
      }
  }

  /**
   * This method installs the UI for the given JComponent.
   *
   * @param c The JComponent to install a UI for.
   */
  public void installUI(JComponent c)
  {
    super.installUI(c);

    if (c instanceof JToolBar)
      {
	toolBar = (JToolBar) c;
	toolBar.setOpaque(true);
	installDefaults();
	installComponents();
	installListeners();
	installKeyboardActions();
      }
  }

  /**
   * This method returns whether the JToolBar is floating.
   *
   * @return Whether the JToolBar is floating.
   */
  public boolean isFloating()
  {
    return floatFrame.isVisible();
  }

  /**
   * This method returns whether rollover borders have been set.
   *
   * @return Whether rollover borders have been set.
   */
  public boolean isRolloverBorders()
  {
    return toolBar.isRollover();
  }

  /**
   * This method navigates in the given direction giving focus to the next
   * component in the given direction.
   *
   * @param direction The direction to give focus to.
   */
  protected void navigateFocusedComp(int direction)
  {
    // FIXME: Implement.
  }

  /**
   * This method sets the border of the given component to a non rollover
   * border.
   *
   * @param c The Component whose border needs to be set.
   */
  protected void setBorderToNonRollover(Component c)
  {
    if (c instanceof JButton)
      {
	JButton b = (JButton) c;
	b.setRolloverEnabled(false);
	b.setBorder(nonRolloverBorder);
      }
  }

  /**
   * This method sets the border of the given component to its original value.
   *
   * @param c The Component whose border needs to be set.
   */
  protected void setBorderToNormal(Component c)
  {
    if (c instanceof JButton)
      {
	JButton b = (JButton) c;
	Border border = (Border) borders.get(b);
	b.setBorder(border);
      }
  }

  /**
   * This method sets the border of the given component to a rollover border.
   *
   * @param c The Component whose border needs to be set.
   */
  protected void setBorderToRollover(Component c)
  {
    if (c instanceof JButton)
      {
	JButton b = (JButton) c;
	b.setRolloverEnabled(true);
	b.setBorder(rolloverBorder);
      }
  }

  /**
   * This method sets the docking color.
   *
   * @param c The docking color.
   */
  public void setDockingColor(Color c)
  {
    dockingColor = c;
  }

  /**
   * This method sets the floating property for the JToolBar.
   *
   * @param b Whether the JToolBar is floating.
   * @param p FIXME
   */
  public void setFloating(boolean b, Point p)
  {
    // FIXME: use p for something. It's not location
    // since we already have setFloatingLocation.
    floatFrame.setVisible(b);
  }

  /**
   * This method sets the color displayed when the JToolBar is not in a
   * dockable area.
   *
   * @param c The floating color.
   */
  public void setFloatingColor(Color c)
  {
    floatingColor = c;
  }

  /**
   * This method sets the floating location of the JToolBar.
   *
   * @param x The x coordinate for the floating frame.
   * @param y The y coordinate for the floating frame.
   */
  public void setFloatingLocation(int x, int y)
  {
    // x,y are the coordinates of the new JFrame created to store the toolbar
    // XXX: The floating location is bogus is not floating.
    floatFrame.setLocation(x, y);
    floatFrame.invalidate();
    floatFrame.validate();
    floatFrame.repaint();
  }

  /**
   * This is a convenience method for changing the orientation of the
   * JToolBar.
   *
   * @param orientation The new orientation.
   */
  public void setOrientation(int orientation)
  {
    toolBar.setOrientation(orientation);
  }

  /**
   * This method changes the child components to have rollover borders if the
   * given parameter is true. Otherwise, the components are set to have non
   * rollover borders.
   *
   * @param rollover Whether the children will have rollover borders.
   */
  public void setRolloverBorders(boolean rollover)
  {
    if (rollover)
      installRolloverBorders(toolBar);
    else
      installNonRolloverBorders(toolBar);
  }

  /**
   * This method uninstall UI installed components from the JToolBar.
   */
  protected void uninstallComponents()
  {
    installNormalBorders(toolBar);
    borders = null;
    rolloverBorder = null;
    nonRolloverBorder = null;
    cachedBounds = null;

    floatFrame = null;
    dragWindow = null;
  }

  /**
   * This method removes the defaults installed by the Look and Feel.
   */
  protected void uninstallDefaults()
  {
    toolBar.setBackground(null);
    toolBar.setForeground(null);
    toolBar.setFont(null);

    dockingBorderColor = null;
    dockingColor = null;
    floatingBorderColor = null;
    floatingColor = null;
  }

  /**
   * This method uninstalls keyboard actions installed by the UI.
   */
  protected void uninstallKeyboardActions()
  {
    // FIXME: implement.
  }

  /**
   * This method uninstalls listeners installed by the UI.
   */
  protected void uninstallListeners()
  {
    toolBar.removeFocusListener(toolBarFocusListener);
    toolBarFocusListener = null;

    floatFrame.removeWindowListener(windowListener);
    windowListener = null;

    toolBar.removeContainerListener(toolBarContListener);
    toolBarContListener = null;

    toolBar.removeMouseMotionListener(dockingListener);
    toolBar.removeMouseListener(dockingListener);
    dockingListener = null;
  }

  /**
   * This method uninstalls the UI.
   *
   * @param c The JComponent that is having this UI removed.
   */
  public void uninstallUI(JComponent c)
  {
    uninstallKeyboardActions();
    uninstallListeners();
    uninstallComponents();
    uninstallDefaults();
    toolBar = null;
  }

  /**
   * This is the MouseHandler class that allows the user to drag the JToolBar
   * in and out of the parent and dock it if it can.
   */
  public class DockingListener implements MouseInputListener
  {
    /** Whether the JToolBar is being dragged. */
    protected boolean isDragging;

    /**
     * The origin point. This point is saved from the beginning press and is
     * used until the end of the drag session.
     */
    protected Point origin;

    /** The JToolBar being dragged. */
    protected JToolBar toolBar;

    /**
     * Creates a new DockingListener object.
     *
     * @param t The JToolBar this DockingListener is being used for.
     */
    public DockingListener(JToolBar t)
    {
      toolBar = t;
    }

    /**
     * This method is called when the mouse is clicked.
     *
     * @param e The MouseEvent.
     */
    public void mouseClicked(MouseEvent e)
    {
      // Don't care.
    }

    /**
     * This method is called when the mouse is dragged. It delegates the drag
     * painting to the dragTo method.
     *
     * @param e The MouseEvent.
     */
    public void mouseDragged(MouseEvent e)
    {
      if (isDragging)
	dragTo(e.getPoint(), origin);
    }

    /**
     * This method is called when the mouse enters the JToolBar.
     *
     * @param e The MouseEvent.
     */
    public void mouseEntered(MouseEvent e)
    {
      // Don't care (yet).
    }

    /**
     * This method is called when the mouse exits the JToolBar.
     *
     * @param e The MouseEvent.
     */
    public void mouseExited(MouseEvent e)
    {
      // Don't care (yet).
    }

    /**
     * This method is called when the mouse is moved in the JToolBar.
     *
     * @param e The MouseEvent.
     */
    public void mouseMoved(MouseEvent e)
    {
      // TODO: What should be done here, if anything?
    }

    /**
     * This method is called when the mouse is pressed in the JToolBar. If the
     * press doesn't occur in a place where it causes the JToolBar to be
     * dragged, it returns. Otherwise, it starts a drag session.
     *
     * @param e The MouseEvent.
     */
    public void mousePressed(MouseEvent e)
    {
      if (! toolBar.isFloatable())
	return;

      Point ssd = e.getPoint();
      Insets insets = toolBar.getInsets();

      // Verify that this click occurs in the top inset.
      if (toolBar.getOrientation() == SwingConstants.HORIZONTAL)
        {
	  if (e.getX() > insets.left)
	    return;
        }
      else
        {
	  if (e.getY() > insets.top)
	    return;
        }

      origin = new Point(0, 0);
      if (toolBar.isShowing())
        SwingUtilities.convertPointToScreen(ssd, toolBar);

      if (! (SwingUtilities.getAncestorOfClass(Window.class, toolBar) instanceof UIResource))
	// Need to know who keeps the toolBar if it gets dragged back into it.
	origParent = toolBar.getParent();
      
      if (toolBar.isShowing())
        SwingUtilities.convertPointToScreen(origin, toolBar);

      isDragging = true;

      if (dragWindow != null)
	dragWindow.setOffset(new Point(e.getX(), e.getY()));

      dragTo(e.getPoint(), origin);
    }

    /**
     * This method is called when the mouse is released from the JToolBar.
     *
     * @param e The MouseEvent.
     */
    public void mouseReleased(MouseEvent e)
    {
      if (! isDragging || ! toolBar.isFloatable())
	return;

      isDragging = false;
      floatAt(e.getPoint(), origin);
      dragWindow.hide();
    }
  }

  /**
   * This is the window that appears when the JToolBar is being dragged
   * around.
   */
  protected class DragWindow extends Window
  {
    /**
     * The current border color. It changes depending on whether the JToolBar
     * is over a place that allows it to dock.
     */
    private Color borderColor;

    /** The between the mouse and the top left corner of the window. */
    private Point offset;

    /**
     * Creates a new DragWindow object.
     * This is package-private to avoid an accessor method.
     */
    DragWindow()
    {
      super(owner);
    }

    /**
     * The color that the border should be.
     *
     * @return The border color.
     */
    public Color getBorderColor()
    {
      if (borderColor == null)
	return Color.BLACK;

      return borderColor;
    }

    /**
     * This method returns the insets for the DragWindow.
     *
     * @return The insets for the DragWindow.
     */
    public Insets getInsets()
    {
      // This window has no decorations, so insets are empty.
      return new Insets(0, 0, 0, 0);
    }

    /**
     * This method returns the mouse offset from the top left corner of the
     * DragWindow.
     *
     * @return The mouse offset.
     */
    public Point getOffset()
    {
      return offset;
    }

    /**
     * This method paints the DragWindow.
     *
     * @param g The Graphics object to paint with.
     */
    public void paint(Graphics g)
    {
      //  No visiting children necessary.
      Color saved = g.getColor();
      Rectangle b = getBounds();

      g.setColor(getBorderColor());
      g.drawRect(0, 0, b.width - 1, b.height - 1);

      g.setColor(saved);
    }

    /**
     * This method changes the border color.
     *
     * @param c The new border color.
     */
    public void setBorderColor(Color c)
    {
      borderColor = c;
    }

    /**
     * This method changes the mouse offset.
     *
     * @param p The new mouse offset.
     */
    public void setOffset(Point p)
    {
      offset = p;
    }

    /**
     * FIXME: Do something.
     *
     * @param o DOCUMENT ME!
     */
    public void setOrientation(int o)
    {
      // FIXME: implement.
    }
  }

  /**
   * This helper class listens for Window events from the floatable window and
   * if it is closed, returns the JToolBar to the last known good location.
   */
  protected class FrameListener extends WindowAdapter
  {
    /**
     * This method is called when the floating window is closed.
     *
     * @param e The WindowEvent.
     */
    public void windowClosing(WindowEvent e)
    {
      Container parent = toolBar.getParent();
      parent.remove(toolBar);

      if (origParent != null)
        {
	  origParent.add(toolBar,
	                 (constraintBeforeFloating != null)
	                 ? constraintBeforeFloating : BorderLayout.NORTH);
	  toolBar.setOrientation(lastGoodOrientation);
        }

      origParent.invalidate();
      origParent.validate();
      origParent.repaint();
    }
  }

  /**
   * This helper class listens for PropertyChangeEvents from the JToolBar.
   */
  protected class PropertyListener implements PropertyChangeListener
  {
    /**
     * This method is called when a property from the JToolBar is changed.
     *
     * @param e The PropertyChangeEvent.
     */
    public void propertyChange(PropertyChangeEvent e)
    {
      // FIXME: need name properties so can change floatFrame title.
      if (e.getPropertyName().equals("rollover"))
	setRolloverBorders(toolBar.isRollover());
    }
  }

  /**
   * This helper class listens for components added to and removed from the
   * JToolBar.
   */
  protected class ToolBarContListener implements ContainerListener
  {
    /**
     * This method is responsible for setting rollover or non rollover for new
     * buttons added to the JToolBar.
     *
     * @param e The ContainerEvent.
     */
    public void componentAdded(ContainerEvent e)
    {
      if (e.getChild() instanceof JButton)
        {
	  JButton b = (JButton) e.getChild();

	  if (b.getBorder() != null)
	    borders.put(b, b.getBorder());
        }

      if (isRolloverBorders())
	setBorderToRollover(e.getChild());
      else
	setBorderToNonRollover(e.getChild());

      cachedBounds = toolBar.getPreferredSize();
      cachedOrientation = toolBar.getOrientation();
    }

    /**
     * This method is responsible for giving the child components their
     * original borders when they are removed.
     *
     * @param e The ContainerEvent.
     */
    public void componentRemoved(ContainerEvent e)
    {
      setBorderToNormal(e.getChild());
      cachedBounds = toolBar.getPreferredSize();
      cachedOrientation = toolBar.getOrientation();
    }
  }

  /**
   * This is the floating window that is returned when getFloatingWindow is
   * called.
   */
  private class ToolBarDialog extends JDialog implements UIResource
  {
    /**
     * Creates a new ToolBarDialog object with the name given by the JToolBar.
     */
    public ToolBarDialog()
    {
      super();
      setName((toolBar.getName() != null) ? toolBar.getName() : "");
    }
  }

  /**
   * DOCUMENT ME!
   */
  protected class ToolBarFocusListener implements FocusListener
  {
    /**
     * Creates a new ToolBarFocusListener object.
     */
    protected ToolBarFocusListener()
    {
      // FIXME: implement.
    }

    /**
     * DOCUMENT ME!
     *
     * @param e DOCUMENT ME!
     */
    public void focusGained(FocusEvent e)
    {
      // FIXME: implement.
    }

    /**
     * DOCUMENT ME!
     *
     * @param e DOCUMENT ME!
     */
    public void focusLost(FocusEvent e)
    {
      // FIXME: implement.
    }
  }

  /**
   * This helper class acts as the border for the JToolBar.
   */
  private static class ToolBarBorder implements Border
  {
    /** The size of the larger, draggable side of the border. */
    private static final int offset = 10;

    /** The other sides. */
    private static final int regular = 2;

    /**
     * This method returns the border insets for the JToolBar.
     *
     * @param c The Component to find insets for.
     *
     * @return The border insets.
     */
    public Insets getBorderInsets(Component c)
    {
      if (c instanceof JToolBar)
        {
	  JToolBar tb = (JToolBar) c;
	  int orientation = tb.getOrientation();

	  if (! tb.isFloatable())
	    return new Insets(regular, regular, regular, regular);
	  else if (orientation == SwingConstants.HORIZONTAL)
	    return new Insets(regular, offset, regular, regular);
	  else
	    return new Insets(offset, regular, regular, regular);
        }

      return new Insets(0, 0, 0, 0);
    }

    /**
     * This method returns whether the border is opaque.
     *
     * @return Whether the border is opaque.
     */
    public boolean isBorderOpaque()
    {
      return false;
    }

    /**
     * This method paints the ribbed area of the border.
     *
     * @param g The Graphics object to paint with.
     * @param x The x coordinate of the area.
     * @param y The y coordinate of the area.
     * @param w The width of the area.
     * @param h The height of the area.
     * @param size The size of the bump.
     * @param c The color of the bumps.
     */
    private void paintBumps(Graphics g, int x, int y, int w, int h, int size,
                            Color c)
    {
      Color saved = g.getColor();
      g.setColor(c);

      int hgap = 2 * size;
      int vgap = 4 * size;
      int count = 0;

      for (int i = x; i < (w + x); i += hgap)
	for (int j = ((count++ % 2) == 0) ? y : (y + (2 * size)); j < (h + y);
	     j += vgap)
	  g.fillRect(i, j, size, size);

      g.setColor(saved);
    }

    /**
     * This method paints the border around the given Component.
     *
     * @param c The Component whose border is being painted.
     * @param g The Graphics object to paint with.
     * @param x The x coordinate of the component.
     * @param y The y coordinate of the component.
     * @param width The width of the component.
     * @param height The height of the component.
     */
    public void paintBorder(Component c, Graphics g, int x, int y, int width,
                            int height)
    {
      if (c instanceof JToolBar)
        {
	  JToolBar tb = (JToolBar) c;

	  int orientation = tb.getOrientation();

	  if (orientation == SwingConstants.HORIZONTAL)
	    {
	      paintBumps(g, x, y, offset, height, 1, Color.WHITE);
	      paintBumps(g, x + 1, y + 1, offset - 1, height - 1, 1, Color.GRAY);
	    }
	  else
	    {
	      paintBumps(g, x, y, width, offset, 1, Color.WHITE);
	      paintBumps(g, x + 1, y + 1, width - 1, offset - 1, 1, Color.GRAY);
	    }
        }
    }
  }
}
