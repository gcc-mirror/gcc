/* BasicToolBarUI.java
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

import java.awt.Component;
import java.awt.ComponentOrientation;
import java.awt.Container;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Point;
import java.util.Enumeration;
import javax.swing.JFrame;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JToolBar;
import javax.swing.RootPaneContainer;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.event.MouseInputListener;

import java.beans.PropertyChangeListener;
import java.awt.event.ContainerListener;
import java.awt.event.FocusListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowListener;

import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.ToolBarUI;

public class BasicToolBarUI extends ToolBarUI
{

  public class DragWindow
  {}

    protected String constraintBeforeFloating;
    protected Color dockingBorderColor;
    protected Color dockingColor;
    protected MouseInputListener dockingListener;
    //protected KeyStroke downKey
    //          Deprecated. As of Java 2 platform v1.3.
    protected BasicToolBarUI.DragWindow dragWindow;
    protected Color floatingBorderColor;
    protected Color floatingColor;
    protected int focusedCompIndex;
    //protected KeyStroke leftKey;
    //          Deprecated. As of Java 2 platform v1.3.
    protected PropertyChangeListener propertyListener;
    //protected KeyStroke rightKey;
    //          Deprecated. As of Java 2 platform v1.3.
    protected JToolBar toolBar;
    protected ContainerListener toolBarContListener;
    protected FocusListener toolBarFocusListener;
    //        protected KeyStroke upKey;
    //        Deprecated. As of Java 2 platform v1.3.

    private Dimension maximumSize;
    private Dimension minimumSize;
    private Dimension preferredSize;
    private boolean floating;
    private boolean rolloverBorders;

    BasicToolBarUI(JToolBar b)
    {
	super();
    }

    /* Can Component c dock at Point p? */
    boolean canDock(Component c, Point p)
    {
	return false;
    }

    protected MouseInputListener createDockingListener()
    {
      return null;
    }

    protected BasicToolBarUI.DragWindow createDragWindow(JToolBar toolbar)
    {
      return null;
    }

    protected JFrame createFloatingFrame(JToolBar toolbar)
    {
      return null;
    }

    protected RootPaneContainer createFloatingWindow(JToolBar toolbar)
    {
      return null;
    }

    protected WindowListener createFrameListener()
    {
      return null;
    }

    protected Border createNonRolloverBorder()
    {
      return null;
    }

    protected PropertyChangeListener createPropertyListener()
    {
      return null;
    }

    protected Border createRolloverBorder()
    {
      return null;
    }

    protected ContainerListener createToolBarContListener()
    {
      return null;
    }

    protected FocusListener createToolBarFocusListener()
    {
      return null;
    }

    public static ComponentUI createUI(JComponent c)
    {
	return new BasicToolBarUI((JToolBar) c);
    }

    protected void dragTo(Point position, Point origin)
    {
    }

    protected void floatAt(Point position, Point origin)
    {
    }

    /* Return the Color which is displayed when over a docking area */
    Color getDockingColor()
    {
	return dockingColor;
    }

    /* Return the Color which is displayed when over a floating area */
    Color getFloatingColor()
    {
	return floatingColor;
    }

    /* See ComponentUI */
  public Dimension getMaximumSize(JComponent c)
    {
      return null;
    }

    /* See ComponentUI */
  public Dimension getMinimumSize(JComponent c)
    {
      return null;
    }

    /* See ComponentUI */
  public Dimension getPreferredSize(JComponent c)
    {
      return null;
    }

    protected void installComponents()
    {
    }

    protected void installDefaults()
    {
    }

    protected void installKeyboardActions()
    {
    }

    protected void installListeners(JToolBar toolbar)
    {
    	toolbar.addMouseListener(new MouseAdapter() {
		public void mouseClicked(MouseEvent event) {
			System.err.println("toolbar clicked");
		}
	} );
    }

    /* Call setBorderToNonRollover for each child component of c */
    protected void installNonRolloverBorders(JComponent c)
    {
    }

    /* Call setBorderToNormal for each child component of c */
    protected void installNormalBorders(JComponent c)
    {
    }

    /* Call setBorderToRollover for each child component of c */
    protected void installRolloverBorders(JComponent c)
    {
    }

    public void installUI(JComponent c)
    {
	super.installUI(c);
	if (c instanceof JToolBar)
	{
	    toolBar = (JToolBar) c;
	    toolBar.setOpaque(true);
	    switch (toolBar.getOrientation()) {
		case 0: toolBar.setLayout(new GridLayout(1, 0, 4, 4));
				 break;
		case 1: toolBar.setLayout(new GridLayout(0, 1, 4, 4));
			       break;
	    }
	    installListeners(toolBar);
	}
    }

    boolean isFloating()
    {
	return false;
    }

    boolean isRolloverBorders()
    {
	return false;
    }

    protected void navigateFocusedComp(int direction)
    {
    }

    /* Make Component c have a non-rollover border (created by
       createNonRolloverBorder). */
    protected void setBorderToNonRollover(Component c)
    {
    }

    /* Make Component c have the border that it originally had before being
       added to the toolbar. */
    protected void setBorderToNormal(Component c)
    {
    }

    /* Make Component c have a rollover border (created by
       createRolloverBorder). */
    protected void setBorderToRollover(Component c)
    {
    }

    /* Display in Color c when over a docking area */
    void setDockingColor(Color c)
    {
	dockingColor = c;
    }

    void setFloating(boolean b, Point p)
    {
    }

    /* Display in Color c when over a floating area */
    void setFloatingColor(Color c)
    {
	floatingColor = c;
    }

    void setFloatingLocation(int x, int y)
    {
    }

    void setOrientation(int orientation)
    {
    }

    /* Set flag to enable rollover borders for the toolbar */
    void setRolloverBorders(boolean rollover)
    {
	rolloverBorders = rollover;
    }

    protected void uninstallComponents()
    {
    }

    protected void uninstallDefaults()
    {
    }

    protected void uninstallKeyboardActions()
    {
    }

    protected void uninstallListeners()
    {
    }

  public void uninstallUI(JComponent c)
    {
    }

}
