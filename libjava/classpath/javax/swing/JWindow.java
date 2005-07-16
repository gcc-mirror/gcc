/* JWindow.java --
   Copyright (C) 2002, 2003, 2004, 2005  Free Software Foundation, Inc.

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

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.GraphicsConfiguration;
import java.awt.LayoutManager;
import java.awt.Window;
import java.awt.event.KeyEvent;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;

/**
 * Unlike JComponent derivatives, JWindow inherits from
 * java.awt.Window. But also lets a look-and-feel component to its work.
 *
 * @author Ronald Veldema (rveldema@cs.vu.nl)
 */
public class JWindow extends Window implements Accessible, RootPaneContainer
{
  private static final long serialVersionUID = 5420698392125238833L;
  
  protected JRootPane rootPane;

  /**
   * @specnote rootPaneCheckingEnabled is false to comply with J2SE 5.0
   */
  protected boolean rootPaneCheckingEnabled = false;

  protected AccessibleContext accessibleContext;

  /**
   * Tells us if we're in the initialization stage.
   * If so, adds go to top-level Container, otherwise they go
   * to the content pane for this container.
   */
  private boolean initStageDone = false;

  public JWindow()
  {
    super(SwingUtilities.getOwnerFrame());
    windowInit();
  }

  public JWindow(GraphicsConfiguration gc)
  {
    super(SwingUtilities.getOwnerFrame(), gc);
    windowInit();
  }
  
  public JWindow(Frame owner)
  {
    super(owner);
    windowInit();
  }

  public JWindow(Window owner)
  {
    super(owner);
    windowInit();
  }

  public JWindow(Window owner, GraphicsConfiguration gc)
  {
    super(owner, gc);
    windowInit();
  }

  protected void windowInit()
  {
    super.setLayout(new BorderLayout(1, 1));
    getRootPane(); // will do set/create
    // Now we're done init stage, adds and layouts go to content pane.
    initStageDone = true;
  }

  public Dimension getPreferredSize()
  {
    return super.getPreferredSize();
  }

  public void setLayout(LayoutManager manager)
  {
    // Check if we're in initialization stage.  If so, call super.setLayout
    // otherwise, valid calls go to the content pane.
    if (initStageDone)
      {
        if (isRootPaneCheckingEnabled())
          throw new Error("Cannot set layout. Use getContentPane().setLayout()"
                           + " instead.");
        getContentPane().setLayout(manager);
      }
    else
      super.setLayout(manager);
  }

  public void setLayeredPane(JLayeredPane layeredPane)
  {
    getRootPane().setLayeredPane(layeredPane);
  }

  public JLayeredPane getLayeredPane()
  {
    return getRootPane().getLayeredPane();
  }

  public JRootPane getRootPane()
  {
    if (rootPane == null)
      setRootPane(createRootPane());
    return rootPane;
  }

  protected void setRootPane(JRootPane root)
  {
    if (rootPane != null)
      remove(rootPane);

    rootPane = root;
    add(rootPane, BorderLayout.CENTER);
  }

  protected JRootPane createRootPane()
  {
    return new JRootPane();
  }

  public Container getContentPane()
  {
    return getRootPane().getContentPane();
  }

  public void setContentPane(Container contentPane)
  {
    getRootPane().setContentPane(contentPane);
  }

  public Component getGlassPane()
  {
    return getRootPane().getGlassPane();
  }

  public void setGlassPane(Component glassPane)
  {
    getRootPane().setGlassPane(glassPane);
  }


  protected void addImpl(Component comp, Object constraints, int index)
  {
    // If we're adding in the initialization stage use super.add.
    // otherwise pass the add onto the content pane.
    if (!initStageDone)
      super.addImpl(comp, constraints, index);
    else
      {
        if (isRootPaneCheckingEnabled())
          throw new Error("Do not use add() on JWindow directly. Use "
                          + "getContentPane().add() instead");
        getContentPane().add(comp, constraints, index);
      }
  }

  public void remove(Component comp)
  {
    // If we're removing the root pane, use super.remove.  Otherwise
    // pass it on to the content pane instead.
    if (comp == rootPane)
      super.remove(rootPane);
    else
      getContentPane().remove(comp);
  }

  protected boolean isRootPaneCheckingEnabled()
  {
    return rootPaneCheckingEnabled;
  }

  protected void setRootPaneCheckingEnabled(boolean enabled)
  {
    rootPaneCheckingEnabled = enabled;
  }

  public void update(Graphics g)
  {
    paint(g);
  }

  protected void processKeyEvent(KeyEvent e)
  {
    super.processKeyEvent(e);
  }

  public AccessibleContext getAccessibleContext()
  {
    return null;
  }

  protected String paramString()
  {
    return "JWindow";
  }
}
