/* JApplet.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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


package javax.swing;

import java.applet.Applet;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.LayoutManager;
import java.awt.event.KeyEvent;

import javax.accessibility.AccessibleContext;

public class JApplet extends Applet
  implements RootPaneContainer
{
  private static final long serialVersionUID = 7269359214497372587L;
  
  protected JRootPane rootPane;
  protected boolean rootPaneCheckingEnabled;

  public JApplet()
  {
    super.setLayout(new BorderLayout(1, 1));
    getRootPane(); // will do set/create
  }

  public Dimension getPreferredSize()
  {
    return super.getPreferredSize();
  }

  public void setLayout(LayoutManager manager)
  {
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
    super.addImpl(comp, constraints, index);
  }

  public AccessibleContext getAccessibleContext()
  {
    return null;
  }

  public JMenuBar getJMenuBar()
  {
    return getRootPane().getJMenuBar();
  }

  public void setJMenuBar(JMenuBar menubar)
  {
    getRootPane().setJMenuBar(menubar);
  }

  protected String paramString()
  {
    return "JFrame";
  }

  protected void processKeyEvent(KeyEvent e)
  {
    super.processKeyEvent(e);
  }
  
  public void remove(Component comp)
  {
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
}
