/* ViewportLayout.java --
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.LayoutManager;
import java.awt.Point;
import java.awt.Rectangle;
import java.io.Serializable;

/**
 * ViewportLayout
 * @author	Andrew Selkirk
 * @author	Graydon Hoare
 */
public class ViewportLayout implements LayoutManager, Serializable
{
  static final long serialVersionUID = -788225906076097229L;

  public ViewportLayout() 
  {
  }
  public void addLayoutComponent(String name, Component c) 
  {
  }
  public void removeLayoutComponent(Component c) 
  {
  }
  public Dimension preferredLayoutSize(Container parent) 
  {
    JViewport vp = (JViewport)parent;
    Component view = vp.getView();
    if (view instanceof Scrollable)
      {
        Scrollable sc = (Scrollable) view;
        Dimension d = sc.getPreferredScrollableViewportSize();
        // System.err.println(this + ".preferredLayoutSize() : scrollable -> " + d);
        return d;
      }
    else
      return view.getPreferredSize();
  }
  public Dimension minimumLayoutSize(Container parent) 
  {
    JViewport vp = (JViewport)parent;
    Component view = vp.getView();
    return view.getMinimumSize();
  }
  public void layoutContainer(Container parent) 
  {
    JViewport vp = (JViewport)parent;
    Component view = vp.getView();
    Rectangle portBounds = vp.getBounds();
    Dimension viewMinimum = view.getMinimumSize();
    int width = Math.max(portBounds.width, 
                         viewMinimum.width);
    int height = Math.max(portBounds.height, 
                          viewMinimum.height);
    int x = Math.min(0, portBounds.width - width);
    int y = Math.min(0, portBounds.height - height);
    // System.err.println(this + ".layoutContainer() : width = " + width + ", height = " + height);
    vp.setViewPosition(new Point(x, y));
    vp.setViewSize(new Dimension(width, height));
  }
}
