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
  private static final long serialVersionUID = -788225906076097229L;

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
    if (view != null)
      return view.getPreferredSize();
    else
      return new Dimension();
  }

  public Dimension minimumLayoutSize(Container parent) 
  {
    JViewport vp = (JViewport)parent;
    Component view = vp.getView();
    if (view != null)
      return view.getMinimumSize();
    else
      return new Dimension();
  }

  /**
   * Layout the view and viewport to respect the following rules. These are
   * not precisely the rules described in sun's javadocs, but they are the
   * rules which sun's swing implementation follows, if you watch its
   * behavior:
   *
   * <ol> 
   * 
   * <li>If the port is larger than the view's minimum size, put the port
   * at view position <code>(0,0)</code> and make the view's size equal to
   * the port's.</li>
   *
   * <li>If the port is smaller than the view, leave the view at its
   * minimum size. also, do not move the port, <em>unless</em> the port
   * extends into space <em>past</em> the edge of the view. If so, move the
   * port up or to the left, in view space, by the amount of empty space
   * (keep the lower and right edges lined up)</li>
   *
   * </ol>
   *
   * @see JViewport#getViewSize
   * @see JViewport#setViewSize
   * @see JViewport#getViewPosition
   * @see JViewport#setViewPosition
   */

  public void layoutContainer(Container parent) 
  {
    // The way to interpret this function is basically to ignore the names
    // of methods it calls, and focus on the variable names here. getViewRect
    // doesn't, for example, return the view; it returns the port bounds in
    // view space. Likwise setViewPosition doesn't reposition the view; it 
    // positions the port, in view coordinates.

    JViewport port = (JViewport) parent;    
    Component view = port.getView();

    if (view == null)
      return;

    // These dimensions and positions are in *view space*.  Do not mix
    // variables in here from port space (eg. parent.getBounds()). This
    // function should be entirely in view space, because the methods on
    // the viewport require inputs in view space.

    Rectangle portBounds = port.getViewRect();
    Dimension viewPref = view.getPreferredSize();
    Dimension viewMinimum = view.getMinimumSize();
    Point portLowerRight = new Point(portBounds.x + portBounds.width,
                                     portBounds.y + portBounds.height);
        
    // vertical implementation of the above rules
    if (portBounds.height >= viewMinimum.height)
      {
        portBounds.y = 0;
        viewPref.height = portBounds.height;
      }
    else
      {
        viewPref.height = viewMinimum.height;
        int overextension = portLowerRight.y - viewPref.height;
        if (overextension > 0)
            portBounds.y -= overextension;
      }

    // horizontal implementation of the above rules
    if (portBounds.width >= viewMinimum.width)
      {
        portBounds.x = 0;
        viewPref.width = portBounds.width;
      }
    else
      {
        viewPref.width = viewMinimum.width;
        int overextension = portLowerRight.x - viewPref.width;
        if (overextension > 0)
            portBounds.x -= overextension;
      }

    port.setViewPosition(portBounds.getLocation());
    port.setViewSize(viewPref);
  }
}
