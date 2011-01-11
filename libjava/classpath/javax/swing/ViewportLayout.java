/* ViewportLayout.java --
   Copyright (C) 2002, 2004, 2006 Free Software Foundation, Inc.

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
import java.awt.Container;
import java.awt.Dimension;
import java.awt.LayoutManager;
import java.awt.Point;
import java.awt.Rectangle;
import java.io.Serializable;

/**
 * The default layout for {@link JViewport}. The viewport makes its view the
 * same size as itself, but not smaller than its minimum size.
 *
 * If the port extends extends into space <em>past</em> the edge of the view,
 * this layout manager moves the port up or to the left, in view space, by the
 * amount of empty space (keep the lower and right edges lined up).
 *
 * @author  Andrew Selkirk
 * @author  Graydon Hoare
 * @author  Audrius Meskauskas (audriusa@Bioinformatics.org)
 */
public class ViewportLayout implements LayoutManager, Serializable
{
  private static final long serialVersionUID = -788225906076097229L;

  public ViewportLayout()
  {
    // Nothing to do here.
  }

  /**
   * The method is not used with this manager.
   */
  public void addLayoutComponent(String name, Component c)
  {
    // Nothing to do here.
  }

  /**
   * The method is not used with this manager.
   */
  public void removeLayoutComponent(Component c)
  {
    // Nothing to do here.
  }

  /**
   * Get the preferred layout size. If the view implements
   * {@link Scrollable}, this method returns
   * {@link Scrollable#getPreferredScrollableViewportSize}.
   * Otherwise, it returns {@link Component#getPreferredSize()}.
   *
   * @return the preferred layout size, as described about.
   */
  public Dimension preferredLayoutSize(Container parent)
  {
    JViewport vp = (JViewport)parent;
    Component view = vp.getView();
    if (view != null)
      {
        if (view instanceof Scrollable)
          return ((Scrollable)view).getPreferredScrollableViewportSize();
        return view.getPreferredSize();
      }
    else
      return new Dimension();
  }

  /**
   * Get the minimum layout size. Normally this method returns the value,
   * returned by the view method {@link Component#getMinimumSize()}.
   *
   * If the view is not set, the zero size is returned.
   *
   * @param parent the viewport
   * @return the minimum layout size.
   */
  public Dimension minimumLayoutSize(Container parent)
  {
    // These values have been determined by the Mauve test for this method.
    return new Dimension(4, 4);
  }

  /**
   * Layout the view and viewport to respect the following rules. These are not
   * precisely the rules described in sun's javadocs, but they are the rules
   * which sun's swing implementation follows, if you watch its behavior:
   * <ol>
   * <li>If the port is smaller than the view, leave the view at its current
   * size.</li>
   * <li>If the view is smaller than the port, the view is top aligned.</li>
   * <li>If the view tracks the port size, the view position is always zero and
   * the size equal to the viewport size</li>
   * <li>In {@link JViewport#setViewSize(Dimension)}, the view size is never
   * set smaller that its minimum size.</li>
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

    // These dimensions and positions are in *view space*. Do not mix
    // variables in here from port space (eg. parent.getBounds()). This
    // function should be entirely in view space, because the methods on
    // the viewport require inputs in view space.

    Rectangle portBounds = port.getViewRect();
    Dimension viewPref = new Dimension(view.getPreferredSize());

    Point portLowerRight = new Point(portBounds.x + portBounds.width,
                                     portBounds.y + portBounds.height);

    // vertical implementation of the above rules
    if (view instanceof Scrollable)
      {
        Scrollable sView = (Scrollable) view;

        // If the view size matches viewport size, the port offset can
        // only be zero.
        if (sView.getScrollableTracksViewportWidth())
          {
            viewPref.width = portBounds.width;
            portBounds.x = 0;
          }
        if (sView.getScrollableTracksViewportHeight())
          {
            viewPref.height = portBounds.height;
            portBounds.y = 0;
          }
      }

     if (viewPref.width < portBounds.width)
       viewPref.width = portBounds.width;
     if (viewPref.height < portBounds.height)
       viewPref.height = portBounds.height;

    // If the view is larger than the port, the port is top and right
    // aligned.
    if (portLowerRight.x > viewPref.width)
      portBounds.x = 0;

    if (portLowerRight.y > viewPref.height)
      portBounds.y = 0;

    port.setViewSize(viewPref);
    port.setViewPosition(portBounds.getLocation());
  }

}
