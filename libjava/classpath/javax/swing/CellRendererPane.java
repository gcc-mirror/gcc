/* CellRendererPane.java --
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
import java.awt.Graphics;
import java.awt.Rectangle;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;

/**
 * Paints the cells of JList, JTable and JTree.
 * It intercepts the usual paint tree, so that we don't walk up and
 * repaint everything.
 *
 * @author Andrew Selkirk
 */
public class CellRendererPane
  extends Container
  implements Accessible
{
  private static final long serialVersionUID = -7642183829532984273L;

  /**
   * Provides accessibility support for CellRendererPanes.
   */
  protected class AccessibleCellRendererPane extends AccessibleAWTContainer
  {
    private static final long serialVersionUID = -8981090083147391074L;

    /**
     * Constructor AccessibleCellRendererPane
     */
    protected AccessibleCellRendererPane()
    {
    }

    /**
     * getAccessibleRole
     * @returns AccessibleRole
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.PANEL;
    }
  }

  /**
   * accessibleContext
   */
  protected AccessibleContext accessibleContext = null;


  //-------------------------------------------------------------
  // Initialization ---------------------------------------------
  //-------------------------------------------------------------

  /**
   * Constructs a new CellRendererPane.
   */
  public CellRendererPane()
  {
  } // CellRendererPane()


  //-------------------------------------------------------------
  // Methods ----------------------------------------------------
  //-------------------------------------------------------------

  /**
   * Should not be called.
   *
   * @param graphics not used here
   */
  public void update(Graphics graphics)
  {
  } // update()

  /**
   * Despite normal behaviour this does <em>not</em> cause the container
   * to be invalidated. This prevents propagating up the paint tree.
   */
  public void invalidate()
  {
  } // invalidate()

  /**
   * Should not be called.
   *
   * @param graphics not used here
   */
  public void paint(Graphics graphics)
  {
  }

  /**
   * Overridden to check if a component is already a child of this Container.
   * If it's already a child, nothing is done. Otherwise we pass this to
   * <code>super.addImpl()</code>.
   *
   * @param c the component to add
   * @param constraints not used here
   * @param index not used here
   */
  protected void addImpl(Component c, Object constraints, int index)
  {
    if (!isAncestorOf(c))
      {
        super.addImpl(c, constraints, index);
      }
  } // addImpl()

  /**
   * Paints the specified component <code>c</code> on the {@link Graphics}
   * context <code>graphics</code>. The Graphics context is tranlated to
   * (x,y) and the components bounds are set to (w,h). If
   * <code>shouldValidate</code>
   * is set to true, then the component is validated before painting.
   *
   * @param graphics the graphics context to paint on
   * @param c the component to be painted
   * @param p the parent of the component
   * @param x the X coordinate of the upper left corner where c should
            be painted
   * @param y the Y coordinate of the upper left corner where c should
            be painted
   * @param w the width of the components drawing area
   * @param h the height of the components drawing area
   * @param shouldValidate if <code>c</code> should be validated before
   *        painting
   */
  public void paintComponent(Graphics graphics, Component c,
                             Container p, int x, int y, int w, int h, 
                             boolean shouldValidate)
  {
    // reparent c
    addImpl(c, null, 0);

    // translate to (x,y)
    graphics.translate(x, y);

    // set bounds of c
    c.setBounds(0, 0, w, h);

    // validate if necessary
    if (shouldValidate)
      {
        c.validate();
      }

    // paint component
    c.paint(graphics);

    // untranslate g
    graphics.translate(-x, -y);

  } // paintComponent()

  /**
   * Paints the specified component <code>c</code> on the {@link Graphics}
   * context <code>graphics</code>. The Graphics context is tranlated to (x,y)
   * and the components bounds are set to (w,h). The component is <em>not</em>
   * validated before painting.
   *
   * @param graphics the graphics context to paint on
   * @param c the component to be painted
   * @param p the parent of the component
   * @param x the X coordinate of the upper left corner where c should
            be painted
   * @param y the Y coordinate of the upper left corner where c should
            be painted
   * @param w the width of the components drawing area
   * @param h the height of the components drawing area
   */
  public void paintComponent(Graphics graphics, Component c,
                             Container p, int x, int y, int w, int h)
  {
    paintComponent(graphics, c, p, x, y, w, h, false);
  } // paintComponent()

  /**
   * Paints the specified component <code>c</code> on the {@link Graphics}
   * context <code>g</code>. The Graphics context is tranlated to (r.x,r.y) and
   * the components bounds are set to (r.width,r.height).
   * The component is <em>not</em>
   * validated before painting.
   *
   * @param graphics the graphics context to paint on
   * @param c the component to be painted
   * @param p the component on which we paint
   * @param r the bounding rectangle of c
   */
  public void paintComponent(Graphics graphics, Component c,
                             Container p, Rectangle r)
  {
    paintComponent(graphics, c, p, r.x, r.y, r.width, r.height);
  } // paintComponent()

  /**
   * getAccessibleContext <em>TODO</em>
   * @return AccessibleContext
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleCellRendererPane();

    return accessibleContext;
  }
}
