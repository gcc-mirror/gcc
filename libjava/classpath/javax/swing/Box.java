/* Box.java --
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

import java.awt.AWTError;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.LayoutManager;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;

/**
 * A component that uses a {@link BoxLayout} as Layout Manager.
 *
 * In addition to that, this class provides a set of static methods for
 * creating some filler components ('struts' and 'glue') for use in
 * containers that are laid out using BoxLayout.
 *
 * @author Ronald Veldema (rveldema@cs.vu.nl)
 */
public class Box extends JComponent implements Accessible
{
  private static final long serialVersionUID = 1525417495883046342L;

  /**
   * Provides accessibility support for <code>Box</code>es.
   */
  protected class AccessibleBox extends Container.AccessibleAWTContainer
  {
    private static final long serialVersionUID = -7775079816389931944L;

    protected AccessibleBox()
    {
      // Nothing to do here.
    }

    public AccessibleRole getAccessibleRole()
    {
      return null;
    }
  }

  /**
   * A component that servers as a filler in BoxLayout controlled containers.
   */
  public static class Filler extends JComponent implements Accessible
  {
    private static final long serialVersionUID = -1204263191910183998L;

    /**
     * Provides accessibility support for <code>Box.Filler</code>.
     */
    protected class AccessibleBoxFiller
      extends Component.AccessibleAWTComponent
    {
      private static final long serialVersionUID = 164963348357479321L;

      protected AccessibleBoxFiller()
      {
        // Nothing to do here.
      }

      public AccessibleRole getAccessibleRole()
      {
        return null;
      }
    }

    private transient Dimension min, pref, max;

    /**
     * Creates a new instance of Filler.
     *
     * @param min the minimum size of the filler.
     * @param pref the preferred size of the filler.
     * @param max the maximum size of the filler.
     */
    public Filler(Dimension min, Dimension pref, Dimension max)
    {
      changeShape(min, pref, max);
    }

    /**
     * Changes the dimensions of this Filler.
     *
     * @param min the new minimum size of the filler.
     * @param pref the new preferred size of the filler.
     * @param max the new maximum size of the filler.
     */
    public void changeShape(Dimension min, Dimension pref, Dimension max)
    {
      this.min = min;
      this.pref = pref;
      this.max = max;
    }

    public AccessibleContext getAccessibleContext()
    {
      if (accessibleContext == null)
        accessibleContext = new AccessibleBoxFiller();
      return accessibleContext;
    }

    /**
     * Returns the maximum size of this Filler.
     *
     * @return the maximum size of this Filler.
     */
    public Dimension getMaximumSize()
    {
      return max;
    }

    /**
     * Returns the minimum size of this Filler.
     *
     * @return the minimum size of this Filler.
     */
    public Dimension getMinimumSize()
    {
      return min;
    }

    /**
     * Returns the preferred size of this Filler.
     *
     * @return the preferred size of this Filler.
     */
    public Dimension getPreferredSize()
    {
      return pref;
    }
  }

  /**
   * Creates a new Box component, that lays out its children according
   * to the <code>axis</code> parameter.
   *
   * @param axis the orientation of the BoxLayout.
   *
   * @see BoxLayout#X_AXIS
   * @see BoxLayout#Y_AXIS
   * @see BoxLayout#LINE_AXIS
   * @see BoxLayout#PAGE_AXIS
   */
  public Box(int axis)
  {
    super.setLayout(new BoxLayout(this, axis));
  }

  /**
   * Creates a filler component which acts as glue between components.
   * It does not take space unless some extra space is available. If extra
   * space is available, this component can expand in both X and Y directions.
   *
   * @return a glue-like filler component.
   */
  public static Component createGlue()
  {
    Filler glue = new Filler(new Dimension(0, 0), new Dimension(0, 0),
                             new Dimension(Short.MAX_VALUE, Short.MAX_VALUE));
    return glue;
  }

  public static Box createHorizontalBox()
  {
    return new Box(BoxLayout.X_AXIS);
  }

  /**
   * Creates a filler component which acts as glue between components.
   * It does not take space unless some extra space is available. If extra
   * space is available, this component can expand in the X direction.
   *
   * @return a glue-like filler component.
   */
  public static Component createHorizontalGlue()
  {
    Filler glue = new Filler(new Dimension(0, 0), new Dimension(0, 0),
                             new Dimension(Short.MAX_VALUE, 0));
    return glue;
  }

  /**
   * Creates a filler component which acts as strut between components.
   * It will fill exactly the specified horizontal size.
   *
   * @param width the width of this strut in pixels.
   *
   * @return a strut-like filler component.
   */
  public static Component createHorizontalStrut(int width)
  {
    Filler strut = new Filler(new Dimension(width, 0),
                              new Dimension(width, 0),
                              new Dimension(width, Integer.MAX_VALUE));
    return strut;
  }

  public static Component createRigidArea(Dimension d)
  {
    return new Filler(d, d, d);
  }

  public static Box createVerticalBox()
  {
    return new Box(BoxLayout.Y_AXIS);
  }

  /**
   * Creates a filler component which acts as glue between components.
   * It does not take space unless some extra space is available. If extra
   * space is available, this component can expand in the Y direction.
   *
   * @return a glue-like filler component.
   */
  public static Component createVerticalGlue()
  {
    return createGlue();
  }

  /**
   * Creates a filler component which acts as strut between components.
   * It will fill exactly the specified vertical size.
   *
   * @param height the height of this strut in pixels.
   *
   * @return a strut-like filler component.
   */
  public static Component createVerticalStrut(int height)
  {
    Filler strut = new Filler(new Dimension(0, height),
                              new Dimension(0, height),
                              new Dimension(Integer.MAX_VALUE, height));
    return strut;
  }

  public void setLayout(LayoutManager l)
  {
    throw new AWTError("Not allowed to set layout managers for boxes.");
  }

  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleBox();
    return accessibleContext;
  }


}
