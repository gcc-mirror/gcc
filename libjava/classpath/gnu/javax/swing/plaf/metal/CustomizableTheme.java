/* CustomizableTheme.java -- A customizable metal theme
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.javax.swing.plaf.metal;

import java.awt.Color;

import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.metal.DefaultMetalTheme;

/**
 * A Metal theme that can be customized by setting the primary and secondary
 * colors.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class CustomizableTheme
    extends DefaultMetalTheme
    implements Cloneable
{

  /**
   * The primary1 color.
   */
  private ColorUIResource primary1;

  /**
   * The primary2 color.
   */
  private ColorUIResource primary2;

  /**
   * The primary3 color.
   */
  private ColorUIResource primary3;

  /**
   * The secondary1 color.
   */
  private ColorUIResource secondary1;

  /**
   * The secondary2 color.
   */
  private ColorUIResource secondary2;

  /**
   * The secondary3 color.
   */
  private ColorUIResource secondary3;

  /**
   * Sets the primary1 color of the theme.
   *
   * @param c the primary1 color to set
   */
  public void setPrimary1(Color c)
  {
    primary1 = new ColorUIResource(c);
  }

  /**
   * Returns the primary1 color of this theme.
   *
   * @return the primary1 color of this theme
   */
  public ColorUIResource getPrimary1()
  {
    return primary1 == null ? super.getPrimary1() : primary1;
  }


  /**
   * Sets the primary2 color of the theme.
   *
   * @param c the primary2 color to set
   */
  public void setPrimary2(Color c)
  {
    primary2 = new ColorUIResource(c);
  }

  /**
   * Returns the primary2 color of this theme.
   *
   * @return the primary2 color of this theme
   */
  public ColorUIResource getPrimary2()
  {
    return primary2 == null ? super.getPrimary2() : primary2;
  }

  /**
   * Sets the primary3 color of the theme.
   *
   * @param c the primary3 color to set
   */
  public void setPrimary3(Color c)
  {
    primary3 = new ColorUIResource(c);
  }

  /**
   * Returns the primary3 color of this theme.
   *
   * @return the primary3 color of this theme
   */
  public ColorUIResource getPrimary3()
  {
    return primary3 == null ? super.getPrimary3() : primary3;
  }

  /**
   * Sets the secondary1 color of the theme.
   *
   * @param c the secondary1 color to set
   */
  public void setSecondary1(Color c)
  {
    secondary1 = new ColorUIResource(c);
  }

  /**
   * Returns the secondary1 color of this theme.
   *
   * @return the secondary1 color of this theme
   */
  public ColorUIResource getSecondary1()
  {
    return secondary1 == null ? super.getSecondary1() : secondary1;
  }

  /**
   * Sets the secondary2 color of the theme.
   *
   * @param c the secondary2 color to set
   */
  public void setSecondary2(Color c)
  {
    secondary2 = new ColorUIResource(c);
  }

  /**
   * Returns the secondary2 color of this theme.
   *
   * @return the secondary2 color of this theme
   */
  public ColorUIResource getSecondary2()
  {
    return secondary2 == null ? super.getSecondary2() : secondary2;
  }

  /**
   * Sets the secondary3 color of the theme.
   *
   * @param c the secondary3 color to set
   */
  public void setSecondary3(Color c)
  {
    secondary3 = new ColorUIResource(c);
  }

  /**
   * Returns the secondary3 color of this theme.
   *
   * @return the secondary3 color of this theme
   */
  public ColorUIResource getSecondary3()
  {
    return secondary3 == null ? super.getSecondary3() : secondary3;
  }

  /**
   * Returns a clone of this theme.
   *
   * @return a clone of this theme
   */
  public Object clone()
    throws CloneNotSupportedException
  {
    return super.clone();
  }
}
