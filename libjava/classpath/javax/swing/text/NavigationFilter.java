/* NavigationFilter.java --
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.

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


package javax.swing.text;

public class NavigationFilter
{
  public abstract static class FilterBypass
  {
    public FilterBypass()
    {
      // Do nothing here.
    }

    public abstract Caret getCaret();
    public abstract void moveDot(int dot, Position.Bias bias);
    public abstract void setDot(int dot, Position.Bias bias);
  }
  
  public NavigationFilter()
  {
    // Do nothing here.
  }

  public void moveDot(NavigationFilter.FilterBypass fb, int dot,
		      Position.Bias bias)
  {
    fb.moveDot(dot, bias);
  }

  public void setDot(NavigationFilter.FilterBypass fb, int dot,
		     Position.Bias bias)
  {
    fb.setDot(dot, bias);
  }

  /**
   * Returns the next visual position in the specified direction at which one
   * would place a caret. The default implementation forwards to the text
   * component's root view. Subclasses may wish to restrict that more.
   *
   * @param c the text component
   * @param pos the current model position
   * @param bias the bias of <code>pos</code>
   * @param dir the direction, one of {@link javax.swing.SwingConstants#NORTH},
   *        {@link javax.swing.SwingConstants#SOUTH},
   *        {@link javax.swing.SwingConstants#WEST} or
   *        {@link javax.swing.SwingConstants#EAST}
   * @param retBias the bias of the returned position
   *
   * @return the next model location to place the caret
   *
   * @throws BadLocationException when <code>pos</code> is not a valid model
   *         position
   */
  public int getNextVisualPositionFrom(JTextComponent c, int pos,
                                       Position.Bias bias, int dir,
                                       Position.Bias[] retBias)
    throws BadLocationException
  {
    return c.getUI().getNextVisualPositionFrom(c, pos, bias, dir, retBias);
  }
}
