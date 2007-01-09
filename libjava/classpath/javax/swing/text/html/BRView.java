/* BRView.java -- HTML BR tag view
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


package javax.swing.text.html;

import javax.swing.text.Element;

/**
 * Handled the HTML BR tag.
 */
class BRView
  extends InlineView
{
  /**
   * Creates the new BR view.
   * 
   * @param elem the HTML element, representing the view.
   */
  public BRView(Element elem)
  {
    super(elem);
  }
  
  /**
   * Always return ForcedBreakWeight for the X_AXIS, BadBreakWeight for the
   * Y_AXIS.
   */
  public int getBreakWeight(int axis, float pos, float len)
  {
    if (axis == X_AXIS)
      return ForcedBreakWeight;
    else
      return super.getBreakWeight(axis, pos, len);
  }
}
