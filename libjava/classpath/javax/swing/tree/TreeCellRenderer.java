/* TreeCellRenderer.java --
   Copyright (C) 2002, 2004, 2006,  Free Software Foundation, Inc.

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


package javax.swing.tree;

import java.awt.Component;

import javax.swing.JTree;

/**
 * A <code>TreeCellRenderer</code> is used by the {@link JTree} component to
 * paint individual tree elements (nodes).
 * 
 * @author Andrew Selkirk
 */
public interface TreeCellRenderer
{
  /**
   * Returns a component that has been configured to display one element (or 
   * node) in a {@link JTree} component.  The arguments to this method are used
   * to pass in the value and state of the element to be rendered.
   * 
   * @param tree  the tree.
   * @param value  the value to render.
   * @param selected  is the tree element selected?
   * @param expanded  is the tree element expanded?
   * @param leaf  is the tree element a leaf node?
   * @param row  the row index.
   * @param hasFocus  does the tree element have the focus?
   * 
   * @return A component that is configured for rendering the tree element.
   */
  Component getTreeCellRendererComponent(JTree tree, Object value,
                                         boolean selected, boolean expanded,
                                         boolean leaf, int row,
                                         boolean hasFocus);

}
