/* TreeNode.java --
   Copyright (C) 2002, 2006, Free Software Foundation, Inc.

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

import java.util.Enumeration;

/**
 * A tree node.
 *
 * @author Andrew Selkirk
 */
public interface TreeNode
{

  /**
   * Returns the parent node for this tree node, or <code>null</code> if this
   * node has no parent.
   *
   * @return The parent node (possibly <code>null</code>).
   */
  TreeNode getParent();

  /**
   * Returns the index of the specified child node, or -1 if the node is not
   * in fact a child of this node.
   *
   * @param node  the node (<code>null</code> not permitted).
   *
   * @return The index of the specified child node, or -1.
   *
   * @throws IllegalArgumentException if <code>node</code> is <code>null</code>.
   */
  int getIndex(TreeNode node);

  /**
   * Returns the child node at the given index.
   *
   * @param index  the index (in the range <code>0</code> to
   *     <code>getChildCount() - 1</code>).
   *
   * @return The child node at the given index.
   */
  TreeNode getChildAt(int index);

  /**
   * Returns the number of children for this node.
   *
   * @return The number of children for this node.
   */
  int getChildCount();

  /**
   * Returns <code>true</code> if this node allows children, and
   * <code>false</code> otherwise.
   *
   * @return A boolean.
   */
  boolean getAllowsChildren();

  /**
   * Returns <code>true</code> if this node is a leaf node, and
   * <code>false</code> otherwise.
   *
   * @return A boolean.
   */
  boolean isLeaf();

  /**
   * Returns an enumeration of the children of this node, or an empty
   * enumeration if this node has no children.
   *
   * @return An enumeration of the children of this node.
   */
  @SuppressWarnings("unchecked") // Required for API compatibility
  Enumeration children();

}
