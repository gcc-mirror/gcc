/* TreeUI.java
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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


package javax.swing.plaf;

import java.awt.Rectangle;
import javax.swing.JTree;
import javax.swing.tree.TreePath;


/**
 * An abstract base class for delegates that provide the user
 * interface for <code>JTree</code>.
 *
 * @see javax.swing.JTree
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public abstract class TreeUI
  extends ComponentUI
{
  /**
   * Constructs a new <code>TreeUI</code>.
   */
  public TreeUI()
  {
  }


  /**
   * Determines the geometric extent of the label that is
   * drawn for a path.
   *
   * @param tree the <code>JTree</code> for which this delegate
   *        object provides the user interface.
   *
   * @param path the path whose label extent is requested.
   *
   * @return a rectangle enclosing the label, or <code>null</code>
   *         if <code>path</code> contains invalid nodes.
   */
  public abstract Rectangle getPathBounds(JTree tree, TreePath path);


  /**
   * Creates a <code>TreePath</code> for the specified row.
   *
   * @param tree the <code>JTree</code> for which this delegate
   *        object provides the user interface.
   *
   * @param row the index of the row, which should be a number
   *        in the range <code>[0, getRowCount(tree) - 1]</code>.
   *
   * @return a <code>TreePath</code> for the specified row, or
   *         <code>null</code> if <code>row</code> is outside
   *         the valid range.
   */
  public abstract TreePath getPathForRow(JTree tree, int row);


  /**
   * Determines in which row a <code>TreePath</code> is currently
   * being displayed.
   *
   * @param tree the <code>JTree</code> for which this delegate
   *        object provides the user interface.
   *
   * @param path the path for which the caller wants to know
   *        in which row it is being displayed.
   *
   * @return a number in the range <code>[0, getRowCount(tree)
   *         - 1]</code> if the path is currently on display;
   *         <code>-1</code> if the path is not shown to the
   *        user.
   */
  public abstract int getRowForPath(JTree tree, TreePath path);


  /**
   * Counts how many rows are currently displayed.
   *
   * @param tree the <code>JTree</code> for which this delegate
   *        object provides the user interface.
   *
   * @return the number of visible rows.
   */
  public abstract int getRowCount(JTree tree);


  /**
   * Finds the path that is closest to the specified position.
   *
   * <p><img src="doc-files/TreeUI-1.png" width="300" height="250"
   * alt="[A screen shot of a JTree]" />
   *
   * <p>As shown by the above illustration, the bounds of the
   * closest path do not necessarily need to contain the passed
   * location.
   *
   * @param tree the <code>JTree</code> for which this delegate
   *        object provides the user interface.
   *
   * @param x the horizontal location, relative to the origin
   *        of <code>tree</code>.
   *
   * @param y the vertical location, relative to the origin
   *        of <code>tree</code>.
   *
   * @return the closest path, or <code>null</code> if the
   *         tree is currenlty not displaying any paths at all.
   */
  public abstract TreePath getClosestPathForLocation(JTree tree,
                                                     int x, int y);


  /**
   * Determines whether the user is currently editing a tree cell.
   *
   * @param tree the <code>JTree</code> for which this delegate
   *        object provides the user interface.
   *
   * @see #getEditingPath
   */
  public abstract boolean isEditing(JTree tree);


  /**
   * Stops editing a tree cell, committing the entered value into the
   * tree&#x2019;s model. If no editing session is active, or if the
   * active editor does not agree to stopping, nothing happens.  In
   * some look and feels, this action happens when the user has
   * pressed the enter key.
   *
   * @param tree the <code>JTree</code> for which this delegate
   *        object provides the user interface.
   *
   * @return <code>false</code> if the editing still goes on because
   *         the cell editor has objected to stopping the session;
   *         <code>true</code> if editing has been stopped.
   */
  public abstract boolean stopEditing(JTree tree);


  /**
   * Cancels editing a tree cell, discarding any entered value.
   * If no editing session is active, nothing happens. The cell
   * editor is not given an opportunity to veto the canceling.
   * In some look and feels, this action happens when the user has
   * pressed the escape key.
   *
   * @param tree the <code>JTree</code> for which this delegate
   *        object provides the user interface.
   */
  public abstract void cancelEditing(JTree tree);


  /**
   * Starts a session to edit a tree cell. If the cell editor
   * rejects editing the cell, it will just be selected.
   *
   * @param tree the <code>JTree</code> for which this delegate
   *        object provides the user interface.
   *
   * @param path the cell to edit.
   */
  public abstract void startEditingAtPath(JTree tree, TreePath path);


  /**
   * Retrieves the tree cell that is currently being edited.
   *
   * @return the currently edited path, or <code>null</code>
   *         if no editing session is currently active.
   */
  public abstract TreePath getEditingPath(JTree tree);
}
