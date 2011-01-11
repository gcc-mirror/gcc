/* TreeSelectionEvent.java --
   Copyright (C) 2002, 2004, 2005, 2006, Free Software Foundation, Inc.

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


package javax.swing.event;

import java.util.EventObject;

import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

/**
 * An event that carries information about a change to a
 * {@link TreeSelectionModel}.
 *
 * @see TreeSelectionListener
 *
 * @author Andrew Selkirk
 */
public class TreeSelectionEvent extends EventObject
{

  /**
   * The paths that have been added or removed from the selection.
   */
  protected TreePath[] paths;

  /**
   * Flags indicating if the paths were added (<code>true</code>) or removed
   * (<code>false</code>) from the selection.
   */
  protected boolean[] areNew;

  /**
   * The old lead selection path (may be <code>null</code>).
   */
  protected TreePath oldLeadSelectionPath;

  /**
   * The new lead selection path (may be <code>null</code>).
   */
  protected TreePath newLeadSelectionPath;

  /**
   * Creates a new <code>TreeSelectionEvent</code>.
   *
   * @param source  the source (usually a {@link TreeSelectionModel},
   *                <code>null</code> not permitted).
   * @param paths  an array of the paths that have been added to or removed
   *     from the selection.
   * @param areNew  a flag for each path where <code>true</code> indicates the
   *     corresponding path has been added to the selection and
   *     <code>false</code> indicates the path has been removed.
   * @param oldLeadSelectionPath  the old lead selection path (<code>null</code>
   *     permitted).
   * @param newLeadSelectionPath  the new lead selection path (<code>null</code>
   *     permitted).
   *
   * @throws IllegalArgumentException if <code>source</code> is
   *     <code>null</code>.
   */
  public TreeSelectionEvent(Object source, TreePath[] paths,
                            boolean[] areNew, TreePath oldLeadSelectionPath,
                            TreePath newLeadSelectionPath)
  {
    super(source);
    this.paths                                  = paths;
    this.areNew                                 = areNew;
    this.oldLeadSelectionPath   = oldLeadSelectionPath;
    this.newLeadSelectionPath   = newLeadSelectionPath;
  }

  /**
   * Creates a new <code>TreeSelectionEvent</code>.
   *
   * @param source  the event source (usually a {@link TreeSelectionModel},
   *     <code>null</code> not permitted).
   * @param path  the path.
   * @param isNew <code>true</code> indicates that <code>path</code> has been
   *     added to the selection, and <code>false</code> indicates that it has
   *     been removed.
   * @param oldLeadSelectionPath  the old lead selection path (<code>null</code>
   *     permitted).
   * @param newLeadSelectionPath  the new lead selection path (<code>null</code>
   *     permitted).
   *
   * @throws IllegalArgumentException if <code>source</code> is
   *     <code>null</code>.
   */
  public TreeSelectionEvent(Object source, TreePath path,
                            boolean isNew, TreePath oldLeadSelectionPath,
                            TreePath newLeadSelectionPath)
  {
    super(source);
    this.paths = new TreePath[]{path};
    this.areNew = new boolean[]{isNew};
    this.oldLeadSelectionPath   = oldLeadSelectionPath;
    this.newLeadSelectionPath   = newLeadSelectionPath;
  }

  /**
   * Returns the first path element.
   *
   * @return The first path element.
   *
   * @see #getPaths()
   */
  public TreePath getPath()
  {
    return paths[0];
  }

  /**
   * Returns an array of the paths that changed in the selection.
   *
   * @return The paths that changed in the selection.
   *
   * @see #isAddedPath(TreePath)
   */
  public TreePath[] getPaths()
  {
    return (TreePath[]) paths.clone();
  }

  /**
   * Returns <code>true</code> if the path returned by {@link #getPath()} has
   * been added to the selection, and <code>false</code> if it has been
   * removed.
   *
   * @return A boolean.
   *
   * @see #isAddedPath(int)
   */
  public boolean isAddedPath()
  {
    return areNew[0];
  }

  /**
   * Returns <code>true</code> if <code>path</code> has been added to the
   * selection, and <code>false</code> if the path has been removed from the
   * selection.
   *
   * @param path  the path to check.
   *
   * @return A flag indicating whether the path has been added to, or removed
   *     from, the selection.
   *
   * @throw IllegalArgumentException if <code>path</code> is not one of the
   *     paths in {@link #getPaths()}.
   *
   * @see #isAddedPath(int)
   */
  public boolean isAddedPath(TreePath path)
  {
    for (int i = paths.length - 1; i >= 0; i--)
      if (paths[i].equals(path))
        return areNew[i];

    throw new IllegalArgumentException("Unknown 'path' argument.");
  }

  /**
   * Returns <code>true</code> if the path at the specified index has been
   * added to the selection, and <code>false</code> if the path has been
   * removed from the selection.
   *
   * @param index  the path index.
   *
   * @return A flag indicating whether the path has been added to, or removed
   *     from, the selection.
   *
   * @since 1.3
   *
   * @see #isAddedPath(TreePath)
   */
  public boolean isAddedPath(int index)
  {
    return areNew[index];
  }

  /**
   * Returns the old lead selection path.
   *
   * @return The old lead selection path (possibly <code>null</code>).
   *
   * @see #getNewLeadSelectionPath()
   */
  public TreePath getOldLeadSelectionPath()
  {
    return oldLeadSelectionPath;
  }

  /**
   * Returns the new lead selection path.
   *
   * @return The new lead selection path (possibly <code>null</code>).
   *
   * @see #getOldLeadSelectionPath()
   */
  public TreePath getNewLeadSelectionPath()
  {
    return newLeadSelectionPath;
  }

  /**
   * Creates a shallow copy of this <code>TreeSelectionEvent</code>, replacing
   * the source with <code>source</code>.
   *
   * @param source  the new event source (<code>null</code> not permitted).
   *
   * @return A cloned event with another event source.
   *
   * @throws IllegalArgumentException if <code>source</code> is
   *     <code>null</code>.
   */
  public Object cloneWithSource(Object source)
  {
    return new TreeSelectionEvent (source, paths, areNew, oldLeadSelectionPath,
        newLeadSelectionPath);
  }

}
