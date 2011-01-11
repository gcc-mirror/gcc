/* TreePath.java --
   Copyright (C) 2002, 2005, 2006,  Free Software Foundation, Inc.

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

import java.io.Serializable;
import java.util.Arrays;

/**
 * A <code>TreePath</code> represents a sequence of tree elements that form
 * a path starting from the root of a tree.  A tree element can be represented
 * by any {@link Object}.
 *
 * @author Andrew Selkirk
 */
public class TreePath implements Serializable
{
  static final long serialVersionUID = 4380036194768077479L;

  /**
   * The actual patch. The {@link DefaultTreeSelectionModel#clone()}
   * assumes that the TreePath is immutable, so it is marked final here.
   */
  private final Object[] path;

  /**
   * The parent path (to be reused).
   */
  private transient TreePath parentPath;


  /**
   * Creates a path from the list of objects representing tree elements.  The
   * incoming array is copied so that subsequent changes do not affect this
   * tree path.
   *
   * @param path  the elements in the path (<code>null</code> not permitted).
   *
   * @throws IllegalArgumentException if <code>path</code> is <code>null</code>.
   */
  public TreePath(Object[] path)
  {
    if (path == null)
      throw new IllegalArgumentException("Null 'path' not permitted.");
    this.path = new Object[path.length];
    System.arraycopy(path, 0, this.path, 0, path.length);
  }

  /**
   * Creates a new path from a single element.
   *
   * @param element the element (<code>null</code> not permitted).
   *
   * @throws IllegalArgumentException if <code>element</code> is
   *         <code>null</code>.
   */
  public TreePath(Object element)
  {
    path = new Object[1];
    path[0] = element;
  }

  /**
   * Creates a new tree path by adding the specified <code>element</code> to
   * the <code>path</code>.
   *
   * @param path  a tree path.
   * @param element  a path element.
   */
  protected TreePath(TreePath path, Object element)
  {
    if (element == null)
      throw new NullPointerException("Null 'element' argument.");
    Object[] treepath = path.getPath();

    // Create Tree Path
    this.path = new Object[treepath.length + 1];
    System.arraycopy(treepath, 0, this.path, 0, treepath.length);
    this.path[treepath.length] = element;
  }

  /**
   * Creates a new tree path using the first <code>length</code> elements
   * from the given array.
   *
   * @param path  the path elements.
   * @param length  the path length.
   */
  protected TreePath(Object[] path, int length)
  {
    // Create Path
    this.path = new Object[length];
    System.arraycopy(path, 0, this.path, 0, length);
  }

  /**
   * Default constructor.
   */
  protected TreePath()
  {
    path = new Object[0];
  }


  /**
   * Returns a hashcode for the path.
   *
   * @return A hashcode.
   */
  public int hashCode()
  {
    return getLastPathComponent().hashCode();
  }

  /**
   * Tests this path for equality with an arbitrary object.  An object is
   * considered equal to this path if and only if:
   * <ul>
   * <li>the object is not <code>null</code>;</li>
   * <li>the object is an instanceof {@link TreePath};</li>
   * <li>the object contains the same elements in the same order as this
   * {@link TreePath};</li>
   * </ul>
   *
   * @param object  the object (<code>null</code> permitted).
   *
   * @return <code>true</code> if <code>obj</code> is equal to this tree path,
   *         and <code>false</code> otherwise.
   */
  public boolean equals(Object object)
  {
    Object[] treepath;
    int index;

    if (object instanceof TreePath)
      {
        treepath = ((TreePath) object).getPath();
        if (treepath.length != path.length)
          return false;
        for (index = 0; index < path.length; index++)
          {
            if (!path[index].equals(treepath[index]))
              return false;
          }

        // Tree Path's are equals
        return true;
      }

    // Unequal
    return false;
  }

  /**
   * Returns a string representation of this path.
   *
   * @return A string representation of this path.
   */
  public String toString()
  {
    if (path.length == 1)
      return String.valueOf(path[0]);
    else
      return Arrays.asList(path).toString();
  }

  /**
   * Returns an array containing the path elements.
   *
   * @return An array containing the path elements.
   */
  public Object[] getPath()
  {
    return (Object[]) path.clone();
  }

  /**
   * Returns the last object in the path.
   *
   * @return The last object in the path.
   */
  public Object getLastPathComponent()
  {
    return path[path.length - 1];
  }

  /**
   * Returns the number of elements in the path.
   *
   * @return The number of elements in the path.
   */
  public int getPathCount()
  {
    return path.length;
  }

  /**
   * Returns the element at the specified position in the path.
   *
   * @param position the element position (<code>0 &lt N - 1</code>, where
   *                 <code>N</code> is the number of elements in the path).
   *
   * @return The element at the specified position.
   *
   * @throws IllegalArgumentException if <code>position</code> is outside the
   *         valid range.
   */
  public Object getPathComponent(int position)
  {
    if (position < 0 || position >= getPathCount())
      throw new IllegalArgumentException("Invalid position: " + position);
    return path[position];
  }

  /**
   * Returns <code>true</code> if <code>path</code> is a descendant of this
   * path, and <code>false</code> otherwise.  If <code>path</code> is
   * <code>null</code>, this method returns <code>false</code>.
   *
   * @param path  the path to check (<code>null</code> permitted).
   *
   * @return <code>true</code> if <code>path</code> is a descendant of this
   *         path, and <code>false</code> otherwise
   */
  public boolean isDescendant(TreePath path)
  {
    if (path == null)
      return false;
    int count = getPathCount();
    int otherPathLength = path.getPathCount();
    if (otherPathLength < count)
      return false;
    while (otherPathLength > count)
      {
        otherPathLength--;
        path = path.getParentPath();
      }

    return equals(path);
  }

  /**
   * Creates a new path that is equivalent to this path plus the specified
   * element.
   *
   * @param element  the element.
   *
   * @return A tree path.
   */
  public TreePath pathByAddingChild(Object element)
  {
    return new TreePath(this, element);
  }

  /**
   * Returns the parent path, which is a path containing all the same elements
   * as this path, except for the last one.  If this path contains only one
   * element, the method returns <code>null</code>.
   *
   * @return The parent path, or <code>null</code> if this path has only one
   *         element.
   */
  public TreePath getParentPath()
  {
    // If this path has only one element, then we return null. That
    // is what the JDK does.
    if (path.length <= 1)
      return null;

    // Reuse the parent path, if possible. The parent path is requested
    // during the tree repainting, so reusing generates a lot less garbage.
    if (parentPath == null)
      parentPath = new TreePath(this.getPath(), path.length - 1);

    return parentPath;
  }
}
