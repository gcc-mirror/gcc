/* FixedHeightLayoutCache.java -- Fixed cell height tree layout cache
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

import gnu.javax.swing.tree.GnuPath;

import java.awt.Rectangle;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.Set;
import java.util.Vector;

import javax.swing.event.TreeModelEvent;


/**
 * The fixed height tree layout. This class assumes that all cells in the tree
 * have the same fixed height. This may be not the case, for instance, if leaves
 * and branches have different height, of if the tree rows may have arbitrary
 * variable height. This class will also work if the NodeDimensions are not
 * set.  
 * 
 * @author Audrius Meskauskas
 * @author Andrew Selkirk 
 */
public class FixedHeightLayoutCache
		extends VariableHeightLayoutCache
{
  /**
   * The cached node record.
   */
  class NodeRecord
  {
    NodeRecord(int aRow, int aDepth, Object aNode, Object aParent)
    {
      row = aRow;
      depth = aDepth;
      parent = aParent;
      node = aNode;
      
      isExpanded = expanded.contains(aNode); 
    }
    
    /**
     * The row, where the tree node is displayed.
     */
    final int row;    
    
    /**
     * The nesting depth
     */
    final int depth;
    
    /**
     * The parent of the given node, null for the root node.
     */
    final Object parent;
    
    /**
     * This node.
     */
    final Object node;
    
    /**
     * True for the expanded nodes. The value is calculated in constructor.
     * Using this field saves one hashtable access operation.
     */
    final boolean isExpanded;
    
    /**
     * The cached bounds of the tree row.
     */
    Rectangle bounds;
    
    /**
     * The path from the tree top to the given node (computed under first
     * demand)
     */
    private TreePath path;
    
    /**
     * Get the path for this node. The derived class is returned,
     * making check for the last child of some parent easier.
     */
    TreePath getPath()
    {
      if (path == null)
        {
          boolean lastChild = false;
          if (parent != null)
            {
              int nc = treeModel.getChildCount(parent);
              if (nc > 0)
                {
                  int n = treeModel.getIndexOfChild(parent, node);
                  if (n == nc - 1)
                    lastChild = true;
                }
            }

          LinkedList<Object> lpath = new LinkedList<Object>();
          NodeRecord rp = this;
          while (rp != null)
            {
              lpath.addFirst(rp.node);
              if (rp.parent != null)
                {
                  Object parent = rp.parent;
                  rp = (NodeRecord) nodes.get(parent);
                  // Add the root node, even if it is not visible.
                  if (rp == null)
                    lpath.addFirst(parent);
                }
              else
                rp = null;
            }
          path = new GnuPath(lpath.toArray(), lastChild);
        }
      return path;
    }
    
    /**
     * Get the rectangle bounds (compute, if required).
     */
    Rectangle getBounds()
    {
      // This method may be called in the context when the tree rectangle is
      // not known. To work around this, it is assumed near infinitely large.
      if (bounds == null)
        bounds = getNodeDimensions(node, row, depth, isExpanded, 
                                   new Rectangle());
      return bounds;      
    }
  }
  
  /**
   * The set of all expanded tree nodes.
   */
  Set<Object> expanded = new HashSet<Object>();
  
  /**
   * Maps nodes to the row numbers.
   */
  Hashtable<Object,NodeRecord> nodes = new Hashtable<Object,NodeRecord>();
  
  /**
   * Maps row numbers to nodes.
   */
  Hashtable<Integer,Object> row2node = new Hashtable<Integer,Object>();
  
  /**
   * If true, the row map must be recomputed before using.
   */
  boolean dirty;
  
  /**
   * The cumulative height of all rows.
   */
  int totalHeight;
  
  /**
   * The maximal width.
   */
  int maximalWidth;

  /**
   * Creates the unitialised instance. Before using the class, the row height
   * must be set with the {@link #setRowHeight(int)} and the model must be set
   * with {@link #setModel(TreeModel)}. The node dimensions may not be set.
   */
  public FixedHeightLayoutCache()
  {
    // Nothing to do here.
  } 

  /**
   * Get the total number of rows in the tree. Every displayed node occupies the
   * single row. The root node row is included if the root node is set as
   * visible (false by default).
   * 
   * @return int the number of the displayed rows.
   */
  public int getRowCount()
  {
    if (dirty) update();
    return row2node.size();
  } 
  
  /**
   * Refresh the row map.
   */
  private final void update()
  {
    nodes.clear();
    row2node.clear();
    
    totalHeight = maximalWidth = 0;

    Object root = treeModel.getRoot();

    if (rootVisible)
      {
        countRows(root, null, 0);
      }
    else
      {
        int sc = treeModel.getChildCount(root);
        for (int i = 0; i < sc; i++)
          {
            Object child = treeModel.getChild(root, i);
            countRows(child, root, 0);
          }
      }
    dirty = false;
  }
  
  /**
   * Recursively counts all rows in the tree.
   */
  private final void countRows(Object node, Object parent, int depth)
  {
    Integer n = new Integer(row2node.size());
    row2node.put(n, node);
    
    NodeRecord nr = new NodeRecord(n.intValue(), depth, node, parent);
    nodes.put(node, nr);
     
    // For expanded nodes and for the root node.
    if (expanded.contains(node))
      {
        int sc = treeModel.getChildCount(node);
        int deeper = depth + 1;
        for (int i = 0; i < sc; i++)
          {
            Object child = treeModel.getChild(node, i);
            countRows(child, node, deeper);
          }
      }
  }

  /**
   * Discard the bound information for the given path.
   * 
   * @param path the path, for that the bound information must be recomputed.
   */
  public void invalidatePathBounds(TreePath path)
  {
    NodeRecord r = (NodeRecord) nodes.get(path.getLastPathComponent());
    if (r != null)
      r.bounds = null;
  } 

  /**
   * Mark all cached information as invalid.
   */
  public void invalidateSizes()
  {
    dirty = true;
  } 

  /**
   * Set the expanded state of the given path. The expansion states must be
   * always updated when expanding and colapsing the tree nodes. Otherwise 
   * other methods will not work correctly after the nodes are collapsed or
   * expanded.
   *
   * @param path the tree path, for that the state is being set.
   * @param isExpanded the expanded state of the given path.
   */
  public void setExpandedState(TreePath path, boolean isExpanded)
  {
    if (isExpanded)
      expanded.add(path.getLastPathComponent());
    else
      expanded.remove(path.getLastPathComponent());
    
    dirty = true;
  }
  
  /**
   * Get the expanded state for the given tree path.
   * 
   * @return true if the given path is expanded, false otherwise.
   */
  public boolean isExpanded(TreePath path)
  {
    return expanded.contains(path.getLastPathComponent());
  } 

  /**
   * Get bounds for the given tree path.
   * 
   * @param path the tree path
   * @param rect the rectangle that will be reused to return the result.
   * @return Rectangle the bounds of the last line, defined by the given path.
   */
  public Rectangle getBounds(TreePath path, Rectangle rect)
  {
    if (path == null)
      return null;
    if (dirty)
      update();
    Object last = path.getLastPathComponent();
    NodeRecord r = nodes.get(last);
    if (r == null)
    // This node is not visible.
      {
        rect.x = rect.y = rect.width = rect.height = 0;
      }
    else
      {
        if (r.bounds == null)
          {
            Rectangle dim = getNodeDimensions(last, r.row, r.depth,
                                              r.isExpanded, rect);
            r.bounds = dim;
          }

        rect.setRect(r.bounds);
      }
    return rect;
  } 

  /**
   * Get the path, the last element of that is displayed in the given row.
   * 
   * @param row the row
   * @return TreePath the path
   */
  public TreePath getPathForRow(int row)
  {
    if (dirty)
      update();
    Object last = row2node.get(new Integer(row));
    if (last == null)
      return null;
    else
      {
        NodeRecord r = nodes.get(last);
        return r.getPath();
      }
  } 

  /**
   * Get the row, displaying the last node of the given path.
   * 
   * @param path the path
   * @return int the row number or -1 if the end of the path is not visible.
   */
  public int getRowForPath(TreePath path)
  {
    if (path == null)
      return -1;
    
    if (dirty) update();

    NodeRecord r = nodes.get(path.getLastPathComponent());
    if (r == null)
      return - 1;
    else
      return r.row;
  } 

  /**
   * Get the path, closest to the given point.
   * 
   * @param x the point x coordinate
   * @param y the point y coordinate
   * @return the tree path, closest to the the given point
   */
  public TreePath getPathClosestTo(int x, int y)
  {
    if (dirty)
      update();

    // As the rows have arbitrary height, we need to iterate.
    NodeRecord best = null;
    NodeRecord r;
    Enumeration<NodeRecord> en = nodes.elements();
    
    int dist = Integer.MAX_VALUE;

    while (en.hasMoreElements() && dist > 0)
      {
        r = en.nextElement();
        if (best == null)
          {
            best = r;
            dist = distance(r.getBounds(), x, y);
          }
        else
          {
            int rr = distance(r.getBounds(), x, y);
            if (rr < dist)
              {
                best = r;
                dist = rr;
              }
          }
      }

    if (best == null)
      return null;
    else
      return best.getPath();
  } 
  
  /**
   * Get the closest distance from this point till the given rectangle. Only
   * vertical distance is taken into consideration.
   */
  int distance(Rectangle r, int x, int y)
  {
    if (y < r.y)
      return r.y - y;
    else if (y > r.y + r.height)
      return y - (r.y + r.height);
    else
      return 0;
  }

  /**
   * Get the number of the visible childs for the given tree path. If the node
   * is not expanded, 0 is returned. Otherwise, the number of children is
   * obtained from the model as the number of children for the last path
   * component.
   * 
   * @param path the tree path
   * @return int the number of the visible childs (for row).
   */
  public int getVisibleChildCount(TreePath path)  
  {
    if (isExpanded(path))
      return 0; 
    else
      return treeModel.getChildCount(path.getLastPathComponent());
  } 

  /**
   * Get the enumeration over all visible paths that start from the given
   * parent path.
   * 
   * @param parentPath the parent path
   * @return the enumeration over pathes
   */
  public Enumeration<TreePath> getVisiblePathsFrom(TreePath parentPath)
  {
    if (dirty)
      update();
    Vector p = new Vector(parentPath.getPathCount());
    Object node;
    NodeRecord nr;

    for (int i = 0; i < parentPath.getPathCount(); i++)
      {
        node = parentPath.getPathComponent(i);
        nr = nodes.get(node);
        if (nr.row >= 0)
          p.add(node);
      }
    return p.elements();
  }

  /**
   * Return the expansion state of the given tree path. The expansion state
   * must be previously set with the 
   * {@link #setExpandedState(TreePath, boolean)}
   * 
   * @param path the path being checked
   * @return true if the last node of the path is expanded, false otherwise.
   */
  public boolean getExpandedState(TreePath path)
  {
    return expanded.contains(path.getLastPathComponent());
  }

  /**
   * The listener method, called when the tree nodes are changed.
   * 
   * @param event the change event
   */
  public void treeNodesChanged(TreeModelEvent event)
  {
    dirty = true;
  } 

  /**
   * The listener method, called when the tree nodes are inserted.
   * 
   * @param event the change event
   */
  public void treeNodesInserted(TreeModelEvent event)
  {
    dirty = true;
  } 

  /**
   * The listener method, called when the tree nodes are removed.
   * 
   * @param event the change event
   */
  public void treeNodesRemoved(TreeModelEvent event)
  {
    dirty = true;
  } 

  /**
   * Called when the tree structure has been changed. 
   * 
   * @param event the change event
   */
  public void treeStructureChanged(TreeModelEvent event)
  {
    dirty = true;
  } 
  
  /**
   * Set the tree model that will provide the data.
   */
  public void setModel(TreeModel newModel)
  {
    treeModel = newModel;
    // The root node is expanded by default.
    expanded.add(treeModel.getRoot());
    dirty = true;
  }
  
  /**
   * Inform the instance if the tree root node is visible. If this method
   * is not called, it is assumed that the tree root node is not visible.
   * 
   * @param visible true if the tree root node is visible, false
   * otherwise.
   */
  public void setRootVisible(boolean visible)
  {
    rootVisible = visible;
    dirty = true;
  }

  /**
   * Get the sum of heights for all rows.
   */
  public int getPreferredHeight()
  {
    if (dirty)
      update();
    totalHeight = 0;
    Enumeration<NodeRecord> en = nodes.elements();
    while (en.hasMoreElements())
      {
        NodeRecord nr = en.nextElement();
        Rectangle r = nr.getBounds();
        totalHeight += r.height;
      }
    return totalHeight;
  }

  /**
   * Get the maximal width.
   */
  public int getPreferredWidth(Rectangle value)
  {
    if (dirty)
      update();
    
    maximalWidth = 0;
    Enumeration<NodeRecord> en = nodes.elements();
    while (en.hasMoreElements())
      {
        NodeRecord nr = en.nextElement();
        Rectangle r = nr.getBounds();
        if (r.x + r.width > maximalWidth)
          maximalWidth = r.x + r.width;
      }
    return maximalWidth;
  }
  
  /**
   * Returns true if this layout supposes that all rows have the fixed
   * height.
   * 
   * @return boolean true if all rows in the tree must have the fixed
   * height (true by default).
   */
  protected boolean isFixedRowHeight()
  {
    return true; 
  }
  
}
