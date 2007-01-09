/* VariableHeightLayoutCache.java --
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
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.Set;
import java.util.Vector;

import javax.swing.event.TreeModelEvent;

/**
 * The fixed height tree layout. This class requires the NodeDimensions to be
 * set and ignores the row height property.
 * 
 * @specnote the methods, of this class, returning TreePath, actually returns
 * the derived class GnuPath that provides additional information for optimized
 * painting. See the GnuPath code for details.
 * 
 * @author Audrius Meskauskas
 */
public class VariableHeightLayoutCache
  extends AbstractLayoutCache
{

  private static final Rectangle RECT_CACHE = new Rectangle();

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
      bounds = new Rectangle(0, -1, 0, 0);
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
     * Get the path for this node. The derived class is returned, making check
     * for the last child of some parent easier.
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

          LinkedList lpath = new LinkedList();
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
      return bounds;      
    }
  }
  
  /**
   * The set of all expanded tree nodes.
   */
  Set expanded = new HashSet();
  
  /**
   * Maps nodes to the row numbers.
   */
  Hashtable nodes = new Hashtable();
  
  /**
   * Maps row numbers to nodes.
   */
  ArrayList row2node = new ArrayList();
  
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
  public VariableHeightLayoutCache()
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

    if (treeModel == null)
      return;

    Object root = treeModel.getRoot();
    countRows(root, null, 0, 0);
    dirty = false;
  }
  
  /**
   * Recursively counts all rows in the tree.
   */
  private final int countRows(Object node, Object parent, int depth, int y)
  {
    boolean visible = node != treeModel.getRoot() || rootVisible;
    int row = row2node.size();
    if (visible)
      {
        row2node.add(node);
      }
    NodeRecord nr = new NodeRecord(row, depth, node, parent);
    NodeDimensions d = getNodeDimensions();
    Rectangle r = RECT_CACHE;
    if (d != null)
      r = d.getNodeDimensions(node, row, depth, nr.isExpanded, r);
    else
      r.setBounds(0, 0, 0, 0);

    if (! visible)
      r.y = -1;
    else
      r.y = Math.max(0, y);

    if (isFixedRowHeight())
      r.height = getRowHeight();

    nr.bounds.setBounds(r);
    nodes.put(node, nr);

    if (visible)
      y += r.height;

    int sc = treeModel.getChildCount(node);
    int deeper = depth + 1;
    if (expanded.contains(node))
      {
        for (int i = 0; i < sc; i++)
          {
            Object child = treeModel.getChild(node, i);
            y = countRows(child, node, deeper, y);
          }
      }
    return y;
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
      {
        int length = path.getPathCount();
        for (int i = 0; i < length; i++)
          expanded.add(path.getPathComponent(i));
      }
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
    Rectangle result = null;
    NodeRecord r = (NodeRecord) nodes.get(last);
    if (r != null)
      {
        // The RI allows null arguments for rect, in which case a new Rectangle
        // is created.
        result = rect;
        if (result == null)
          result = new Rectangle(r.bounds);
        else
          result.setBounds(r.bounds);
      }
    return result;
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

    TreePath path = null;
    // Search row in the nodes map. TODO: This is inefficient, optimize this.
    Enumeration nodesEnum = nodes.elements();
    while (nodesEnum.hasMoreElements() && path == null)
      {
        NodeRecord record = (NodeRecord) nodesEnum.nextElement();
        if (record.row == row)
          path = record.getPath();
      }
    return path;
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

    if (dirty)
      update();

    NodeRecord r = (NodeRecord) nodes.get(path.getLastPathComponent());
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
    Enumeration en = nodes.elements();
    
    int dist = Integer.MAX_VALUE;

    while (en.hasMoreElements() && dist > 0)
      {
        r = (NodeRecord) en.nextElement();
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
    else if (y > r.y + r.height - 1)
      return y - (r.y + r.height - 1);
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
    if (! isExpanded(path) || treeModel == null)
      return 0; 
    else
      return treeModel.getChildCount(path.getLastPathComponent());
  } 

  /**
   * Get the enumeration over all visible pathes that start from the given
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
        nr = (NodeRecord) nodes.get(node);
        if (nr != null && nr.row >= 0)
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
    dirty = true;
    if (treeModel != null)
      {
        // The root node is expanded by default.
        expanded.add(treeModel.getRoot());
      }
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
    int height = 0;
    int rowCount = getRowCount();
    if (rowCount > 0)
      {
        NodeRecord last = (NodeRecord) nodes.get(row2node.get(rowCount - 1));
        height = last.bounds.y + last.bounds.height;
      }
    return height;
  }

  /**
   * Get the maximal width.
   */
  public int getPreferredWidth(Rectangle value)
  {
    if (dirty)
      update();
    
    maximalWidth = 0;
    Enumeration en = nodes.elements();
    while (en.hasMoreElements())
      {
        NodeRecord nr = (NodeRecord) en.nextElement();
        if (nr != null)
          {
            Rectangle r = nr.getBounds();
            int width = r.x + r.width;
            if (width > maximalWidth)
              maximalWidth = width;
          }
      }
    return maximalWidth;
  }

  /**
   * Sets the node dimensions and invalidates the cached layout.
   *
   * @param dim the dimensions to set
   */
  public void setNodeDimensions(NodeDimensions dim)
  {
    super.setNodeDimensions(dim);
    dirty = true;
  }

  /**
   * Sets the row height and marks the layout as invalid.
   *
   * @param height the row height to set
   */
  public void setRowHeight(int height)
  {
    super.setRowHeight(height);
    dirty = true;
  }
}
