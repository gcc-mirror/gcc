/* GridBagLayout - Layout manager for components according to GridBagConstraints
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


package java.awt;

import java.io.Serializable;
import java.util.Hashtable;
import java.util.HashMap;

/**
 * @author Michael Koch <konqueror@gmx.de>
 * @author Jeroen Frijters <jeroen@frijters.net>
 */
public class GridBagLayout
    implements Serializable, LayoutManager2
{
    private static final long serialVersionUID = 8838754796412211005L;

    protected static final int MINSIZE = 1;
    protected static final int PREFERREDSIZE = 2;
    protected static final int MAXGRIDSIZE = 512;

    // comptable remembers the original contraints given to us.
    // internalcomptable is used to keep track of modified constraint values
    // that we calculate, particularly when we are given RELATIVE and
    // REMAINDER constraints.
    // Constraints kept in comptable are never modified, and constraints
    // kept in internalcomptable can be modified internally only.
    protected Hashtable comptable;
    private Hashtable internalcomptable;
    protected GridBagLayoutInfo layoutInfo;
    protected GridBagConstraints defaultConstraints;

    public double[] columnWeights;
    public int[] columnWidths;
    public double[] rowWeights;
    public int[] rowHeights;

    public GridBagLayout ()
    {
	this.comptable = new Hashtable();
	this.internalcomptable = new Hashtable();
	this.defaultConstraints= new GridBagConstraints();
    }

    /**
     * Helper method to calc the sum of a range of elements in an int array.
     */
    private int sumIntArray (int[] array, int upto)
    {
	int result = 0;

	for (int i = 0; i < upto; i++)
	    result += array [i];

	return result;
    }

    /**
     * Helper method to calc the sum of all elements in an int array.
     */
    private int sumIntArray (int[] array)
    {
	return sumIntArray(array, array.length);
    }

    /**
     * Helper method to calc the sum of all elements in an double array.
     */
    private double sumDoubleArray (double[] array)
    {
	double result = 0;

	for (int i = 0; i < array.length; i++)
	    result += array [i];

	return result;
    }

    public void addLayoutComponent (String name, Component component)
    {
	// do nothing here.
    }

    public void removeLayoutComponent (Component component)
    {
	// do nothing here
    }

    public void addLayoutComponent (Component component, Object constraints)
    {
	if (constraints == null)
	    return;

	if (!(constraints instanceof GridBagConstraints))
	    throw new IllegalArgumentException();

	setConstraints (component, (GridBagConstraints) constraints);
    }

    public Dimension preferredLayoutSize (Container parent)
    {
	if (parent == null)
	    return new Dimension (0, 0);
    
	GridBagLayoutInfo li = getLayoutInfo (parent, PREFERREDSIZE);
	return getMinSize (parent, li);
    }

    public Dimension minimumLayoutSize (Container parent)
    {
	if (parent == null)
	    return new Dimension (0, 0);
    
	GridBagLayoutInfo li = getLayoutInfo (parent, MINSIZE);
	return getMinSize (parent, li);
    }

    public Dimension maximumLayoutSize (Container target)
    {
	return new Dimension (Integer.MAX_VALUE, Integer.MAX_VALUE);
    }

    public void layoutContainer (Container parent)
    {
      arrangeGrid (parent);
    }

    public float getLayoutAlignmentX (Container target)
    {
	return Component.CENTER_ALIGNMENT;
    }

    public float getLayoutAlignmentY (Container target)
    {
	return Component.CENTER_ALIGNMENT;
    }

    public void invalidateLayout (Container target)
    {
	this.layoutInfo = null;
    }

    public void setConstraints (Component component,
	GridBagConstraints constraints)
    {
	GridBagConstraints clone = (GridBagConstraints) constraints.clone();

	if (clone.gridx < 0)
	    clone.gridx = GridBagConstraints.RELATIVE;
    
	if (clone.gridy < 0)
	    clone.gridy = GridBagConstraints.RELATIVE;

	if (clone.gridwidth == 0)
	    clone.gridwidth = GridBagConstraints.REMAINDER;
	else if (clone.gridwidth < 0
	    && clone.gridwidth != GridBagConstraints.REMAINDER
	    && clone.gridwidth != GridBagConstraints.RELATIVE)
	    clone.gridwidth = 1;
    
	if (clone.gridheight == 0)
	    clone.gridheight = GridBagConstraints.REMAINDER;
	else if (clone.gridheight < 0
	    && clone.gridheight != GridBagConstraints.REMAINDER
	    && clone.gridheight != GridBagConstraints.RELATIVE)
	    clone.gridheight = 1;
    
	comptable.put (component, clone);
    }

    public GridBagConstraints getConstraints (Component component)
    {
	return (GridBagConstraints) (lookupConstraints (component).clone());
    }

    protected GridBagConstraints lookupConstraints (Component component)
    {
	GridBagConstraints result = (GridBagConstraints) comptable.get (component);

	if (result == null)
	{
	    setConstraints (component, defaultConstraints);
	    result = (GridBagConstraints) comptable.get (component);
	}
    
	return result;
    }

    private GridBagConstraints lookupInternalConstraints (Component component)
    {
	GridBagConstraints result =
            (GridBagConstraints) internalcomptable.get (component);

	if (result == null)
	{
	    result = (GridBagConstraints) lookupConstraints(component).clone();
	    internalcomptable.put (component, result);
	}
    
	return result;
    }

    /**
     * @since 1.1
     */
    public Point getLayoutOrigin ()
    {
	if (layoutInfo == null)
	    return new Point (0, 0);
    
	return new Point (layoutInfo.pos_x, layoutInfo.pos_y);
    }

    /**
     * @since 1.1
     */
    public int[][] getLayoutDimensions ()
    {
	int[][] result = new int [2][];
	if (layoutInfo == null)
	  {
	    result[0] = new int[0];
	    result[1] = new int[0];

	    return result;
	  }

	result [0] = new int [layoutInfo.cols];
	System.arraycopy (layoutInfo.colWidths, 0, result [0], 0, layoutInfo.cols);
	result [1] = new int [layoutInfo.rows];
	System.arraycopy (layoutInfo.rowHeights, 0, result [1], 0, layoutInfo.rows);
	return result;
    }

    public double[][] getLayoutWeights ()
    {
	double[][] result = new double [2][];
	if (layoutInfo == null)
	  {
	    result[0] = new double[0];
	    result[1] = new double[0];

	    return result;
	  }

	result [0] = new double [layoutInfo.cols];
	System.arraycopy (layoutInfo.colWeights, 0, result [0], 0, layoutInfo.cols);
	result [1] = new double [layoutInfo.rows];
	System.arraycopy (layoutInfo.rowWeights, 0, result [1], 0, layoutInfo.rows);
	return result;
    }

    /**
     * @since 1.1
     */
    public Point location (int x, int y)
    {
	if (layoutInfo == null)
	    return new Point (0, 0);

	int col;
	int row;
	int pixel_x = layoutInfo.pos_x;
	int pixel_y = layoutInfo.pos_y;

	for (col = 0; col < layoutInfo.cols; col++)
	{
	    int w = layoutInfo.colWidths [col];
	    if (x < pixel_x + w)
		break;

	    pixel_x += w;
	}

	for (row = 0; row < layoutInfo.rows; row++)
	{
	    int h = layoutInfo.rowHeights [row];
	    if (y < pixel_y + h)
		break;

	    pixel_y += h;
	}

	return new Point (col, row);
    }

    /**
     * Obsolete.
     */
    protected void AdjustForGravity (GridBagConstraints gbc, Rectangle rect)
    {
      // FIXME
      throw new Error ("Not implemented");
    }

    /**
     * Obsolete.
     */
    protected void ArrangeGrid (Container parent)
    {
      Component[] components = parent.getComponents();

      if (components.length == 0)
        return;

      GridBagLayoutInfo info = getLayoutInfo (parent, MINSIZE);
      if (info.cols == 0 && info.rows == 0)
        return;
      layoutInfo = info;

      // DEBUG
      //dumpLayoutInfo (layoutInfo);
    
      for(int i = 0; i < components.length; i++)
	{
          Component component = components [i];
		
          // If component is not visible we dont have to care about it.
          if (!component.isVisible())
            continue;
		
          GridBagConstraints constraints =
              lookupInternalConstraints(component);

          int cellx = sumIntArray(layoutInfo.colWidths, constraints.gridx);
          int celly = sumIntArray(layoutInfo.rowHeights, constraints.gridy);
          int cellw = sumIntArray(layoutInfo.colWidths,
                                  constraints.gridx + constraints.gridwidth) - cellx;
          int cellh = sumIntArray(layoutInfo.rowHeights,
                                  constraints.gridy + constraints.gridheight) - celly;

          Insets insets = constraints.insets;
          if (insets != null)
	    {
              cellx += insets.left;
              celly += insets.top;
              cellw -= insets.left + insets.right;
              cellh -= insets.top + insets.bottom;
	    }

          Dimension dim = component.getPreferredSize();

          // Note: Documentation says that padding is added on both sides, but
          // visual inspection shows that the Sun implementation only adds it
          // once, so we do the same.
          dim.width += constraints.ipadx;
          dim.height += constraints.ipady;

          switch(constraints.fill)
	    {
            case GridBagConstraints.HORIZONTAL:
              dim.width = cellw;
              break;
            case GridBagConstraints.VERTICAL:
              dim.height = cellh;
              break;
            case GridBagConstraints.BOTH:
              dim.width = cellw;
              dim.height = cellh;
              break;
	    }

          int x;
          int y;

          switch(constraints.anchor)
	    {
            case GridBagConstraints.NORTH:
              x = cellx + (cellw - dim.width) / 2;
              y = celly;
              break;
            case GridBagConstraints.SOUTH:
              x = cellx + (cellw - dim.width) / 2;
              y = celly + cellh - dim.height;
              break;
            case GridBagConstraints.WEST:
              x = cellx;
              y = celly + (cellh - dim.height) / 2;
              break;
            case GridBagConstraints.EAST:
              x = cellx + cellw - dim.width;
              y = celly + (cellh - dim.height) / 2;
              break;
            case GridBagConstraints.NORTHEAST:
              x = cellx + cellw - dim.width;
              y = celly;
              break;
            case GridBagConstraints.NORTHWEST:
              x = cellx;
              y = celly;
              break;
            case GridBagConstraints.SOUTHEAST:
              x = cellx + cellw - dim.width;
              y = celly + cellh - dim.height;
              break;
            case GridBagConstraints.SOUTHWEST:
              x = cellx;
              y = celly + cellh - dim.height;
              break;
            default:
              x = cellx + (cellw - dim.width) / 2;
              y = celly + (cellh - dim.height) / 2;
              break;
	    }

          component.setBounds(layoutInfo.pos_x + x, layoutInfo.pos_y + y, dim.width, dim.height);
	}

      // DEBUG
      //dumpLayoutInfo (layoutInfo);
    }

    /**
     * Obsolete.
     */
    protected GridBagLayoutInfo GetLayoutInfo (Container parent, int sizeflag)
    {
      if (sizeflag != MINSIZE && sizeflag != PREFERREDSIZE)
        throw new IllegalArgumentException();

      Dimension parentDim = parent.getSize ();
      Insets parentInsets = parent.getInsets ();
      parentDim.width -= parentInsets.left + parentInsets.right;
      parentDim.height -= parentInsets.top + parentInsets.bottom;
   
      int current_y = 0;
      int max_x = 0;
      int max_y = 0;

      // Guaranteed to contain the last component added to the given row
      // or column, whose gridwidth/height is not REMAINDER.
      HashMap lastInRow = new HashMap();
      HashMap lastInCol = new HashMap();

      // STEP 1: first we figure out how many rows/columns
      Component[] components = parent.getComponents();
      for (int i = 0; i < components.length; i++)
	{
          Component component = components [i];
		
          // If component is not visible we dont have to care about it.
          if (!component.isVisible())
            continue;
		
          // When looking up the constraint for the first time, check the
          // original unmodified constraint.  After the first time, always
          // refer to the internal modified constraint.
          GridBagConstraints originalConstraints = lookupConstraints (component);
          GridBagConstraints constraints = (GridBagConstraints) originalConstraints.clone();
          internalcomptable.put(component, constraints);

          // Cases:
          //
          // 1. gridy == RELATIVE, gridx == RELATIVE
          //
          //       use y as the row number; check for the next
          //       available slot at row y
          //
          // 2. only gridx == RELATIVE
          //
          //       check for the next available slot at row gridy
          //
          // 3. only gridy == RELATIVE
          //
          //       check for the next available slot at column gridx
          //
          // 4. neither gridx or gridy == RELATIVE
          //
          //       nothing to check; just add it


          // cases 1 and 2
          if(constraints.gridx == GridBagConstraints.RELATIVE)
            {
              if (constraints.gridy == GridBagConstraints.RELATIVE)
              constraints.gridy = current_y;

              int x;

              // Check the component that occupies the right-most spot in this
              // row. We want to add this component after it.
              // If this row is empty, add to the 0 position.
              if (!lastInRow.containsKey(new Integer(constraints.gridy))) 
                x = 0;
              else
                {
                  Component lastComponent = (Component) lastInRow.get(new Integer(constraints.gridy));
                  GridBagConstraints lastConstraints = lookupInternalConstraints(lastComponent);
                  x = lastConstraints.gridx + Math.max(1, lastConstraints.gridwidth);
                }

              // Determine if this component will fit in the slot vertically.
              // If not, bump it over to where it does fit.
              for (int y = constraints.gridy + 1; y < constraints.gridy + Math.max(1, constraints.gridheight); y++)
                {
                  if (lastInRow.containsKey(new Integer(y)))
                    {
                      Component lastComponent = (Component) lastInRow.get(new Integer(y));
                      GridBagConstraints lastConstraints = lookupInternalConstraints(lastComponent);
                      x = Math.max (x,
                                    lastConstraints.gridx + Math.max(1, lastConstraints.gridwidth));
                    }
                }

              constraints.gridx = x;
            }
          // case 3
          else if(constraints.gridy == GridBagConstraints.RELATIVE)
            {
              int y;
              // Check the component that occupies the bottom-most spot in
              // this column. We want to add this component below it.
              // If this column is empty, add to the 0 position.
              if (!lastInCol.containsKey(new Integer(constraints.gridx))) 
                y = 0;
              else
                {
                  Component lastComponent = (Component)lastInCol.get(new Integer(constraints.gridx));
                  GridBagConstraints lastConstraints = lookupInternalConstraints(lastComponent);
                  y = lastConstraints.gridy + Math.max(1, lastConstraints.gridheight);
                }

              // Determine if this component will fit in the slot horizontally.
              // If not, bump it down to where it does fit.
              for (int x = constraints.gridx + 1; x < constraints.gridx + Math.max(1, constraints.gridwidth); x++)
                {
                  if (lastInCol.containsKey(new Integer(x)))
                    {
                      Component lastComponent = (Component) lastInCol.get(new Integer(x));
                      GridBagConstraints lastConstraints = lookupInternalConstraints(lastComponent);
                      y = Math.max (y,
                                    lastConstraints.gridy + Math.max(1, lastConstraints.gridheight));
                    }
                }

              constraints.gridy = y;
            }
          // case 4: do nothing

          max_x = Math.max(max_x, 
                           constraints.gridx + Math.max(1, constraints.gridwidth));
          max_y = Math.max(max_y,
                           constraints.gridy + Math.max(1, constraints.gridheight));

          // Update our reference points for RELATIVE gridx and gridy.
          if(constraints.gridwidth == GridBagConstraints.REMAINDER)
	    {
              current_y = constraints.gridy + Math.max(1, constraints.gridheight);
	    }
          else if (constraints.gridwidth != GridBagConstraints.REMAINDER)
	    {
              for (int y = constraints.gridy; y < constraints.gridy + Math.max(1, constraints.gridheight); y++)
                {
                  if(lastInRow.containsKey(new Integer(y)))
                    {
                      Component lastComponent = (Component) lastInRow.get(new Integer(y));
                      GridBagConstraints lastConstraints = lookupInternalConstraints(lastComponent);
                      if (constraints.gridx > lastConstraints.gridx)
                        {
                          lastInRow.put(new Integer(y), component);
                        }
                    }
                  else
                    {
                      lastInRow.put(new Integer(y), component);
                    }
                }

              for (int x = constraints.gridx; x < constraints.gridx + Math.max(1, constraints.gridwidth); x++)
                {
                  if(lastInCol.containsKey(new Integer(x)))
                    {
                      Component lastComponent = (Component) lastInCol.get(new Integer(x));
                      GridBagConstraints lastConstraints = lookupInternalConstraints(lastComponent);
                      if (constraints.gridy > lastConstraints.gridy)
                        {
                          lastInCol.put(new Integer(x), component);
                        }
                    }
                  else
                    {
                      lastInCol.put(new Integer(x), component);
                    }
                }
	    }
	} // end of STEP 1
	
      boolean[] colIsOccupied = new boolean[max_x];
      boolean[] rowIsOccupied = new boolean[max_y];

      // STEP 2: Determine which cells the components occupy.
      for (int i = 0; i < components.length; i++)
        {
          Component component = components [i];
			
          // If component is not visible we dont have to care about it.
          if (!component.isVisible())
            continue;
			
          GridBagConstraints constraints = lookupInternalConstraints (component);

          // Fix up any REMAINDER and RELATIVE cells.
          if(constraints.gridwidth == GridBagConstraints.REMAINDER)
            {
              for (int y = constraints.gridy; y < constraints.gridy + Math.max(1, constraints.gridheight); y++)
                {
                  if (lastInRow.containsKey(new Integer(y)))
                    {
                      Component lastComponent = (Component) lastInRow.get(new Integer(y));
                      GridBagConstraints lastConstraints = lookupInternalConstraints(lastComponent);

                      if (lastConstraints.gridwidth == GridBagConstraints.RELATIVE)
                        {
                          constraints.gridx = max_x - 1;
                          break;
                        }
                      else
                        {
                          constraints.gridx = Math.max (constraints.gridx,
                                                        lastConstraints.gridx + Math.max (1, lastConstraints.gridwidth));
                        }
                    }
                }
              constraints.gridwidth = max_x - constraints.gridx;
            }
          else if (constraints.gridwidth == GridBagConstraints.RELATIVE)
            {
              constraints.gridwidth = max_x - constraints.gridx - 1;
            }

          if(constraints.gridheight == GridBagConstraints.REMAINDER)
            {
              for (int x = constraints.gridx; x < constraints.gridx + Math.max(1, constraints.gridwidth); x++)
                {
                  if (lastInCol.containsKey(new Integer(x)))
                    {
                      Component lastComponent = (Component) lastInRow.get(new Integer(x));
                      GridBagConstraints lastConstraints = lookupInternalConstraints(lastComponent);

                      if (lastConstraints.gridheight == GridBagConstraints.RELATIVE)
                        {
                          constraints.gridy = max_y - 1;
                          break;
                        }
                      else
                        {
                          constraints.gridy = Math.max (constraints.gridy,
                                                        lastConstraints.gridy + Math.max (1, lastConstraints.gridheight));
                        }
                    }
                }
              constraints.gridheight = max_y - constraints.gridy;
            }
          else if (constraints.gridheight == GridBagConstraints.RELATIVE)
            {
              constraints.gridheight = max_y - constraints.gridy - 1;
            }

          // For now, a row or a column is "occupied" iff a component
          // both begins and ends in that row or column.
          if (constraints.gridwidth == 1)
            colIsOccupied[constraints.gridx] = true;
          if (constraints.gridheight == 1)
            rowIsOccupied[constraints.gridy] = true;
        } // end of STEP 2

      GridBagLayoutInfo info = new GridBagLayoutInfo(max_x, max_y);

      // Check if column widths and row heights are overridden.

      for (int x = 0; x < max_x; x++)
        {
          if(columnWidths != null && columnWidths.length > x)
            info.colWidths[x] = columnWidths[x];
          if(columnWeights != null && columnWeights.length > x)
            info.colWeights[x] = columnWeights[x];
        }

      for (int y = 0; y < max_y; y++)
        {
          if(rowHeights != null && rowHeights.length > y)
            info.rowHeights[y] = rowHeights[y];
          if(rowWeights != null && rowWeights.length > y)
            info.rowWeights[y] = rowWeights[y];
        }

      // STEP 3: Distribute the weights and min sizes among rows/columns.
      for (int i = 0; i < components.length; i++)
        {
          Component component = components [i];
			
          // If component is not visible we dont have to care about it.
          if (!component.isVisible())
            continue;

          GridBagConstraints constraints = lookupInternalConstraints (component);
          GridBagConstraints originalConstraints = lookupConstraints (component);

          // Distribute the width.

          int width = (sizeflag == PREFERREDSIZE) ?
                      component.getPreferredSize().width :
                      component.getMinimumSize().width;

          if(constraints.insets != null)
            width += constraints.insets.left + constraints.insets.right;

          width += constraints.ipadx;

          int occupiedCols = constraints.gridwidth;
          int lastOccupiedCol = -1;

          for(int w = constraints.gridx; w < constraints.gridx + constraints.gridwidth; w++)
            {
              if(colIsOccupied[w])
                lastOccupiedCol = w;
              else
                occupiedCols--;
            }

          // A component needs to occupy at least one column.
          if(occupiedCols == 0)
            {
              colIsOccupied[constraints.gridx + constraints.gridwidth - 1] = true;
              lastOccupiedCol = constraints.gridx + constraints.gridwidth - 1;
            }

          for(int w = constraints.gridx; w < constraints.gridx + constraints.gridwidth - 1; w++)
            {
              if(colIsOccupied[w])
                width -= info.colWidths[w];
            }

          info.colWidths[lastOccupiedCol] = Math.max(info.colWidths[lastOccupiedCol], width);
          info.colWeights[lastOccupiedCol] = Math.max(info.colWeights[lastOccupiedCol], constraints.weightx);


          // Distribute the height.

          int height = (sizeflag == PREFERREDSIZE) ?
                       component.getPreferredSize().height :
                       component.getMinimumSize().height;

          if(constraints.insets != null)
            height += constraints.insets.top + constraints.insets.bottom;

          height += constraints.ipady;

          int occupiedRows = constraints.gridheight;
          int lastOccupiedRow = -1;

          for(int h = constraints.gridy; h < constraints.gridy + constraints.gridheight; h++)
            {
              if(rowIsOccupied[h])
                lastOccupiedRow = h;
              else
                occupiedRows--;
            }

          // A component needs to occupy at least one row.
          if(occupiedRows == 0)
            {
              rowIsOccupied[constraints.gridy + constraints.gridheight - 1] = true;
              lastOccupiedRow = constraints.gridy + constraints.gridheight - 1;
            }

          for(int h = constraints.gridy; h < constraints.gridy + constraints.gridheight; h++)
            {
              if(rowIsOccupied[h])
                height -= info.rowHeights[h];
            }

          info.rowHeights[lastOccupiedRow] = Math.max(info.rowHeights[lastOccupiedRow], height);
          info.rowWeights[lastOccupiedRow] = Math.max(info.rowWeights[lastOccupiedRow], constraints.weighty);

        } // end of STEP 3

      calcCellSizes (info.colWidths, info.colWeights, parentDim.width);
      calcCellSizes (info.rowHeights, info.rowWeights, parentDim.height);

      int totalWidth = sumIntArray(info.colWidths);
      int totalHeight = sumIntArray(info.rowHeights);
      info.pos_x = parentInsets.left + (parentDim.width - totalWidth) / 2;
      info.pos_y = parentInsets.top + (parentDim.height - totalHeight) / 2;

      // DEBUG
      //dumpLayoutInfo (info);

      return info;
    }

    /**
     * Obsolete.
     */
    protected Dimension GetMinSize (Container parent, GridBagLayoutInfo info)
    {
      if (parent == null || info == null)
        return new Dimension (0, 0);

      Insets insets = parent.getInsets();
      int width = sumIntArray (info.colWidths) + insets.left + insets.right;
      int height = sumIntArray (info.rowHeights) + insets.top + insets.bottom;
      return new Dimension (width, height);
    }

    /**
     * @since 1.4
     */
    protected Dimension getMinSize (Container parent, GridBagLayoutInfo info)
    {
      return GetMinSize (parent, info);
    }

    private void calcCellSizes (int[] sizes, double[] weights, int range)
    {
      int totalSize = sumIntArray (sizes);
      double totalWeight = sumDoubleArray (weights);

      // Rows or columns with size 0 should not be weighted in the calculation.
      for (int i = 0; i < weights.length; i++)
        {
          if (sizes[i] == 0)
            totalWeight -= weights[i];
        }

      int diff = range - totalSize;

      if (diff == 0)
        return;

      for (int i = 0; i < sizes.length; i++)
        {
          // A row or column with zero size cannot all of a sudden gain size.
          if (sizes[i] != 0.0)
            {
              int newsize = (int) (sizes[i] + (((double) diff) * weights [i] / totalWeight ));

              if (newsize > 0)
                sizes[i] = newsize;
            }
        }
    }

    private void dumpLayoutInfo (GridBagLayoutInfo info)
    {
	System.out.println ("GridBagLayoutInfo:");
	System.out.println ("cols: " + info.cols + ", rows: " + info.rows);
	System.out.print ("colWidths: ");
	dumpArray(info.colWidths);
	System.out.print ("rowHeights: ");
	dumpArray(info.rowHeights);
	System.out.print ("colWeights: ");
	dumpArray(info.colWeights);
	System.out.print ("rowWeights: ");
	dumpArray(info.rowWeights);
    }

    private void dumpArray(int[] array)
    {
	String sep = "";
	for(int i = 0; i < array.length; i++)
	{
	    System.out.print(sep);
	    System.out.print(array[i]);
	    sep = ", ";
	}
	System.out.println();
    }

    private void dumpArray(double[] array)
    {
	String sep = "";
	for(int i = 0; i < array.length; i++)
	{
	    System.out.print(sep);
	    System.out.print(array[i]);
	    sep = ", ";
	}
	System.out.println();
    }
  
    /**
     * @since 1.4
     */
    protected void arrangeGrid (Container parent)
    {
      ArrangeGrid (parent);
    }

    /**
     * @since 1.4
     */
    protected GridBagLayoutInfo getLayoutInfo (Container parent, int sizeflag)
    {
      return GetLayoutInfo (parent, sizeflag);
    }

    /**
     * @since 1.4
     */
    protected void adjustForGravity (GridBagConstraints gbc, Rectangle rect)
    {
      AdjustForGravity (gbc, rect);
    }
}
