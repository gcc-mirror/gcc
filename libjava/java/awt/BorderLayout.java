/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

public class BorderLayout implements LayoutManager2, java.io.Serializable
{
  public static final String NORTH  = "North",
                             SOUTH  = "South",
                             EAST   = "East",
                             WEST   = "West",
                             CENTER = "Center";
  
  // FIXME: use these too
  public static final String BEFORE_FIRST_LINE = "First",
                             AFTER_LAST_LINE = "Last",
                             BEFORE_LINE_BEGINS = "Before",
                             AFTER_LINE_ENDS = "After";


  // FIXME: check serialization of fields
  int hgap;
  int vgap;
  Component north, south, east, west, center;

  public BorderLayout ()
  {
    this (0, 0);
  }

  public BorderLayout (int hgap, int vgap)
  {
    this.hgap = hgap;
    this.vgap = vgap;
  }

  public int getHgap()
  {
    return hgap;
  }    

  public void setHgap(int hgap)
  {
    this.hgap = hgap;
  }

  public int getVgap()
  {
    return vgap;
  }    

  public void setVgap(int vgap)
  {
    this.vgap = vgap;
  }
  
  public void addLayoutComponent(Component comp, Object constraints)
  {
    if ((constraints == null) || CENTER.equals(constraints))
      {
	center = comp; 
      }
    else if (NORTH.equals(constraints))
      {
	north = comp;
      }
    else if (SOUTH.equals(constraints))
      {
	south = comp;
      }
    else if (EAST.equals(constraints))
      {
	east = comp;
      }
    else if (WEST.equals(constraints))
      {
	west = comp;
      }
  }
    
  public void addLayoutComponent(String name, Component comp)
  {
    addLayoutComponent(comp, name);
  }

  public void removeLayoutComponent(Component comp)
  {
    if (center == comp)
      {
	center = null;
      }
    else if (north == comp)
      {
	north = null;
      }
    else if (south == comp)
      {
	south = null;
      }
    else if (east == comp)
      {
	east = null;
      }
    else if (west == comp)
      {
	west = null;
      }
  }

  public Dimension minimumLayoutSize(Container target)
  {
    return calcSize(getMinimumSize(center),
		    getMinimumSize(north),
		    getMinimumSize(south),
		    getMinimumSize(east),
		    getMinimumSize(west),
		    target);
  }

  public Dimension preferredLayoutSize(Container target)
  {
    return calcSize(getPreferredSize(center),
		    getPreferredSize(north),
		    getPreferredSize(south),
		    getPreferredSize(east),
		    getPreferredSize(west),
		    target);	
  }
    
  /**
   * Completely disregards the requested maximum sizes of the
   * components, and states that the container has no upper size
   * limit.
   *
   * @return a dimension of width and height Integer.MAX_VALUE.
   */
  public Dimension maximumLayoutSize(Container target)
  {
    return (Dimension) DIM_MAX.clone();
  }	

  public float getLayoutAlignmentX(Container parent)
  {
    return Component.CENTER_ALIGNMENT;
  }

  public float getLayoutAlignmentY(Container parent)
  {
    return Component.CENTER_ALIGNMENT;
  }
    
  public void invalidateLayout(Container target)
  {
    // TODO... implement caching?
  }

  public void layoutContainer(Container target)
  {
    Insets i = target.getInsets();
    Dimension c = getPreferredSize(center);
    Dimension n = getPreferredSize(north);
    Dimension s = getPreferredSize(south);
    Dimension e = getPreferredSize(east);
    Dimension w = getPreferredSize(west);
    Dimension t = target.getSize();
    
    /*
             <-> hgap     <-> hgap
      +----------------------------+          }
      |t                           |          } i.top
      |  +----------------------+  |  --- y1  }
      |  |n                     |  |
      |  +----------------------+  |          } vgap
      |  +---+ +----------+ +---+  |  --- y2  }        }
      |  |w  | |c         | |e  |  |                   } hh
      |  +---+ +----------+ +---+  |          } vgap   }
      |  +----------------------+  |  --- y3  }
      |  |s                     |  |
      |  +----------------------+  |          }
      |                            |          } i.bottom
      +----------------------------+          }
         |x1   |x2          |x3
         <---------------------->
      <-->         ww           <-->
     i.left                    i.right
    */

    int x1 = i.left;
    int x2 = x1 + w.width + hgap;
    int x3 = t.width - i.right - e.width;
    int ww = t.width - i.right - i.left;

    int y1 = i.top;
    int y2 = y1 + n.height + vgap;
    int y3 = t.height - i.bottom - s.height;
    int hh = y3-y2-vgap;

    setBounds(center, x2, y2, x3-x2-hgap, hh);
    setBounds(north, x1, y1, ww, n.height);
    setBounds(south, x1, y3, ww, s.height);
    setBounds(west, x1, y2, w.width, hh);
    setBounds(east, x3, y2, e.width, hh);
  }
  
  public String toString()
  {
    return getClass().getName() + "[hgap=" + hgap + ",vgap=" + vgap + "]";
  }


  // Support:

  static final Dimension DIM_0   = new Dimension(0, 0);
  static final Dimension DIM_MAX = new Dimension(Integer.MAX_VALUE,
						 Integer.MAX_VALUE);

  void setBounds(Component comp, int x, int y, int w, int h)
  {
    if (comp == null)
      return;
    comp.setBounds(x, y, w, h);
  }

  Dimension getMinimumSize(Component comp)
  {
    if (comp == null)
      return DIM_0;
    return comp.getMinimumSize();
  }

  Dimension getPreferredSize(Component comp)
  {
    if (comp == null)
      return DIM_0;
    return comp.getPreferredSize();
  }

  Dimension calcSize(Dimension c, Dimension n, Dimension s,
		     Dimension e, Dimension w, Container target)
  {
    Insets i = target.getInsets();
    
    return new Dimension(c.width + e.width + w.width + hgap*2 +
			 i.left + i.right,
			 c.height + n.height + s.height + vgap*2 +
			 i.top + i.bottom
			 );
  }
}
