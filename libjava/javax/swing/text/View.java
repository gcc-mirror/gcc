/* View.java -- 
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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

package javax.swing.text;

import java.awt.Container;
import java.awt.Graphics;
import java.awt.Shape;
import java.util.Vector;
import javax.swing.SwingConstants;

public abstract class View implements SwingConstants
{
  public static final int BadBreakWeight = 0;
  public static final int ExcellentBreakWeight = 2000;
  public static final int ForcedBreakWeight = 3000;
  public static final int GoodBreakWeight = 1000;

  public static final int X_AXIS = 0;
  public static final int Y_AXIS = 1;
    
  private float width, height;
  private Element elt;
  private View parent;

    /** 
   * Creates a new <code>View</code> instance.
   *
   * @param elem an <code>Element</code> value
     */
    public View(Element elem)
    {
	elt = elem;
    }

  public abstract void paint(Graphics g, Shape s);

  public void setParent(View a)
    {
    parent = a;
    }

  public View getParent()
    {
    return parent;
    }
    
  public void setSize(int w, int h)
    {
    width = w;
    height = h;
    }
    
  public Container getContainer()
    {
    return parent != null ? parent.getContainer() : null;
    }	   
    
  public Document getDocument()
    {
    return getElement().getDocument();
    }
	
  public Element getElement()
    {
    return elt;
    }

  public abstract float getPreferredSpan(int axis);
  
  public float getAlignment(int axis)
    {
    return 0.5f;
    }
    
  public AttributeSet getAttributes()
    {
    return elt.getAttributes();
    }
    
  public boolean isVisible()
    {
    return true;
    }

  public int getViewCount()
    {
    return 0;
    }
    
  public View getView(int index)
    {
    return null;
    }

  public ViewFactory getViewFactory()
    {
    return parent != null ? parent.getViewFactory() : null;
  }

  public void replace(int offset, int length, View[] views)
	    {
    // Default implementation does nothing.
  }

  public void insert(int offset, View view)
		{
    View[] array = { view };
    replace(offset, 1, array);
		}

  public void append(View view)
  {
    View[] array = { view };
    replace(getViewCount(), 1, array);
  }

  public void removeAll()
  {
    replace(0, getViewCount(), null); 
	    }

  public void remove(int index)
  {
    replace(index, 1, null); 
  }

  public View createFragment(int p0, int p1)
  {
    // The default implementation doesn't support fragmentation.
    return this;
  }

  public int getStartOffset()
  {
    return elt.getStartOffset();
  }

  public int getEndOffset()
  {
    return elt.getEndOffset();
  }

  public Shape getChildAllocation(int index, Shape a)
  {
    return null;
  }
  
  /**
   * @since 1.4
   */
  public int getViewIndex(float x, float y, Shape allocation)
  {
    return -1;
  }
  
  /**
   * @since 1.4
   */
  public String getToolTipText(float x, float y, Shape allocation)
  {
    int index = getViewIndex(x, y, allocation);

    if (index < -1)
      return null;

    Shape childAllocation = getChildAllocation(index, allocation);

    if (childAllocation.getBounds().contains(x, y))
      return getView(index).getToolTipText(x, y, childAllocation);

    return null;
  }
}

