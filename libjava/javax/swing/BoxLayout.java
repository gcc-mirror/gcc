/* BoxLayout.java -- A layout for swing components.
   Copyright (C) 2002 Free Software Foundation, Inc.

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

package javax.swing;

import java.awt.*;

/**
 * A layout for swing components.
 * This implementation delegates its methods to
 * java.awt.GridLayout to do its work.
 *
 * @author Ronald Veldema (rveldema@cs.vu.nl)
 */
public class BoxLayout implements LayoutManager2
{
    GridLayout      gridbag;
    
    final static int X_AXIS = 0;
    final static int Y_AXIS = 1;

    int way = X_AXIS;

    BoxLayout(JComponent p,
	      int way)
    {
	int width = 0;
	int height = 0;

	this.way = way;

	if (way == X_AXIS)
	    {
		width = 1;
	    }
	else
	    {
		height = 1;
	    }
	

	gridbag = new GridLayout(width, height);
    }
    
    BoxLayout(int way)
    {
	this(null,way);
    }
    

    public void addLayoutComponent(String name, Component comp)
    {
	if (way == X_AXIS)
	    {
		gridbag.setColumns( gridbag.getColumns() + 1);
	    }
	else
	    {
		gridbag.setRows( gridbag.getRows() + 1);
	    }
    }

    public void removeLayoutComponent(Component comp)
    {
	gridbag.removeLayoutComponent(comp);
	if (way == X_AXIS)
	    {
		gridbag.setColumns( gridbag.getColumns() - 1);
	    }
	else
	    {
		gridbag.setRows( gridbag.getRows() - 1);
	    }
    }

    public Dimension preferredLayoutSize(Container parent)
    {
	return gridbag.preferredLayoutSize(parent);
    }

    public Dimension minimumLayoutSize(Container parent)
    {
	return gridbag.minimumLayoutSize(parent);
    }

    public void layoutContainer(Container parent)
    {	
	gridbag.layoutContainer(parent);
    }

    public void addLayoutComponent ( Component child, Object constraints )
    {
	addLayoutComponent("", child);
    }

    public float getLayoutAlignmentX ( Container parent )
    {
	return 0;
    }

    public float getLayoutAlignmentY ( Container parent )
    {
	return 0;
    }

    public void invalidateLayout ( Container parent )
    {
    }

    public Dimension maximumLayoutSize ( Container parent )
    {
	return preferredLayoutSize(parent);
    }
}
