/* BasicListUI.java
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


package javax.swing.plaf.basic;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.ListCellRenderer;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.ListUI;

public class BasicListUI extends ListUI
{
    int gap_between_cells;
    Color textColor, disabledTextColor, pressedBackgroundColor, normalBackgroundColor;
    

    public static ComponentUI createUI(final JComponent c) 
    {
	return new BasicButtonUI();
    }

    
    public void installUI(final JComponent c) 
    {
	super.installUI(c);

	textColor                = new Color(0,0,0);
	disabledTextColor        = new Color(130, 130, 130);
	pressedBackgroundColor   = new Color(150,150,150);
	normalBackgroundColor    = new Color(192,192,192);
    }

    public Dimension getPreferredSize(JComponent c) 
    {
	JList l = (JList) c;

	System.out.println("XXXXXXXXXXXXXXXxx   getPreferredSize------------> " + l);

	
	int rows = l.getVisibleRowCount();

	ListCellRenderer render = l.getCellRenderer();
	
	int width  = 200;
	int height = rows * 16; 
	
	if (l.getModel().getSize() == 0)
	    {
		return new Dimension(width, height);
	    }

	System.out.println("BASIC_LIST_UI ====-> " + l.getModel().getElementAt(0));

	Component elt = render.getListCellRendererComponent(l,
							    l.getModel().getElementAt(0),
							    0,            
							    false,
							    false);
	Dimension a = elt.getPreferredSize();
	if (a == null)
	    {
		return new Dimension(width, height);
	    }

	return new Dimension(a.width,
			     a.height * rows);
    }

    public void paintBackground(Graphics g,
			 JComponent c)
    {
	Dimension size = getPreferredSize(c);

	g.setColor(normalBackgroundColor);
	g.fillRect(0,0,size.width, size.height);  
    }

    public void paint(Graphics g, 
		      JComponent c)
    {      
	JList l = (JList) c;

	int rows = l.getVisibleRowCount();

	ListCellRenderer render = l.getCellRenderer();

	System.out.println("RENDER-JLIST: " + rows + ", " + l.getModel().getSize());

	paintBackground(g, c);

	if (l.getModel().getSize() == 0)
	    return;

	// use element 0 to figure out how big we are:
	Component elt = render.getListCellRendererComponent(l,
							    l.getModel().getElementAt(0),
							    0,       
							    false,
							    false);
	Dimension dim = elt.getPreferredSize();
	
	Rectangle a = new Rectangle(0,
				    0,
				    dim.width,
				    dim.height);

	for (int i=0;i<l.getModel().getSize();i++)
	    {
		boolean is_sel = false;
		boolean has_focus = false;

		Component comp = render.getListCellRendererComponent(l,
								     l.getModel().getElementAt(i),
								     i,            
								     is_sel,
								     has_focus);

		//System.out.println("AAAAA=> " + a + ", " + comp + ", index = " + i);

		comp.setBounds(a);

		comp.paint(g);

		a.y += dim.height + gap_between_cells;
	    }
    }

  public int locationToIndex(JList list, Point location)
  {
    throw new Error ("Not implemented");
  }

  public Point indexToLocation(JList list, int index)
  {
    throw new Error ("Not implemented");
  }

  public Rectangle getCellBounds(JList list, int index1, int index2)
  {
    throw new Error ("Not implemented");
  }
}
