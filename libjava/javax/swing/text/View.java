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

import java.awt.Graphics;
import java.awt.Shape;
import java.util.Vector;
import javax.swing.SwingConstants;

public abstract class View implements SwingConstants
{
    static int BadBreakWeight;    
    static int ExcellentBreakWeight;
    static int ForcedBreakWeight;
    static int GoodBreakWeight;

    public final static int X_AXIS = 0;
    public final static int Y_AXIS = 1;
    
    float width, height;
    Element elt;
    View parent;

    /** 
     * this vector contains the views ordered at offsets...
     */
    Vector v = new Vector();


    public View(Element elem)
    {
	elt = elem;
    }

    public int getViewCount() 
    {
	return v.size();
    }

    public View getView(int a)
    {
	return (View) v.get(a);
    }
    
    public void remove(int i)
    {
	v.removeElementAt(i);
    }
    
    public void insert(int off, View view)
    {
	v.insertElementAt(view, off);	
    }	   
    
    public void append(View view)
    {
	v.addElement(view);
    }
	
    public void paint(Graphics g, Shape allocation)
    {
	System.out.println("view.paint() !!!!");
    }

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
	width  = w;
	height = h;
    }

    public Document getDocument()
    {
	return getElement().getDocument();
    }
    
    public Element getElement()
    {
        return elt;
    }

    public float getPreferredSpan(int a)
    {
	switch (a)
	    {
	    case X_AXIS:  return width;
	    case Y_AXIS:  return height;
	    default:
		{
		    System.err.println("I sure wish Java had enums !!! ");
		    return 0;
		}
	    }
    }
}

