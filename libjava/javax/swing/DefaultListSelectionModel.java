/* DefaultListSelectionModel.java -- 
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

import java.io.Serializable;
import java.util.EventListener;
import java.util.Vector;
import javax.swing.event.EventListenerList;
import javax.swing.event.ListSelectionListener;

public class DefaultListSelectionModel implements Cloneable, ListSelectionModel, Serializable
{
    int mode = SINGLE_SELECTION;

    Vector sel = new Vector();

    Vector listeners;

    Vector get_listeners()
    {
	if (listeners == null)
	    listeners = new Vector();
	return listeners;
    }
    

    public void addListSelectionListener(ListSelectionListener listener)
    {
	get_listeners().addElement(listener);
    }

    public void removeListSelectionListener(ListSelectionListener listener)
    {
	get_listeners().removeElement(listener);
    }
    
    class Range
    {
	int i0, i1;

	Range(int a, int b)
	{
	    if (a > b)
		{
		    i0 = b;
		    i1 = a;
		}
	    else
		{
		    i0 = a; 
		    i1 = b;
		}
	}
    }

    
    public int getMinSelectionIndex()
    {
	if (isSelectionEmpty())
	    return -1;
	
	boolean first = true;
	int min = -1;
	for (int i=0;i<sel.size();i++)
	    {
		Range r = (Range) sel.get(i);

		if (first)
		    {
			min = r.i0;
			first = false;
		    }	
		else
		    {
			if (r.i0 > min)
			    {
				min = r.i0;
			    }
		    }
	    }
	return min;
    }

    public int getMaxSelectionIndex()
    {
	if (isSelectionEmpty())
	    return -1;

	boolean first = true;
	int max = -1;
	for (int i=1;i<sel.size();i++)
	    {
		Range r = (Range) sel.get(i);
		
		if (first)
		    {
			max = r.i1;
		    }
		else
		    {
			if (r.i1 > max)
			    {
				max = r.i1;
			    }
		    }
	    }
	return max;
    }

    public boolean isSelectedIndex(int a)
    {
	for (int i=0;i<sel.size();i++)
	    {
		Range r = (Range) sel.get(i);
		if (r.i0 <= a &&
		    r.i1 >= a)
		    {
			return true;
		    }
	    }
	return false;
    }


    public int getSelectionMode()
    { return mode; }
    public void setSelectionMode(int a)
    {	mode = a;    }

    boolean isSelectionEmpty() 
    {
	return sel.size() == 0;
    }

    public void clearSelection()
    {
	sel.removeAllElements();
    }

    public void setSelectionInterval(int index0, int index1) 
    {
	if (mode == SINGLE_SELECTION)
	    {
		sel.removeAllElements();
	    }

	sel.addElement(new Range(index0, index1));
    }
}
