/* ButtonGroup.java -- 
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
import java.util.Enumeration;
import java.util.Vector;

public class ButtonGroup implements Serializable 
{
  static final long serialVersionUID = 4259076101881721375L;

    Vector v = new Vector();
    ButtonModel sel;
    
    public ButtonGroup() {}
    
    public void add(AbstractButton b) 
    {
	b.getModel().setGroup(this);
	v.addElement(b);
    }
    
    public void remove(AbstractButton b)
    {
	b.getModel().setGroup(null);
	v.removeElement(b);
    }


    public Enumeration getElements() {
        return v.elements();
    }

    public ButtonModel getSelection() {
        return sel;
    }

    AbstractButton FindButton(ButtonModel m)
    {
	for (int i=0;i<v.size();i++)
	    {
	    AbstractButton a = (AbstractButton) v.get(i);
	    if (a.getModel()== m)
	    {
		return a;
	    }
	}
	return null;
    }

    public void setSelected(ButtonModel m, boolean b)
    {
	if ((m == sel) &&
	    (b == true))
	    {
		// clicked on sam item twice.
		System.out.println("PRESSED TWICE:" + m + ", sel="+sel);
		return;
	    }	
	
	if (sel != null)
	    {

		System.out.println("DESELECTING: " + sel);
		sel.setSelected(!b);

		AbstractButton but = FindButton(sel);
		if (but != null)
		    {
			System.out.println("REPAINT-REQUIST: " + but.text);
			//but.revalidate();
			but.repaint();
		    }
	    }
	else
	    {
		System.out.println("NO SELECTION YET");
	    }
	
	sel = m;
    }
    
    public boolean isSelected(ButtonModel m) 
    {
        return (m == sel);
    }

    public int getButtonCount() 
    {
	return v.size();
    }

}









