/* DefaultButtonModel.java -- 
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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.Serializable;
import java.util.EventListener;
import java.util.Vector;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.EventListenerList;

public class DefaultButtonModel
  implements ButtonModel, Serializable
{
  static final long serialVersionUID = -5342609566534980231L;

    Vector actions          = new Vector();

    Vector items    = new Vector();
    Vector changes  = new Vector();
    ButtonGroup group;
    JComponent comp;

    
    DefaultButtonModel(JComponent a)
    {
	comp = a;
    }


    public Object[] getSelectedObjects()
    {
	return null;
    }


    public void fireItemStateChanged(ItemEvent event)
    {
	for (int i=0;i<items.size();i++)
	    {
		ItemListener a = (ItemListener) items.get(i);
		a.itemStateChanged(event);
	    }
    }
    public void fireStateChanged(ChangeEvent event)
    {
	for (int i=0;i<changes.size();i++)
	    {
		ChangeListener a = (ChangeListener) changes.get(i);
		a.stateChanged(event);
	    }
    }
    public void fireActionPerformed(ActionEvent event)
    {
	for (int i=0;i<actions.size();i++)
	    {
		ActionListener a = (ActionListener) actions.get(i);
		a.actionPerformed(event);
	    }
    }

    boolean arm;
    public boolean isArmed()          { return arm; }
    public void setArmed(boolean b)   { arm = b; }

    boolean enabled = true;
    public boolean isEnabled()         { return enabled; }
    public void setEnabled(boolean b)  { enabled = b; }

    boolean pressed;
    public void setPressed(boolean b)  
    {
	pressed = b; 
    }
    public boolean isPressed()         { return pressed; }


    public void removeActionListener(ActionListener l) { actions.removeElement(l); }
    public void addActionListener(ActionListener l)    
    {	
	//	comp.enableEvents( AWTEvent.ACTION_EVENT_MASK );
	actions.addElement(l);    
    }

    public void addItemListener(ItemListener l)        { items.addElement(l); }
    public void removeItemListener(ItemListener l)     { items.removeElement(l); }

    public void addChangeListener(ChangeListener l)    { changes.addElement(l); }
    public void removeChangeListener(ChangeListener l) { changes.removeElement(l); }

    boolean roll;
    public void setRollover(boolean b) { roll = b; }
    public boolean isRollover()        { return roll; }

    int mne;  
    public int  getMnemonic()        { return mne; }
    public void setMnemonic(int key) { mne = key; }

    String com;
    public void setActionCommand(String s) { com = s; }
    public String getActionCommand()       { return com; }

    public void setGroup(ButtonGroup group)
    {
	this.group = group;
    }

    boolean sel;
    public void setSelected(boolean b) 
    { 
	if (group != null)
	    {
		if (b == true)
		    {
			System.out.println("selected button in group:"+this);
			group.setSelected(this, b);
			sel = true;
		    }
		else
		    {
			System.out.println("deselected button in group: " + this);
			sel = false;
		    }
	    } 
	else
	    {
		sel = b;
	    }
    }
    public boolean isSelected()        { return sel; }
}







