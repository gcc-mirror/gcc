/* JTabbedPane.java -- 
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

import java.awt.Component;
import java.util.Vector;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.TabbedPaneUI;

public class JTabbedPane extends JComponent implements Accessible, SwingConstants
{
    class Tab
    {
	Icon icon;
	String name, descr;
	Component tab;

	Tab(String name,
	    Icon icon,
	    Component tab,
	    String descr)
	{
	    this.name = name;
	    this.icon = icon;
	    this.tab  = tab;
	    this.descr = descr;
	}
    }
    
    private Vector tabs = new Vector();

    public JTabbedPane()
    {
    }

    public void addTab(String name,
		Component panel)		
    {
	addTab(name, null, panel, null);
    }
    public void addTab(String name,
		Icon icon,
		Component panel)		
    {
	addTab(name, icon, panel, null);
    }
    public void addTab(String name,
		Icon icon,
		Component panel,
		String descr)
    {
	tabs.addElement(new Tab(name, icon, panel, descr));
    }

    public int getTabCount()
    {
	return tabs.size();
    }
    public Component getComponentAt(int i)
    {
	Tab t = (Tab) tabs.elementAt(i);
	return t.tab;
    }
    
    public String getUIClassID()
    {	return "JTabbedPane";    }


    public void setUI(TabbedPaneUI ui) {
        super.setUI(ui);
    }
    
    public TabbedPaneUI getUI() {
        return (TabbedPaneUI)ui;
    }
    
    public void updateUI()
    {
        setUI((TabbedPaneUI)UIManager.getUI(this));
    }
    
    public AccessibleContext getAccessibleContext()
    {
	return null;
    }
    
   protected  String paramString()
    {
	return "JTabbedPane";
    }
}
