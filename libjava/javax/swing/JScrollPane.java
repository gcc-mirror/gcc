/* JScrollPane.java -- 
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
import javax.accessibility.Accessible;
import javax.swing.plaf.ScrollPaneUI;

public class JScrollPane extends JComponent implements Accessible, ScrollPaneConstants
{
    protected JViewport  columnHeader;
    protected JViewport  rowHeader;

    protected Component  lowerLeft;
    protected Component  lowerRight;
    protected Component  upperLeft;
    protected Component  upperRight;

    protected JScrollBar horizontalScrollBar;
    protected int        horizontalScrollBarPolicy;
    protected JScrollBar verticalScrollBar;
    protected int        verticalScrollBarPolicy;

    protected JViewport  viewport;


    public JScrollPane() 
    {
	this(null, 0, 0);
    }
    
    public JScrollPane(Component view) 
    {
	this(view, 0, 0);
    }
    
    
    public JScrollPane(int vsbPolicy, int hsbPolicy) 
    {
	this(null, 0, 0);
    }

    public JScrollPane(Component view, int vsbPolicy, int hsbPolicy) 
    {
	setViewportView(view);
        setOpaque(true);
	updateUI();
    }

    public String getUIClassID()
    {
	//Returns a string that specifies the name of the L&F class that renders this component.  
	return "JScrollPane";
    }

    public JViewport getViewport()
    {
	return viewport;
    }

    public JViewport createViewport()
    {
	return new JViewport();
    }
    
    public void setViewport(JViewport v)
    {
	if (viewport != null)
	    remove(viewport);

	viewport = v;	
	
	add(v);
	
	revalidate();
	repaint();
    }
    
   public  void updateUI()
    {
	ScrollPaneUI b = (ScrollPaneUI)UIManager.getUI(this);
	setUI(b);
    }


    public void setViewportView(Component view)
    {
	if (getViewport() == null)
	    {
		setViewport(createViewport());
	    }
	
	if (view != null)
	    {
		getViewport().setView(view);
	    }
    }
}
