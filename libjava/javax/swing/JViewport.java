/* JViewport.java -- 
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
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import javax.accessibility.Accessible;
import javax.swing.plaf.ViewportUI;

public class JViewport extends JComponent
{
    Component c;

    JViewport()
    {
	setOpaque(true);
	updateUI();
    }

    void setView(Component c)
    {
	if (this.c != null)
	    remove(c);

	this.c = c;

	add(c);
    }

    public String getUIClassID()
    {
	return "JViewport";
    }

    public void updateUI()
    {
	ViewportUI vp = (ViewportUI) UIManager.getUI(this);
	setUI(vp);
    }

    Container GetHeavy(Container parent)
    {
	if (parent == null)
	    return null;

	while (isLightweightComponent(parent))
	    {
		Container p = parent.getParent();

		if (p == null)
		    {
			System.out.println("GetHeavy FAILED, no heavy weight component found");
			return parent;
		    }
		
		parent = p;
	    }
	return parent;
    }
    
    
    public void paint(Graphics g)
    {
	paintChildren(g);

	System.out.println("XXXXXXXXXXXXXXXXXXXXXXXXXXXX   JViewport -----> paint()");

	Container parent = GetHeavy(getParent());
	
	System.out.println("parent = " + parent + ", " + getParent());

	//parent.paint();

	Graphics wg = parent.getGraphics();
	
	int x = 0;
	int y = 0;
	int w = getWidth();
	int h = getHeight();

	Rectangle r = new Rectangle(x, y, w, h);

	int ox = 0;
	int oy = 0;

	wg.copyArea(r.x,
		    r.y,
		    r.width,
		    r.height,
		    ox,
		    oy);

	wg.dispose();
    }
}







