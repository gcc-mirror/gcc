/* BasicLabelUI.java
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


package javax.swing.plaf.basic;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.LabelUI;

public class BasicLabelUI extends LabelUI
  implements PropertyChangeListener
{
    int gap = 3;
    Color foreground;

    
    public static ComponentUI createUI(final JComponent c)  {
	return new BasicLabelUI();
    }
    
    
    public void installUI(final JComponent c)  {
	super.installUI(c);
	
	foreground = new Color(0,0,250);
    }
    

    public Dimension getPreferredSize(JComponent c) 
    {
	JLabel b = (JLabel)c;
        /*
          We cannot use this method because it is not part of the
          official Swing API.

	Dimension d = BasicGraphicsUtils.getPreferredSize(b, 
							  gap,
							  b.getText(),
							  b.getIcon(),
							  b.getVerticalAlignment(),
							  b.getHorizontalAlignment(),
							  b.getHorizontalTextPosition(),
							  b.getVerticalTextPosition());
	System.out.println("JLABEL->^^^^^^^^^^^^^^^^^^^^^^   BASIC-PREF="+d + ",T="+b.getText());
        */
        return new Dimension(100, 30);
    }
    

    public void paint(Graphics g, JComponent c)
    {      
	JLabel b = (JLabel) c;

	Rectangle tr = new Rectangle();
	Rectangle ir = new Rectangle();
	Rectangle vr = new Rectangle();

        Font f = c.getFont();

        g.setFont(f);

        FontMetrics fm = g.getFontMetrics(f);

        Insets i = c.getInsets();

	Rectangle bound = c.getBounds();
	
	System.out.println("BOUND=" + bound + ", insets = " + i + ", " + b.getText());
	
	if (bound == null)
	    {
		vr.x      = i.left;
		vr.y      = i.top;
		vr.width  = b.getWidth() - (i.right  + i.left);
		vr.height = b.getHeight() - (i.bottom + i.top);
	    }
	else
	    {
		vr.x      = bound.x + i.left;
		vr.y      = bound.y + i.top;
		vr.width  = bound.width - (i.right  + i.left);
		vr.height = bound.height - (i.bottom + i.top);
	    }

	System.out.println("             VIEW-RECT-JLABEL="+vr+", insets="+i+", FONTM="+fm);

	String text = SwingUtilities.layoutCompoundLabel(c,
							 fm, 
							 b.getText(),
							 b.getIcon(),
							 b.getVerticalAlignment(), 
							 b.getHorizontalAlignment(),
							 b.getVerticalTextPosition(), 
							 b.getHorizontalTextPosition(),
							 vr,
							 ir,
							 tr,
							 gap);

	paintIcon(g, c, ir);
	paintText(g, c, tr, b.getText());
	paintFocus(g, c, vr, tr, ir);
    }


    protected void paintFocus(Graphics g, 
			      JComponent c,
			      Rectangle vr,
			      Rectangle tr,
			      Rectangle ir)
    {
    }

    protected void paintIcon(Graphics g, 
			     JComponent c, 
			     Rectangle iconRect)
    {
	JLabel b = (JLabel) c;
	if (b.getIcon() != null)
	    {
		int x = iconRect.x;
		int y = iconRect.y;

		System.out.println("WE HAVE AN ICON: " + b.getIcon());
 
		b.getIcon().paintIcon(c, g, x, y);
	    }
	else
	    {
		//System.out.println("NO ICON FOR BUTTON:" + b.text);
	    }
    }

    
    protected void paintText(Graphics g,
			     JComponent c,
			     Rectangle textRect,
			     String text) 
    {
	//        AbstractLabel b = (AbstractLabel) c;
	
	System.out.println("JLabel: drawing string: " + text + ", at:" + textRect);
	
	g.setColor(foreground);
	//g.setBackColor(new Color(190,190,190));

	g.drawLine(0,0,100,100);
	
	BasicGraphicsUtils.drawString(g, text, 0, 0 /*textRect.x*/, 0 /*textRect.y*/);
    }

  public void propertyChange (PropertyChangeEvent event)
  {
    throw new Error ("Not implemented");
  }
}
