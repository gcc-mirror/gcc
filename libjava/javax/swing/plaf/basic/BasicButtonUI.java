/* BasicButtonUI.java
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
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;
import javax.swing.AbstractButton;
import javax.swing.JComponent;
import javax.swing.SwingUtilities;
import javax.swing.plaf.ButtonUI;
import javax.swing.plaf.ComponentUI;

public class BasicButtonUI extends ButtonUI
{
    int gap = 3;
    //    int y_text_space = 2, x_text_space + 5;

    Color textColor, disabledTextColor;
    Color pressedBackgroundColor;
    Color normalBackgroundColor;


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
	pressedBackgroundColor   = new Color(150,150,150);
	normalBackgroundColor    = new Color(192,192,192);
    }
    

    public Dimension getPreferredSize(JComponent c) 
    {
	AbstractButton b = (AbstractButton)c;
	Dimension d = BasicGraphicsUtils.getPreferredButtonSize(b, gap);
	//	System.out.println("^^^^^^^^^^^^^^^^^^^^^^   BASIC-PREF="+d + ",T="+b.text);
	return d;
    }
    

    public void paint(Graphics g, JComponent c)
    {      
	AbstractButton b = (AbstractButton) c;

	Rectangle tr = new Rectangle();
	Rectangle ir = new Rectangle();
	Rectangle vr = new Rectangle();

        Font f = c.getFont();

        g.setFont(f);

        FontMetrics fm = g.getFontMetrics(f);

        Insets i = c.getInsets();

        vr.x      = i.left;
        vr.y      = i.top;
        vr.width  = b.getWidth()  - (i.right  + vr.x);
        vr.height = b.getHeight() - (i.bottom + vr.y);
	
	//System.out.println("             VIEW-RECT-BUTTON="+vr+", insets="+i+", FONTM="+fm);
	
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

        if (b.getModel().isPressed() ||
	    b.getModel().isSelected())
	    {
	      //System.out.println("paint pressed");
		paintButtonPressed(g, c);
	    }
	else
	    {
	      //System.out.println("paint normal");
		paintButtonNormal(g, c);
	    }
	
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
	AbstractButton b = (AbstractButton) c;
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

    protected void paintButtonPressed(Graphics g,
				      JComponent b)
    {
	Dimension size = b.getSize();
	
	g.setColor(pressedBackgroundColor);
	g.fillRect(1,1,size.width-2, size.height-2);                

    }
    
    protected void paintButtonNormal(Graphics g,
				     JComponent b)
    {
	Dimension size = b.getSize();
	
	g.setColor(normalBackgroundColor);
	g.fillRect(1,1,size.width-2, size.height-2);                

    }
    
    protected void paintText(Graphics g,
			     JComponent c,
			     Rectangle textRect,
			     String text) 
    {	
	Font f = c.getFont();

        g.setFont(f);

        FontMetrics fm = g.getFontMetrics(f);

	g.setColor(c.isEnabled() ? textColor : disabledTextColor);

	BasicGraphicsUtils.drawString(g,
				      text, 
				      0,
				      textRect.x, 
				      textRect.y + fm.getAscent()/2);
    } 
}




