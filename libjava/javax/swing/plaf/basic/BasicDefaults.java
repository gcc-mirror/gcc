/* BasicDefaults.java
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

import javax.swing.*;
import java.awt.*;
import javax.swing.border.*;

class BasicBorder extends MatteBorder
{
    static Color BtnPointClr = new Color( 180, 180, 180);

	BasicBorder()
	{
	    super(5,5,5,5, null);
	}
	
	public void paintBorder(Component  c,
				Graphics  g, 
				int  x, 
				int  y, 
				int  width, 
				int  height)
	{
	    //    System.out.println("PAINT-------------------------------------------BORDER");

	    if (g != null)
		{
		    g.setColor( BtnPointClr);
		    g.draw3DRect( 0, 0, width-1, height-1, true);
		}
	}
 }

class PanelBorder extends MatteBorder
{
    PanelBorder()
    {
	super(5,5,5,5, null);
	}
	
	public void paintBorder(Component  c,
				Graphics  g, 
				int  x, 
				int  y, 
				int  width, 
				int  height)
	{
	    //    System.out.println("PAINT-------------------------------------------BORDER");
	    super.paintBorder(c, g, x, y, width, height);
	}
 }

public class BasicDefaults extends UIDefaults
{
    public BasicDefaults()
    {
	//	System.out.println("BasicDefaults !!!!!!!!!!!!!!!!!!!!!!!!!");
	put("JButton", new BasicButtonUI());
	put("JLabel",  new BasicLabelUI());
	
	put("JPanel",  new BasicPanelUI());
	put("JCheckBox",  new BasicCheckBoxUI());
	put("JRadioButton",  new BasicRadioButtonUI());
	put("JToggleButton",  new BasicToggleButtonUI());
	put("JOptionPane",  new BasicOptionPaneUI());
	put("JList",  new BasicListUI());
	put("JTree",  new BasicTreeUI());
	put("JTextComponent",  new BasicTextUI());
	put("JTabbedPane",  new BasicTabbedPaneUI());
	put("JScrollPane", new BasicScrollPaneUI());
	put("JViewport",   new BasicViewportUI());

	put("JButton.border",      new BasicBorder());
	put("JPanel.border",       new PanelBorder());

	put("JToggleButton.border", new PanelBorder());
	put("JCheckBox.border", new PanelBorder());
	put("JRadioButton.border", new PanelBorder());
    }
    
}


