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

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;
import javax.swing.UIDefaults;
import javax.swing.border.MatteBorder;

class BasicBorder extends MatteBorder
{
    static Color BtnPointClr = new Color( 180, 180, 180);

	BasicBorder()
	{
		super(5,5,5,5, Color.black);
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
	super(5,5,5,5, Color.black);
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
	put("JButton", "javax.swing.plaf.basic.BasicButtonUI");
	put("JLabel",  "javax.swing.plaf.basic.BasicLabelUI");
	
	put("JPanel",  "javax.swing.plaf.basic.BasicPanelUI");
	put("JCheckBox",  "javax.swing.plaf.basic.BasicCheckBoxUI");
	put("JRadioButton",  "javax.swing.plaf.basic.BasicRadioButtonUI");
	put("JToggleButton",  "javax.swing.plaf.basic.BasicToggleButtonUI");
	put("JOptionPane",  "javax.swing.plaf.basic.BasicOptionPaneUI");
	put("JList",  "javax.swing.plaf.basic.BasicListUI");
	put("JTree",  "javax.swing.plaf.basic.BasicTreeUI");
	put("JTextComponent",  "javax.swing.plaf.basic.BasicTextUI");
	put("JTabbedPane",  "javax.swing.plaf.basic.BasicTabbedPaneUI");
	put("JScrollPane", "javax.swing.plaf.basic.BasicScrollPaneUI");
	put("JViewport",   "javax.swing.plaf.basic.BasicViewportUI");

	put("JButton.border",      "javax.swing.plaf.basic.BasicBorder");
	put("JPanel.border",       "javax.swing.plaf.basic.PanelBorder");

	put("JToggleButton.border", "javax.swing.plaf.basic.PanelBorder");
	put("JCheckBox.border", "javax.swing.plaf.basic.PanelBorder");
	put("JRadioButton.border", "javax.swing.plaf.basic.PanelBorder");

	put("AbstractUndoableEdit.undoText", "Undo");
	put("AbstractUndoableEdit.redoText", "Redo");
    }
    
}


