/* BasicTextUI.java
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import javax.swing.JComponent;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.TextUI;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.EditorKit;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.Position;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;

public class BasicTextUI extends TextUI
  implements ViewFactory
{
    int gap = 3;
    View view = null; // was: new RootView();
    Color textColor, disabledTextColor, normalBackgroundColor;
    EditorKit kit = new DefaultEditorKit();
    
    /* *****************************************************************
     * This View is way too incomplete to be of any use. To avoid errors
     * when compiling with the Sun JDK, it has been commented out.
     *                            -- Sascha Brawer (brawer@dandelis.ch)
     *
     * (begin of commented out section)
    class RootView extends View
    {
	RootView()
	{
	    super(null);
	}
        public void paint(Graphics g, Shape s)
	{
	    if (view != null)
		{
		    Rectangle r = s.getBounds();

		    view.setSize((int)r.getWidth(),
				 (int)r.getHeight());
		    view.paint(g, s);
		}
        }
    }
    * (end of commented out section)
    *************************************************************** */

    public BasicTextUI()
    {
    }

    public static ComponentUI createUI(final JComponent c) 
    {
	return new BasicTextUI();
    }

    
    public void installUI(final JComponent c) 
    {
	super.installUI(c);

	textColor                = new Color(0,0,0);
	disabledTextColor        = new Color(130, 130, 130);
	normalBackgroundColor    = new Color(192,192,192);
    }
    
    public Dimension getPreferredSize(JComponent c) 
    {
	JTextComponent b = (JTextComponent) c;

	View v = getRootView(b);

	float w = v.getPreferredSpan(View.X_AXIS);
	float h = v.getPreferredSpan(View.Y_AXIS);

	return new Dimension((int)w, (int) h);
    }
    

    public void paint(Graphics g, JComponent c)
    {      
	//	view.paint(
    }

    public void damageRange(JTextComponent t, int p0, int p1)
    {
	damageRange(t, p0, p1, null, null);
    }    

    public void damageRange(JTextComponent t, 
		     int p0, int p1, 
		     Position.Bias firstBias,
		     Position.Bias secondBias)
    {
    }

    public EditorKit getEditorKit(JTextComponent t)
    {
	return kit;
    }
    
    public int getNextVisualPositionFrom(JTextComponent t, 
				  int pos,
				  Position.Bias b, 
				  int direction,
				  Position.Bias[] biasRet)
        throws BadLocationException
    {
	return 0;
    }
    
    public View getRootView(JTextComponent t)
    {
	return view;
    }
    
    public Rectangle modelToView(JTextComponent t, int pos)
      throws BadLocationException
    {
	return modelToView(t, pos, null);
    }
    
    public Rectangle modelToView(JTextComponent t, int pos, Position.Bias bias)
      throws BadLocationException
    {
	return null;
    }
    
    public int viewToModel(JTextComponent t, Point pt)
    {
	return viewToModel(t, pt, null);
    }
    
    public int viewToModel(JTextComponent t, Point pt, Position.Bias[] biasReturn)
    {
	return 0;
    } 

  public View create (Element elem)
  {
    // subclasses have to implement this to get this functionality
    return null;
  }
}
