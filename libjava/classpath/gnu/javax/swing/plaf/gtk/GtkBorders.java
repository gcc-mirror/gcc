/* GtkBorders.java
   Copyright (c) 1999 by Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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

package gnu.javax.swing.plaf.gtk;
import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.plaf.*;

/**
 * Optional class, can be used to define nifty borders.
 * 
 * @author Brian Jones
 * @see javax.swing.LookAndFeel
 */
public class GtkBorders
{
    public static class ButtonBorder extends AbstractBorder 
	implements UIResource
    {
	private Border raised;  // use by default
	private Border lowered;  // use this one when pressed

	// creat the border
        public ButtonBorder() 
	{
	    raised = BorderFactory.createRaisedBevelBorder();
	    lowered = BorderFactory.createLoweredBevelBorder();
	}

	// define the insets (in terms of one of the others)
	public Insets getBorderInsets(Component c)
	{
	    return raised.getBorderInsets(c);
	}
	
	public void paintBorder(Component c, Graphics g, int x, int y, 
				int width, int height)
	{
	    AbstractButton b = (AbstractButton)c;
	    ButtonModel model = b.getModel();
	    
	    if (model.isPressed() && model.isArmed()) 
		lowered.paintBorder(c, g, x, y, width, height);
	    else
		raised.paintBorder(c, g, x, y, width, height);
	}
    }
}
