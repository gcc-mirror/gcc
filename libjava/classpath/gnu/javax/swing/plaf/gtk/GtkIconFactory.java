/* GtkIconFactory.java
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
import javax.swing.plaf.*;
import java.io.Serializable;

/**
 *
 * @author Brian Jones
 * @see javax.swing.LookAndFeel
 */
public class GtkIconFactory implements Serializable
{
    private static Icon radioButtonIcon;
    private static Icon checkBoxIcon;

    public static Icon getRadioButtonIcon() 
    {
	if (radioButtonIcon == null)
	    radioButtonIcon = new RadioButtonIcon();
	return radioButtonIcon;
    }
    
    private static class RadioButtonIcon 
	implements Icon, UIResource, Serializable
    {
	private static final int size = 15;
	
	public int getIconWidth() { return size; }
	public int getIconHeight() { return size; }

	public void paintIcon(Component c, Graphics g, int x, int y) 
	{
	    System.out.println("radiobuttonicon: paintIcon()");
	    // get the button and model containing the state we are 
	    // supposed to show
	    AbstractButton b = (AbstractButton)c;
	    ButtonModel model = b.getModel();

	    // If the button is being pressed (& armed), change the 
	    // background color 
	    // Note: could also do something different if the button is 
	    // disabled
	    
	    if (model.isPressed() && model.isArmed())
		{
		    System.out.println("radiobuttonicon: pressed & armed");
		    g.setColor(UIManager.getColor("RadioButton.pressed"));
		    g.fillOval(x,y,size-1, size-1);
		}
	    // draw an outer circle
	    g.setColor(UIManager.getColor("RadioButton.foreground"));
	    g.drawOval(x,y,size-1, size-1);
	    
	    // fill a small circle inside if the button is selected
	    if (model.isSelected()) {
		g.fillOval(x+4, y+4, size-8, size-8);
		System.out.println("radiobuttonicon: is selected");
	    }
	}
    }
}
