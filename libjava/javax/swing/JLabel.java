/* JLabel.java -- 
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
import java.awt.Image;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.swing.plaf.LabelUI;

public class JLabel extends JComponent implements Accessible, SwingConstants
{
    String text;
    Icon icon;
    int gap;
    int align;

    int hor_align;
    int hor_text_pos;

    int vert_align;
    int vert_text_pos;

    public JLabel()
    {
	this("", null, 0);
    }

    public JLabel(Icon image)
    {
	this("", image, 0);
    }

    public JLabel(Icon image, int horizontalAlignment)
    {
	this("", image, horizontalAlignment);
    }

    public JLabel(String text)
    {
	this(text, null, 0);
    }

    public JLabel(String text, int horizontalAlignment)
    {
	this(text, null, horizontalAlignment);
    }

    public JLabel(String text, Icon icon, int horizontalAlignment)
    {
	// do the work.....
	this.text = text;
	setIcon(icon);
	this.align     = horizontalAlignment;

	updateUI(); // get a proper ui
    } 


    protected  int checkHorizontalKey(int key, String message)
    {
	//    Verify that key is a legal value for the horizontalAlignment properties. 
	return 0;
    }
    protected  int checkVerticalKey(int key, String message)
    {
	//      Verify that key is a legal value for the verticalAlignment or verticalTextPosition properties.  
	return 0;
    }
    public AccessibleContext getAccessibleContext()
    {
	//          Get the AccessibleContext of this object 
	return null;
    }
    public Icon getDisabledIcon()
    {
	//          Returns the value of the disabledIcon property if it's been set, If it hasn't been set and the value of the icon property is an ImageIcon, we compute a "grayed out" version of the icon and update the disabledIcon property with that.  
	return null;
    }
    public int getDisplayedMnemonic()
    {
	//          Return the keycode that indicates a mnemonic key.   
	return 0;
    }
    public int getHorizontalAlignment()
    {
	//          Returns the alignment of the label's contents along the X axis.   
	return hor_align;
    }
    public int getHorizontalTextPosition()
    {
	//          Returns the horizontal position of the label's text, relative to its image.    
	return hor_text_pos;
    }

    public Icon getIcon()
    {	return icon;    }

    public int getIconTextGap()
    {
	//          Returns the amount of space between the text and the icon displayed in this label.   
	return 0;
    }
    public Component getLabelFor()
    {
	//          Get the component this is labelling.  
	return null;
    }
    public String getText()
    {	return text;    }

    public String getUIClassID()
    {	return "JLabel";    }

    public int getVerticalAlignment()
    {
	//          Returns the alignment of the label's contents along the Y axis. 
	return vert_align;
    }
    public int getVerticalTextPosition()
    {
	//          Returns the vertical position of the label's text, relative to its image. 
	return vert_text_pos;
    }

    public boolean imageUpdate(Image img, int infoflags, int x, int y, int w, int h)
    {
	//          This is overriden to return false if the current Icon's Image is not equal to the passed in Image img. 
	return (img == icon);
    }
    protected  String paramString()
    {
	//          Returns a string representation of this JLabel.  
	return "JLabel";
    }
    public void setDisabledIcon(Icon disabledIcon)
    {
	//          Set the icon to be displayed if this JLabel is "disabled" (JLabel.setEnabled(false)).  
    }
    public void setDisplayedMnemonic(char aChar)
    {
	//          Specifies the displayedMnemonic as a char value.  
    }
    public void setDisplayedMnemonic(int key)
    {
	//          Specify a keycode that indicates a mnemonic key.  
    }
    public void setHorizontalAlignment(int alignment)
    {
	//          Sets the alignment of the label's contents along the X axis.  
	hor_align = alignment;
    }
    public void setHorizontalTextPosition(int textPosition)
    {
	//          Sets the horizontal position of the label's text, relative to its image.  
	hor_text_pos = textPosition;
    }
    public void setIcon(Icon icon)
    {
	this.icon = icon;
	if (icon != null)
	    {
                  // XXX FIXME - icons do not know their parent
//  		icon.setParent(this);
	    }
	revalidate();
	repaint();
    }

    public void setIconTextGap(int iconTextGap)
    {
	gap = iconTextGap;
    }
  
    public void setLabelFor(Component c)
    {
	//          Set the component this is labelling.  
    }
    public void setText(String text)
    {
	this.text = text;
	revalidate();
	repaint();
    }
  
    public void setVerticalAlignment(int alignment)
    {
	//          Sets the alignment of the label's contents along the Y axis.  
	vert_align = alignment;
    }
    public void setVerticalTextPosition(int textPosition)
    {
	//          Sets the vertical position of the label's text, relative to its image.  
	vert_text_pos = textPosition;
    }
    public void updateUI()
    {	
	LabelUI b = (LabelUI)UIManager.getUI(this);
	setUI(b);
    }
}
