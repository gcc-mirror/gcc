/* BasicIconFactory.java
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

import java.io.Serializable;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Polygon;
import javax.swing.AbstractButton;
import javax.swing.Icon;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
/**
 * STUBBED
 */
public class BasicIconFactory implements Serializable
{
  static final long serialVersionUID = 5605588811185324383L;

  static private class DummyIcon 
    implements Icon
  {    
    public int getIconHeight() { return 10; }
    public int getIconWidth() { return 10; }
    public void paintIcon(Component c, Graphics g, int x, int y)
    {
      Color save = g.getColor();
      g.setColor(c.getForeground());
      g.drawRect(x, y, 10, 10);
      g.setColor(save);
    }
  }


  public BasicIconFactory()
  {
  }
  public static Icon getMenuItemCheckIcon()
  {
    return new DummyIcon();
  }
  public static Icon getMenuItemArrowIcon()
  {
    return new DummyIcon();
  }
  public static Icon getMenuArrowIcon()
  {
    return new Icon()
      {
	public int getIconHeight()
	{
	  return 12;
	}

	public int getIconWidth()
	{
	  return 12;
	}

	public void paintIcon(Component c, Graphics g, int x, int y)
	{
	  g.translate(x, y);

	  Color saved = g.getColor();

	  g.setColor(Color.BLACK);

	  g.fillPolygon(new Polygon(new int[] { 3, 9, 3 },
                                  new int[] { 2, 6, 10 },
                                  3));

	  g.setColor(saved);
	  g.translate(-x, -y);
	}
      };
  }

  public static Icon getCheckBoxIcon()
  {
    return new Icon()
      {        
        public int getIconHeight() 
        { 
          return 10; 
        }
        public int getIconWidth() 
        { 
          return 10; 
        }
        public void paintIcon(Component c, Graphics g, int x, int y)
        {
          if (c instanceof AbstractButton)
            {
              UIDefaults defaults;
              defaults = UIManager.getLookAndFeelDefaults();
              Color hi = defaults.getColor("CheckBox.highlight");
              Color low = defaults.getColor("CheckBox.darkShadow");
              Color sel = defaults.getColor("CheckBox.foreground");
              Color dim = defaults.getColor("CheckBox.shadow");
              Polygon check = new Polygon(new int[] {x+3, x+3, x+8},
                                          new int[] {y+5, y+9, y+3}, 3);
              AbstractButton b = (AbstractButton) c;
              Color saved = g.getColor();
              if (b.isEnabled())
                {
                  g.setColor(low);
                  g.drawRect(x, y, 10, 10);
                  g.setColor(hi);
                  g.drawRect(x+1, y+1, 10, 10);
                  if (b.isSelected())
                    {
                      g.setColor(sel);
                      if (b.isSelected())
                        {
                          g.drawLine(x+3, y+5, x+3, y+8);
                          g.drawLine(x+4, y+5, x+4, y+8);
                          g.drawLine(x+3, y+8, x+8, y+3);
                          g.drawLine(x+4, y+8, x+8, y+3);
                        }
                    }
                }
              else
                {                  
                  g.setColor(hi);
                  g.drawRect(x, y, 10, 10);
                  if (b.isSelected())
                    {
                      g.drawLine(x+3, y+5, x+3, y+9);
                      g.drawLine(x+3, y+9, x+8, y+3);
                    }
                }
              g.setColor(saved);
            }
        }
      };
  }

  public static Icon getRadioButtonIcon()
  {
    return new Icon()
      {        
        public int getIconHeight() 
        { 
          return 12; 
        }
        public int getIconWidth() 
        { 
          return 12; 
        }
        public void paintIcon(Component c, Graphics g, int x, int y)
        {
          UIDefaults defaults;      
          defaults = UIManager.getLookAndFeelDefaults();
          Color hi = defaults.getColor("RadioButton.highlight");
          Color low = defaults.getColor("RadioButton.darkShadow");
          Color sel = defaults.getColor("RadioButton.foreground");
          Color dim = defaults.getColor("RadioButton.shadow");

          if (c instanceof AbstractButton)
            {
              AbstractButton b = (AbstractButton) c;
              Color saved = g.getColor();
              if (b.isEnabled())
                {
                  g.setColor(low);
                  g.drawOval(x, y, 12, 12);
                  g.setColor(hi);
                  g.drawOval(x+1, y+1, 12, 12);
                  if (b.isSelected())
                    {
                      g.setColor(sel);
                      g.fillOval(x+4, y+4, 6, 6);
                    }
                }
              else
                {                  
                  g.setColor(hi);
                  g.drawOval(x, y, 12, 12);
                  if (b.isSelected())
                    g.fillOval(x+4, y+4, 6, 6);
                }
              g.setColor(saved);
            }
        }
      };
  }
  public static Icon getCheckBoxMenuItemIcon()
  {
    return getCheckBoxIcon();
  }
  public static Icon getRadioButtonMenuItemIcon()
  {
    return getRadioButtonIcon();
  }
  public static Icon createEmptyFrameIcon()
  {
    return new DummyIcon();
  }
} // class BasicIconFactory
