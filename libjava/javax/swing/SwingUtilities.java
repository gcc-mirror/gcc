/* SwingUtilities.java -- 
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
import java.awt.EventQueue;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.lang.reflect.InvocationTargetException;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleStateSet;

public class SwingUtilities implements SwingConstants
{
  public static FontMetrics getFontMetrics (Font font)
  {
    return Toolkit.getDefaultToolkit ().getFontMetrics (font);
  }

  public static JRootPane getRootPane (Component a)
  {
    if (a instanceof JRootPane)
      return (JRootPane) a;
	
    a = a.getParent();

    if (a != null)
      {
        return getRootPane(a);
      }
    
    return null;
  }

  public static void updateComponentTreeUI(JFrame comp)
  {
  }

  public static String layoutCompoundLabel(JComponent c, 
                                           FontMetrics fm,
                                           String text,
                                           Icon i,
                                           int vert_a, 
                                           int hor_i, 
                                           int vert_text_pos,
                                           int hor_text_pos, 
                                           Rectangle vr,
                                           Rectangle ir, 
                                           Rectangle tr,
                                           int gap)
  {
    // view rect 'vr' already ok, 
    // we need to compute ir (icon rect) and tr (text-rect)
	
    int next_x = 0;//vr.x;
    int next_y = 0;//vr.y;
	
    ir.height = ir.width = ir.y = ir.x = 0;

    if (i != null)
      {
        ir.x = vr.x;
        ir.y = vr.y;
        ir.width = i.getIconWidth();
        ir.height = i.getIconWidth();


        next_x += gap + i.getIconWidth();
        next_y += gap + i.getIconHeight();
      }
	
    tr.x = next_x;
    tr.y = vr.y + (vr.height/2);

    tr.width  = fm.stringWidth(text);
    tr.height = fm.getHeight() +  fm.getAscent()/2;

    return text;
  }

}









