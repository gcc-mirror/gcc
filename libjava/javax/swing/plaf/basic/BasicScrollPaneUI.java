/* BasicScrollPaneUI.java
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

import java.awt.Dimension;
import java.awt.Graphics;

import javax.swing.JComponent;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.ScrollPaneLayout;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.ScrollPaneUI;

public class BasicScrollPaneUI extends ScrollPaneUI
  implements ScrollPaneConstants
{
  
  public static ComponentUI createUI(final JComponent c) 
  {
    return new BasicScrollPaneUI();
  }

  protected void installDefaults(JScrollPane p)
  {
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();
    p.setForeground(defaults.getColor("ScrollPane.foreground"));
    p.setBackground(defaults.getColor("ScrollPane.background"));
    p.setFont(defaults.getFont("ScrollPane.font"));
    p.setBorder(defaults.getBorder("ScrollPane.border"));
    p.setOpaque(true);
  }

  protected void uninstallDefaults(JScrollPane p)
  {
    p.setForeground(null);
    p.setBackground(null);
    p.setFont(null);
    p.setBorder(null);
  }
    
  public void installUI(final JComponent c) 
  {
    super.installUI(c);
    this.installDefaults((JScrollPane)c);
  }

  public void uninstallUI(final JComponent c) 
  {
    super.uninstallUI(c);
    this.uninstallDefaults((JScrollPane)c);
  }

    
  public Dimension getMinimumSize(JComponent c) 
  {
    JScrollPane p = (JScrollPane ) c;
    ScrollPaneLayout sl = (ScrollPaneLayout) p.getLayout();
    return sl.minimumLayoutSize(c);
  }

  public Dimension getPreferredSize(JComponent c) 
  {
    JScrollPane p = (JScrollPane ) c;
    ScrollPaneLayout sl = (ScrollPaneLayout) p.getLayout();
    return sl.preferredLayoutSize(c);
  }


  public void paint(Graphics g, JComponent c)
  {      
    // do nothing; the normal painting-of-children algorithm, along with
    // ScrollPaneLayout, does all the relevant work.
  }
}












