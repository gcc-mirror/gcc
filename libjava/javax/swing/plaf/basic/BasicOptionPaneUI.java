/* BasicOptionPaneUI.java
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

import java.awt.Component;
import java.awt.Dimension;
import java.awt.LayoutManager;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.OptionPaneUI;

public class BasicOptionPaneUI extends OptionPaneUI
{
    JOptionPane pane;

    BasicOptionPaneUI()
    {
    }

    public static ComponentUI createUI(JComponent x) 
    {
        return new BasicOptionPaneUI();
    }

    public void installUI(JComponent c)
    {
	super.installUI(c);
	pane = (JOptionPane)c;

	System.out.println("     -------------: " + pane);

	JLabel  message   = null;
	JButton ok_button = new JButton("Ok");	

	ok_button.addActionListener(new ActionListener()
	    {
		public void actionPerformed(ActionEvent a)
		{
		    System.out.println("ACTION ---> " + a);
		    //		    pane.dialog.dispose();

		    if (pane.dialog.isModal())
			{
			    System.out.println("modal dialog !!");
			    pane.dialog.setModal(false);
			}
		    pane.dialog.setVisible(false);
		}
	    });

	Object[] options = null;
	if (options != null)
	    {
		for (int i=0; i<options.length; i++)
		    {
			Object o = options[i];
			if (o != null)
			    {
				if (o instanceof String)
				    {
					String s = (String) o;
					JLabel m = new JLabel(s);
					pane.add(m);
				    }
				else if (o instanceof Component)
				    {
					Component com = (Component) o;
					pane.add(com);
				    }
				else
				    {
					System.out.println("UNRECOGNIZED ARG: " + o);
				    }
			    }
		    }
	    }

	pane.add(message);
	pane.add(ok_button);
    }

    Dimension getMinimumOptionPaneSize()
    {
	return new Dimension(300,100);
    }

    public Dimension getPreferredSize(JComponent c)
    {
	if (c == null)
	    return getMinimumOptionPaneSize();

	if (c != pane)
	    return null;

	LayoutManager l  = c.getLayout();
	if (l == null)
	    return getMinimumOptionPaneSize();

	Dimension d1 = l.preferredLayoutSize(c);
	Dimension d2 = getMinimumOptionPaneSize();
	
	d1.width = Math.max(d1.width, d2.width);
	d1.height = Math.max(d1.height, d2.height);

	return d2;
    }

  public void selectInitialValue(JOptionPane op)
  {
     throw new Error ("Not implemented");
  }

  public boolean containsCustomComponents(JOptionPane op)
  {
     throw new Error ("Not implemented");
  }
}
