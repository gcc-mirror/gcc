/* JRootPane.java --
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

import java.awt.*;
import java.awt.event.*;

import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleState;
import javax.accessibility.AccessibleStateSet;

/**
 * This class is where JComponents are added to.
 * Unlike awt where you could just say frame.add(),
 * with swing you need to say frame.getRootPane() 
 * (which delivers an instance of this class)
 * and add your components to that.
 *
 * It is implemented by several 'layers' (pane() should be read as plane()) 
 * each on top of the others
 * where you can add components to. 
 * (getContentPane(), getGlassPane(), getLayeredPane())
 *
 * @author Ronald Veldema (rveldema@cs.vu.nl)
 */
public class JRootPane extends JComponent
{
    //  The class used to obtain the accessible role for this object.
    static protected class AccessibleJRootPane
    {
    }
  
    //A custom layout manager  
    static protected class RootLayout extends BorderLayout
    {
      public Dimension preferredLayoutSize ( Container c )
	{	    
	  Dimension p = super.preferredLayoutSize(c);
	  System.out.println("              PREF-SIZE from RootLayout = " + p);
	  return p;
	}        
    }
  
    /***********************************************************/

  
    //The glass pane that overlays the menu bar and content pane, so it can intercept mouse movements and such.
    protected  Component glassPane;
  
    //The layered pane that manages the menu bar and content pane.
    protected  JLayeredPane layeredPane;
  
    // The menu bar.
    protected  JMenuBar menuBar;
  
    protected Container contentPane;

    /********************************************************/

    public String getUIClassID()
    {	return "JPanel";    }

    
    void setJMenuBar(JMenuBar m)
    {  menuBar = m; }

    JMenuBar getJMenuBar()
    {  return menuBar; }
    

    public Container getContentPane()
    {
	if (contentPane == null)
	    {
		setContentPane(createContentPane());
	    }
	return contentPane;
    }

    public void setContentPane(Container p)
    {
	contentPane = p;    
	getLayeredPane().add(contentPane, 0);
    }

    protected void addImpl(Component comp,
			  Object constraints,
			  int index)
    {
	super.addImpl(comp, constraints, index);
	//System.out.println("don't do that !");
    } 

    public Component getGlassPane() 
    {
	if (glassPane == null)
	    setGlassPane(createGlassPane());
	return glassPane;    
    }

    public void setGlassPane(Component f)
    {
	if (glassPane != null)
	    remove(glassPane);

	glassPane = f; 

	glassPane.setVisible(false);
	add(glassPane, 0);
    }

    public JLayeredPane getLayeredPane() 
    {
	if (layeredPane == null)
	    setLayeredPane(createLayeredPane());
	return layeredPane;    
    }
    public void setLayeredPane(JLayeredPane f)
    {
	if (layeredPane != null)
	    remove(layeredPane);
	
	layeredPane = f; 
	add(f, -1);
    }
    

    /********************************************************/

    JRootPane()
    {
	setLayout(createRootLayout());
	
	getGlassPane();
	getLayeredPane();
	getContentPane();

	setDoubleBuffered(true);
	updateUI();
    }

    protected LayoutManager createRootLayout() {
        return new RootLayout();
    } 

    JComponent createContentPane()
    {
	JPanel p = new JPanel();
	p.setName(this.getName()+".contentPane");
	p.setLayout(new BorderLayout());
	//	p.setVisible(true);

	System.out.println("Created ContentPane: " + p);
	return p;
    }

    Component createGlassPane()
    {
	JPanel p = new JPanel();
	p.setName(this.getName()+".glassPane");
	p.setLayout(new BorderLayout());
	p.setVisible(false);
	
	System.out.println("created the glasspane: "+p);
	return p;
    }

    JLayeredPane createLayeredPane()
    {
	JLayeredPane l = new JLayeredPane();
	return l;
    }    
}
