/* JRootPane.java --
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.LayoutManager;
import java.awt.LayoutManager2;
import javax.swing.plaf.RootPaneUI;

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

    // Custom Layout Manager for JRootPane. It positions contentPane and 
    // menuBar withing its layeredPane.
    protected class RootLayout extends Object implements LayoutManager2
    {
      public void addLayoutComponent(Component comp, Object constraints)
      {
      }

      public void addLayoutComponent(String name, Component comp)
      {
      }

      public float getLayoutAlignmentX(Container target)
      {
        return target.getAlignmentX();
      }

      public float getLayoutAlignmentY(Container target)
      {
        return target.getAlignmentY();
      }

      public void invalidateLayout(Container target)
      {
      }

      public void layoutContainer(Container c)
      {
        Dimension menuBarSize;
        Dimension containerSize = c.getSize(null);
        Dimension contentPaneSize = contentPane.getPreferredSize();

        /*
         if size of top-level window wasn't set then just set
         contentPane and menuBar to its preferred sizes.
         Otherwise, if the size of top-level window was specified then
         set menuBar to its preferred size and make content pane
         to fit into the remaining space


         +-------------------------------+
         |  JLayeredPane                 |  
         |  +--------------------------+ |
         |  | menuBar                  | |
         |  +--------------------------+ |
         |  +--------------------------+ |
         |  |contentPane               | |
         |  |                          | |
         |  |                          | |
         |  |                          | |
         |  +--------------------------+ |
         +-------------------------------+

        */
        if (containerSize.width == 0 && containerSize.height == 0)
          {
	      if (menuBar != null)
	      {
	        int maxWidth;
	        menuBarSize = menuBar.getPreferredSize();
	        maxWidth = Math.max(menuBarSize.width, contentPaneSize.width);
	        menuBar.setBounds(0, 0, maxWidth, menuBarSize.height);
	        contentPane.setBounds(0, menuBarSize.height, maxWidth,
	                              contentPaneSize.height);
	        layeredPane.setSize(maxWidth,
	                            menuBarSize.height + contentPaneSize.height);
	      }
	    else
	      {
	        contentPane.setBounds(0, 0, contentPaneSize.width,
	                              contentPaneSize.height);
	        layeredPane.setSize(contentPaneSize.width, contentPaneSize.height);
	      }
          }
        else
          {
	    if (menuBar != null)
	      {
	        menuBarSize = menuBar.getPreferredSize();
	        if (menuBarSize.height > containerSize.height)
		   menuBarSize.height = containerSize.height;
	        menuBar.setBounds(0, 0, containerSize.width, menuBarSize.height);
	        int remainingHeight = containerSize.height - menuBarSize.height;
	        contentPane.setBounds(0, menuBarSize.height,
	                              containerSize.width,
	                              (containerSize.height - menuBarSize.height));
	      }
	    else
	      contentPane.setBounds(0, 0, containerSize.width,
	                            containerSize.height);

	    layeredPane.setSize(containerSize.width, containerSize.height);
          }
      }
      
      public Dimension maximumLayoutSize(Container target)
      {
        return preferredLayoutSize(target);
      }

      public Dimension minimumLayoutSize(Container target)
      {
        return preferredLayoutSize(target);
      }

      public Dimension preferredLayoutSize(Container c)
      {
        Dimension menuBarSize;
        Dimension prefSize;

        Dimension containerSize = c.getSize();
        Dimension contentPaneSize = contentPane.getPreferredSize();

        if (containerSize.width == 0 && containerSize.height == 0)
          {
	    if (menuBar != null)
	      {
	        int maxWidth;
	        menuBarSize = menuBar.getPreferredSize();
	        maxWidth = Math.max(menuBarSize.width, contentPaneSize.width);
	        prefSize = new Dimension(maxWidth,
	                               contentPaneSize.height
	                               + menuBarSize.height);
	      }
	    else
	      prefSize = contentPaneSize;
          }
        else
          prefSize = c.getSize();

        return prefSize;
      }

      public void removeLayoutComponent(Component comp)
      {
      }
    }
  
    protected  Component glassPane;
    protected  JLayeredPane layeredPane;  
    protected  JMenuBar menuBar;  
    protected Container contentPane;
  
    
    void setJMenuBar(JMenuBar m)
    {  
      menuBar = m; 
      getLayeredPane().add(menuBar, JLayeredPane.FRAME_CONTENT_LAYER);
    }

    JMenuBar getJMenuBar()
    {  return menuBar; }

  public boolean isValidateRoot()
  {
    return true;
  }
    

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
	getLayeredPane().add(contentPane, JLayeredPane.FRAME_CONTENT_LAYER);
    }

    protected void addImpl(Component comp,
			  Object constraints,
			  int index)
    {
	super.addImpl(comp, constraints, index);
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
	return p;
    }

    Component createGlassPane()
    {
	JPanel p = new JPanel();
	p.setName(this.getName()+".glassPane");
	p.setLayout(new BorderLayout());
	p.setVisible(false);
	return p;
    }

    JLayeredPane createLayeredPane()
    {
	JLayeredPane l = new JLayeredPane();
	l.setLayout(null);
	return l;
    }    


  public RootPaneUI getUI()
  {
    return (RootPaneUI) ui;
  }

  public void setUI(RootPaneUI ui)
  {
    super.setUI(ui);
  }

  public void updateUI()
  {
    setUI((RootPaneUI) UIManager.getUI(this));
  }

  public String getUIClassID()
  {
    return "RootPaneUI";
  }
}
