/* JFrame.java --
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


package javax.swing;

import java.awt.AWTEvent;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.LayoutManager;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;

import javax.accessibility.AccessibleContext;

/**
 * Unlike JComponent derivatives, JFrame inherits from
 * java.awt.Frame. But also lets a look-and-feel component to its work.
 *
 * @author Ronald Veldema (rveldema@cs.vu.nl)
 */
public class JFrame extends Frame implements WindowConstants, RootPaneContainer
{
  private static final long serialVersionUID = -3362141868504252139L;
  
  protected  AccessibleContext accessibleContext;
  
  private int close_action = HIDE_ON_CLOSE;    
  
  private static boolean defaultLookAndFeelDecorated = false;    

  public static void setDefaultLookAndFeelDecorated(boolean d)
  {
    defaultLookAndFeelDecorated = d;
  }

  public static boolean isDefaultLookAndFeelDecorated()
  {
    return defaultLookAndFeelDecorated;
  }

    /***************************************************
     *
     *  initia
     *
     *
     *************/
    

    public JFrame()
    {
	super("JFrame");
	frameInit();
    }
  
    public JFrame(String title)
    {
	super(title);
	frameInit();
    }
  

    /***************************************************
     *
     *
     *  methods, this part is shared with JDialog, JFrame
     *
     *
     *************/

  
    private boolean checking;
    protected  JRootPane         rootPane;


    protected  void frameInit()
    {
      super.setLayout(new BorderLayout(1, 1));
      enableEvents(AWTEvent.WINDOW_EVENT_MASK);
      getRootPane(); // will do set/create
    }
  
  public Dimension getPreferredSize()
  {
    Dimension d = super.getPreferredSize();
    return d;
  }

  public JMenuBar getJMenuBar()
    {    return getRootPane().getJMenuBar();   }
    
  public void setJMenuBar(JMenuBar menubar)
    {    getRootPane().setJMenuBar(menubar); }
    

  public  void setLayout(LayoutManager manager)
  {    super.setLayout(manager);  }

  public void setLayeredPane(JLayeredPane layeredPane) 
    {   getRootPane().setLayeredPane(layeredPane);   }
  
  public JLayeredPane getLayeredPane()
    {   return getRootPane().getLayeredPane();     }
  
  public JRootPane getRootPane()
    {
	if (rootPane == null)
	    setRootPane(createRootPane());
	return rootPane;          
    }

  public void setRootPane(JRootPane root)
    {
	if (rootPane != null)
	    remove(rootPane);
	    
	rootPane = root; 
	add(rootPane, BorderLayout.CENTER);
    }

  public JRootPane createRootPane()
    {   return new JRootPane();    }

  public Container getContentPane()
    {    return getRootPane().getContentPane();     }

  public void setContentPane(Container contentPane)
    {    getRootPane().setContentPane(contentPane);    }
  
  public Component getGlassPane()
    {    return getRootPane().getGlassPane();   }
  
  public void setGlassPane(Component glassPane)
    {   getRootPane().setGlassPane(glassPane);   }

    
    protected  void addImpl(Component comp, Object constraints, int index)
    {	super.addImpl(comp, constraints, index);    }


    public void remove(Component comp)
    {   getContentPane().remove(comp);  }
  
    protected  boolean isRootPaneCheckingEnabled()
    {    return checking;        }


    protected  void setRootPaneCheckingEnabled(boolean enabled)
    { checking = enabled;  }


    public void update(Graphics g)
    {   paint(g);  }

    protected  void processKeyEvent(KeyEvent e)
    {	super.processKeyEvent(e);    }

    /////////////////////////////////////////////////////////////////////////////////
  
  public AccessibleContext getAccessibleContext()
  {
    return accessibleContext;
  }
  
    public int getDefaultCloseOperation()
    {    return close_action;   }

    
    
    protected  String paramString()
    {   return "JFrame";     }


    protected  void processWindowEvent(WindowEvent e)
    {
	super.processWindowEvent(e); 
	switch (e.getID())
	    {
	    case WindowEvent.WINDOW_CLOSING:
		{
		    switch(close_action)
			{
			case EXIT_ON_CLOSE:
			    {
				System.exit(0);
				break;
			    }
			case DISPOSE_ON_CLOSE:
			    {
				dispose();
				break;
			    }
			case HIDE_ON_CLOSE:
			    {
				setVisible(false);
				break;
			    }
			case DO_NOTHING_ON_CLOSE:
			    break;
			}
		    break;
		}
		
	    case WindowEvent.WINDOW_CLOSED:
	    case WindowEvent.WINDOW_OPENED:
	    case WindowEvent.WINDOW_ICONIFIED:
	    case WindowEvent.WINDOW_DEICONIFIED:
	    case WindowEvent.WINDOW_ACTIVATED:
	    case WindowEvent.WINDOW_DEACTIVATED:
		break;
	    }
    }   
 
    /**
     * Defines what happens when this frame is closed. Can be one off
     * <code>EXIT_ON_CLOSE</code>,
     * <code>DISPOSE_ON_CLOSE</code>,
     * <code>HIDE_ON_CLOSE</code> or
     * <code>DO_NOTHING_ON_CLOSE</code>.
     * The default is <code>HIDE_ON_CLOSE</code>.
     * When <code>EXIT_ON_CLOSE</code> is specified this method calls
     * <code>SecurityManager.checkExit(0)</code> which might throw a
     * <code>SecurityException</code>. When the specified operation is
     * not one of the above a <code>IllegalArgumentException</code> is
     * thrown.
     */
    public void setDefaultCloseOperation(int operation)
    {
      SecurityManager sm = System.getSecurityManager();
      if (sm != null && operation == EXIT_ON_CLOSE)
	sm.checkExit(0);

      if (operation != EXIT_ON_CLOSE && operation != DISPOSE_ON_CLOSE
	  && operation != HIDE_ON_CLOSE && operation != DO_NOTHING_ON_CLOSE)
	throw new IllegalArgumentException("operation = " + operation);
	  
      close_action = operation;
    }

}
