/* JApplet.java -- 
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

import java.applet.Applet;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.LayoutManager;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;
import javax.accessibility.AccessibleContext;

public class JApplet extends Applet implements RootPaneContainer
{
  private static final long serialVersionUID = 7269359214497372587L;

    public final static int HIDE_ON_CLOSE        = 0;
    public final static int EXIT_ON_CLOSE        = 1;
    public final static int DISPOSE_ON_CLOSE     = 2;
    public final static int DO_NOTHING_ON_CLOSE  = 3;

    private int close_action = EXIT_ON_CLOSE;
    private boolean checking;  
    protected  JRootPane         rootPane;

    public JApplet()
    {
	frameInit();
    }
  
    public JApplet(String title)
    {
	frameInit();
    }

    protected  void frameInit()
    {
      super.setLayout(new BorderLayout(1, 1));
      getRootPane(); // will do set/create
    }

  public Dimension getPreferredSize()
  {
    Dimension d = super.getPreferredSize();
    System.out.println("JFrame.getPrefSize(): " + d + " , comp="+ getComponentCount () + ", layout=" + getLayout());
    return d;
  }

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
  
   public  Component getGlassPane()
    {    return getRootPane().getGlassPane();   }
  
   public void setGlassPane(Component glassPane)
    {   getRootPane().setGlassPane(glassPane);   }


    /////////////////////////////////////////////////////////////////////////////////
    protected  void addImpl(Component comp, Object constraints, int index)
    {   super.addImpl(comp, constraints, index);    }
  
    public AccessibleContext getAccessibleContext()
    {    return null;  }
  
    int getDefaultCloseOperation()
    {    return close_action;   }

    
    public JMenuBar getJMenuBar()
    {    return getRootPane().getJMenuBar();   }
    
    public void setJMenuBar(JMenuBar menubar)
    {    getRootPane().setJMenuBar(menubar); }
    
    
    protected  String paramString()
    {   return "JFrame";     }

    protected  void processKeyEvent(KeyEvent e)
    {   super.processKeyEvent(e);    }

    protected  void processWindowEvent(WindowEvent e)
    {
        //      System.out.println("PROCESS_WIN_EV-1: " + e);

	//        super.processWindowEvent(e); 

        //      System.out.println("PROCESS_WIN_EV-2: " + e);
        switch (e.getID())
            {
            case WindowEvent.WINDOW_CLOSING:
                {
                    switch(close_action)
                        {
                        case EXIT_ON_CLOSE:
                            {
                                System.out.println("user requested exit on close");
                                System.exit(1);
                                break;
                            }
                        case DISPOSE_ON_CLOSE:
                            {
                                System.out.println("user requested dispose on close");
                                //dispose();
                                break;
                            }
                        case HIDE_ON_CLOSE:

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
    

    public void remove(Component comp)
    {   getContentPane().remove(comp);  }
  

    void setDefaultCloseOperation(int operation)
    {  close_action = operation;   }



    protected  boolean isRootPaneCheckingEnabled()
    {    return checking;        }


    protected  void setRootPaneCheckingEnabled(boolean enabled)
    { checking = enabled;  }

    public void update(Graphics g)
    {   paint(g);  }
}
