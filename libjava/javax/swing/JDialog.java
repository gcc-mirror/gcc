/* JDialog.java --
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

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.GraphicsConfiguration;
import java.awt.LayoutManager;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;
import java.awt.IllegalComponentStateException;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;


/**
 * Unlike JComponent derivatives, JDialog inherits from java.awt.Dialog. But
 * also lets a look-and-feel component to its work.
 *
 * @author Ronald Veldema (rveldema_AT_cs.vu.nl)
 */
public class JDialog extends Dialog implements Accessible, WindowConstants,
                                               RootPaneContainer
{
  private static final long serialVersionUID = -864070866424508218L;

  /** DOCUMENT ME! */
  protected AccessibleContext accessibleContext;

  /** The single RootPane in the Dialog. */
  protected JRootPane rootPane;

  /** Whether checking is enabled on the RootPane */
  protected boolean rootPaneCheckingEnabled = true;

  /** The default action taken when closed. */
  private int close_action = HIDE_ON_CLOSE;
  
  /** Whether JDialogs are decorated by the Look and Feel. */
  private static boolean decorated = false;

  /**
   * Creates a new non-modal JDialog with no title 
   * using a shared Frame as the owner.
   */
  public JDialog()
  {
    this(SwingUtilities.getOwnerFrame(), "", false, null);
  }

  /**
   * Creates a new non-modal JDialog with no title
   * using the given owner.
   *
   * @param owner The owner of the JDialog.
   */
  public JDialog(Dialog owner)
  {
    this(owner, "", false, null);
  }

  /**
   * Creates a new JDialog with no title using the
   * given modal setting and owner.
   *
   * @param owner The owner of the JDialog.
   * @param modal Whether the JDialog is modal.
   */
  public JDialog(Dialog owner, boolean modal)
  {
    this(owner, "", modal, null);
  }

  /**
   * Creates a new non-modal JDialog using the 
   * given title and owner.
   *
   * @param owner The owner of the JDialog.
   * @param title The title of the JDialog.
   */
  public JDialog(Dialog owner, String title)
  {
    this(owner, title, false, null);
  }

  /**
   * Creates a new JDialog using the given modal 
   * settings, title, and owner.
   *
   * @param owner The owner of the JDialog.
   * @param title The title of the JDialog.
   * @param modal Whether the JDialog is modal.
   */
  public JDialog(Dialog owner, String title, boolean modal)
  {
    this(owner, title, modal, null);
  }

  /**
   * Creates a new JDialog using the given modal 
   * settings, title, owner and graphics configuration.
   *
   * @param owner The owner of the JDialog.
   * @param title The title of the JDialog.
   * @param modal Whether the JDialog is modal.
   * @param gc The Graphics Configuration to use.
   */
  public JDialog(Dialog owner, String title, boolean modal,
                 GraphicsConfiguration gc)
  {
    super(owner, title, modal, gc);
    dialogInit();
  }

  /**
   * Creates a new non-modal JDialog with no title
   * using the given owner.
   *
   * @param owner The owner of the JDialog.
   */
  public JDialog(Frame owner)
  {
    this(owner, "", false, null);
  }

  /**
   * Creates a new JDialog with no title using the
   * given modal setting and owner.
   *
   * @param owner The owner of the JDialog.
   * @param modal Whether the JDialog is modal.
   */
  public JDialog(Frame owner, boolean modal)
  {
    this(owner, "", modal, null);
  }

  /**
   * Creates a new non-modal JDialog using the 
   * given title and owner.
   *
   * @param owner The owner of the JDialog.
   * @param title The title of the JDialog.
   */
  public JDialog(Frame owner, String title)
  {
    this(owner, title, false, null);
  }

  /**
   * Creates a new JDialog using the given modal 
   * settings, title, and owner.
   *
   * @param owner The owner of the JDialog.
   * @param title The title of the JDialog.
   * @param modal Whether the JDialog is modal.
   */
  public JDialog(Frame owner, String title, boolean modal)
  {
    this(owner, title, modal, null);
  }

  /**
   * Creates a new JDialog using the given modal 
   * settings, title, owner and graphics configuration.
   *
   * @param owner The owner of the JDialog.
   * @param title The title of the JDialog.
   * @param modal Whether the JDialog is modal.
   * @param gc The Graphics Configuration to use.
   */
  public JDialog(Frame owner, String title, boolean modal,
                 GraphicsConfiguration gc)
  {
    super((owner == null) ? SwingUtilities.getOwnerFrame() : owner, 
          title, modal, gc);
    dialogInit();
  }

  /**
   * This method is called to initialize the 
   * JDialog. It sets the layout used, the locale, 
   * and creates the RootPane.
   */
  protected void dialogInit()
  {
    // FIXME: Do a check on GraphicsEnvironment.isHeadless()
    setRootPaneCheckingEnabled(false);
    setLocale(JComponent.getDefaultLocale());       
    getRootPane(); // will do set/create  
    setRootPaneCheckingEnabled(true);    
    invalidate();

  }

  /**
   * This method returns whether JDialogs will have their
   * window decorations provided by the Look and Feel.
   *
   * @return Whether the window decorations are Look and Feel provided.
   */
  public static boolean isDefaultLookAndFeelDecorated()
  {
    return decorated;
  }

  /**
   * This method sets whether JDialogs will have their
   * window decorations provided by the Look and Feel.
   *
   * @param defaultLookAndFeelDecorated Whether the window
   * decorations are Look and Feel provided.
   */
  public static void setDefaultLookAndFeelDecorated(boolean defaultLookAndFeelDecorated)
  {
    decorated = defaultLookAndFeelDecorated;
  }

  /**
   * This method returns the preferred size of 
   * the JDialog.
   *
   * @return The preferred size.
   */
  public Dimension getPreferredSize()
  {
    Dimension d = super.getPreferredSize();
    return d;
  }

  /**
   * This method returns the JMenuBar used
   * in this JDialog.
   *
   * @return The JMenuBar in the JDialog.
   */
  public JMenuBar getJMenuBar()
  {
    return getRootPane().getJMenuBar();
  }

  /**
   * This method sets the JMenuBar used 
   * in this JDialog.
   *
   * @param menubar The JMenuBar to use.
   */
  public void setJMenuBar(JMenuBar menubar)
  {
    getRootPane().setJMenuBar(menubar);
  }

  /**
   * This method sets the LayoutManager used in the JDialog.
   * This method will throw an Error if rootPaneChecking is 
   * enabled.
   *
   * @param manager The LayoutManager to use.
   */
  public void setLayout(LayoutManager manager)
  {
    if (isRootPaneCheckingEnabled())
      throw new Error("rootPaneChecking is enabled - cannot set layout.");
    super.setLayout(manager);
  }

  /**
   * This method sets the JLayeredPane used in the JDialog.
   * If the given JLayeredPane is null, then this method
   * will throw an Error.
   *
   * @param layeredPane The JLayeredPane to use.
   */
  public void setLayeredPane(JLayeredPane layeredPane)
  {
    if (layeredPane == null)
      throw new IllegalComponentStateException("layeredPane cannot be null.");
    getRootPane().setLayeredPane(layeredPane);
  }

  /**
   * This method returns the JLayeredPane used with this JDialog.
   *
   * @return The JLayeredPane used with this JDialog.
   */
  public JLayeredPane getLayeredPane()
  {
    return getRootPane().getLayeredPane();
  }

  /**
   * This method returns the JRootPane used with this JDialog.
   *
   * @return The JRootPane used with this JDialog.
   */
  public JRootPane getRootPane()
  {
    if (rootPane == null)
      setRootPane(createRootPane());
    return rootPane;
  }

  /**
   * This method sets the JRootPane used with this JDialog.
   *
   * @param root The JRootPane to use.
   */
  protected void setRootPane(JRootPane root)
  {
    if (rootPane != null)
      remove(rootPane);

    rootPane = root;
    rootPane.show();
    add(rootPane);
  }

  /**
   * This method creates a new JRootPane.
   *
   * @return A new JRootPane.
   */
  protected JRootPane createRootPane()
  {
    return new JRootPane();
  }

  /**
   * This method returns the ContentPane
   * in the JRootPane.
   *
   * @return The ContentPane in the JRootPane.
   */
  public Container getContentPane()
  {
    return getRootPane().getContentPane();
  }

  /**
   * This method sets the ContentPane to use with this
   * JDialog. If the ContentPane given is null, this method
   * will throw an exception.
   *
   * @param contentPane The ContentPane to use with the JDialog.
   */
  public void setContentPane(Container contentPane)
  {
    if (contentPane == null)
      throw new IllegalComponentStateException("contentPane cannot be null.");
    getRootPane().setContentPane(contentPane);
  }

  /**
   * This method returns the GlassPane for this JDialog.
   *
   * @return The GlassPane for this JDialog.
   */
  public Component getGlassPane()
  {
    return getRootPane().getGlassPane();
  }

  /**
   * This method sets the GlassPane for this JDialog.
   *
   * @param glassPane The GlassPane for this JDialog.
   */
  public void setGlassPane(Component glassPane)
  {
    getRootPane().setGlassPane(glassPane);
  }

  /**
   * This method is called when a component is added to the 
   * the JDialog. Calling this method with rootPaneCheckingEnabled
   * will cause an Error to be thrown.
   *
   * @param comp The component to add.
   * @param constraints The constraints.
   * @param index The position of the component.
   */
  protected void addImpl(Component comp, Object constraints, int index)
  {
    if (isRootPaneCheckingEnabled())
      throw new Error("rootPaneChecking is enabled - adding components disallowed.");
    super.addImpl(comp, constraints, index);
  }

  /**
   * This method removes a component from the JDialog.
   *
   * @param comp The component to remove.
   */
  public void remove(Component comp)
  {
    // The path changes if the component == root.
    if (comp == rootPane)
      super.remove(rootPane);
    else 
      getContentPane().remove(comp);
  }

  /**
   * This method returns whether rootPane checking is enabled.
   *
   * @return Whether rootPane checking is enabled.
   */
  protected boolean isRootPaneCheckingEnabled()
  {
    return rootPaneCheckingEnabled;
  }

  /**
   * This method sets whether rootPane checking is enabled.
   *
   * @param enabled Whether rootPane checking is enabled.
   */
  protected void setRootPaneCheckingEnabled(boolean enabled)
  {
    rootPaneCheckingEnabled = enabled;
  }

  /**
   * This method simply calls paint and returns.
   *
   * @param g The Graphics object to paint with.
   */
  public void update(Graphics g)
  {
    paint(g);
  }
  
  
  /**
   * This method handles window events. This allows the JDialog
   * to honour its default close operation.
   *
   * @param e The WindowEvent.
   */
  protected void processWindowEvent(WindowEvent e)
  {
    //	System.out.println("PROCESS_WIN_EV-1: " + e);
    super.processWindowEvent(e);
    //	System.out.println("PROCESS_WIN_EV-2: " + e);
    switch (e.getID())
      {
      case WindowEvent.WINDOW_CLOSING:
        {
	  switch (getDefaultCloseOperation())
	    {
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
   * This method sets the action to take
   * when the JDialog is closed.
   *
   * @param operation The action to take.
   */
  public void setDefaultCloseOperation(int operation)
  {
    if (operation == DO_NOTHING_ON_CLOSE ||
    	operation == HIDE_ON_CLOSE ||
	operation == DISPOSE_ON_CLOSE)
      close_action = operation;
    else
      throw new IllegalArgumentException("Default close operation must be one of DO_NOTHING_ON_CLOSE, HIDE_ON_CLOSE, or DISPOSE_ON_CLOSE");
  }

  /**
   * This method returns the action taken when
   * the JDialog is closed.
   *
   * @return The action to take.
   */
  public int getDefaultCloseOperation()
  {
    return close_action;
  }

  /**
   * This method returns a String describing the JDialog.
   *
   * @return A String describing the JDialog.
   */
  protected String paramString()
  {
    return "JDialog";
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public AccessibleContext getAccessibleContext()
  {
    return null;
  }
}
