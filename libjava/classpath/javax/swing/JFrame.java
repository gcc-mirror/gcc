/* JFrame.java --
   Copyright (C) 2002, 2004, 2005, 2006,  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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

import gnu.java.lang.CPStringBuilder;

import java.awt.AWTEvent;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.GraphicsConfiguration;
import java.awt.LayoutManager;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;

/**
 * A window that supports window decorations (titlebar and borders).
 * This is an extension of {@link java.awt.Frame} that provides support
 * for the Swing architecture. Most importantly it contains a {@link JRootPane}
 * as it's only top-level child, that manages the content pane, the menu and
 * a glass pane.
 *
 * Also, unlike <code>java.awt.Frame</code>s, JFrames support the
 * Swing Pluggable Look &amp; Feel architecture.
 * 
 * @author Ronald Veldema (rveldema@cs.vu.nl)
 */
public class JFrame extends Frame
  implements WindowConstants, RootPaneContainer, Accessible
{
  /**
   * Provides accessibility support for <code>JFrame</code>s.
   */
  protected class AccessibleJFrame extends Frame.AccessibleAWTFrame
  {
    /**
     * Creates a new instance of <code>AccessibleJFrame</code>.
     */
    protected AccessibleJFrame()
    {
      super();
      // Nothing to do here.
    }
  }

  /**
   * A flag for {@link #setDefaultCloseOperation(int)}, indicating that the
   * application should be exited, when this <code>JFrame</code> is closed.
   * Note that in version 1.4, the equivalent constant has been added to
   * {@link WindowConstants}.
   *
   * @since 1.3
   */
  public static final int EXIT_ON_CLOSE = 3;

  private static final long serialVersionUID = -3362141868504252139L;
  private static boolean defaultLookAndFeelDecorated;
  private int closeAction = HIDE_ON_CLOSE;
  protected AccessibleContext accessibleContext;
  protected JRootPane rootPane;
  
  /**
   * @specnote rootPaneCheckingEnabled is false to comply with J2SE 5.0
   */
  protected boolean rootPaneCheckingEnabled = false;

  /**
   * Creates a new frame with an empty string for the title.
   */
  public JFrame()
  {
    super("");
    frameInit();
  }

  /**
   * Creates a new <code>JFrame</code> with the specified title.
   * 
   * @param title  the frame title (<code>null</code> permitted).
   */
  public JFrame(String title)
  {
    super(title);
    frameInit();
  }

  /**
   * Creates a new JFrame in the specified {@link GraphicsConfiguration}
   * and with an empty title.
   *
   * @param gc the <code>GraphicsConfiguration</code> that is used for
   *     the new <code>JFrame</code>
   *
   * @see Frame#Frame(GraphicsConfiguration)
   */
  public JFrame(GraphicsConfiguration gc)
  {
    super(gc);
    frameInit();
  }

  /**
   * Creates a new JFrame in the specified {@link GraphicsConfiguration}
   * and with the specified title.
   *
   * @param title the title for the new <code>JFrame</code>
   * @param gc the <code>GraphicsConfiguration</code> that is used for
   *     the new <code>JFrame</code>
   *
   * @see Frame#Frame(String, GraphicsConfiguration)
   */
  public JFrame(String title, GraphicsConfiguration gc)
  {
    super(title, gc);
    frameInit();
  }

  protected void frameInit()
  {
    // We need to explicitly enable events here so that our processKeyEvent()
    // and processWindowEvent() gets called.
    enableEvents(AWTEvent.WINDOW_EVENT_MASK | AWTEvent.KEY_EVENT_MASK);

    super.setLayout(new BorderLayout());
    setBackground(UIManager.getDefaults().getColor("control"));
    enableEvents(AWTEvent.WINDOW_EVENT_MASK);
    getRootPane(); // will do set/create

    // Setup the defaultLookAndFeelDecoration if requested.
    if (isDefaultLookAndFeelDecorated()
        && UIManager.getLookAndFeel().getSupportsWindowDecorations())
      {
        setUndecorated(true);
        getRootPane().setWindowDecorationStyle(JRootPane.FRAME);
      }

    // We're now done the init stage.
    setRootPaneCheckingEnabled(true);
  }

  public Dimension getPreferredSize()
  {
    return super.getPreferredSize();
  }

  public JMenuBar getJMenuBar()
  {
    return getRootPane().getJMenuBar();
  }

  public void setJMenuBar(JMenuBar menubar)
  {
    getRootPane().setJMenuBar(menubar);
  }

  public void setLayout(LayoutManager manager)
  {
    // Check if we're in initialization stage.  If so, call super.setLayout
    // otherwise, valid calls go to the content pane.
    if (isRootPaneCheckingEnabled())
      getContentPane().setLayout(manager);
    else
      super.setLayout(manager);
  }

  public void setLayeredPane(JLayeredPane layeredPane)
  {
    getRootPane().setLayeredPane(layeredPane);
  }

  public JLayeredPane getLayeredPane()
  {
    return getRootPane().getLayeredPane();
  }

  public JRootPane getRootPane()
  {
    if (rootPane == null)
      setRootPane(createRootPane());
    return rootPane;
  }

  protected void setRootPane(JRootPane root)
  {
    if (rootPane != null)
      remove(rootPane);

    rootPane = root;
    add(rootPane, BorderLayout.CENTER);
  }

  protected JRootPane createRootPane()
  {
    return new JRootPane();
  }

  public Container getContentPane()
  {
    return getRootPane().getContentPane();
  }

  public void setContentPane(Container contentPane)
  {
    getRootPane().setContentPane(contentPane);
  }

  public Component getGlassPane()
  {
    return getRootPane().getGlassPane();
  }

  public void setGlassPane(Component glassPane)
  {
    getRootPane().setGlassPane(glassPane);
  }

  protected void addImpl(Component comp, Object constraints, int index)
  {
    // If we're adding in the initialization stage use super.add.
    // Otherwise pass the add onto the content pane.
    if (isRootPaneCheckingEnabled() && comp != rootPane)
      getContentPane().add(comp,constraints,index);
    else
      super.addImpl(comp, constraints, index);
  }

  public void remove(Component comp)
  {
    // If we're removing the root pane, use super.remove. Otherwise
    // pass it on to the content pane instead.
    if (comp==rootPane)
      super.remove(rootPane);
    else
      getContentPane().remove(comp);
  }

  protected boolean isRootPaneCheckingEnabled()
  {
    return rootPaneCheckingEnabled;
  }

  protected void setRootPaneCheckingEnabled(boolean enabled)
  {
    rootPaneCheckingEnabled = enabled;
  }

  public void update(Graphics g)
  {
    paint(g);
  }

  protected void processKeyEvent(KeyEvent e)
  {
    super.processKeyEvent(e);
  }

  public static void setDefaultLookAndFeelDecorated(boolean decorated)
  {
    defaultLookAndFeelDecorated = decorated;
  }

  public static boolean isDefaultLookAndFeelDecorated()
  {
    return defaultLookAndFeelDecorated;
  }

  /**
   * Returns the object that provides accessibility features for this 
   * <code>JFrame</code>.
   *
   * @return The accessible context (an instance of {@link AccessibleJFrame}).
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJFrame();
    return accessibleContext;
  }

  /**
   * Returns a code for the default operation when the frame is closed.  The
   * default value is {@link WindowConstants#HIDE_ON_CLOSE}.
   * 
   * @return One of: {@link WindowConstants#DO_NOTHING_ON_CLOSE},
   *     {@link WindowConstants#HIDE_ON_CLOSE}, 
   *     {@link WindowConstants#DISPOSE_ON_CLOSE}, {@link #EXIT_ON_CLOSE}.
   * 
   * @see #setDefaultCloseOperation(int)
   */
  public int getDefaultCloseOperation()
  {
    return closeAction;
  }

  /**
   * Returns a string describing the attributes for the <code>JFrame</code>,
   * for use in debugging.  The return value is guaranteed to be 
   * non-<code>null</code>, but the format may vary between implementations.
   * 
   * @return A string describing the attributes of the <code>JFrame</code>.
   */
  protected String paramString()
  {
    CPStringBuilder sb = new CPStringBuilder(super.paramString());
    sb.append(",defaultCloseOperation=");
    sb.append(SwingUtilities.convertWindowConstantToString(
        getDefaultCloseOperation()));
    sb.append(",rootPane=");
    if (rootPane != null)
      sb.append(rootPane);
    sb.append(",rootPaneCheckingEnabled=").append(rootPaneCheckingEnabled);
    return sb.toString();
  }

  protected void processWindowEvent(WindowEvent e)
  {
    super.processWindowEvent(e);
    if (e.getID() == WindowEvent.WINDOW_CLOSING)
      {
        switch (closeAction)
	  {
	  case EXIT_ON_CLOSE:
	    System.exit(0);
	    break;
	  case DISPOSE_ON_CLOSE:
	    dispose();
	    break;
	  case HIDE_ON_CLOSE:
	    setVisible(false);
	    break;
	  case DO_NOTHING_ON_CLOSE:
	    break;
	  }
      }
  }

  /**
   * Sets the default operation that is performed when this frame is closed.
   * The default is <code>HIDE_ON_CLOSE</code>.  When 
   * <code>EXIT_ON_CLOSE</code> is specified this method calls
   * <code>SecurityManager.checkExit(0)</code> which might throw a
   * <code>SecurityException</code>.
   * 
   * @param operation  a code for the operation (one of: 
   *     {@link WindowConstants#DO_NOTHING_ON_CLOSE}, 
   *     {@link WindowConstants#HIDE_ON_CLOSE}, 
   *     {@link WindowConstants#DISPOSE_ON_CLOSE} and 
   *     {@link WindowConstants#EXIT_ON_CLOSE}).
   * 
   * @throws IllegalArgumentException if <code>operation</code> is not one of
   *     the specified codes.
   * 
   * @see #getDefaultCloseOperation()
   */
  public void setDefaultCloseOperation(int operation)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null && operation == EXIT_ON_CLOSE)
      sm.checkExit(0);

    if (operation != EXIT_ON_CLOSE && operation != DISPOSE_ON_CLOSE
        && operation != HIDE_ON_CLOSE && operation != DO_NOTHING_ON_CLOSE)
      throw new IllegalArgumentException("operation must be EXIT_ON_CLOSE, " 
          + "HIDE_ON_CLOSE, DISPOSE_ON_CLOSE, or DO_NOTHING_ON_CLOSE");

    closeAction = operation;
  }
}
