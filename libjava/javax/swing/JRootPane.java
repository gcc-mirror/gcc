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
import java.io.Serializable;

import javax.accessibility.AccessibleRole;
import javax.swing.plaf.RootPaneUI;

/**
 * This class is where JComponents are added to. Unlike awt where you could
 * just say frame.add(), with swing you need to say frame.getRootPane()
 * (which delivers an instance of this class) and add your components to
 * that. It is implemented by several 'layers' (pane() should be read as
 * plane()) each on top of the others where you can add components to.
 * (getContentPane(), getGlassPane(), getLayeredPane())
 *
 * @author Ronald Veldema (rveldema@cs.vu.nl)
 */
public class JRootPane extends JComponent
{
  //  The class used to obtain the accessible role for this object.
  protected static class AccessibleJRootPane
  {
    /**
     * For compatability with Sun's JDK
     */
    private static final long serialVersionUID = 1082432482784468088L;

    /**
     * Creates a new <code>AccessibleJRootPane</code> object.
     */
    protected AccessibleJRootPane()
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.ROOT_PANE;
    }
  }

  // Custom Layout Manager for JRootPane. It positions contentPane and 
  // menuBar withing its layeredPane.
  protected class RootLayout implements LayoutManager2, Serializable
  {
    /** DOCUMENT ME! */
    private static final long serialVersionUID = -4100116998559815027L;

    /**
     * Creates a new <code>RootLayout</code> object.
     */
    protected RootLayout()
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @param comp DOCUMENT ME!
     * @param constraints DOCUMENT ME!
     */
    public void addLayoutComponent(Component comp, Object constraints)
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     * @param comp DOCUMENT ME!
     */
    public void addLayoutComponent(String name, Component comp)
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @param target DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public float getLayoutAlignmentX(Container target)
    {
      return target.getAlignmentX();
    }

    /**
     * DOCUMENT ME!
     *
     * @param target DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public float getLayoutAlignmentY(Container target)
    {
      return target.getAlignmentY();
    }

    /**
     * DOCUMENT ME!
     *
     * @param target DOCUMENT ME!
     */
    public void invalidateLayout(Container target)
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @param c DOCUMENT ME!
     */
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
	      glassPane.setBounds(0, menuBarSize.height, maxWidth,
	                          contentPaneSize.height);
	      contentPane.setBounds(0, menuBarSize.height, maxWidth,
	                            contentPaneSize.height);
	      layeredPane.setSize(maxWidth,
	                          menuBarSize.height + contentPaneSize.height);
	    }
	  else
	    {
	      glassPane.setBounds(0, 0, contentPaneSize.width,
	                          contentPaneSize.height);
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
	      glassPane.setBounds(0, menuBarSize.height, containerSize.width,
	                          containerSize.height - menuBarSize.height);
	      contentPane.setBounds(0, menuBarSize.height,
	                            containerSize.width,
	                            (containerSize.height - menuBarSize.height));
	    }
	  else
	    {
	      glassPane.setBounds(0, 0, containerSize.width,
	                          containerSize.height);
	      contentPane.setBounds(0, 0, containerSize.width,
	                            containerSize.height);
	    }

	  layeredPane.setSize(containerSize.width, containerSize.height);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param target DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Dimension maximumLayoutSize(Container target)
    {
      return preferredLayoutSize(target);
    }

    /**
     * DOCUMENT ME!
     *
     * @param target DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Dimension minimumLayoutSize(Container target)
    {
      return preferredLayoutSize(target);
    }

    /**
     * DOCUMENT ME!
     *
     * @param c DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
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

    /**
     * DOCUMENT ME!
     *
     * @param comp DOCUMENT ME!
     */
    public void removeLayoutComponent(Component comp)
    {
    }
  }

  /** DOCUMENT ME! */
  private static final long serialVersionUID = 8690748000348575668L;

  public static final int NONE = 0;
  public static final int FRAME = 1;
  public static final int PLAIN_DIALOG = 2;
  public static final int INFORMATION_DIALOG = 3;
  public static final int ERROR_DIALOG = 4;
  public static final int COLOR_CHOOSER_DIALOG = 5;
  public static final int FILE_CHOOSER_DIALOG = 6;
  public static final int QUESTION_DIALOG = 7;
  public static final int WARNING_DIALOG = 8;
	  
  /** DOCUMENT ME! */
  protected Component glassPane;

  /** DOCUMENT ME! */
  protected JLayeredPane layeredPane;

  /** DOCUMENT ME! */
  protected JMenuBar menuBar;

  /** DOCUMENT ME! */
  protected Container contentPane;

  protected JButton defaultButton;

  /**
   * @since 1.4
   */
  private int windowDecorationStyle = NONE;
  
  /**
   * DOCUMENT ME!
   *
   * @param m DOCUMENT ME!
   */
  public void setJMenuBar(JMenuBar m)
  {
    JLayeredPane jlPane = getLayeredPane();
    if (menuBar != null)
      jlPane.remove(menuBar);
    menuBar = m;
    if (menuBar != null)
      jlPane.add(menuBar, JLayeredPane.FRAME_CONTENT_LAYER);
  }

  /**
   * @deprecated Replaced by <code>setJMenuBar()</code>
   */
  public void setMenuBar(JMenuBar m)
  {
    setJMenuBar(m);
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public JMenuBar getJMenuBar()
  {
    return menuBar;
  }

  /**
   * @deprecated Replaced by <code>getJMenuBar()</code>
   */
  public JMenuBar getMenuBar()
  {
    return getJMenuBar();
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isValidateRoot()
  {
    return true;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Container getContentPane()
  {
    if (contentPane == null)
      setContentPane(createContentPane());
    return contentPane;
  }

  /**
   * DOCUMENT ME!
   *
   * @param p DOCUMENT ME!
   */
  public void setContentPane(Container p)
  {
    contentPane = p;
    getLayeredPane().add(contentPane, JLayeredPane.FRAME_CONTENT_LAYER);
  }

  /**
   * DOCUMENT ME!
   *
   * @param comp DOCUMENT ME!
   * @param constraints DOCUMENT ME!
   * @param index DOCUMENT ME!
   */
  protected void addImpl(Component comp, Object constraints, int index)
  {
    super.addImpl(comp, constraints, index);
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Component getGlassPane()
  {
    if (glassPane == null)
      setGlassPane(createGlassPane());
    return glassPane;
  }

  /**
   * DOCUMENT ME!
   *
   * @param f DOCUMENT ME!
   */
  public void setGlassPane(Component f)
  {
    if (glassPane != null)
      remove(glassPane);

    glassPane = f;

    glassPane.setVisible(false);
    add(glassPane, 0);
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public JLayeredPane getLayeredPane()
  {
    if (layeredPane == null)
      setLayeredPane(createLayeredPane());
    return layeredPane;
  }

  /**
   * DOCUMENT ME!
   *
   * @param f DOCUMENT ME!
   */
  public void setLayeredPane(JLayeredPane f)
  {
    if (layeredPane != null)
      remove(layeredPane);

    layeredPane = f;
    add(f, -1);
  }

  /**
   * Creates a new <code>JRootPane</code> object.
   */
  public JRootPane()
  {
    setLayout(createRootLayout());
    getGlassPane();
    getLayeredPane();
    getContentPane();
    setDoubleBuffered(true);
    updateUI();
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected LayoutManager createRootLayout()
  {
    return new RootLayout();
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected Container createContentPane()
  {
    JPanel p = new JPanel();
    p.setName(this.getName() + ".contentPane");
    p.setLayout(new BorderLayout());
    return p;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected Component createGlassPane()
  {
    JPanel p = new JPanel();
    p.setName(this.getName() + ".glassPane");
    p.setLayout(new BorderLayout());
    p.setVisible(false);
    p.setOpaque(false);
    return p;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected JLayeredPane createLayeredPane()
  {
    JLayeredPane l = new JLayeredPane();
    l.setLayout(null);
    return l;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public RootPaneUI getUI()
  {
    return (RootPaneUI) ui;
  }

  /**
   * DOCUMENT ME!
   *
   * @param ui DOCUMENT ME!
   */
  public void setUI(RootPaneUI ui)
  {
    super.setUI(ui);
  }

  /**
   * DOCUMENT ME!
   */
  public void updateUI()
  {
    setUI((RootPaneUI) UIManager.getUI(this));
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public String getUIClassID()
  {
    return "RootPaneUI";
  }

  public JButton getDefaultButton()
  {
    return defaultButton;
  }
  
  public void setDefaultButton(JButton newButton)
  {
    if (defaultButton == newButton)
      return;
    
    JButton oldButton = defaultButton;
    defaultButton = newButton;
    firePropertyChange("defaultButton", oldButton, newButton);
  }

  /**
   * @since 1.4
   */
  public int getWindowDecorationStyle()
  {
    return windowDecorationStyle;
  }

  /**
   * @since 1.4
   */
  public void setWindowDecorationStyle(int style)
  {
    if (style != NONE
	&& style != FRAME
	&& style != INFORMATION_DIALOG
	&& style != ERROR_DIALOG
	&& style != COLOR_CHOOSER_DIALOG
	&& style != FILE_CHOOSER_DIALOG
	&& style != QUESTION_DIALOG
	&& style != WARNING_DIALOG)
      throw new IllegalArgumentException("invalid style");
    
    int oldStyle = windowDecorationStyle;
    windowDecorationStyle = style;
    firePropertyChange("windowDecorationStyle", oldStyle, style);
  }
}
