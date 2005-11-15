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

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.IllegalComponentStateException;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.LayoutManager2;
import java.awt.Rectangle;
import java.io.Serializable;

import javax.accessibility.Accessible;
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
public class JRootPane extends JComponent implements Accessible
{
  //  The class used to obtain the accessible role for this object.
  protected class AccessibleJRootPane extends AccessibleJComponent
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
      // Nothing to do here.
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
     * The cached layout info for the glass pane.
     */
    private Rectangle glassPaneBounds;

    /**
     * The cached layout info for the layered pane.
     */
    private Rectangle layeredPaneBounds;

    /**
     * The cached layout info for the content pane.
     */
    private Rectangle contentPaneBounds;

    /**
     * The cached layout info for the menu bar.
     */
    private Rectangle menuBarBounds;

    /**
     * The cached preferred size.
     */
    private Dimension prefSize;

    /**
     * Creates a new <code>RootLayout</code> object.
     */
    protected RootLayout()
    {
      // Nothing to do here. 
    }

    /**
     * DOCUMENT ME!
     *
     * @param comp DOCUMENT ME!
     * @param constraints DOCUMENT ME!
     */
    public void addLayoutComponent(Component comp, Object constraints)
    {
      // Nothing to do here.
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     * @param comp DOCUMENT ME!
     */
    public void addLayoutComponent(String name, Component comp)
    {
      // Nothing to do here.
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
      return 0.0F;
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
      return 0.0F;
    }

    /**
     * DOCUMENT ME!
     *
     * @param target DOCUMENT ME!
     */
    public void invalidateLayout(Container target)
    {
      synchronized (this)
        {
          glassPaneBounds = null;
          layeredPaneBounds = null;
          contentPaneBounds = null;
          menuBarBounds = null;
          prefSize = null;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param c DOCUMENT ME!
     */
    public void layoutContainer(Container c)
    {
      if (glassPaneBounds == null || layeredPaneBounds == null
          || contentPaneBounds == null || menuBarBounds == null)
        {
          Insets i = getInsets();
          int containerWidth = c.getBounds().width - i.left - i.right;
          int containerHeight = c.getBounds().height - i.top - i.bottom;

          // 1. the glassPane fills entire viewable region (bounds - insets).
          // 2. the layeredPane filles entire viewable region.
          // 3. the menuBar is positioned at the upper edge of layeredPane.
          // 4. the contentPane fills viewable region minus menuBar, if present.
      

          // +-------------------------------+
          // |  JLayeredPane                 |
          // |  +--------------------------+ |
          // |  | menuBar                  | |
          // |  +--------------------------+ |
          // |  +--------------------------+ |
          // |  |contentPane               | |
          // |  |                          | |
          // |  |                          | |
          // |  |                          | |
          // |  +--------------------------+ |
          // +-------------------------------+

          if (menuBar != null)
            {
              Dimension menuBarSize = menuBar.getPreferredSize();
              if (menuBarSize.height > containerHeight)
                menuBarSize.height = containerHeight;
              menuBarBounds = new Rectangle(0, 0, containerWidth,
                                            menuBarSize.height);
              contentPaneBounds = new Rectangle(0, menuBarSize.height,
                                                containerWidth,
                                         containerHeight - menuBarSize.height);
            }
          else
            contentPaneBounds = new Rectangle(0, 0, containerWidth,
                                              containerHeight);
              
          glassPaneBounds = new Rectangle(i.left, i.top, containerWidth, containerHeight);
          layeredPaneBounds = new Rectangle(i.left, i.top, containerWidth, containerHeight);
        }

      glassPane.setBounds(glassPaneBounds);
      layeredPane.setBounds(layeredPaneBounds);
      if (menuBar != null)
        menuBar.setBounds(menuBarBounds);
      contentPane.setBounds(contentPaneBounds);
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
      // We must synchronize here, otherwise we cannot guarantee that the
      // prefSize is still non-null when returning.
      synchronized (this)
        {
          if (prefSize == null)
            {
              Insets i = getInsets();
              prefSize = new Dimension(i.left + i.right, i.top + i.bottom);
              Dimension contentPrefSize = contentPane.getPreferredSize();
              prefSize.width += contentPrefSize.width;
              prefSize.height += contentPrefSize.height;
              if (menuBar != null)
                {
                  Dimension menuBarSize = menuBar.getPreferredSize();
                  if (menuBarSize.width > contentPrefSize.width)
                    prefSize.width += menuBarSize.width - contentPrefSize.width;
                  prefSize.height += menuBarSize.height;
                }
            }
          // Return a copy here so the cached value won't get trashed by some
          // other component.
          return new Dimension(prefSize);
      }
    }

    /**
     * DOCUMENT ME!
     *
     * @param comp DOCUMENT ME!
     */
    public void removeLayoutComponent(Component comp)
    {
      // Nothing to do here.
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
   * This field is unused since JDK1.3. To override the default action you
   * should modify the JRootPane's ActionMap.
   *
   * @deprecated since JDK1.3
   *
   * @specnote the specs indicate that the type of this field is
   *           a package private inner class
   *           javax.swing.JRootPane.DefaultAction. I assume that the closest
   *           public superclass is javax.swing.Action.
   */
  protected Action defaultPressAction;

  /**
   * This field is unused since JDK1.3. To override the default action you
   * should modify the JRootPane's ActionMap.
   *
   * @deprecated since JDK1.3
   *
   * @specnote the specs indicate that the type of this field is
   *           a package private inner class
   *           javax.swing.JRootPane.DefaultAction. I assume that the closest
   *           public superclass is javax.swing.Action.
   */
  protected Action defaultReleaseAction;

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
   * Sets the JRootPane's content pane.  The content pane should typically be
   * opaque for painting to work properly.  This method also 
   * removes the old content pane from the layered pane.
   *
   * @param p the Container that will be the content pane
   * @throws IllegalComponentStateException if p is null
   */
  public void setContentPane(Container p)
  {
    if (p == null)
      throw new IllegalComponentStateException ("cannot " +
            "have a null content pane");
    else
      {
        if (contentPane != null && contentPane.getParent() == layeredPane)
          layeredPane.remove(contentPane);
        contentPane = p;
        getLayeredPane().add(contentPane, JLayeredPane.FRAME_CONTENT_LAYER);
      }
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
        && style != WARNING_DIALOG
        && style != PLAIN_DIALOG)
      throw new IllegalArgumentException("invalid style");
    
    int oldStyle = windowDecorationStyle;
    windowDecorationStyle = style;
    firePropertyChange("windowDecorationStyle", oldStyle, style);
  }
}
