/* JDesktopPane.java --
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

import java.awt.Component;
import java.beans.PropertyVetoException;
import java.io.IOException;
import java.io.ObjectOutputStream;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.swing.plaf.DesktopPaneUI;


/**
 * JDesktopPane is a container (usually for JInternalFrames) that simulates a
 * desktop. Typically, the user will create JInternalFrames and place thme in
 * a JDesktopPane. The user can then interact with JInternalFrames like they
 * usually would with JFrames. The actions (minimize, maximize, close, etc)
 * are done by using a DesktopManager that is associated with the
 * JDesktopPane.
 */
public class JDesktopPane extends JLayeredPane implements Accessible
{
  /** DOCUMENT ME! */
  private static final long serialVersionUID = 766333777224038726L;

  /**
   * This specifies that when dragged, a JInternalFrame should be completely
   * visible.
   */
  public static int LIVE_DRAG_MODE = 0;

  /**
   * This specifies that when dragged, a JInternalFrame should only be visible
   * as an outline.
   */
  public static int OUTLINE_DRAG_MODE = 1;

  /** The selected frame in the JDesktopPane. */
  private transient JInternalFrame selectedFrame;

  /** The JDesktopManager to use for acting on JInternalFrames. */
  transient DesktopManager desktopManager;

  /** The drag mode used by the JDesktopPane. */
  private transient int dragMode = LIVE_DRAG_MODE;

  /**
   * AccessibleJDesktopPane
   */
  protected class AccessibleJDesktopPane extends AccessibleJComponent
  {
    /** DOCUMENT ME! */
    private static final long serialVersionUID = 6079388927946077570L;

    /**
     * Constructor AccessibleJDesktopPane
     */
    protected AccessibleJDesktopPane()
    {
    }

    /**
     * getAccessibleRole
     *
     * @return AccessibleRole
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.DESKTOP_PANE;
    }
  }

  /**
   * Creates a new JDesktopPane object.
   */
  public JDesktopPane()
  {
    setLayout(null);
    updateUI();
  }

  /**
   * This method returns the UI used with the JDesktopPane.
   *
   * @return The UI used with the JDesktopPane.
   */
  public DesktopPaneUI getUI()
  {
    return (DesktopPaneUI) ui;
  }

  /**
   * This method sets the UI used with the JDesktopPane.
   *
   * @param ui The UI to use with the JDesktopPane.
   */
  public void setUI(DesktopPaneUI ui)
  {
    super.setUI(ui);
  }

  /**
   * This method sets the drag mode to use with the JDesktopPane.
   *
   * @param mode The drag mode to use.
   *
   * @throws IllegalArgumentException If the drag mode given is not
   *         LIVE_DRAG_MODE or OUTLINE_DRAG_MODE.
   */
  public void setDragMode(int mode)
  {
    if ((mode != LIVE_DRAG_MODE) && (mode != OUTLINE_DRAG_MODE))
      throw new IllegalArgumentException("Drag mode not valid.");

    // FIXME: Unsupported mode.
    if (mode == OUTLINE_DRAG_MODE)
      throw new IllegalArgumentException("Outline drag modes are unsupported.");

    dragMode = mode;
  }

  /**
   * This method returns the drag mode used with the JDesktopPane.
   *
   * @return The drag mode used with the JDesktopPane.
   */
  public int getDragMode()
  {
    return dragMode;
  }

  /**
   * This method returns the DesktopManager used with the JDesktopPane.
   *
   * @return The DesktopManager to use with the JDesktopPane.
   */
  public DesktopManager getDesktopManager()
  {
    return desktopManager;
  }

  /**
   * This method sets the DesktopManager to use with the JDesktopPane.
   *
   * @param manager The DesktopManager to use with the JDesktopPane.
   */
  public void setDesktopManager(DesktopManager manager)
  {
    desktopManager = manager;
  }

  /**
   * This method restores the UI used with the JDesktopPane to the default.
   */
  public void updateUI()
  {
    setUI((DesktopPaneUI) UIManager.getUI(this));
    invalidate();
  }

  /**
   * This method returns a String identifier that allows the UIManager to know
   * which class will act as JDesktopPane's UI.
   *
   * @return A String identifier for the UI class to use.
   */
  public String getUIClassID()
  {
    return "DesktopPaneUI";
  }

  /**
   * This method returns all JInternalFrames that are in the JDesktopPane.
   *
   * @return All JInternalFrames that are in the JDesktopPane.
   */
  public JInternalFrame[] getAllFrames()
  {
    return getFramesFromComponents(getComponents());
  }

  /**
   * This method returns the currently selected frame in the JDesktopPane.
   *
   * @return The currently selected frame in the JDesktopPane.
   */
  public JInternalFrame getSelectedFrame()
  {
    return selectedFrame;
  }

  /**
   * This method sets the selected frame in the JDesktopPane.
   *
   * @param frame The selected frame in the JDesktopPane.
   */
  public void setSelectedFrame(JInternalFrame frame)
  {
    if (selectedFrame != null)
      {
	try
	  {
	    selectedFrame.setSelected(false);
	  }
	catch (PropertyVetoException e)
	  {
	  }
      }
    selectedFrame = null;

    try
      {
	if (frame != null)
	  frame.setSelected(true);

	selectedFrame = frame;
      }
    catch (PropertyVetoException e)
      {
      }
  }

  /**
   * This method returns all the JInternalFrames in the given layer.
   *
   * @param layer The layer to grab frames in.
   *
   * @return All JInternalFrames in the given layer.
   */
  public JInternalFrame[] getAllFramesInLayer(int layer)
  {
    return getFramesFromComponents(getComponentsInLayer(layer));
  }

  /**
   * This method always returns true to indicate that it is not transparent.
   *
   * @return true.
   */
  public boolean isOpaque()
  {
    return true;
  }

  /**
   * This method returns a String that describes the JDesktopPane.
   *
   * @return A String that describes the JDesktopPane.
   */
  protected String paramString()
  {
    return "JDesktopPane";
  }

  /**
   * This method returns all the JInternalFrames in the given Component array.
   *
   * @param components An array to search for JInternalFrames in.
   *
   * @return An array of JInternalFrames found in the Component array.
   */
  private static JInternalFrame[] getFramesFromComponents(Component[] components)
  {
    int count = 0;

    for (int i = 0; i < components.length; i++)
	if (components[i] instanceof JInternalFrame)
	  count++;
	  
    JInternalFrame[] value = new JInternalFrame[count];
    for (int i = 0, j = 0; i < components.length && j != count; i++)
      if (components[i] instanceof JInternalFrame)
	value[j++] = (JInternalFrame) components[i];
    return value;
  }

  /**
   * getAccessibleContext
   *
   * @return AccessibleContext
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJDesktopPane();

    return accessibleContext;
  }
}
