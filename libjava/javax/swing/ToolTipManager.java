/* ToolTipManager.java --
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
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.LayoutManager;
import java.awt.Panel;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;

/**
 * This class is responsible for the registration of JToolTips to Components
 * and for displaying them when appropriate.
 */
public class ToolTipManager extends MouseAdapter implements MouseMotionListener
{
  /**
   * This ActionListener is associated with the Timer that listens to whether
   * the JToolTip can be hidden after four seconds.
   */
  protected class stillInsideTimerAction implements ActionListener
  {
    /**
     * This method creates a new stillInsideTimerAction object.
     */
    protected stillInsideTimerAction()
    {
    }

    /**
     * This method hides the JToolTip when the Timer has finished.
     *
     * @param event The ActionEvent.
     */
    public void actionPerformed(ActionEvent event)
    {
      hideTip();
    }
  }

  /**
   * This Actionlistener is associated with the Timer that listens to whether
   * the mouse cursor has re-entered the JComponent in time for an immediate
   * redisplay of the JToolTip.
   */
  protected class outsideTimerAction implements ActionListener
  {
    /**
     * This method creates a new outsideTimerAction object.
     */
    protected outsideTimerAction()
    {
    }

    /**
     * This method is called when the Timer that listens to whether the mouse
     * cursor has re-entered the JComponent has run out.
     *
     * @param event The ActionEvent.
     */
    public void actionPerformed(ActionEvent event)
    {
    }
  }

  /**
   * This ActionListener is associated with the Timer that listens to whether
   * it is time for the JToolTip to be displayed after the mouse has entered
   * the JComponent.
   */
  protected class insideTimerAction implements ActionListener
  {
    /**
     * This method creates a new insideTimerAction object.
     */
    protected insideTimerAction()
    {
    }

    /**
     * This method displays the JToolTip when the Mouse has been still for the
     * delay.
     *
     * @param event The ActionEvent.
     */
    public void actionPerformed(ActionEvent event)
    {
      showTip();
      if (insideTimer != null)
	insideTimer.start();
    }
  }

  /**
   * The Timer that determines whether the Mouse has been still long enough
   * for the JToolTip to be displayed.
   */
  Timer enterTimer;

  /**
   * The Timer that determines whether the Mouse has re-entered the JComponent
   * quickly enough for the JToolTip to be displayed immediately.
   */
  Timer exitTimer;

  /**
   * The Timer that determines whether the JToolTip has been displayed long
   * enough for it to be hidden.
   */
  Timer insideTimer;

  /** A global enabled setting for the ToolTipManager. */
  private transient boolean enabled = true;

  /** lightWeightPopupEnabled */
  protected boolean lightWeightPopupEnabled = true;

  /** heavyWeightPopupEnabled */
  protected boolean heavyWeightPopupEnabled = false;

  /** The shared instance of the ToolTipManager. */
  private static ToolTipManager shared;

  /** The current component the tooltip is being displayed for. */
  private static Component currentComponent;

  /** The current tooltip. */
  private static JToolTip currentTip;

  /** The last known position of the mouse cursor. */
  private static Point currentPoint;

  /**
   * The panel that holds the tooltip when the tooltip is displayed fully
   * inside the current container.
   */
  private static Container containerPanel;

  /**
   * The window used when the tooltip doesn't fit inside the current
   * container.
   */
  private static JWindow tooltipWindow;

  /**
   * Creates a new ToolTipManager and sets up the timers.
   */
  ToolTipManager()
  {
    enterTimer = new Timer(750, new insideTimerAction());
    enterTimer.setRepeats(false);

    insideTimer = new Timer(4000, new stillInsideTimerAction());
    insideTimer.setRepeats(false);

    exitTimer = new Timer(500, new outsideTimerAction());
    exitTimer.setRepeats(false);
  }

  /**
   * This method returns the shared instance of ToolTipManager used by all
   * JComponents.
   *
   * @return The shared instance of ToolTipManager.
   */
  public static ToolTipManager sharedInstance()
  {
    if (shared == null)
      shared = new ToolTipManager();

    return shared;
  }

  /**
   * This method sets whether ToolTips are enabled or disabled for all
   * JComponents.
   *
   * @param enabled Whether ToolTips are enabled or disabled for all
   *        JComponents.
   */
  public void setEnabled(boolean enabled)
  {
    if (! enabled)
      {
	enterTimer.stop();
	exitTimer.stop();
	insideTimer.stop();
      }

    this.enabled = enabled;
  }

  /**
   * This method returns whether ToolTips are enabled.
   *
   * @return Whether ToolTips are enabled.
   */
  public boolean isEnabled()
  {
    return enabled;
  }

  /**
   * This method returns whether LightweightToolTips are enabled.
   *
   * @return Whether LighweightToolTips are enabled.
   */
  public boolean isLightWeightPopupEnabled()
  {
    return lightWeightPopupEnabled;
  }

  /**
   * This method sets whether LightweightToolTips are enabled. If you mix
   * Lightweight and Heavyweight components, you must set this to false to
   * ensure that the ToolTips popup above all other components.
   *
   * @param enabled Whether LightweightToolTips will be enabled.
   */
  public void setLightWeightPopupEnabled(boolean enabled)
  {
    lightWeightPopupEnabled = enabled;
    heavyWeightPopupEnabled = ! enabled;
  }

  /**
   * This method returns the initial delay before the ToolTip is shown when
   * the mouse enters a Component.
   *
   * @return The initial delay before the ToolTip is shown.
   */
  public int getInitialDelay()
  {
    return enterTimer.getDelay();
  }

  /**
   * This method sets the initial delay before the ToolTip is shown when the
   * mouse enters a Component.
   *
   * @param delay The initial delay before the ToolTip is shown.
   */
  public void setInitialDelay(int delay)
  {
    enterTimer.setDelay(delay);
  }

  /**
   * This method returns the time the ToolTip will be shown before being
   * hidden.
   *
   * @return The time the ToolTip will be shown before being hidden.
   */
  public int getDismissDelay()
  {
    return insideTimer.getDelay();
  }

  /**
   * This method sets the time the ToolTip will be shown before being hidden.
   *
   * @param delay The time the ToolTip will be shown before being hidden.
   */
  public void setDismissDelay(int delay)
  {
    insideTimer.setDelay(delay);
  }

  /**
   * This method returns the amount of delay where if the mouse re-enters a
   * Component, the tooltip will be shown immediately.
   *
   * @return The reshow delay.
   */
  public int getReshowDelay()
  {
    return exitTimer.getDelay();
  }

  /**
   * This method sets the amount of delay where if the mouse re-enters a
   * Component, the tooltip will be shown immediately.
   *
   * @param delay The reshow delay.
   */
  public void setReshowDelay(int delay)
  {
    exitTimer.setDelay(delay);
  }

  /**
   * This method registers a JComponent with the ToolTipManager.
   *
   * @param component The JComponent to register with the ToolTipManager.
   */
  public void registerComponent(JComponent component)
  {
    component.addMouseListener(this);
    component.addMouseMotionListener(this);
  }

  /**
   * This method unregisters a JComponent with the ToolTipManager.
   *
   * @param component The JComponent to unregister with the ToolTipManager.
   */
  public void unregisterComponent(JComponent component)
  {
    component.removeMouseMotionListener(this);
    component.removeMouseListener(this);
  }

  /**
   * This method is called whenever the mouse enters a JComponent registered
   * with the ToolTipManager. When the mouse enters within the period of time
   * specified by the reshow delay, the tooltip will be displayed
   * immediately. Otherwise, it must wait for the initial delay before
   * displaying the tooltip.
   *
   * @param event The MouseEvent.
   */
  public void mouseEntered(MouseEvent event)
  {
    if (currentComponent != null
        && getContentPaneDeepestComponent(event) == currentComponent)
      return;
    currentPoint = event.getPoint();
    currentComponent = (Component) event.getSource();

    if (exitTimer.isRunning())
      {
	exitTimer.stop();
	showTip();
	insideTimer.start();
	return;
      }

    // This should always be stopped unless we have just fake-exited.
    if (! enterTimer.isRunning())
      enterTimer.start();
  }

  /**
   * This method is called when the mouse exits a JComponent registered with
   * the ToolTipManager. When the mouse exits, the tooltip should be hidden
   * immediately.
   *
   * @param event The MouseEvent.
   */
  public void mouseExited(MouseEvent event)
  {
    if (getContentPaneDeepestComponent(event) == currentComponent)
      return;

    currentPoint = event.getPoint();
    currentComponent = null;
    hideTip();

    if (! enterTimer.isRunning() && insideTimer.isRunning())
      exitTimer.start();
    if (enterTimer.isRunning())
      enterTimer.stop();
    if (insideTimer.isRunning())
      insideTimer.stop();
  }

  /**
   * This method is called when the mouse is pressed on a JComponent
   * registered with the ToolTipManager. When the mouse is pressed, the
   * tooltip (if it is shown) must be hidden immediately.
   *
   * @param event The MouseEvent.
   */
  public void mousePressed(MouseEvent event)
  {
    currentPoint = event.getPoint();
    if (enterTimer.isRunning())
      enterTimer.restart();
    else if (insideTimer.isRunning())
      {
	insideTimer.stop();
	hideTip();
      }
    currentComponent.invalidate();
    currentComponent.validate();
    currentComponent.repaint();
  }

  /**
   * This method is called when the mouse is dragged in a JComponent
   * registered with the ToolTipManager.
   *
   * @param event The MouseEvent.
   */
  public void mouseDragged(MouseEvent event)
  {
    currentPoint = event.getPoint();
    if (enterTimer.isRunning())
      enterTimer.restart();
  }

  /**
   * This method is called when the mouse is moved in a JComponent registered
   * with the ToolTipManager.
   *
   * @param event The MouseEvent.
   */
  public void mouseMoved(MouseEvent event)
  {
    currentPoint = event.getPoint();
    if (currentTip != null)
      {
	if (currentComponent == null)
	  currentComponent = (Component) event.getSource();
	
	String text = ((JComponent) currentComponent).getToolTipText(event);
	currentTip.setTipText(text);
      }
    if (enterTimer.isRunning())
      enterTimer.restart();
  }

  /**
   * This method displays the ToolTip. It can figure out the method needed to
   * show it as well (whether to display it in heavyweight/lightweight panel
   * or a window.)  This is package-private to avoid an accessor method.
   */
  void showTip()
  {
    if (! enabled || currentComponent == null)
      return;

    if (currentTip == null
        || currentTip.getComponent() != currentComponent
        && currentComponent instanceof JComponent)
      currentTip = ((JComponent) currentComponent).createToolTip();
    Point p = currentPoint;
    Dimension dims = currentTip.getPreferredSize();
    if (canToolTipFit(currentTip))
      {
	JLayeredPane pane = ((JRootPane) SwingUtilities.getAncestorOfClass(JRootPane.class,
	                                                                   currentComponent))
	                    .getLayeredPane();

	// This should never happen, but just in case.
	if (pane == null)
	  return;

	if (containerPanel != null)
	  hideTip();
	if (isLightWeightPopupEnabled())
	  {
	    containerPanel = new Panel();
	    JRootPane root = new JRootPane();
	    root.getContentPane().add(currentTip);
	    containerPanel.add(root);
	  }
	else
	  {
	    containerPanel = new JPanel();
	    containerPanel.add(currentTip);
	  }
	LayoutManager lm = containerPanel.getLayout();
	if (lm instanceof FlowLayout)
	  {
	    FlowLayout fm = (FlowLayout) lm;
	    fm.setVgap(0);
	    fm.setHgap(0);
	  }

	p = getGoodPoint(p, pane, currentTip, dims);

	pane.add(containerPanel);
	containerPanel.setBounds(p.x, p.y, dims.width, dims.height);
	currentTip.setBounds(0, 0, dims.width, dims.height);

	pane.revalidate();
	pane.repaint();
      }
    else
      {
	SwingUtilities.convertPointToScreen(p, currentComponent);
	tooltipWindow = new JWindow();
	tooltipWindow.getContentPane().add(currentTip);
	tooltipWindow.setFocusable(false);
	tooltipWindow.pack();
	tooltipWindow.setBounds(p.x, p.y, dims.width, dims.height);
	tooltipWindow.show();
      }
    currentTip.setVisible(true);
  }

  /**
   * This method hides the ToolTip.
   * This is package-private to avoid an accessor method.
   */
  void hideTip()
  {
    if (currentTip == null || ! currentTip.isVisible() || ! enabled)
      return;
    currentTip.setVisible(false);
    if (containerPanel != null)
      {
	Container parent = containerPanel.getParent();
	if (parent == null)
	  return;
	parent.remove(containerPanel);
	parent.invalidate();
	parent.validate();
	parent.repaint();

	parent = currentTip.getParent();
	if (parent == null)
	  return;
	parent.remove(currentTip);

	containerPanel = null;
      }
    if (tooltipWindow != null)
      {
	tooltipWindow.hide();
	tooltipWindow.dispose();
	tooltipWindow = null;
      }
  }

  /**
   * This method returns a point in the LayeredPane where the ToolTip can be
   * shown. The point returned (if the ToolTip is to be displayed at the
   * preferred dimensions) will always place the ToolTip inside the
   * currentComponent if possible.
   *
   * @param p The last known good point for the mouse.
   * @param c The JLayeredPane in the first RootPaneContainer up from the
   *        currentComponent.
   * @param tip The ToolTip to display.
   * @param dims The ToolTip preferred dimensions (can be null).
   *
   * @return A good point to place the ToolTip.
   */
  private Point getGoodPoint(Point p, JLayeredPane c, JToolTip tip,
                             Dimension dims)
  {
    if (dims == null)
      dims = tip.getPreferredSize();
    Rectangle bounds = currentComponent.getBounds();
    if (p.x + dims.width > bounds.width)
      p.x = bounds.width - dims.width;
    if (p.y + dims.height > bounds.height)
      p.y = bounds.height - dims.height;

    p = SwingUtilities.convertPoint(currentComponent, p, c);
    return p;
  }

  /**
   * This method returns the deepest component in the content pane for the
   * first RootPaneContainer up from the currentComponent. This method is
   * used in conjunction with one of the mouseXXX methods.
   *
   * @param e The MouseEvent.
   *
   * @return The deepest component in the content pane.
   */
  private Component getContentPaneDeepestComponent(MouseEvent e)
  {
    Component source = (Component) e.getSource();
    Container parent = (Container) SwingUtilities.getAncestorOfClass(JRootPane.class,
                                                                     currentComponent);
    if (parent == null)
      return null;
    parent = ((JRootPane) parent).getContentPane();
    Point p = e.getPoint();
    p = SwingUtilities.convertPoint(source, p, parent);
    Component target = SwingUtilities.getDeepestComponentAt(parent, p.x, p.y);
    return target;
  }

  /**
   * This method returns whether the ToolTip can fit in the first
   * RootPaneContainer up from the currentComponent.
   *
   * @param tip The ToolTip.
   *
   * @return Whether the ToolTip can fit.
   */
  private boolean canToolTipFit(JToolTip tip)
  {
    JRootPane root = (JRootPane) SwingUtilities.getAncestorOfClass(JRootPane.class,
                                                                   currentComponent);
    if (root == null)
      return false;
    Dimension pref = tip.getPreferredSize();
    Dimension rootSize = root.getSize();
    if (rootSize.width > pref.width && rootSize.height > pref.height)
      return true;
    return false;
  }
}
