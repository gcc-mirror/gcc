/* BasicProgressBarUI.java --
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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


package javax.swing.plaf.basic;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.geom.AffineTransform;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JComponent;
import javax.swing.JProgressBar;
import javax.swing.LookAndFeel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.UIManager;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.ProgressBarUI;

/**
 * The Basic Look and Feel UI delegate for the 
 * JProgressBar.
 */
public class BasicProgressBarUI extends ProgressBarUI
{
  /**
   * A helper class that listens for ChangeEvents 
   * from the progressBar's model.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class ChangeHandler implements ChangeListener
  {
    /**
     * Called every time the state of the model changes.
     *
     * @param e The ChangeEvent given by the model.
     */
    public void stateChanged(ChangeEvent e)
    {
      // Nothing to do but repaint.
      progressBar.repaint();
    }
  }

  /**
   * This helper class is used to listen for 
   * PropertyChangeEvents from the progressBar.
   */
  private class PropertyChangeHandler implements PropertyChangeListener
  {
    /**
     * Called every time the properties of the 
     * progressBar change.
     *
     * @param e The PropertyChangeEvent given by the progressBar.
     */
    public void propertyChange(PropertyChangeEvent e)
    {
      // Only need to listen for indeterminate changes.
      // All other things are done on a repaint.
      if (e.getPropertyName().equals("indeterminate"))
        if (((Boolean) e.getNewValue()).booleanValue()
            && progressBar.isShowing())
          startAnimationTimer();
        else
          stopAnimationTimer();
    }
  }

  /**
   * Receives notification when the progressbar is becoming visible or
   * invisible and starts/stops the animation timer accordingly.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  private class AncestorHandler implements AncestorListener
  {

    /**
     * Receives notification when the progressbar is becoming visible. This
     * starts the animation timer if the progressbar is indeterminate.
     *
     * @param event the ancestor event
     */
    public void ancestorAdded(AncestorEvent event)
    {
      if (progressBar.isIndeterminate())
        startAnimationTimer();
    }

    /**
     * Receives notification when the progressbar is becoming invisible. This
     * stops the animation timer if the progressbar is indeterminate.
     *
     * @param event the ancestor event
     */
    public void ancestorRemoved(AncestorEvent event)
    {
      stopAnimationTimer();
    }

    /**
     * Receives notification when an ancestor has been moved. We don't need to
     * do anything here.
     */
    public void ancestorMoved(AncestorEvent event)
    {
      // Nothing to do here.
    }
    
  }

  /**
   * This helper class is used to listen for 
   * the animationTimer's intervals. On every interval,
   * the bouncing box should move.
   */
  private class Animator implements ActionListener
  {
    /**
     * Called every time the animationTimer reaches
     * its interval.
     *
     * @param e The ActionEvent given by the timer.
     */
    public void actionPerformed(ActionEvent e)
    {
      // Incrementing the animation index will cause
      // a repaint.
      incrementAnimationIndex();
    }
  }

  /**
   * Receives notification when the size of the progress bar changes and
   * invalidates the layout information for the box calculation in
   * {@link BasicProgressBarUI#getBox(Rectangle)}.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  private class ComponentHandler extends ComponentAdapter
  {
    /**
     * Receives notification when the size of the progress bar changes and
     * invalidates the layout information for the box calculation in
     * {@link BasicProgressBarUI#getBox}.
     *
     * @param e the component event
     */
    public void componentResized(ComponentEvent e)
    {
      boxDependent = -1;
      boxIndependent = -1;
      incr = -1;
    }
  }

  /**
   * Holds the value of the bouncing box that is returned by {@link #getBox}.
   *
   * @since 1.5
   */ 
  protected Rectangle boxRect;

  /** The timer used to move the bouncing box. */
  private transient Timer animationTimer;

  // The total number of frames must be an even number.
  // The total number of frames is calculated from
  // the cycleTime and repaintInterval given by
  // the basic Look and Feel defaults.
  //
  // +-----------------------------------------------+
  // | frame0 | frame1 | frame2 | frame 3 | frame 4  |
  // |        | frame7 | frame6 | frame 5 |          |
  // +-----------------------------------------------+
  
  /** The current animation index. */
  private transient int animationIndex;

  /** The total number of frames.*/
  private transient int numFrames;

  /** The helper that moves the bouncing box. */
  private transient Animator animation;

  /** The helper that listens for property change events. */
  private transient PropertyChangeHandler propertyListener;

  /** The Listener for the model. */
  protected ChangeListener changeListener;

  /** The progressBar for this UI. */
  protected JProgressBar progressBar;


  /**
   * The size of the box returned by {@link #getBox} in the orientation
   * direction of the progress bar. This is package private to avoid accessor
   * method.
   */
  transient double boxDependent = - 1;

  /**
   * The size of the box returned by {@link #getBox} against the orientation
   * direction of the progress bar. This is package private to avoid accessor
   * method. 
   */
  transient int boxIndependent = - 1;

  /**
   * The increment for box animation. This is package private to avoid accessor
   * method.
   */
  transient double incr = -1;

  /** The length of the cell. The cell is the painted part. */
  private transient int cellLength;

  /** The gap between cells. */
  private transient int cellSpacing;

  /** The color of the text when the bar is not over it.*/
  private transient Color selectionBackground;

  /** The color of the text when the bar is over it. */
  private transient Color selectionForeground;

  /**
   * Listens for notification when the component becomes showing and
   * starts/stops the animation timer.
   */
  private AncestorListener ancestorListener;

  /**
   * Listens for resize events on the progress bar and invalidates some
   * layout info.
   */
  private ComponentListener componentListener;

  /**
   * Creates a new BasicProgressBarUI object.
   */
  public BasicProgressBarUI()
  {
    super();
  }

  /**
   * Creates a new BasicProgressBarUI for the component.
   *
   * @param x The JComponent to create the UI for.
   *
   * @return A new BasicProgressBarUI.
   */
  public static ComponentUI createUI(JComponent x)
  {
    return new BasicProgressBarUI();
  }

  /**
   * This method returns the length of the bar (from the minimum)
   * in pixels (or units that the Graphics object draws in) based
   * on the progressBar's getPercentComplete() value.
   *
   * @param b The insets of the progressBar.
   * @param width The width of the progressBar.
   * @param height The height of the progressBar.
   *
   * @return The length of the bar that should be painted in pixels.
   */
  protected int getAmountFull(Insets b, int width, int height)
  {
    double percentDone = progressBar.getPercentComplete();
    if (progressBar.getOrientation() == JProgressBar.HORIZONTAL)
      return (int) (percentDone * (width - b.left - b.right));
    else
      return (int) (percentDone * (height - b.top - b.bottom));
  }

  /**
   * The current animation index.
   *
   * @return The current animation index.
   */
  protected int getAnimationIndex()
  {
    return animationIndex;
  }

  /**
   * This method returns the size and position of the bouncing box
   * for the current animation index. It stores the values in the 
   * given rectangle and returns it. It returns null if no box should
   * be drawn.
   *
   * @param r The bouncing box rectangle.
   *
   * @return The bouncing box rectangle.
   */
  protected Rectangle getBox(Rectangle r)
  {
    if (!progressBar.isIndeterminate())
      return null;
    if (r == null)
      r = new Rectangle();

    Rectangle vr = new Rectangle();
    SwingUtilities.calculateInnerArea(progressBar, vr);

    // Recalculate the metrics only when size of the progressbar has changed.
    if (incr == -1 || boxDependent == -1 || boxIndependent == -1)
      {
        //numFrames has to be an even number as defined by spec.
        int iterations = numFrames / 2;
        if (progressBar.getOrientation() == JProgressBar.HORIZONTAL)
          {
            boxDependent = vr.width / 6.;
            incr = ((double) (vr.width - boxDependent)) / (double) iterations;
            boxIndependent = vr.height;
          }
        else
          {
            boxDependent = vr.height / 6.;
            incr = ((double) (vr.height - boxDependent)) / (double) iterations;
            boxIndependent = vr.width;
          }
      }

    int index = getAnimationIndex();
    if (animationIndex > numFrames / 2)
      index = numFrames - getAnimationIndex();

    if (progressBar.getOrientation() == JProgressBar.HORIZONTAL)
      {
        r.x = vr.x + (int) (incr * index);
        r.y = vr.y;
        r.width = (int) boxDependent;
        r.height = (int) boxIndependent;
      }
    else
      {
        r.x = vr.x;
        r.y = vr.height - (int) (incr * index) + vr.y - (int) boxDependent;
        r.width = (int) boxIndependent;
        r.height = (int) boxDependent;
      }
    return r;
  }

  /**
   * This method returns the length of the cells.
   *
   * @return The cell length.
   */
  protected int getCellLength()
  {
    return cellLength;
  }

  /**
   * This method returns the spacing between cells.
   *
   * @return The cell gap.
   */
  protected int getCellSpacing()
  {
    return cellSpacing;
  }

  /**
   * This method returns the maximum size of the JComponent.
   * If it returns null, it is up to the LayoutManager
   * to give it a size.
   *
   * @param c The component to find a maximum size for.
   *
   * @return The maximum size.
   */
  public Dimension getMaximumSize(JComponent c)
  {
    Insets insets = c.getInsets();
    Dimension ret;
    int orientation = progressBar.getOrientation();
    if (orientation == JProgressBar.VERTICAL)
      {
        ret = getPreferredInnerVertical();
        ret.height = Short.MAX_VALUE;
        ret.width += insets.left + insets.right;
      }
    else
      {
        ret = getPreferredInnerHorizontal();
        ret.width = Short.MAX_VALUE;
        ret.height += insets.top + insets.bottom;
      }
    return ret;
  }

  /**
   * This method returns the minimum size of the JComponent.
   * If it returns null, it is up to the LayoutManager to
   * give it a size.
   *
   * @param c The component to find a minimum size for.
   *
   * @return The minimum size.
   */
  public Dimension getMinimumSize(JComponent c)
  {
    Insets insets = c.getInsets();
    Dimension ret;
    int orientation = progressBar.getOrientation();
    if (orientation == JProgressBar.VERTICAL)
      {
        ret = getPreferredInnerVertical();
        ret.height = 10;
        ret.width += insets.left + insets.right;
      }
    else
      {
        ret = getPreferredInnerHorizontal();
        ret.width = 10;
        ret.height += insets.top + insets.bottom;
      }
    return ret;
  }

  /**
   * This method returns the preferred size of the inner
   * rectangle (the bounds without the insets) if the
   * progressBar is horizontal.
   *
   * @return The preferred size of the progressBar minus 
   *         insets if it's horizontal.
   */
  protected Dimension getPreferredInnerHorizontal()
  {
    Font font = progressBar.getFont();
    FontMetrics fm = progressBar.getFontMetrics(font);

    int stringWidth = 0;
    String str = progressBar.getString();
    if (str != null)
      stringWidth = fm.stringWidth(progressBar.getString());
    Insets i = progressBar.getInsets();
    int prefWidth = Math.max(200 - i.left - i.right, stringWidth);

    int stringHeight = 0;
    if (str != null)
      stringHeight = fm.getHeight();
    int prefHeight = Math.max(16 - i.top - i.bottom, stringHeight);

    return new Dimension(prefWidth, prefHeight);
  }

  /**
   * This method returns the preferred size of the inner
   * rectangle (the bounds without insets) if the 
   * progressBar is vertical.
   *
   * @return The preferred size of the progressBar minus
   *         insets if it's vertical.
   */
  protected Dimension getPreferredInnerVertical()
  {
    Font font = progressBar.getFont();
    FontMetrics fm = progressBar.getFontMetrics(font);

    int stringWidth = 0;
    String str = progressBar.getString();
    if (str != null)
      stringWidth = fm.stringWidth(progressBar.getString());
    Insets i = progressBar.getInsets();
    int prefHeight = Math.max(200 - i.left - i.right, stringWidth);

    int stringHeight = 0;
    if (str != null)
      stringHeight = fm.getHeight();
    int prefWidth = Math.max(16 - i.top - i.bottom, stringHeight);

    return new Dimension(prefWidth, prefHeight);
  }

  /**
   * This method returns the preferred size of the 
   * given JComponent. If it returns null, then it
   * is up to the LayoutManager to give it a size.
   *
   * @param c The component to find the preferred size for.
   *
   * @return The preferred size of the component.
   */
  public Dimension getPreferredSize(JComponent c)
  {
    Insets insets = c.getInsets();
    Dimension ret;
    int orientation = progressBar.getOrientation();
    if (orientation == JProgressBar.VERTICAL)
      ret = getPreferredInnerVertical();
    else
      ret = getPreferredInnerHorizontal();
    ret.width += insets.left + insets.right;
    ret.height += insets.top + insets.bottom;
    return ret;
  }

  /**
   * This method returns the Color that the text is shown in when the bar is
   * not over the text.
   *
   * @return The color of the text when the bar is not over it.
   */
  protected Color getSelectionBackground()
  {
    return selectionBackground;
  }

  /**
   * This method returns the Color that the text is shown in  when the bar is
   * over the text.
   *
   * @return The color of the text when the bar is over it.
   */
  protected Color getSelectionForeground()
  {
    return selectionForeground;
  }

  /**
   * This method returns the point (the top left of the bounding box)
   * where the text should be painted. 
   *
   * @param g The Graphics object to measure FontMetrics with.
   * @param progressString The string to paint.
   * @param x The x coordinate of the overall bounds box.
   * @param y The y coordinate of the overall bounds box.
   * @param width The width of the overall bounds box.
   * @param height The height of the overall bounds box.
   *
   * @return The top left of the bounding box where text should be painted.
   */
  protected Point getStringPlacement(Graphics g, String progressString, int x,
                                     int y, int width, int height)
  {
    Rectangle tr = new Rectangle();
    Rectangle vr = new Rectangle();
    Rectangle ir = new Rectangle();
    
    if (progressBar.getOrientation() == JProgressBar.HORIZONTAL)
      vr.setBounds(x, y, width, height);
    else
      vr.setBounds(y, x, height, width);

    Font f = g.getFont();
    FontMetrics fm = g.getFontMetrics(f);

    SwingUtilities.layoutCompoundLabel(progressBar, fm, progressString, null,
                                       SwingConstants.CENTER,
                                       SwingConstants.CENTER,
                                       SwingConstants.CENTER,
                                       SwingConstants.CENTER, vr, ir, tr, 0);
    
    if (progressBar.getOrientation() == JProgressBar.HORIZONTAL)
      return new Point(tr.x, tr.y);
    else
      return new Point(tr.y, tr.x);
  }

  /**
   * This method increments the animation index.
   */
  protected void incrementAnimationIndex()
  {
    animationIndex++;
    //numFrames is like string length, it should be named numFrames or something
    if (animationIndex >= numFrames)
      animationIndex = 0;
    progressBar.repaint();
  }

  /**
   * This method paints the progressBar. It delegates its responsibilities
   * to paintDeterminate and paintIndeterminate.
   *
   * @param g The Graphics object to paint with.
   * @param c The JComponent to paint.
   */
  public void paint(Graphics g, JComponent c)
  {
    if (! progressBar.isIndeterminate())
      paintDeterminate(g, c);
    else
      paintIndeterminate(g, c);
  }

  /**
   * This method is called if the painting to be done is 
   * for a determinate progressBar.
   *
   * @param g The Graphics object to paint with.
   * @param c The JComponent to paint.
   */
  protected void paintDeterminate(Graphics g, JComponent c)
  {
    Color saved = g.getColor();
    int space = getCellSpacing();
    int len = getCellLength();
    int max = progressBar.getMaximum();
    int min = progressBar.getMinimum();
    int value = progressBar.getValue();

    Rectangle vr = SwingUtilities.calculateInnerArea(c, new Rectangle());
    Rectangle or = progressBar.getBounds();
    Insets insets = c.getInsets();

    int amountFull = getAmountFull(insets, or.width, or.height);

	if (progressBar.getOrientation() == JProgressBar.HORIZONTAL)
	  {
	    g.setColor(c.getForeground());
	    g.fillRect(vr.x, vr.y, amountFull, vr.height);
	  }
	else
	  {
	    g.setColor(c.getForeground());
	    g.fillRect(vr.x, vr.y + vr.height - amountFull, vr.width, 
                       amountFull);
	  }

    if (progressBar.isStringPainted() && !progressBar.getString().equals(""))
      paintString(g, 0, 0, or.width, or.height, amountFull, insets);
    g.setColor(saved);
  }

  /**
   * This method is called if the painting to be done is for
   * an indeterminate progressBar.
   *
   * @param g The Graphics object to paint with.
   * @param c The JComponent to paint.
   */
  protected void paintIndeterminate(Graphics g, JComponent c)
  {
    //need to paint the box at it's current position. no text is painted since
    //all we're doing is bouncing back and forth
    Color saved = g.getColor();
    Insets insets = c.getInsets();

    Rectangle or = c.getBounds();
    Rectangle vr = new Rectangle();
    SwingUtilities.calculateInnerArea(c, vr);

    g.setColor(c.getBackground());
    g.fillRect(vr.x, vr.y, vr.width, vr.height);

    boxRect = getBox(boxRect);

    g.setColor(c.getForeground());
    g.fillRect(boxRect.x, boxRect.y, boxRect.width, boxRect.height);

    if (progressBar.isStringPainted() && !progressBar.getString().equals(""))
      paintString(g, 0, 0, or.width, or.height,
                  getAmountFull(insets, or.width, or.height), insets);

    g.setColor(saved);
  }

  /**
   * This method paints the string for the progressBar.
   *
   * @param g The Graphics object to paint with.
   * @param x The x coordinate of the progressBar.
   * @param y The y coordinate of the progressBar.
   * @param width The width of the progressBar.
   * @param height The height of the progressBar.
   * @param amountFull The amount of the progressBar that has its bar filled.
   * @param b The insets of the progressBar.
   */
  protected void paintString(Graphics g, int x, int y, int width, int height,
                             int amountFull, Insets b)
  {
    String str = progressBar.getString();
    int full = getAmountFull(b, width, height);
    Point placement = getStringPlacement(g, progressBar.getString(),
                                         x + b.left, y + b.top, 
                                         width - b.left - b.right,
                                         height - b.top - b.bottom);
    Color savedColor = g.getColor();
    Shape savedClip = g.getClip();
    FontMetrics fm = g.getFontMetrics(progressBar.getFont());
    
    if (progressBar.getOrientation() == JProgressBar.VERTICAL)
      {
        AffineTransform rotate = AffineTransform.getRotateInstance(Math.PI / 2);
        g.setFont(progressBar.getFont().deriveFont(rotate));
        placement.x = width - placement.x - fm.getAscent();
      }
    else
      {
        placement.y += fm.getAscent();
      }
    
    g.setColor(getSelectionForeground());
    g.setClip(0, 0, full + b.left, height);
    g.drawString(str, placement.x, placement.y);
    g.setColor(getSelectionBackground());
    g.setClip(full + b.left, 0, width - full, height);
    g.drawString(str, placement.x, placement.y);
    g.setClip(savedClip);
    g.setColor(savedColor);
  }

  /**
   * This method sets the current animation index. If the index is greater than
   * the number of frames, it resets to 0.
   * 
   * @param newValue The new animation index.
   */
  protected void setAnimationIndex(int newValue)
  {
    animationIndex = (newValue <= numFrames) ? newValue : 0;
    progressBar.repaint();
  }

  /**
   * This method sets the cell length.
   *
   * @param cellLen The cell length.
   */
  protected void setCellLength(int cellLen)
  {
    cellLength = cellLen;
  }

  /**
   * This method sets the cell spacing.
   *
   * @param cellSpace The cell spacing.
   */
  protected void setCellSpacing(int cellSpace)
  {
    cellSpacing = cellSpace;
  }

  /**
   * This method starts the animation timer. It is called
   * when the propertyChangeListener detects that the progressBar
   * has changed to indeterminate mode.
   *
   * @since 1.4
   */
  protected void startAnimationTimer()
  {
    if (animationTimer != null)
      animationTimer.start();
  }

  /**
   * This method stops the animation timer. It is called when
   * the propertyChangeListener detects that the progressBar
   * has changed to determinate mode.
   *
   * @since 1.4
   */
  protected void stopAnimationTimer()
  {
    if (animationTimer != null)
      animationTimer.stop();
    setAnimationIndex(0);
  }

  /**
   * This method changes the settings for the progressBar to
   * the defaults provided by the current Look and Feel.
   */
  protected void installDefaults()
  {
    LookAndFeel.installColorsAndFont(progressBar, "ProgressBar.background",
                                     "ProgressBar.foreground",
                                     "ProgressBar.font");
    LookAndFeel.installBorder(progressBar, "ProgressBar.border");
    progressBar.setOpaque(true);

    selectionForeground = UIManager.getColor("ProgressBar.selectionForeground");
    selectionBackground = UIManager.getColor("ProgressBar.selectionBackground");
    cellLength = UIManager.getInt("ProgressBar.cellLength");
    cellSpacing = UIManager.getInt("ProgressBar.cellSpacing");

    int repaintInterval = UIManager.getInt("ProgressBar.repaintInterval");
    int cycleTime = UIManager.getInt("ProgressBar.cycleTime");

    if (cycleTime % repaintInterval != 0
        && (cycleTime / repaintInterval) % 2 != 0)
      {
	int div = (cycleTime / repaintInterval) + 2;
	div /= 2;
	div *= 2;
	cycleTime = div * repaintInterval;
      }
    setAnimationIndex(0);
    numFrames = cycleTime / repaintInterval;
    animationTimer.setDelay(repaintInterval);
  }

  /**
   * The method uninstalls any defaults that were
   * set by the current Look and Feel.
   */
  protected void uninstallDefaults()
  {
    progressBar.setFont(null);
    progressBar.setForeground(null);
    progressBar.setBackground(null);

    selectionForeground = null;
    selectionBackground = null;
  }

  /**
   * This method registers listeners to all the 
   * components that this UI delegate needs to listen to.
   */
  protected void installListeners()
  {
    changeListener = new ChangeHandler();
    propertyListener = new PropertyChangeHandler();
    animation = new Animator();

    progressBar.addChangeListener(changeListener);
    progressBar.addPropertyChangeListener(propertyListener);
    animationTimer.addActionListener(animation);

    ancestorListener = new AncestorHandler();
    progressBar.addAncestorListener(ancestorListener);

    componentListener = new ComponentHandler();
    progressBar.addComponentListener(componentListener);
  }

  /**
   * This method unregisters listeners to all the 
   * components that were listened to.
   */
  protected void uninstallListeners()
  {
    progressBar.removeChangeListener(changeListener);
    progressBar.removePropertyChangeListener(propertyListener);
    animationTimer.removeActionListener(animation);

    changeListener = null;
    propertyListener = null;
    animation = null;

    if (ancestorListener != null)
      progressBar.removeAncestorListener(ancestorListener);
    ancestorListener = null;

    if (componentListener != null)
      progressBar.removeComponentListener(componentListener);
    componentListener = null;
  }

  /**
   * This method installs the UI for the given JComponent.
   * This includes setting up defaults and listeners as
   * well as initializing any values or objects that
   * the UI may need.
   *
   * @param c The JComponent that is having this UI installed.
   */
  public void installUI(JComponent c)
  {
    super.installUI(c);
    if (c instanceof JProgressBar)
      {
	progressBar = (JProgressBar) c;

	animationTimer = new Timer(200, null);
	animationTimer.setRepeats(true);

	installDefaults();
	installListeners();
      }
    if (progressBar.isIndeterminate())
      startAnimationTimer();
  }

  /**
   * This method removes the UI for the given JComponent.
   * This includes removing any listeners or defaults
   * that the installUI may have set up.
   *
   * @param c The JComponent that is having this UI uninstalled.
   */
  public void uninstallUI(JComponent c)
  {
    super.uninstallUI(c);
    uninstallListeners();
    uninstallDefaults();

    animationTimer = null;
    progressBar = null;
  }

}
