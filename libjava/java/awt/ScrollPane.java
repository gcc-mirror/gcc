/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

import java.awt.event.AdjustmentListener;
import java.awt.peer.ScrollPanePeer;

/** A ScrollPane is a component that has vertical and horizontal
 * scrollbars as well as a single child which is scrolled by them.
 * @author Tom Tromey <tromey@redhat.com>
 * @date December 31, 2000
 */
public class ScrollPane extends Container
{
  /** This indicates that scrollbars should only be displayed when
   * needed.  */
  public static final int SCROLLBARS_AS_NEEDED = 0;
  /** This indicates that scrollbars should always be displayed.  */
  public static final int SCROLLBARS_ALWAYS = 1;
  /** This indicates that scrollbars should never be displayed.  */
  public static final int SCROLLBARS_NEVER = 2;

  /** Create a new ScrollPane object using the indicated scrollbar
   * display policy.  If the policy is not specified it defaults to
   * SCROLLBARS_AS_NEEDED.  The default size of this component is
   * 100x100.
   * @param policy The scrollbar display policy
   */
  public ScrollPane ()
  {
    this (SCROLLBARS_AS_NEEDED);
  }

  public ScrollPane (int policy)
  {
    if (policy != SCROLLBARS_AS_NEEDED
	&& policy != SCROLLBARS_ALWAYS
	&& policy != SCROLLBARS_NEVER)
      throw new IllegalArgumentException ("invalid value for policy");

    this.policy = policy;
    setSize (100, 100);
  }

  /** Add a component to this ScrollPane.
   * @param comp The component to add
   * @param constraints Constraints.  This is ignored.
   * @param pos Position.  This must be <= 0, but is otherwise ignored.
   */
  protected final void addImpl (Component comp, Object constraints,
				int pos)
  {
    if (pos > 0)
      throw new IllegalArgumentException ("pos must be <= 0");

    if (ncomponents > 0)
      remove (component[0]);

    if (comp.isLightweight ())
      {
	Panel p = new Panel ();
	p.add (comp);
	comp = p;
      }

    super.addImpl (comp, constraints, pos);
  }

  /** This creates the component's peer.  */
  public void addNotify ()
  {
    if (peer == null)
      peer = getToolkit ().createScrollPane (this);
    super.addNotify ();
  }

  /** Lays out the components in this container.  */
  public void doLayout ()
  {
    ScrollPanePeer spp = (ScrollPanePeer) peer;
    Dimension c = component[0].getPreferredSize ();
    component[0].setSize (c.width, c.height);
    spp.childResized (c.width, c.height);
    // Update the scrollbar position to the closest valid value.
    setScrollPosition (hscroll.getValue (), vscroll.getValue ());
  }

  /** Returns an Adjustable representing the horizontal scrollbar.
   * The methods setMaximum, setMinimum, and setVisibleAmount should
   * not be called on this Adjustable.  They will throw AWTError if
   * called.
   */
  public Adjustable getHAdjustable ()
  {
    return hscroll;
  }

  /** Returns the height of the horizontal scrollbar.  */
  public int getHScrollbarHeight ()
  {
    if (peer == null)
      return 0;
    ScrollPanePeer spp = (ScrollPanePeer) peer;
    return spp.getHScrollbarHeight ();
  }

  /** Returns the scrollbar display policy.  */
  public int getScrollbarDisplayPolicy ()
  {
    return policy;
  }

  /** Returns the viewport's scroll position.  */
  public Point getScrollPosition ()
  {
    return new Point (hscroll.getValue (), vscroll.getValue ());
  }

  /** Returns an Adjustable representing the vertical scrollbar.
   * The methods setMaximum, setMinimum, and setVisibleAmount should
   * not be called on this Adjustable.  They will throw AWTError if
   * called.
   */
  public Adjustable getVAdjustable ()
  {
    return vscroll;
  }

  /** Returns the size of the viewport.  */
  public Dimension getViewportSize ()
  {
    // Note: according to the online docs, the Insets are
    // automatically updated by the peer to include the scrollbar
    // sizes.
    Insets ins = getInsets ();
    int myw = width - ins.left - ins.right;
    int myh = height - ins.top - ins.bottom;

    Dimension cs;
    if (ncomponents > 0)
      cs = component[0].getPreferredSize ();
    else
      cs = new Dimension (myw, myh);

    // A little optimization -- reuse the Dimension.
    cs.setSize (myw, myh);
    return cs;
  }

  /** Returns the width of the vertical scrollbar.  */
  public int getVScrollbarWidth ()
  {
    if (peer == null)
      return 0;
    ScrollPanePeer spp = (ScrollPanePeer) peer;
    return spp.getVScrollbarWidth ();
  }

  /** Generates a String representation of this ScrollPane's state.  */
  public String paramString ()
  {
    return ("[" + getClass ().getName ()
	    + ": " + ((ncomponents > 0) ? component[0].paramString () : "")
	    + "]");
  }

  /** Set the layout manager for this component.  ScrollPane has its
   * own layout manager and overrides this method so that the layout
   * manager cannot be changed.
   * @param m The new layout manager (ignored)
   */
  public final void setLayout (LayoutManager m)
  {
    // Nothing.
  }

  /** Sets the scroll position for this ScrollPane.  If the point if
   * out of range it is silently moved within range.
   * @param x The x coordinate
   * @param y The y coordinate
   */
  public void setScrollPosition (int x, int y)
  {
    // According to the JCL we throw a NullPointerException if there
    // is no child.
    if (ncomponents == 0)
      throw new NullPointerException ("no child in ScrollPane");

    Dimension child_d = component[0].getPreferredSize ();
    Dimension our_d = getViewportSize ();

    int xmax = Math.max (0, child_d.width - our_d.width);
    int ymax = Math.max (0, child_d.height - our_d.height);

    if (x < 0)
      x = 0;
    else if (x > xmax)
      x = xmax;
    if (y < 0)
      y = 0;
    else if (y > ymax)
      y = ymax;

    ScrollPanePeer spp = (ScrollPanePeer) peer;
    spp.setScrollPosition (x, y);
  }

  /** Sets the scroll position for this ScrollPane.  If the point if
   * out of range it is silently moved within range.
   * @param p The new point
   */
  public void setScrollPosition (Point p)
  {
    setScrollPosition (p.x, p.y);
  }

  // This implements the Adjustable for each scrollbar.  The
  // expectation is that the peer will look at these objects directly
  // and modify the values in them when the user manipulates the
  // scrollbars.  This has to be done from CNI to bypass Java
  // protection rules.  The peer should also take care of calling the
  // adjustment listeners.
  class ScrollPaneAdjustable implements Adjustable
  {
    AdjustmentListener listeners;
    int orient;
    int unit;
    int block;
    int value;

    public ScrollPaneAdjustable (int orient)
    {
      this.orient = orient;
    }

    public void addAdjustmentListener (AdjustmentListener l)
    {
      listeners = AWTEventMulticaster.add (listeners, l);
    }

    public int getBlockIncrement ()
    {
      return block;
    }

    public int getMaximum ()
    {
      Dimension child_d = component[0].getPreferredSize ();
      Dimension our_d = getViewportSize ();

      int xmax = Math.max (0, child_d.width - our_d.width);
      int ymax = Math.max (0, child_d.height - our_d.height);

      return (orient == Adjustable.HORIZONTAL) ? xmax : ymax;
    }

    public int getMinimum ()
    {
      return 0;
    }

    public int getOrientation ()
    {
      return orient;
    }

    public int getUnitIncrement ()
    {
      return unit;
    }

    public int getValue ()
    {
      return value;
    }

    public int getVisibleAmount ()
    {
      Dimension d = getViewportSize ();
      return (orient == Adjustable.HORIZONTAL) ? d.width : d.height;
    }

    public void removeAdjustmentListener (AdjustmentListener l)
    {
      listeners = AWTEventMulticaster.remove (listeners, l);
    }

    public void setBlockIncrement (int b)
    {
      throw new AWTError ("can't use setBlockIncrement on this Adjustable");
    }

    public void setMaximum (int max)
    {
      throw new AWTError ("can't use setMaximum on this Adjustable");
    }

    public void setMinimum (int min)
    {
      throw new AWTError ("can't use setMinimum on this Adjustable");
    }

    public void setUnitIncrement (int u)
    {
      unit = u;
      if (peer != null)
	{
	  ScrollPanePeer spp = (ScrollPanePeer) peer;
	  spp.setUnitIncrement (this, u);
	}
    }

    public void setValue (int v)
    {
      value = v;
      if (peer != null)
	{
	  ScrollPanePeer spp = (ScrollPanePeer) peer;
	  spp.setValue (this, v);
	}
    }

    public void setVisibleAmount (int v)
    {
      throw new AWTError ("can't use setVisibleAmount on this Adjustable");
    }
  }

  ScrollPaneAdjustable hscroll
    = new ScrollPaneAdjustable (Adjustable.HORIZONTAL);
  ScrollPaneAdjustable vscroll
    = new ScrollPaneAdjustable (Adjustable.VERTICAL);
  int policy;
}
