/* BlockView.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package javax.swing.text.html;

import gnu.javax.swing.text.html.css.Length;

import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.HashMap;

import javax.swing.SizeRequirements;
import javax.swing.event.DocumentEvent;
import javax.swing.text.AttributeSet;
import javax.swing.text.BoxView;
import javax.swing.text.Element;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;

/**
 * @author Lillian Angel <langel@redhat.com>
 */
public class BlockView extends BoxView
{

  /**
   * Stores information about child positioning according to the
   * CSS attributes position, left, right, top and bottom.
   */
  private static class PositionInfo
  {
    // TODO: Use enums when available.

    /**
     * Static positioning. This is the default and is thus rarely really
     * used.
     */
    static final int STATIC = 0;

    /**
     * Relative positioning. The box is teaked relative to its static
     * computed bounds.
     */
    static final int RELATIVE = 1;

    /**
     * Absolute positioning. The box is moved relative to the parent's box.
     */
    static final int ABSOLUTE = 2;

    /**
     * Like ABSOLUTE, with some fixation against the viewport (not yet
     * implemented).
     */
    static final int FIXED = 3;

    /**
     * The type according to the constants of this class.
     */
    int type;

    /**
     * The left constraint, null if not set.
     */
    Length left;

    /**
     * The right constraint, null if not set.
     */
    Length right;

    /**
     * The top constraint, null if not set.
     */
    Length top;

    /**
     * The bottom constraint, null if not set.
     */
    Length bottom;

    /**
     * Creates a new PositionInfo object.
     *
     * @param typ the type to set
     * @param l the left constraint
     * @param r the right constraint
     * @param t the top constraint
     * @param b the bottom constraint
     */
    PositionInfo(int typ, Length l, Length r, Length t, Length b)
    {
      type = typ;
      left = l;
      right = r;
      top = t;
      bottom = b;
    }
  }

  /**
   * The attributes for this view.
   */
  private AttributeSet attributes;

  /**
   * The box painter for this view.
   *
   * This is package private because the TableView needs access to it.
   */
  StyleSheet.BoxPainter painter;

  /**
   * The width and height as specified in the stylesheet, null if not
   * specified. The first value is the X_AXIS, the second the Y_AXIS. You
   * can index this directly by the X_AXIS and Y_AXIS constants.
   */
  private Length[] cssSpans;

  /**
   * Stores additional CSS layout information.
   */
  private HashMap positionInfo;

  /**
   * Creates a new view that represents an html box.
   * This can be used for a number of elements.
   *
   * @param elem - the element to create a view for
   * @param axis - either View.X_AXIS or View.Y_AXIS
   */
  public BlockView(Element elem, int axis)
  {
    super(elem, axis);
    cssSpans = new Length[2];
    positionInfo = new HashMap();
  }

  /**
   * Creates the parent view for this. It is called before
   * any other methods, if the parent view is working properly.
   * Implemented to forward to the superclass and call
   * setPropertiesFromAttributes to set the paragraph
   * properties.
   *
   * @param parent - the new parent, or null if the view
   * is being removed from a parent it was added to.
   */
  public void setParent(View parent)
  {
    super.setParent(parent);

    if (parent != null)
      setPropertiesFromAttributes();
  }

  /**
   * Calculates the requirements along the major axis.
   * This is implemented to call the superclass and then
   * adjust it if the CSS width or height attribute is specified
   * and applicable.
   *
   * @param axis - the axis to check the requirements for.
   * @param r - the SizeRequirements. If null, one is created.
   * @return the new SizeRequirements object.
   */
  protected SizeRequirements calculateMajorAxisRequirements(int axis,
                                                            SizeRequirements r)
  {
    if (r == null)
      r = new SizeRequirements();

    if (setCSSSpan(r, axis))
      {
        // If we have set the span from CSS, then we need to adjust
        // the margins.
        SizeRequirements parent = super.calculateMajorAxisRequirements(axis,
                                                                       null);
        int margin = axis == X_AXIS ? getLeftInset() + getRightInset()
                                    : getTopInset() + getBottomInset();
        r.minimum -= margin;
        r.preferred -= margin;
        r.maximum -= margin;
        constrainSize(axis, r, parent);
      }
    else
      r = super.calculateMajorAxisRequirements(axis, r);
    return r;
  }

  /**
   * Calculates the requirements along the minor axis.
   * This is implemented to call the superclass and then
   * adjust it if the CSS width or height attribute is specified
   * and applicable.
   *
   * @param axis - the axis to check the requirements for.
   * @param r - the SizeRequirements. If null, one is created.
   * @return the new SizeRequirements object.
   */
  protected SizeRequirements calculateMinorAxisRequirements(int axis,
                                                            SizeRequirements r)
  {
    if (r == null)
      r = new SizeRequirements();

    if (setCSSSpan(r, axis))
      {
        // If we have set the span from CSS, then we need to adjust
        // the margins.
        SizeRequirements parent = super.calculateMinorAxisRequirements(axis,
                                                                       null);
        int margin = axis == X_AXIS ? getLeftInset() + getRightInset()
                                    : getTopInset() + getBottomInset();
        r.minimum -= margin;
        r.preferred -= margin;
        r.maximum -= margin;
        constrainSize(axis, r, parent);
      }
    else
      r = super.calculateMinorAxisRequirements(axis, r);

    // Apply text alignment if appropriate.
    if (axis == X_AXIS)
      {
        Object o = getAttributes().getAttribute(CSS.Attribute.TEXT_ALIGN);
        if (o != null)
          {
            String al = o.toString().trim();
            if (al.equals("center"))
              r.alignment = 0.5f;
            else if (al.equals("right"))
              r.alignment = 1.0f;
            else
              r.alignment = 0.0f;
          }
      }
    return r;
  }

  /**
   * Sets the span on the SizeRequirements object according to the
   * according CSS span value, when it is set.
   *
   * @param r the size requirements
   * @param axis the axis
   *
   * @return <code>true</code> when the CSS span has been set,
   *         <code>false</code> otherwise
   */
  private boolean setCSSSpan(SizeRequirements r, int axis)
  {
    boolean ret = false;
    Length span = cssSpans[axis];
    // We can't set relative CSS spans here because we don't know
    // yet about the allocated span. Instead we use the view's
    // normal requirements.
    if (span != null && ! span.isPercentage())
      {
        r.minimum = (int) span.getValue();
        r.preferred = (int) span.getValue();
        r.maximum = (int) span.getValue();
        ret = true;
      }
    return ret;
  }

  /**
   * Constrains the <code>r</code> requirements according to
   * <code>min</code>.
   *
   * @param axis the axis
   * @param r the requirements to constrain
   * @param min the constraining requirements
   */
  private void constrainSize(int axis, SizeRequirements r,
                             SizeRequirements min)
  {
    if (min.minimum > r.minimum)
      {
        r.minimum = min.minimum;
        r.preferred = min.minimum;
        r.maximum = Math.max(r.maximum, min.maximum);
      }
  }

  /**
   * Lays out the box along the minor axis (the axis that is
   * perpendicular to the axis that it represents). The results
   * of the layout are placed in the given arrays which are
   * the allocations to the children along the minor axis.
   *
   * @param targetSpan - the total span given to the view, also
   * used to layout the children.
   * @param axis - the minor axis
   * @param offsets - the offsets from the origin of the view for
   * all the child views. This is a return value and is filled in by this
   * function.
   * @param spans - the span of each child view. This is a return value and is
   * filled in by this function.
   */
  protected void layoutMinorAxis(int targetSpan, int axis,
                                 int[] offsets, int[] spans)
  {
    int viewCount = getViewCount();
    for (int i = 0; i < viewCount; i++)
      {
        View view = getView(i);
        int min = (int) view.getMinimumSpan(axis);
        int max;
        // Handle CSS span value of child.
        Length length = cssSpans[axis];
        if (length != null)
          {
            min = Math.max((int) length.getValue(targetSpan), min);
            max = min;
          }
        else
          max = (int) view.getMaximumSpan(axis);

        if (max < targetSpan)
          {
            // Align child.
            float align = view.getAlignment(axis);
            offsets[i] = (int) ((targetSpan - max) * align);
            spans[i] = max;
          }
        else
          {
            offsets[i] = 0;
            spans[i] = Math.max(min, targetSpan);
          }

        // Adjust according to CSS position info.
        positionView(targetSpan, axis, i, offsets, spans);
      }
  }

  /**
   * Overridden to perform additional CSS layout (absolute/relative
   * positioning).
   */
  protected void layoutMajorAxis(int targetSpan, int axis,
                                 int[] offsets, int[] spans)
  {
    super.layoutMajorAxis(targetSpan, axis, offsets, spans);

    // Adjust according to CSS position info.
    int viewCount = getViewCount();
    for (int i = 0; i < viewCount; i++)
      {
        positionView(targetSpan, axis, i, offsets, spans);
      }
  }

  /**
   * Positions a view according to any additional CSS constraints.
   *
   * @param targetSpan the target span
   * @param axis the axis
   * @param i the index of the view
   * @param offsets the offsets get placed here
   * @param spans the spans get placed here
   */
  private void positionView(int targetSpan, int axis, int i, int[] offsets,
                            int[] spans)
  {
    View view = getView(i);
    PositionInfo pos = (PositionInfo) positionInfo.get(view);
    if (pos != null)
      {
        int p0 = -1;
        int p1 = -1;
        if (axis == X_AXIS)
          {
            Length l = pos.left;
            if (l != null)
              p0 = (int) l.getValue(targetSpan);
            l = pos.right;
            if (l != null)
              p1 = (int) l.getValue(targetSpan);
          }
        else
          {
            Length l = pos.top;
            if (l != null)
              p0 = (int) l.getValue(targetSpan);
            l = pos.bottom;
            if (l != null)
              p1 = (int) l.getValue(targetSpan);
          }
        if (pos.type == PositionInfo.ABSOLUTE
            || pos.type == PositionInfo.FIXED)
          {
            if (p0 != -1)
              {
                offsets[i] = p0;
                if (p1 != -1)
                  {
                    // Overrides computed width. (Possibly overconstrained
                    // when the width attribute was set too.)
                    spans[i] = targetSpan - p1 - offsets[i];
                  }
              }
            else if (p1 != -1)
              {
                // Preserve any computed width.
                offsets[i] = targetSpan - p1 - spans[i];
              }
          }
        else if (pos.type == PositionInfo.RELATIVE)
          {
            if (p0 != -1)
              {
                offsets[i] += p0;
                if (p1 != -1)
                  {
                    // Overrides computed width. (Possibly overconstrained
                    // when the width attribute was set too.)
                    spans[i] = spans[i] - p0 - p1 - offsets[i];
                  }
              }
            else if (p1 != -1)
              {
                // Preserve any computed width.
                offsets[i] -= p1;
              }
          }
      }
  }

  /**
   * Paints using the given graphics configuration and shape.
   * This delegates to the css box painter to paint the
   * border and background prior to the interior.
   *
   * @param g - Graphics configuration
   * @param a - the Shape to render into.
   */
  public void paint(Graphics g, Shape a)
  {
    Rectangle rect = a instanceof Rectangle ? (Rectangle) a : a.getBounds();

    // Debug output. Shows blocks in green rectangles.
    // g.setColor(Color.GREEN);
    // g.drawRect(rect.x, rect.y, rect.width, rect.height);

    painter.paint(g, rect.x, rect.y, rect.width, rect.height, this);
    super.paint(g, a);
  }

  /**
   * Fetches the attributes to use when painting.
   *
   * @return the attributes of this model.
   */
  public AttributeSet getAttributes()
  {
    if (attributes == null)
      attributes = getStyleSheet().getViewAttributes(this);
    return attributes;
  }

  /**
   * Gets the resize weight.
   *
   * @param axis - the axis to get the resize weight for.
   * @return the resize weight.
   * @throws IllegalArgumentException - for an invalid axis
   */
  public int getResizeWeight(int axis) throws IllegalArgumentException
  {
    // Can't resize the Y_AXIS
    if (axis == Y_AXIS)
      return 0;
    if (axis == X_AXIS)
      return 1;
    throw new IllegalArgumentException("Invalid Axis");
  }

  /**
   * Gets the alignment.
   *
   * @param axis - the axis to get the alignment for.
   * @return the alignment.
   */
  public float getAlignment(int axis)
  {
    if (axis == X_AXIS)
      return super.getAlignment(axis);
    if (axis == Y_AXIS)
      {
        if (getViewCount() == 0)
          return 0.0F;
        float prefHeight = getPreferredSpan(Y_AXIS);
        View first = getView(0);
        float firstRowHeight = first.getPreferredSpan(Y_AXIS);
        return prefHeight != 0 ? (firstRowHeight * first.getAlignment(Y_AXIS))
                                 / prefHeight
                               : 0;
      }
    throw new IllegalArgumentException("Invalid Axis");
  }

  /**
   * Gives notification from the document that attributes were
   * changed in a location that this view is responsible for.
   *
   * @param ev - the change information
   * @param a - the current shape of the view
   * @param f - the factory to use to rebuild if the view has children.
   */
  public void changedUpdate(DocumentEvent ev,
                            Shape a, ViewFactory f)
  {
    super.changedUpdate(ev, a, f);

    // If more elements were added, then need to set the properties for them
    int currPos = ev.getOffset();
    if (currPos <= getStartOffset()
        && (currPos + ev.getLength()) >= getEndOffset())
        setPropertiesFromAttributes();
  }

  /**
   * Determines the preferred span along the axis.
   *
   * @param axis - the view to get the preferred span for.
   * @return the span the view would like to be painted into >=0/
   * The view is usually told to paint into the span that is returned,
   * although the parent may choose to resize or break the view.
   * @throws IllegalArgumentException - for an invalid axis
   */
  public float getPreferredSpan(int axis) throws IllegalArgumentException
  {
    if (axis == X_AXIS || axis == Y_AXIS)
      return super.getPreferredSpan(axis);
    throw new IllegalArgumentException("Invalid Axis");
  }

  /**
   * Determines the minimum span along the axis.
   *
   * @param axis - the axis to get the minimum span for.
   * @return the span the view would like to be painted into >=0/
   * The view is usually told to paint into the span that is returned,
   * although the parent may choose to resize or break the view.
   * @throws IllegalArgumentException - for an invalid axis
   */
  public float getMinimumSpan(int axis) throws IllegalArgumentException
  {
    if (axis == X_AXIS || axis == Y_AXIS)
      return super.getMinimumSpan(axis);
    throw new IllegalArgumentException("Invalid Axis");
  }

  /**
   * Determines the maximum span along the axis.
   *
   * @param axis - the axis to get the maximum span for.
   * @return the span the view would like to be painted into >=0/
   * The view is usually told to paint into the span that is returned,
   * although the parent may choose to resize or break the view.
   * @throws IllegalArgumentException - for an invalid axis
   */
  public float getMaximumSpan(int axis) throws IllegalArgumentException
  {
    if (axis == X_AXIS || axis == Y_AXIS)
      return super.getMaximumSpan(axis);
    throw new IllegalArgumentException("Invalid Axis");
  }

  /**
   * Updates any cached values that come from attributes.
   */
  protected void setPropertiesFromAttributes()
  {
    // Fetch attributes.
    StyleSheet ss = getStyleSheet();
    attributes = ss.getViewAttributes(this);

    // Fetch painter.
    painter = ss.getBoxPainter(attributes);

    // Update insets.
    if (attributes != null)
      {
        setInsets((short) painter.getInset(TOP, this),
                  (short) painter.getInset(LEFT, this),
                  (short) painter.getInset(BOTTOM, this),
                  (short) painter.getInset(RIGHT, this));
      }

    // Fetch width and height.
    float emBase = ss.getEMBase(attributes);
    float exBase = ss.getEXBase(attributes);
    cssSpans[X_AXIS] = (Length) attributes.getAttribute(CSS.Attribute.WIDTH);
    if (cssSpans[X_AXIS] != null)
      cssSpans[X_AXIS].setFontBases(emBase, exBase);
    cssSpans[Y_AXIS] = (Length) attributes.getAttribute(CSS.Attribute.HEIGHT);
    if (cssSpans[Y_AXIS] != null)
      cssSpans[Y_AXIS].setFontBases(emBase, exBase);
  }

  /**
   * Gets the default style sheet.
   *
   * @return the style sheet
   */
  protected StyleSheet getStyleSheet()
  {
    HTMLDocument doc = (HTMLDocument) getDocument();
    return doc.getStyleSheet();
  }

  /**
   * Overridden to fetch additional CSS layout information.
   */
  public void replace(int offset, int length, View[] views)
  {
    // First remove unneeded stuff.
    for (int i = 0; i < length; i++)
      {
        View child = getView(i + offset);
        positionInfo.remove(child);
      }

    // Call super to actually replace the views.
    super.replace(offset, length, views);

    // Now fetch the position infos for the new views.
    for (int i = 0; i < views.length; i++)
      {
        fetchLayoutInfo(views[i]);
      }
  }

  /**
   * Fetches and stores the layout info for the specified view.
   *
   * @param view the view for which the layout info is stored
   */
  private void fetchLayoutInfo(View view)
  {
    AttributeSet atts = view.getAttributes();
    Object o = atts.getAttribute(CSS.Attribute.POSITION);
    if (o != null && o instanceof String && ! o.equals("static"))
      {
        int type;
        if (o.equals("relative"))
          type = PositionInfo.RELATIVE;
        else if (o.equals("absolute"))
          type = PositionInfo.ABSOLUTE;
        else if (o.equals("fixed"))
          type = PositionInfo.FIXED;
        else
          type = PositionInfo.STATIC;

        if (type != PositionInfo.STATIC)
          {
            StyleSheet ss = getStyleSheet();
            float emBase = ss.getEMBase(atts);
            float exBase = ss.getEXBase(atts);
            Length left = (Length) atts.getAttribute(CSS.Attribute.LEFT);
            if (left != null)
              left.setFontBases(emBase, exBase);
            Length right = (Length) atts.getAttribute(CSS.Attribute.RIGHT);
            if (right != null)
              right.setFontBases(emBase, exBase);
            Length top = (Length) atts.getAttribute(CSS.Attribute.TOP);
            if (top != null)
              top.setFontBases(emBase, exBase);
            Length bottom = (Length) atts.getAttribute(CSS.Attribute.BOTTOM);
            if (bottom != null)
              bottom.setFontBases(emBase, exBase);
            if (left != null || right != null || top != null || bottom != null)
              {
                PositionInfo pos = new PositionInfo(type, left, right, top,
                                                    bottom);
                positionInfo.put(view, pos);
              }
          }
      }
  }
}
