/* SizeRequirements.java --
   Copyright (C) 2002, 2005 Free Software Foundation, Inc.

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

import java.io.Serializable;

/**
 * This class calculates information about the size and position requirements
 * of components.
 *
 * Two types of layout are supported:
 * <ul>
 * <li>Tiled: the components are placed at position top-left or bottom-right
 *    position within their allocated space</li>
 * <li>Aligned: the components are placed aligned in their allocated space
 *    according to their alignment value</li>
 * </ul>
 *
 * @author Andrew Selkirk
 * @author Roman Kennke (roman@kennke.org)
 */
public class SizeRequirements implements Serializable
{
  /**
   * The serialVersionUID.
   */
  private static final long serialVersionUID = 9217749429906736553L;

  /**
   * The minimum reasonable width or height of a component.
   */
  public int minimum;

  /**
   * The preferred width or height of a component.
   */
  public int preferred;

  /**
   * The maximum reasonable width or height of a component.
   */
  public int maximum;

  /**
   * The horizontal or vertical alignment of a component.
   */
  public float alignment;

  /**
   * Creates a SizeRequirements object with minimum, preferred and
   * maximum size set to zero, and an alignment value of 0.5.
   */
  public SizeRequirements()
  {
    this (0, 0, 0, 0.5F);
  }

  /**
   * Creates a SizeRequirements object with the specified minimum,
   * preferred, maximum and alignment values.
   *
   * @param min the minimum reasonable size of the component
   * @param pref the preferred size of the component
   * @param max the maximum size of the component
   * @param align the alignment of the component
   */
  public SizeRequirements(int min, int pref, int max, float align)
  {
    minimum = min;
    preferred = pref;
    maximum = max;
    alignment = align;
  }

  /**
   * Returns a String representation of this SizeRequirements object,
   * containing information about the minimum, preferred, maximum and
   * alignment value.
   *
   * @return a String representation of this SizeRequirements object
   */
  public String toString()
  {
    StringBuilder b = new StringBuilder();
    b.append("<[");
    b.append(minimum);
    b.append(',');
    b.append(preferred);
    b.append(',');
    b.append(maximum);
    b.append("]@");
    b.append(alignment);
    b.append('>');
    return b.toString();
  }

  /**
   * Calculates how much space is nessecary to place a set of components
   * end-to-end. The size requirements of the components is specified
   * in <code>children</code>.
   *
   * @param children the SizeRequirements of each of the components
   *
   * @return the SizeRequirements that describe how much space is needed
   *     to place the components end-to-end
   */
  public static SizeRequirements
  getTiledSizeRequirements(SizeRequirements[] children)
  {
    long minimum = 0;
    long preferred = 0;
    long maximum = 0;
    for (int i = 0; i < children.length; i++)
      {
        minimum += children[i].minimum;
        preferred += children[i].preferred;
        maximum += children[i].maximum;
      }
    // Overflow check.
    if (minimum > Integer.MAX_VALUE)
      minimum = Integer.MAX_VALUE;
    if (preferred > Integer.MAX_VALUE)
      preferred = Integer.MAX_VALUE;
    if (maximum > Integer.MAX_VALUE)
      maximum = Integer.MAX_VALUE;
    SizeRequirements result = new SizeRequirements((int) minimum,
                                                   (int) preferred,
                                                   (int) maximum,
                                                   0.5F);
    return result;
  }

  /**
   * Calculates how much space is nessecary to place a set of components
   * aligned according to their alignment value.
   * The size requirements of the components is specified in
   * <code>children</code>.
   *
   * @param children the SizeRequirements of each of the components
   *
   * @return the SizeRequirements that describe how much space is needed
   *     to place the components aligned
   */
  public static SizeRequirements
  getAlignedSizeRequirements(SizeRequirements[] children)
  {
    float minLeft = 0;
    float minRight = 0;
    float prefLeft = 0;
    float prefRight = 0;
    float maxLeft = 0;
    float maxRight = 0;
    for (int i = 0; i < children.length; i++)
      {
        float myMinLeft = children[i].minimum * children[i].alignment;
        float myMinRight = children[i].minimum - myMinLeft;
        minLeft = Math.max(myMinLeft, minLeft);
        minRight = Math.max(myMinRight, minRight);
        float myPrefLeft = children[i].preferred * children[i].alignment;
        float myPrefRight = children[i].preferred - myPrefLeft;
        prefLeft = Math.max(myPrefLeft, prefLeft);
        prefRight = Math.max(myPrefRight, prefRight);
        float myMaxLeft = children[i].maximum * children[i].alignment;
        float myMaxRight = children[i].maximum - myMaxLeft;
        maxLeft = Math.max(myMaxLeft, maxLeft);
        maxRight = Math.max(myMaxRight, maxRight);
      }
    int minSize = (int) (minLeft + minRight);
    int prefSize = (int) (prefLeft + prefRight);
    int maxSize = (int) (maxLeft + maxRight);
    float align = prefLeft / (prefRight + prefLeft);
    if (Float.isNaN(align))
      align = 0;
    return new SizeRequirements(minSize, prefSize, maxSize, align);
  }

  /**
   * Calculate the offsets and spans of the components, when they should
   * be placed end-to-end.
   *
   * You must specify the amount of allocated space in
   * <code>allocated</code>, the total size requirements of the set of
   * components in <code>total</code> (this can be calculated using
   * {@link #getTiledSizeRequirements} and the size requirements of the
   * components in <code>children</code>.
   *
   * The calculated offset and span values for each component are then
   * stored in the arrays <code>offsets</code> and <code>spans</code>.
   *
   * The components are placed in the forward direction, beginning with
   * an offset of 0.
   *
   * @param allocated the amount of allocated space
   * @param total the total size requirements of the components
   * @param children the size requirement of each component
   * @param offsets will hold the offset values for each component
   * @param spans will hold the span values for each component
   */
  public static void calculateTiledPositions(int allocated,
                                             SizeRequirements total,
                                             SizeRequirements[] children,
                                             int[] offsets, int[] spans)
  {
    calculateTiledPositions(allocated, total, children, offsets, spans, true);
  }

  /**
   * Calculate the offsets and spans of the components, when they should
   * be placed end-to-end.
   *
   * You must specify the amount of allocated space in
   * <code>allocated</code>, the total size requirements of the set of
   * components in <code>total</code> (this can be calculated using
   * {@link #getTiledSizeRequirements} and the size requirements of the
   * components in <code>children</code>.
   *
   * The calculated offset and span values for each component are then
   * stored in the arrays <code>offsets</code> and <code>spans</code>.
   *
   * Depending on the value of <code>forward</code> the components are
   * placed in the forward direction (left-right or top-bottom), where
   * the offsets begin with 0, or in the reverse direction
   * (right-left or bottom-top).
   *
   * @param allocated the amount of allocated space
   * @param total the total size requirements of the components
   * @param children the size requirement of each component
   * @param offsets will hold the offset values for each component
   * @param spans will hold the span values for each component
   * @param forward whether the components should be placed in the forward
   *     direction (left-right or top-bottom) or reverse direction
   *     (right-left or bottom-top)
   */
  public static void calculateTiledPositions(int allocated,
                                             SizeRequirements total,
                                             SizeRequirements[] children,
                                             int[] offsets, int[] spans,
                                             boolean forward)
  {
    int span = 0;
    if (forward)
      {
        int offset = 0;
        for (int i = 0; i < children.length; i++)
          {
            offsets[i] = offset;
            spans[i] = children[i].preferred;
            span += spans[i];
            offset += children[i].preferred;
          }
      }
    else
      {
        int offset = allocated;
        for (int i = 0; i < children.length; i++)
          {
            offset -= children[i].preferred;
            offsets[i] = offset;
            span += spans[i];
            spans[i] = children[i].preferred;
          }
      }
    // Adjust spans so that we exactly fill the allocated region. If
    if (span > allocated)
      adjustSmaller(allocated, children, spans, span);
    else if (span < allocated)
      adjustGreater(allocated, children, spans, span);

    // Adjust offsets.
    if (forward)
      {
        int offset = 0;
        for (int i = 0; i < children.length; i++)
          {
            offsets[i] = offset;
            offset += spans[i];
          }
      }
    else
      {
        int offset = allocated;
        for (int i = 0; i < children.length; i++)
          {
            offset -= spans[i];
            offsets[i] = offset;
          }
      }
  }

  private static void adjustSmaller(int allocated, SizeRequirements[] children,
                                    int[] spans, int span)
  {
    // Sum up (prefSize - minSize) over all children
    int sumDelta = 0;
    for (int i = 0; i < children.length; i++)
      sumDelta += children[i].preferred - children[i].minimum;

    // If we have sumDelta == 0, then all components have prefSize == maxSize
    // and we can't do anything about it.
    if (sumDelta == 0)
      return;

    // Adjust all sizes according to their preferred and minimum sizes.
    for (int i = 0; i < children.length; i++)
      {
        double factor = ((double) (children[i].preferred - children[i].minimum))
                        / ((double) sumDelta);
        // In case we have a sumDelta of 0, the factor should also be 0.
        if (Double.isNaN(factor))
          factor = 0;
        spans[i] -= factor * (span - allocated);
      }
  }

  private static void adjustGreater(int allocated, SizeRequirements[] children,
                                    int[] spans, int span)
  {
    // Sum up (maxSize - prefSize) over all children
    long sumDelta = 0;
    for (int i = 0; i < children.length; i++)
      {
        sumDelta += children[i].maximum - children[i].preferred;
      }

    // If we have sumDelta == 0, then all components have prefSize == maxSize
    // and we can't do anything about it.
    if (sumDelta == 0)
      return;

    // Adjust all sizes according to their preferred and minimum sizes.
    for (int i = 0; i < children.length; i++)
      {
        double factor = ((double) (children[i].maximum - children[i].preferred))
                        / ((double) sumDelta);
        spans[i] += factor * (allocated - span);
      }
  }

  /**
   * Calculate the offsets and spans of the components, when they should
   * be placed end-to-end.
   *
   * You must specify the amount of allocated space in
   * <code>allocated</code>, the total size requirements of the set of
   * components in <code>total</code> (this can be calculated using
   * {@link #getTiledSizeRequirements} and the size requirements of the
   * components in <code>children</code>.
   *
   * The calculated offset and span values for each component are then
   * stored in the arrays <code>offsets</code> and <code>spans</code>.
   *
   * The components are tiled in the forward direction, beginning with
   * an offset of 0.
   *
   * @param allocated the amount of allocated space
   * @param total the total size requirements of the components
   * @param children the size requirement of each component
   * @param offsets will hold the offset values for each component
   * @param spans will hold the span values for each component
   */
  public static void calculateAlignedPositions(int allocated,
                                               SizeRequirements total,
                                               SizeRequirements[] children,
                                               int[] offsets, int[] spans)
  {
    calculateAlignedPositions(allocated, total, children, offsets, spans,
                              true);
  }

  /**
   * Calculate the offsets and spans of the components, when they should
   * be placed end-to-end.
   *
   * You must specify the amount of allocated space in
   * <code>allocated</code>, the total size requirements of the set of
   * components in <code>total</code> (this can be calculated using
   * {@link #getTiledSizeRequirements} and the size requirements of the
   * components in <code>children</code>.
   *
   * The calculated offset and span values for each component are then
   * stored in the arrays <code>offsets</code> and <code>spans</code>.
   *
   * Depending on the value of <code>forward</code> the components are
   * placed in the forward direction (left-right or top-bottom), where
   * the offsets begin with 0, or in the reverse direction
   * (right-left or bottom-top).
   *
   * @param allocated the amount of allocated space
   * @param total the total size requirements of the components
   * @param children the size requirement of each component
   * @param spans will hold the span values for each component
   * @param forward whether the components should be placed in the forward
   *     direction (left-right or top-bottom) or reverse direction
   *     (right-left or bottom-top)
   */
  public static void calculateAlignedPositions(int allocated,
                                               SizeRequirements total,
                                               SizeRequirements[] children,
                                               int[] offset, int[] spans,
                                               boolean forward)
  {
    // First we compute the position of the baseline.
    float baseline = allocated * total.alignment;

    // Now we can layout the components along the baseline.
    for (int i = 0; i < children.length; i++)
      {
        float align = children[i].alignment;
        // Try to fit the component into the available space.
        int[] spanAndOffset = new int[2];
        if (align < .5F || baseline == 0)
          adjustFromRight(children[i], baseline, allocated, spanAndOffset);
        else
          adjustFromLeft(children[i], baseline, allocated, spanAndOffset);
        spans[i] = spanAndOffset[0];
        offset[i] = spanAndOffset[1];
      }
  }

  /**
   * Adjusts the span and offset of a component for the aligned layout.
   *
   * @param reqs
   * @param baseline
   * @param allocated
   * @param spanAndOffset
   */
  private static void adjustFromRight(SizeRequirements reqs, float baseline,
                                      int allocated, int[] spanAndOffset)
  {
    float right = allocated - baseline;
    // If the resulting span exceeds the maximum of the component, then adjust
    // accordingly.
    float maxRight = ((float) reqs.maximum) * (1.F - reqs.alignment);
    if (right / (1.F - reqs.alignment) > reqs.maximum)
      right = maxRight;
    // If we have not enough space on the left side, then adjust accordingly.
    if (right / (1.F - reqs.alignment) * reqs.alignment > allocated - baseline)
      right = ((float) (allocated - baseline))
             / reqs.alignment * (1.F - reqs.alignment);

    spanAndOffset[0] = (int) (right / (1.F - reqs.alignment));
    spanAndOffset[1] = (int) (baseline - spanAndOffset[0] * reqs.alignment);
  }

  /**
   * Adjusts the span and offset of a component for the aligned layout.
   *
   * @param reqs
   * @param baseline
   * @param allocated
   * @param spanAndOffset
   */
  private static void adjustFromLeft(SizeRequirements reqs, float baseline,
                                     int allocated, int[] spanAndOffset)
  {
    float left = baseline;
    // If the resulting span exceeds the maximum of the component, then adjust
    // accordingly.
    float maxLeft = ((float) reqs.maximum) * reqs.alignment;
    if (left / reqs.alignment > reqs.maximum)
      left = maxLeft;
    // If we have not enough space on the right side, then adjust accordingly.
    if (left / reqs.alignment * (1.F - reqs.alignment) > allocated - baseline)
      left = ((float) (allocated - baseline))
             / (1.F - reqs.alignment) * reqs.alignment;

    spanAndOffset[0] = (int) (left / reqs.alignment);
    spanAndOffset[1] = (int) (baseline - spanAndOffset[0] * reqs.alignment);
  }

  /**
   * Returns an array of new preferred sizes for the children based on
   * <code>delta</code>. <code>delta</code> specifies a change in the
   * allocated space. The sizes of the children will be shortened or
   * lengthened to accomodate the new allocation.
   *
   * @param delta the change of the size of the total allocation for
   *     the components
   * @param children the size requirements of each component
   *
   * @return the new preferred sizes for each component
   */
  public static int[] adjustSizes(int delta, SizeRequirements[] children)
  {
    return null; // TODO
  }
}
