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
    return null; // TODO
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
    SizeRequirements result = new SizeRequirements();
    for (int i = 0; i < children.length; i++)
      {
        result.minimum += children[i].minimum;
        result.preferred += children[i].preferred;
        result.maximum += children[i].maximum;
      }
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
    return null; // TODO
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
    if (forward)
      {
        int offset = 0;
        for (int i = 0; i < children.length; i++)
          {
            offsets[i] = offset;
            spans[i] = children[i].preferred;
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
            spans[i] = children[i].preferred;
          }
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
  public static void calculateAlignedPositions(int allocated,
                                               SizeRequirements total,
                                               SizeRequirements[] children,
                                               int[] offset, int[] spans,
                                               boolean forward)
  {
    // TODO
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
