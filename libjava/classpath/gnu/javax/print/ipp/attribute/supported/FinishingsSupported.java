/* FinishingsSupported.java --
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.javax.print.ipp.attribute.supported;

import gnu.javax.print.ipp.IppUtilities;

import java.util.Iterator;
import java.util.Set;

import javax.print.attribute.Attribute;
import javax.print.attribute.EnumSyntax;
import javax.print.attribute.SupportedValuesAttribute;
import javax.print.attribute.standard.Finishings;


/**
 * The <code>FinishingsSupported</code> attribute provides the supported
 * values for finishings of a job.
 *
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class FinishingsSupported extends EnumSyntax
  implements SupportedValuesAttribute
{

  /** No finishing. */
  public static final FinishingsSupported NONE = new FinishingsSupported(3);

  /** Staple the document(s) */
  public static final FinishingsSupported STAPLE = new FinishingsSupported(4);

  /** Cover a document */
  public static final FinishingsSupported COVER = new FinishingsSupported(6);

  /**
   * This value indicates that a binding is to be applied to the document.
   * The type and placement of the binding is site-defined.
   */
  public static final FinishingsSupported BIND = new FinishingsSupported(7);

  /**
   * Bind the document(s) with one or more staples (wire stitches)
   * along the middle fold.
   */
  public static final FinishingsSupported SADDLE_STITCH =
    new FinishingsSupported(8);

  /**
   * Bind the document(s) with one or more staples (wire stitches)
   * along one edge.
   */
  public static final FinishingsSupported EDGE_STITCH =
    new FinishingsSupported(9);

  /**
   * Bind the document(s) with one or more staples in the top left
   * corner.
   */
  public static final FinishingsSupported STAPLE_TOP_LEFT =
    new FinishingsSupported(20);

  /**
   * Bind the document(s) with one or more staples in the bottom
   * left corner.
   */
  public static final FinishingsSupported STAPLE_BOTTOM_LEFT =
    new FinishingsSupported(21);

  /**
   * Bind the document(s) with one or more staples in the top right corner.
   */
  public static final FinishingsSupported STAPLE_TOP_RIGHT =
    new FinishingsSupported(22);

  /**
   * Bind the document(s) with one or more staples in the bottom right corner.
   */
  public static final FinishingsSupported STAPLE_BOTTOM_RIGHT =
    new FinishingsSupported(23);

  /**
   * Bind the document(s) with one or more  staples (wire stitches)
   * along the left edge.
   */
  public static final FinishingsSupported EDGE_STITCH_LEFT =
    new FinishingsSupported(24);

  /**
   * Bind the document(s) with one or more staples (wire stitches) along
   * the top edge.
   */
  public static final FinishingsSupported EDGE_STITCH_TOP =
    new FinishingsSupported(25);

  /**
   * Bind the document(s) with one or more staples (wire stitches) along
   * the right edge.
   */
  public static final FinishingsSupported EDGE_STITCH_RIGHT =
    new FinishingsSupported(26);

  /**
   * Bind the document(s) with one or more staples (wire stitches) along
   * the bottom edge.
   */
  public static final FinishingsSupported EDGE_STITCH_BOTTOM =
    new FinishingsSupported(27);

  /**
   * Bind the document(s) with two staples (wire stitches) along the
   * left edge assuming a portrait document.
   */
  public static final FinishingsSupported STAPLE_DUAL_LEFT =
    new FinishingsSupported(28);

  /**
   * Bind the document(s) with two staples (wire stitches) along the
   * top edge assuming a portrait document.
   */
  public static final FinishingsSupported STAPLE_DUAL_TOP =
    new FinishingsSupported(29);

  /**
   * Bind the document(s) with two staples (wire stitches) along the
   * right edge assuming a portrait document.
   */
  public static final FinishingsSupported STAPLE_DUAL_RIGHT =
    new FinishingsSupported(30);

  /**
   * Bind the document(s) with two staples (wire stitches) along the
   * bottom edge assuming a portrait document.
   */
  public static final FinishingsSupported STAPLE_DUAL_BOTTOM =
    new FinishingsSupported(31);

  private static final String[] stringTable = { "none", "staple", null,
                                                "cover", "bind", "saddle-stitch",
                                                "edge-stitch", null, null, null,
                                                null, null, null, null, null,
                                                null, null, "staple-top-left",
                                                "staple-bottom-left",
                                                "staple-top-right",
                                                "staple-bottom-right",
                                                "edge-stitch-left",
                                                "edge-stitch-top",
                                                "edge-stitch-right",
                                                "edge-stitch-bottom",
                                                "staple-dual-left",
                                                "staple-dual-top",
                                                "staple-dual-right",
                                                "staple-dual-bottom" };

  private static final FinishingsSupported[] enumValueTable = { NONE, STAPLE,
                                                       null, COVER, BIND,
                                                       SADDLE_STITCH,
                                                       EDGE_STITCH, null,
                                                       null, null, null,
                                                       null, null, null,
                                                       null, null, null,
                                                       STAPLE_TOP_LEFT,
                                                       STAPLE_BOTTOM_LEFT,
                                                       STAPLE_TOP_RIGHT,
                                                       STAPLE_BOTTOM_RIGHT,
                                                       EDGE_STITCH_LEFT,
                                                       EDGE_STITCH_TOP,
                                                       EDGE_STITCH_RIGHT,
                                                       EDGE_STITCH_BOTTOM,
                                                       STAPLE_DUAL_LEFT,
                                                       STAPLE_DUAL_TOP,
                                                       STAPLE_DUAL_RIGHT,
                                                       STAPLE_DUAL_BOTTOM };

  /**
   * Constructs a <code>FinishingsSupported</code> object.
   *
   * @param value the value
   */
  protected FinishingsSupported(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return the class <code>FinishingsSupported</code> itself
   */
  public Class<? extends Attribute> getCategory()
  {
    return FinishingsSupported.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "finishings-supported".
   */
  public String getName()
  {
    return "finishings-supported";
  }

  /**
   * Returns a table with the enumeration values represented as strings
   * for this object.
   *
   * @return The enumeration values as strings.
   */
  protected String[] getStringTable()
  {
    return stringTable;
  }

  /**
   * Returns a table with the enumeration values for this object.
   *
   * @return The enumeration values.
   */
  protected EnumSyntax[] getEnumValueTable()
  {
    return enumValueTable;
  }

  /**
   * Returns the lowest used value by the enumerations of this class.
   * .
   * @return The lowest value used.
   */
  protected int getOffset()
  {
    return 3;
  }

  /**
   * Returns the equally enum of the standard attribute class
   * of this SupportedValuesAttribute enum.
   *
   * @return The enum of the standard attribute class.
   */
  public Finishings getAssociatedAttribute()
  {
    return (Finishings) IppUtilities.getEnumAttribute(
           "finishings", new Integer(getValue()));
  }

  /**
   * Constructs an array from a set of -supported attributes.
   * @param set set to process
   * @return The constructed array.
   *
   * @see #getAssociatedAttribute()
   */
  public static Finishings[]
    getAssociatedAttributeArray(Set<Attribute> set)
  {
    Finishings[] result = new Finishings[set.size()];
    int j = 0;
    for (Attribute tmp : set)
      {
        result[j] = ((FinishingsSupported) tmp).getAssociatedAttribute();
        j++;
      }
    return result;
  }
}
