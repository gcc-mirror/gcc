/* FinishingsDefault.java --
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


package gnu.javax.print.ipp.attribute.defaults;

import gnu.javax.print.ipp.IppUtilities;
import gnu.javax.print.ipp.attribute.DefaultValueAttribute;

import javax.print.attribute.Attribute;
import javax.print.attribute.EnumSyntax;


/**
 * The <code>FinishingsDefault</code> attribute provides the supported
 * values for finishings of a job.
 *
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class FinishingsDefault extends EnumSyntax
  implements DefaultValueAttribute
{

  /** No finishing. */
  public static final FinishingsDefault NONE = new FinishingsDefault(3);

  /** Staple the document(s) */
  public static final FinishingsDefault STAPLE = new FinishingsDefault(4);

  /** Cover a document */
  public static final FinishingsDefault COVER = new FinishingsDefault(6);

  /**
   * This value indicates that a binding is to be applied to the document.
   * The type and placement of the binding is site-defined.
   */
  public static final FinishingsDefault BIND = new FinishingsDefault(7);

  /**
   * Bind the document(s) with one or more staples (wire stitches)
   * along the middle fold.
   */
  public static final FinishingsDefault SADDLE_STITCH = new FinishingsDefault(8);

  /**
   * Bind the document(s) with one or more staples (wire stitches)
   * along one edge.
   */
  public static final FinishingsDefault EDGE_STITCH = new FinishingsDefault(9);

  /**
   * Bind the document(s) with one or more staples in the top left
   * corner.
   */
  public static final FinishingsDefault STAPLE_TOP_LEFT = new FinishingsDefault(20);

  /**
   * Bind the document(s) with one or more staples in the bottom
   * left corner.
   */
  public static final FinishingsDefault STAPLE_BOTTOM_LEFT = new FinishingsDefault(21);

  /**
   * Bind the document(s) with one or more staples in the top right corner.
   */
  public static final FinishingsDefault STAPLE_TOP_RIGHT = new FinishingsDefault(22);

  /**
   * Bind the document(s) with one or more staples in the bottom right corner.
   */
  public static final FinishingsDefault STAPLE_BOTTOM_RIGHT = new FinishingsDefault(23);

  /**
   * Bind the document(s) with one or more  staples (wire stitches)
   * along the left edge.
   */
  public static final FinishingsDefault EDGE_STITCH_LEFT = new FinishingsDefault(24);

  /**
   * Bind the document(s) with one or more staples (wire stitches) along
   * the top edge.
   */
  public static final FinishingsDefault EDGE_STITCH_TOP = new FinishingsDefault(25);

  /**
   * Bind the document(s) with one or more staples (wire stitches) along
   * the right edge.
   */
  public static final FinishingsDefault EDGE_STITCH_RIGHT = new FinishingsDefault(26);

  /**
   * Bind the document(s) with one or more staples (wire stitches) along
   * the bottom edge.
   */
  public static final FinishingsDefault EDGE_STITCH_BOTTOM = new FinishingsDefault(27);

  /**
   * Bind the document(s) with two staples (wire stitches) along the
   * left edge assuming a portrait document.
   */
  public static final FinishingsDefault STAPLE_DUAL_LEFT = new FinishingsDefault(28);

  /**
   * Bind the document(s) with two staples (wire stitches) along the
   * top edge assuming a portrait document.
   */
  public static final FinishingsDefault STAPLE_DUAL_TOP = new FinishingsDefault(29);

  /**
   * Bind the document(s) with two staples (wire stitches) along the
   * right edge assuming a portrait document.
   */
  public static final FinishingsDefault STAPLE_DUAL_RIGHT = new FinishingsDefault(30);

  /**
   * Bind the document(s) with two staples (wire stitches) along the
   * bottom edge assuming a portrait document.
   */
  public static final FinishingsDefault STAPLE_DUAL_BOTTOM = new FinishingsDefault(31);

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

  private static final FinishingsDefault[] enumValueTable = { NONE, STAPLE, null,
                                                       COVER, BIND,
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
   * Constructs a <code>FinishingsDefault</code> object.
   *
   * @param value the value
   */
  protected FinishingsDefault(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return the class <code>FinishingsDefault</code> itself
   */
  public Class<? extends Attribute> getCategory()
  {
    return FinishingsDefault.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "finishings-default".
   */
  public String getName()
  {
    return "finishings-default";
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
   * of this DefaultValuesAttribute enum.
   *
   * @return The enum of the standard attribute class.
   */
  public Attribute getAssociatedAttribute()
  {
    return IppUtilities.getEnumAttribute("finishings", new Integer(getValue()));
  }
}
