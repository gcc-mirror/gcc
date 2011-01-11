/* Finishings.java --
   Copyright (C) 2004, 2005, 2006 Free Software Foundation, Inc.

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
exception statement from your version.  */


package javax.print.attribute.standard;

import javax.print.attribute.Attribute;
import javax.print.attribute.DocAttribute;
import javax.print.attribute.EnumSyntax;
import javax.print.attribute.PrintJobAttribute;
import javax.print.attribute.PrintRequestAttribute;


/**
 * The <code>Finishings</code> attribute specifies the finishing operations
 * that the Printer applies to every copy of each printed document in the Job.
 * <p>
 * Standard enum values are: <code>NONE</code>, <code>STAPLE</code>,
 * <code>COVER</code>, <code>BIND</code>, <code>SADDLE_STITCH</code>,
 * <code>EDGE_STITCH</code>.
 * <br><br>
 * The following values are more specific:
 * <code>STAPLE_TOP_LEFT</code>, <code>STAPLE_BOTTOM_LEFT</code>,
 * <code>STAPLE_TOP_RIGHT</code>, <code>STAPLE_BOTTOM_RIGHT</code>,
 * <code>EDGE_STITCH_LEFT</code>, <code>EDGE_STITCH_TOP</code>,
 * <code>EDGE_STITCH_RIGHT</code>, <code>EDGE_STITCH_BOTTOM</code>,
 * <code>STAPLE_DUAL_LEFT</code>, <code>STAPLE_DUAL_TOP</code>,
 * <code>STAPLE_DUAL_RIGHT</code>, <code>STAPLE_DUAL_BOTTOM</code>.
 * </p>
 * <p>
 * <b>Note:</b> The effect of this attribute on jobs with multiple documents
 * is controlled by the job attribute
 * {@link javax.print.attribute.standard.MultipleDocumentHandling}.
 * </p>
 * <p>
 * <b>IPP Compatibility:</b> Finishings is an IPP 1.1 attribute. Differences
 * to the IPP specification are that in the Java Print Service API only one
 * enum value is supported (in IPP a set of enums). Further the enum
 * <code>punch</code> is not supported.
 * </p>
 *
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public class Finishings extends EnumSyntax
  implements DocAttribute, PrintJobAttribute, PrintRequestAttribute
{
  private static final long serialVersionUID = -627840419548391754L;

  /**
   * Perform no finishings of the documents.
   */
  public static final Finishings NONE = new Finishings(3);

  /**
   * Selects binding of the documents with one or more staples.
   */
  public static final Finishings STAPLE = new Finishings(4);

  /**
   * Selects the use of a non-printed (or pre-printed) cover for
   * the document.
   */
  public static final Finishings COVER = new Finishings(6);

  /**
   * Selects that a binding is to be applied to the document.
   * The type and placement of the binding is site-defined.
   */
  public static final Finishings BIND = new Finishings(7);

  /**
   * Selects binding of the documents with one or more staples
   * along the middle fold.
   */
  public static final Finishings SADDLE_STITCH = new Finishings(8);

  /**
   * Selects binding of the documents with one or more staples
   * along one edge.
   */
  public static final Finishings EDGE_STITCH = new Finishings(9);

  /**
   * Selects binding of the documents with one or more staples
   * in the top left corner.
   */
  public static final Finishings STAPLE_TOP_LEFT = new Finishings(20);

  /**
   * Selects binding of the documents with one or more staples in the bottom
   * left corner.
   */
  public static final Finishings STAPLE_BOTTOM_LEFT = new Finishings(21);

  /**
   * Selects binding of the documents with one or more staples in
   * the top right corner.
   */
  public static final Finishings STAPLE_TOP_RIGHT = new Finishings(22);

  /**
   * Selects binding of the documents with one or more staples in
   * the bottom right corner.
   */
  public static final Finishings STAPLE_BOTTOM_RIGHT = new Finishings(23);

  /**
   * Selects binding of the documents with one or more staples
   * along the left edge.
   */
  public static final Finishings EDGE_STITCH_LEFT = new Finishings(24);

  /**
   * Selects binding of the documents with one or more staples along
   * the top edge.
   */
  public static final Finishings EDGE_STITCH_TOP = new Finishings(25);

  /**
   * Selects binding of the documents with one or more staples along
   * the right edge.
   */
  public static final Finishings EDGE_STITCH_RIGHT = new Finishings(26);

  /**
   * Selects binding of the documents with one or more staples along
   * the bottom edge.
   */
  public static final Finishings EDGE_STITCH_BOTTOM = new Finishings(27);

  /**
   * Selects binding of the documents with two staples along the
   * left edge assuming a portrait document.
   */
  public static final Finishings STAPLE_DUAL_LEFT = new Finishings(28);

  /**
   * Selects binding of the documents with two staples along the
   * top edge assuming a portrait document.
   */
  public static final Finishings STAPLE_DUAL_TOP = new Finishings(29);

  /**
   * Selects binding of the documents with two staples along the
   * right edge assuming a portrait document.
   */
  public static final Finishings STAPLE_DUAL_RIGHT = new Finishings(30);

  /**
   * Selects binding of the documents with two staples along the
   * bottom edge assuming a portrait document.
   */
  public static final Finishings STAPLE_DUAL_BOTTOM = new Finishings(31);

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

  private static final Finishings[] enumValueTable = { NONE, STAPLE, null,
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
   * Constructs a <code>Finishings</code> object.
   *
   * @param value the value
   */
  protected Finishings(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return the class <code>Finishings</code> itself
   */
  public Class< ? extends Attribute> getCategory()
  {
    return Finishings.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "finishings".
   */
  public final String getName()
  {
    return "finishings";
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
}
