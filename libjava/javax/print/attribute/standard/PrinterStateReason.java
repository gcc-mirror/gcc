/* PrinterStateReason.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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


package javax.print.attribute.standard;

import javax.print.attribute.Attribute;
import javax.print.attribute.EnumSyntax;

/**
 * @author Michael Koch (konqueror@gmx.de)
 */
public class PrinterStateReason extends EnumSyntax
  implements Attribute
{
  private static final long serialVersionUID = -1623720656201472593L;

  public static final PrinterStateReason OTHER = new PrinterStateReason(0);
  public static final PrinterStateReason MEDIA_NEEDED =
    new PrinterStateReason(1);
  public static final PrinterStateReason MEDIA_JAM = new PrinterStateReason(2);
  public static final PrinterStateReason MOVING_TO_PAUSED =
    new PrinterStateReason(3);
  public static final PrinterStateReason PAUSED = new PrinterStateReason(4);
  public static final PrinterStateReason SHUTDOWN = new PrinterStateReason(5);
  public static final PrinterStateReason CONNECTING_TO_DEVICE =
    new PrinterStateReason(6);
  public static final PrinterStateReason TIMED_OUT = new PrinterStateReason(7);
  public static final PrinterStateReason STOPPING = new PrinterStateReason(8);
  public static final PrinterStateReason STOPPED_PARTLY =
    new PrinterStateReason(9);
  public static final PrinterStateReason TONER_LOW =
    new PrinterStateReason(10);
  public static final PrinterStateReason TONER_EMPTY =
    new PrinterStateReason(11);
  public static final PrinterStateReason SPOOL_AREA_FULL =
    new PrinterStateReason(12);
  public static final PrinterStateReason COVER_OPEN =
    new PrinterStateReason(13);
  public static final PrinterStateReason INTERLOCK_OPEN =
    new PrinterStateReason(14);
  public static final PrinterStateReason DOOR_OPEN =
    new PrinterStateReason(15);
  public static final PrinterStateReason INPUT_TRAY_MISSING =
    new PrinterStateReason(16);
  public static final PrinterStateReason MEDIA_LOW =
    new PrinterStateReason(17);
  public static final PrinterStateReason MEDIA_EMPTY =
    new PrinterStateReason(18);
  public static final PrinterStateReason OUTPUT_TRAY_MISSING =
    new PrinterStateReason(19);
  public static final PrinterStateReason OUTPUT_AREA_ALMOST_FULL =
    new PrinterStateReason(20);
  public static final PrinterStateReason OUTPUT_AREA_FULL =
    new PrinterStateReason(21);
  public static final PrinterStateReason MARKER_SUPPLY_LOW =
    new PrinterStateReason(22);
  public static final PrinterStateReason MARKER_SUPPLY_EMPTY =
    new PrinterStateReason(23);
  public static final PrinterStateReason MARKER_WASTE_ALMOST_FULL =
    new PrinterStateReason(24);
  public static final PrinterStateReason MARKER_WASTE_FULL =
    new PrinterStateReason(25);
  public static final PrinterStateReason FUSER_OVER_TEMP =
    new PrinterStateReason(26);
  public static final PrinterStateReason FUSER_UNDER_TEMP =
    new PrinterStateReason(27);
  public static final PrinterStateReason OPC_NEAR_EOL =
    new PrinterStateReason(28);
  public static final PrinterStateReason OPC_LIFE_OVER =
    new PrinterStateReason(29);
  public static final PrinterStateReason DEVELOPER_LOW =
    new PrinterStateReason(30);
  public static final PrinterStateReason DEVELOPER_EMPTY =
    new PrinterStateReason(31);
  public static final PrinterStateReason INTERPRETER_RESOURCE_UNAVAILABLE =
    new PrinterStateReason(32);

  /**
   * Constructs a <code>PrinterStateReason</code> object.
   */
  protected PrinterStateReason(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return the class <code>PrintStateReason</code> itself
   */
  public Class getCategory()
  {
    return PrinterStateReason.class;
  }

  /**
   * Returns name of this class.
   *
   * @return the string "printer-state-reason"
   */
  public String getName()
  {
    return "printer-state-reason";
  }
}
