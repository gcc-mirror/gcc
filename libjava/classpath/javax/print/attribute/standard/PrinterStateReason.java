/* PrinterStateReason.java --
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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


package javax.print.attribute.standard;

import javax.print.attribute.Attribute;
import javax.print.attribute.EnumSyntax;

/**
 * The <code>PrinterStateReason</code> attribute provides additional
 * information about the current state of the printer device. Its always part
 * of the {@link javax.print.attribute.standard.PrinterStateReasons}
 * printing attribute. 
 * <p>
 * <b>IPP Compatibility:</b> PrinterStateReason is not an IPP 1.1 
 * attribute itself but used inside the <code>PrinterStateReasons</code>
 * attribute.
 * </p>
 * 
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public class PrinterStateReason extends EnumSyntax
  implements Attribute
{
  private static final long serialVersionUID = -1623720656201472593L;

  /**
   * Any state other state not listed here.
   */
  public static final PrinterStateReason OTHER = new PrinterStateReason(0);
  
  /**
   * A media tray has run out of media.
   */
  public static final PrinterStateReason MEDIA_NEEDED =
    new PrinterStateReason(1);
  
  /**
   * A media jam occured in the printer device.
   */
  public static final PrinterStateReason MEDIA_JAM = new PrinterStateReason(2);
  
  /**
   * Indicates that the printer has been paused by the pause printer
   * operation and is currently moving to the pause state.
   */
  public static final PrinterStateReason MOVING_TO_PAUSED =
    new PrinterStateReason(3);
  
  /**
   * The printer device has be paused by the pause printer operation.
   */
  public static final PrinterStateReason PAUSED = new PrinterStateReason(4);
  
  /**
   * The printer device has been shutdown or removed from service.
   */
  public static final PrinterStateReason SHUTDOWN = new PrinterStateReason(5);
  
  /**
   * The printer object is connecting to the device. If a printer
   * device is on the network the printer object may be unable to connect. 
   */
  public static final PrinterStateReason CONNECTING_TO_DEVICE =
    new PrinterStateReason(6);
  
  /**
   * The connection to the device has timed out. 
   */
  public static final PrinterStateReason TIMED_OUT = new PrinterStateReason(7);
  
  /**
   * The printer object is stopping the printer device.
   */
  public static final PrinterStateReason STOPPING = new PrinterStateReason(8);
  
  /**
   * The printer object has stopped partly. A printer object may control
   * several physical output devices (e.g. a printer class in CUPS) and
   * stop only some of the devices.
   */
  public static final PrinterStateReason STOPPED_PARTLY =
    new PrinterStateReason(9);
  
  /**
   * The printer device is low on toner.
   */
  public static final PrinterStateReason TONER_LOW =
    new PrinterStateReason(10);
  
  /**
   * The printer device is out of toner.
   */
  public static final PrinterStateReason TONER_EMPTY =
    new PrinterStateReason(11);
  
  /**
   * The printers spool area is currently full. The printer is
   * currently not able to accept jobs.
   */
  public static final PrinterStateReason SPOOL_AREA_FULL =
    new PrinterStateReason(12);
  
  /**
   * One or more covers of the printer device are open.
   */
  public static final PrinterStateReason COVER_OPEN =
    new PrinterStateReason(13);
  
  /**
   * One or more interlocks of the printer device are open.
   */
  public static final PrinterStateReason INTERLOCK_OPEN =
    new PrinterStateReason(14);
  
  /**
   * One or more doors of the printer device are open.
   */
  public static final PrinterStateReason DOOR_OPEN =
    new PrinterStateReason(15);
  
  /**
   * One or more input trays are missing in the printer device.
   */
  public static final PrinterStateReason INPUT_TRAY_MISSING =
    new PrinterStateReason(16);
  
  /**
   * The printer device is low on media.
   */
  public static final PrinterStateReason MEDIA_LOW =
    new PrinterStateReason(17);
  
  /**
   * The printer device is out of media.
   */
  public static final PrinterStateReason MEDIA_EMPTY =
    new PrinterStateReason(18);
  
  /**
   * One or more output trays are missing in the printer device.
   */
  public static final PrinterStateReason OUTPUT_TRAY_MISSING =
    new PrinterStateReason(19);
  
  /**
   * One or more output areas of the printer device are almost full.
   */
  public static final PrinterStateReason OUTPUT_AREA_ALMOST_FULL =
    new PrinterStateReason(20);
  
  /**
   * One or more output areas of the printer device are full.
   */
  public static final PrinterStateReason OUTPUT_AREA_FULL =
    new PrinterStateReason(21);
  
  /**
   * The printer device is low on marker supply.
   */
  public static final PrinterStateReason MARKER_SUPPLY_LOW =
    new PrinterStateReason(22);
  
  /**
   * The printer device is out of marker supply.
   */
  public static final PrinterStateReason MARKER_SUPPLY_EMPTY =
    new PrinterStateReason(23);
  
  /**
   * The marker waste bin of the printer device is almost full.
   */
  public static final PrinterStateReason MARKER_WASTE_ALMOST_FULL =
    new PrinterStateReason(24);
  
  /**
   * The marker waste bin of the printer device is full.
   */
  public static final PrinterStateReason MARKER_WASTE_FULL =
    new PrinterStateReason(25);
  
  /**
   * The fuser of the printer device is over temperature.
   */
  public static final PrinterStateReason FUSER_OVER_TEMP =
    new PrinterStateReason(26);
  
  /**
   * The fuser of the printer device is under the needed temperature.
   */
  public static final PrinterStateReason FUSER_UNDER_TEMP =
    new PrinterStateReason(27);
  
  /**
   * The optical photo conductor is near its end of life (EOL).
   */
  public static final PrinterStateReason OPC_NEAR_EOL =
    new PrinterStateReason(28);
  
  /**
   * The optical photo conductor has reached its end of life.
   */
  public static final PrinterStateReason OPC_LIFE_OVER =
    new PrinterStateReason(29);
  
  /**
   * The printer device is low on developer.
   */
  public static final PrinterStateReason DEVELOPER_LOW =
    new PrinterStateReason(30);
  
  /**
   * The printer device is out of developer.
   */
  public static final PrinterStateReason DEVELOPER_EMPTY =
    new PrinterStateReason(31);
  
  /**
   * An interpreter resource (e.g. font) is unavailable.
   */
  public static final PrinterStateReason INTERPRETER_RESOURCE_UNAVAILABLE =
    new PrinterStateReason(32);

  private static final String[] stringTable = 
    { "other", "media-needed",  "media-jam", "moving-to-paused", "paused", 
    "shutdown", "connecting-to-device", "timed-out", "stopping", 
    "stopped-partly", "toner-low", "toner-empty", "spool-area-full", 
    "cover-open", "interlock-open", "door-open", "input-tray-missing", 
    "media-low", "media-empty", "output-tray-missing", "output-area-almost-full",
    "output-area-full", "marker-supply-low", "marker-supply-empty", 
    "marker-waste-almost-full", "marker-waste-full", "fuser-over-temp", 
    "fuser-under-temp", "opc-near-eol", "opc-life-over", "developer-low", 
    "developer-empty", "interpreter-resource-unavailable" };

  private static final PrinterStateReason[] enumValueTable = 
    { OTHER, MEDIA_NEEDED, MEDIA_JAM, MOVING_TO_PAUSED, PAUSED, SHUTDOWN,
    CONNECTING_TO_DEVICE, TIMED_OUT, STOPPING, STOPPED_PARTLY, TONER_LOW,
    TONER_EMPTY, SPOOL_AREA_FULL, COVER_OPEN, INTERLOCK_OPEN, DOOR_OPEN,
    INPUT_TRAY_MISSING, MEDIA_LOW, MEDIA_EMPTY, OUTPUT_TRAY_MISSING,
    OUTPUT_AREA_ALMOST_FULL, OUTPUT_AREA_FULL, MARKER_SUPPLY_LOW,
    MARKER_SUPPLY_EMPTY, MARKER_WASTE_ALMOST_FULL, MARKER_WASTE_FULL,
    FUSER_OVER_TEMP, FUSER_UNDER_TEMP, OPC_NEAR_EOL, OPC_LIFE_OVER, 
    DEVELOPER_LOW, DEVELOPER_EMPTY, INTERPRETER_RESOURCE_UNAVAILABLE };

  /**
   * Constructs a <code>PrinterStateReason</code> object.
   * 
   * @param value the enum value.
   */
  protected PrinterStateReason(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>PrintStateReason</code> itself.
   */
  public Class getCategory()
  {
    return PrinterStateReason.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "printer-state-reason".
   */
  public String getName()
  {
    return "printer-state-reason";
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
}
