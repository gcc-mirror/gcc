/* PrinterState.java --
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

import javax.print.attribute.EnumSyntax;
import javax.print.attribute.PrintServiceAttribute;


/**
 * The <code>PrinterState</code> printing attribute reports
 * the current state of the printer device.
 * <p>
 * The {@link javax.print.attribute.standard.PrinterStateReasons}
 * attribute provides further detailed information about
 * the given printer state. Detailed information about the printer
 * state and printer state reasons attributes can be found in the 
 * RFC 2911.
 * </p> 
 * <p>
 * <b>IPP Compatibility:</b> PrinterState is an IPP 1.1 attribute.
 * </p>
 * 
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class PrinterState extends EnumSyntax
  implements PrintServiceAttribute
{
  private static final long serialVersionUID = -649578618346507718L;

  /**
   * The state is unknown currently.
   */
  public static final PrinterState UNKNOWN = new PrinterState(0);
  
  /**
   * The printer device is in idle state. New jobs can start
   * processing without waiting.
   */
  public static final PrinterState IDLE = new PrinterState(3);
  
  /**
   * The printer device is in processing state.
   */
  public static final PrinterState PROCESSING = new PrinterState(4);
  
  /**
   * The printer device has stopped. No jobs can be processed and
   * normally manual intervention is needed.
   */
  public static final PrinterState STOPPED = new PrinterState(5);

  private static final String[] stringTable = { "unknown", null, null, 
                                                "idle", "processing", 
                                                "stopped" };
  
  private static final PrinterState[] enumValueTable = { UNKNOWN, null, null,
                                                         IDLE, PROCESSING, 
                                                         STOPPED };
  
  /**
   * Constructs a <code>PrinterState</code> object.
   * 
   * @param value the enum value.
   */
  protected PrinterState(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>PrinterState</code> itself.
   */
  public Class getCategory()
  {
    return PrinterState.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "printer-state".
   */
  public String getName()
  {
    return "printer-state";
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
