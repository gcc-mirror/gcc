/* ServiceUI.java -- 
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


package javax.print;

import gnu.javax.print.PrinterDialog;

import java.awt.GraphicsConfiguration;
import java.awt.GraphicsEnvironment;
import java.awt.HeadlessException;
import java.util.Arrays;

import javax.print.attribute.PrintRequestAttributeSet;

/**
 * <code>ServiceUI</code> provides a method to create a graphical 
 * print dialog.
 * <p>
 * The graphical print dialog enables the user to browse the available
 * print services on the system. It provides user interfaces to interact 
 * with the most common printing attributes likes specifying the number of 
 * copies to print or the page ranges.
 * </p><p>
 * The initial appearance of the print dialog as shown to the user may be 
 * specified by providing the default selected print service as well as 
 * initial values for the printing attributes in the user interface.
 * </p>
 * 
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public class ServiceUI
{

  /**
   * Default constructor.
   */
  public ServiceUI()
  {
    // nothing to do here - only one static method
  }
  
  /**
   * Creates a modal graphical printing dialog at the specified location on 
   * the screen.
   * <p>
   * The dialog will return the user selected print service and the given 
   * attributes set will contain the modified printing attributes. If the 
   * user cancels the printing dialog <code>null</code> will be returned and 
   * the printing attributes set will be unmodified.
   * </p><p>
   * The values of the given attributes set (if not empty) will be displayed
   * initially unless the are unsupported by the print service. If a print 
   * service does not support a particular value it is substituted with the
   * default value of the print service.
   * </p> 
   * 
   * @param gc the screen to use. <code>null</code> is default screen.
   * @param x the coordinate of the upper left edge of the dialog in screen 
   *   coordinates (not relative to the parent frame).
   * @param y the coordinate of the upper left edge of the dialog in screen 
   *   coordinates (not relative to the parent frame).
   * @param services the print services to browse (not null).
   * @param defaultService the default service. If <code>null</code>
   * the first of the print services in the services array will be used.
   * @param flavor the flavours to be printed.
   * @param attributes the attributes requested. Will be updated 
   * by selections done by the user in the dialog. 
   * 
   * @return The selected print service or <code>null</code> if user
   * has cancelled the printer dialog.
   * 
   * @throws HeadlessException if GraphicsEnvironment is headless
   * @throws IllegalArgumentException if services is <code>null</code> or an 
   * empty array, attributes are <code>null</code> or the given default 
   * <code>PrintService<code> is not part of the print service array.
   */
  public static PrintService printDialog(GraphicsConfiguration gc, int x, 
      int y, PrintService[] services, PrintService defaultService,
      DocFlavor flavor, PrintRequestAttributeSet attributes)
    throws HeadlessException
    {       
      if (GraphicsEnvironment.isHeadless())
        throw new HeadlessException("GraphicsEnvironment is headless.");
      
      if (services == null || services.length == 0 || attributes == null)
        throw new IllegalArgumentException("Given print service array / " 
                                           + "attributes may not be null");
      
      if (defaultService != null && 
          ! Arrays.asList(services).contains(defaultService))
        throw new IllegalArgumentException("defaultService is not contained " 
                                           + " in the print service array");
      
      PrinterDialog dialog = new PrinterDialog(gc, services, defaultService, 
                                               flavor, attributes);
      
      dialog.setLocation(x, y);
      dialog.show();
      
      return dialog.getSelectedPrintService();
    }
}
