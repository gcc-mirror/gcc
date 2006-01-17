/* PrinterResolution.java --
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

import javax.print.attribute.DocAttribute;
import javax.print.attribute.PrintJobAttribute;
import javax.print.attribute.PrintRequestAttribute;
import javax.print.attribute.ResolutionSyntax;


/**
 * The <code>PrinterResolution</code> printing attribute specifies a 
 * resolution supported by a print service or to be used by a print job. 
 * <p>
 * <b>IPP Compatibility:</b> PrinterResolution is an IPP 1.1 attribute.
 * </p>
 * 
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class PrinterResolution extends ResolutionSyntax
  implements DocAttribute, PrintJobAttribute, PrintRequestAttribute
{
  private static final long serialVersionUID = 13090306561090558L;

  /**
   * Creates a <code>PrinterResolution</code> object with the given cross
   * feed and feed resolutions.
   *
   * @param crossFeedResolution the cross feed resolution
   * @param feedResolution the feed resolution
   * @param units the unit to use (e.g. {@link #DPCM} or {@link #DPI})
   *
   * @exception IllegalArgumentException if either parameter is &lt; 1
   */
  public PrinterResolution(int crossFeedResolution, int feedResolution,
                           int units)
  {
    super(crossFeedResolution, feedResolution, units);
  }
  
  /**
   * Tests if the given object is equal to this object.
   *
   * @param obj the object to test
   *
   * @return <code>true</code> if both objects are equal, 
   * <code>false</code> otherwise.
   */
  public boolean equals(Object obj)
  {
    if(! (obj instanceof PrinterResolution))
      return false;
    
    return super.equals(obj);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>PrinterResolution</code> itself.
   */
  public Class getCategory()
  {
    return PrinterResolution.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "printer-resolution".
   */
  public String getName()
  {
    return "printer-resolution";
  }
}
