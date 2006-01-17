/* JobKOctetsProcessed.java -- 
   Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.

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

import javax.print.attribute.IntegerSyntax;
import javax.print.attribute.PrintJobAttribute;

/**
 * The <code>JobKOctetsProcessed</code> printing attribute reports
 * the total number of octets already processed in K octets units.
 * <p>
 * The supplied value will be rounded up to the next highest K octets.
 * This attribute will not include a multiplication factor from the number 
 * of copies.
 * </p>
 * <p>
 * This attribute belongs to a group of job progress attributes which are 
 * reporting on the progress of a print job.
 * </p>
 * <p>
 * <b>IPP Compatibility:</b> JobKOctetsProcessed is an IPP 1.1 attribute.
 * </p>
 * @see javax.print.attribute.standard.JobMediaSheetsCompleted
 * @see javax.print.attribute.standard.JobImpressionsCompleted
 * 
 * @author Michael Koch
 */
public final class JobKOctetsProcessed extends IntegerSyntax
  implements PrintJobAttribute
{
  private static final long serialVersionUID = -6265238509657881806L;
  
  /**
   * Creates a <code>JobKOctetsProcessed</code> object.
   * The value is in units of K (1024) octets rounded up to the next highest K. 
   *
   * @param value the number of processed K octets
   *
   * @exception IllegalArgumentException if value &lt; 0
   */
  public JobKOctetsProcessed(int value)
  {
    super(value);

    if (value < 0)
      throw new IllegalArgumentException("value may not be less than 0");
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
    if(! (obj instanceof JobKOctetsProcessed))
      return false;

    return super.equals(obj);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>JobKOctetsProcessed</code> itself.
   */
  public Class getCategory()
  {
    return JobKOctetsProcessed.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "job-k-octets-processed".
   */
  public String getName()
  {
    return "job-k-octets-processed";
  }
}
