/* NumberOfDocuments.java -- 
   Copyright (C) 2003 Free Software Foundation, Inc.

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

import javax.print.attribute.IntegerSyntax;
import javax.print.attribute.PrintJobAttribute;

/**
 * @author Michael Koch
 */
public final class NumberOfDocuments extends IntegerSyntax
  implements PrintJobAttribute
{
  private static final long serialVersionUID = 7891881310684461097L;
  
  /**
   * Creates a <code>NumberOfDocuments</code> object.
   *
   * @param value the number of documents
   *
   * @exception IllegalArgumentException if value < 0
   */
  public NumberOfDocuments(int value)
  {
    super(value);

    if (value < 0)
      throw new IllegalArgumentException("value may not be less than 0");
  }
  
  /**
   * Tests of obj is equal to this object.
   *
   * @param obj the object to test
   *
   * @returns true if both objects are equal, false otherwise.
   */
  public boolean equals(Object obj)
  {
    if(! (obj instanceof NumberOfDocuments))
      return false;

    return super.equals(obj);
  }

  /**
   * Returns category of this class.
   *
   * @return the class <code>NumberOfDocuments</code> itself
   */
  public final Class getCategory()
  {
    return NumberOfDocuments.class;
  }

  /**
   * Returns name of this class.
   *
   * @return the string "number-of-documents"
   */
  public final String getName()
  {
    return "number-of-documents";
  }
}
