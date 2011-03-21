/* ObjectId.java -- Simple object identification mechanism for XML encoding.
 Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.java.beans.encoder;

import java.util.HashMap;

/**
 * <p>
 * ObjectId provides an object identification mechanism which gives each object
 * a name in the form <code>&lt;class&gt;&lt;Nameindex&gt;</code>.
 * </p>
 *
 * <p>
 * Each id can be in an unused state which means that only one instance of the
 * object is in use and a special id is not needed. Certain {@link
 * gnu.java.beans.encoder.elements.Element} subclasses use this feature to find
 * out whether they write the "id" attribute or not.
 * </p>
 * <p>
 * An <code>ObjectId</code> instance is typically given to multiple objects.
 * The second user should then invoke the {@link #init} method to generate the
 * identification string and bring the id in the 'used' state.
 * </p>
 *
 * @author Robert Schuster (robertschuster@fsfe.org)
 */
public class ObjectId
{
  /**
   * Stores the index an object of a specific type should be given.
   */
  private static HashMap nameIndices = new HashMap();

  private String id;

  private Class klass;

  ObjectId(Class klass)
  {
    this.klass = klass;
  }

  public boolean isUnused()
  {
    return id == null;
  }

  public String toString()
  {
    return (id != null) ? id : "<unused id>";
  }

  /**
   * <p>
   * Generates a simple Id by concatenating a class name with a self-increasing
   * number.
   * </p>
   */
  public void init()
  {
    assert (klass != null);

    if (id != null)
      return;

    Integer count = (Integer) nameIndices.get(klass);
    if (count == null)
      {
        count = Integer.valueOf(0);
      }

    if (klass.isArray())
      {
        Class ct = klass.getComponentType();
        if (ct == Boolean.TYPE)
          id = "booleanArray" + count.intValue();
        else if (ct == Byte.TYPE)
          id = "byteArray" + count.intValue();
        else if (ct == Short.TYPE)
          id = "shortArray" + count.intValue();
        else if (ct == Integer.TYPE)
          id = "intArray" + count.intValue();
        else if (ct == Long.TYPE)
          id = "longArray" + count.intValue();
        else if (ct == Float.TYPE)
          id = "floatArray" + count.intValue();
        else if (ct == Double.TYPE)
          id = "doubleArray" + count.intValue();
      }
    else
      id = klass.getName() + count.intValue();

    nameIndices.put(klass, Integer.valueOf(count.intValue() + 1));
  }

}
