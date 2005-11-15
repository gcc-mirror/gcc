/* StubLocator.java --
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


package gnu.CORBA;

import org.omg.CORBA.ORB;
import org.omg.CORBA.portable.ObjectImpl;

/**
 * Finds a stub class like "_HelloStub" that can be instantiated
 * from IOR reference. The returned object can be casted to the
 * used type like "Hello" without using the helper .narrow method,
 * and the object is not unnsecessarily re - instantiated if
 * the .narrow method is used anyway. If no stub, matching the naming
 * conventions, is available, the returned stub replacement can still be used
 * to get the valid request, add parameter and invoke the method by name.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class StubLocator
{
  /**
   * Search for the possibly available default stub.
   *
   * @param orb the current ORB. It is not required to find the binding
   * classes, but is needed to instantiate the default implementation
   * if no binding classes are found.
   *
   * @param ior the IOR, possibly containing the information about the
   * correct implementation class.
   */
  public static ObjectImpl search(ORB orb, IOR ior)
  {
    try
      {
        int a = ior.Id.indexOf(':');
        int b = ior.Id.lastIndexOf(':');

        String s = ior.Id.substring(a + 1, b).replace('/', '.');

        String path;

        b = s.lastIndexOf('.');
        if (b > 0)
          path = s.substring(0, b + 1);
        else
          path = "";

        String stub = "_" + s.substring(b + 1) + "Stub";

        Class stubClass = ObjectCreator.forName(path + stub);

        return (ObjectImpl) stubClass.newInstance();
      }
    catch (Exception failed)
      {
        // Various exceptions can be thrown if the Id cannot be parsed,
        // the class is missing, cannot be instantiated or is not an
        // instance of the ObjectImpl.
        return createDefaultStub(orb, ior);
      }
  }

  /**
   * Return the default stub for the case when the client binding classes
   * are not locally available. The returned stub can still be used
   * to get the valid request, add parameter and invoke the method by name.
   *
   * @return the default implementation.
   */
  protected static ObjectImpl createDefaultStub(ORB orb, IOR ior)
  {
    return new IorObject(orb, ior);
  }
}
