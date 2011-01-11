/* IorObject.java --
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
 * Implements an object, constructed from an IOR reference.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class IorObject
  extends ObjectImpl
  implements IorProvider
{
  /**
   * The IOR, from which the object was constructed.
   */
  protected final IOR ior;

  /**
   * The object id, as defined in IOR.
   */
  protected final String[] id;

  /**
   * Create the object from the given IOR.
   *
   * @param an_ior the IOR.
   */
  public IorObject(ORB orb, IOR an_ior)
  {
    ior = an_ior;
    _set_delegate(new IorDelegate(orb, ior));
    id = new String[] { ior.Id };
  }

  /**
   * Create the object from the given string IOR representation.
   *
   * @param an_ior the IOR in the string form.
   */
  public IorObject(OrbFunctional orb, String an_ior)
  {
    ior = IOR.parse(an_ior);
    _set_delegate(new IorDelegate(orb, ior));
    id = new String[] { ior.Id };
  }

  /**
   * Get the IOR of this object.
   */
  public IOR getIor()
  {
    return ior;
  }

  public String[] _ids()
  {
    return id;
  }

  /**
   * Get a string reference for this object.
   *
   * @return the class name:IOR profile
   */
  public String toString()
  {
    return getClass().getName() + ":IOR:" + ior;
  }

  /**
   * Calls realease on the delegate.
   */
  protected void finalize()
                   throws java.lang.Throwable
  {
    _get_delegate().release(this);
  }
}
