/* Local_delegate.java --
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

import org.omg.CORBA.Context;
import org.omg.CORBA.ContextList;
import org.omg.CORBA.ExceptionList;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.NVList;
import org.omg.CORBA.NamedValue;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Request;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.ObjectImpl;

/**
 * The delegate, implementing the basic functionality only. This delegate
 * is set in {@link ORG.connect(org.omg.CORBA.Object)} if ORB
 * determines that the object is an instance of the
 * {@link org.omg.CORBA.portable.ObjectImpl} and no other delegate is set.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class Simple_delegate
  extends Delegate
{
  /**
   * The orb.
   */
  protected final ORB orb;

  /**
   * The ior.
   */
  protected IOR ior;

  public Simple_delegate(ORB an_orb, IOR an_ior)
  {
    orb = an_orb;
    ior = an_ior;
  }

  /**
   * Set the IOR of this object. The IOR must be newly set if
   * the server reports that the object has permanently moved to a new
   * location.
   *
   * @param an_ior the new IOR.
   */
  public void setIor(IOR an_ior)
  {
    this.ior = an_ior;
  }

  /**
   * Get the IOR of this object.
   */
  public IOR getIor()
  {
    return ior;
  }

  /**
   * Not implemented.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public Request create_request(org.omg.CORBA.Object target, Context context,
                                String operation, NVList parameters,
                                NamedValue returns
                               )
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Not implemented.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public Request create_request(org.omg.CORBA.Object target, Context context,
                                String operation, NVList parameters,
                                NamedValue returns, ExceptionList exceptions,
                                ContextList ctx_list
                               )
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Not implemented.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public org.omg.CORBA.Object duplicate(org.omg.CORBA.Object target)
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Performs direct comparison ('==').
   */
  public boolean equals(org.omg.CORBA.Object self, org.omg.CORBA.Object other)
  {
    return self == other;
  }

  /**
   * Not implemented.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public org.omg.CORBA.Object get_interface_def(org.omg.CORBA.Object target)
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Return the hashcode (0 <= hashcode < maximum).
   */
  public int hash(org.omg.CORBA.Object target, int maximum)
  {
    return target == null ? 0 : target.hashCode() % maximum;
  }

  /**
   * Delegates functionality to java.lang.Object.hashCode();
   */
  public int hashCode(org.omg.CORBA.Object target)
  {
    return target == null ? 0 : target.hashCode();
  }

  /**
   * Check if this object can be referenced by the given repository id.
   *
   * @param target the CORBA object, must be an instance of
   * {@link org.omg.CORBA.portable.ObjectImpl}.
   *
   * @param repositoryIdentifer the repository id.
   *
   * @return true if the passed parameter is a repository id of this
   * CORBA object.
   */
  public boolean is_a(org.omg.CORBA.Object target, String repositoryIdentifer)
  {
    if (!(target instanceof ObjectImpl))
      throw new NO_IMPLEMENT("Supported only for org.omg.CORBA.portable.ObjectImpl");

    ObjectImpl imp = (ObjectImpl) target;
    String[] ids = imp._ids();

    for (int i = 0; i < ids.length; i++)
      {
        if (ids [ i ].equals(repositoryIdentifer))
          return true;
      }
    return false;
  }

  /**
   * Only returns true if the objects are equal ('==').
   */
  public boolean is_equivalent(org.omg.CORBA.Object target,
                               org.omg.CORBA.Object other
                              )
  {
    return target == other;
  }

  /**
   * Returns true by default.
   */
  public boolean is_local(org.omg.CORBA.Object self)
  {
    return true;
  }

  /**
   * Returns true if the target is null.
   */
  public boolean non_existent(org.omg.CORBA.Object target)
  {
    return target == null;
  }

  /**
   * Returns the ORB, passed in constructor,
   * regardless of the argument. This class requires a single instance
   * per each object.
   */
  public ORB orb(org.omg.CORBA.Object target)
  {
    return orb;
  }

  /**
   * Returns without action.
   */
  public void release(org.omg.CORBA.Object target)
  {
  }

  /**
   * This should never be called this type delegate.
   *
   * @throws InternalError, always.
   */
  public Request request(org.omg.CORBA.Object target, String operation)
  {
    throw new InternalError();
  }
}
