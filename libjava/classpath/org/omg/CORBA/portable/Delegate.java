/* Delegate.java --
Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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


package org.omg.CORBA.portable;

import gnu.java.lang.CPStringBuilder;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.Context;
import org.omg.CORBA.ContextList;
import org.omg.CORBA.DomainManager;
import org.omg.CORBA.ExceptionList;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.NVList;
import org.omg.CORBA.NamedValue;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Policy;
import org.omg.CORBA.Request;
import org.omg.CORBA.SetOverrideType;

/**
 * Specifies a vendor specific implementation of the
 * {@link org.omg.CORBA.Object} methods. The calls to these
 * methods are forwarded to the object delegate that can be
 * replaced, if needed. The first parameter is the actual
 * CORBA object to that the operation must be applied.
 *
 * Some methods in this class are not abstract, but no implemented,
 * thowing the {@link NO_IMPLEMENT}. This, however, does not mean that
 * they are not implemented in the derived classes as well.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public abstract class Delegate
{
  /**
   * Explains the reason of throwing the NO_IMPLEMENT.
   */
  private static final String WHY =
    "Following 1.4 API, this Delegate method must not be implemented. Override.";

  /**
   * Create a request to invoke the method of this object.
   *
   * @param target the CORBA object, to that this operation must be applied.
   * @param context a list of additional properties.
   * @param operation the name of method to be invoked.
   * @param parameters the method parameters.
   * @param returns the container for tge method returned value.
   *
   * @return the created reaquest.
   */
  public abstract Request create_request(org.omg.CORBA.Object target,
                                         Context context, String operation,
                                         NVList parameters, NamedValue returns
                                        );

  /**
   * Create a request to invoke the method of this object, specifying
   * context list and the list of the expected exception.
   *
   * @param target the CORBA object, to that this operation must be applied.
   * @param context a list of additional properties.
   * @param operation the name of method to be invoked.
   * @param parameters the method parameters.
   * @param returns the container for tge method returned value.
   * @param exceptions the list of the possible exceptions that the method
   * can throw.
   * @param ctx_list the list of the context strings that need to be
   * resolved and send as a context instance.
   *
   * @return the created reaquest.
   */
  public abstract Request create_request(org.omg.CORBA.Object target,
                                         Context context, String operation,
                                         NVList parameters, NamedValue returns,
                                         ExceptionList exceptions,
                                         ContextList ctx_list
                                        );

  /**
   * Duplicate the object reference. This does not make much sense for
   * java platform and is just included for the sake of compliance with
   * CORBA APIs.
   *
   * @param target the CORBA object, to that this operation must be applied.
   *
   * The method may return the object reference itself.
   *
   * @return as a rule, <code>this</code>.
   */
  public abstract org.omg.CORBA.Object duplicate(org.omg.CORBA.Object target);

  /**
   * Retrieve the domain managers for this object.
   *
   * @param target the CORBA object, to that this operation must be applied.
   *
   * @return the domain managers.
   *
   * @throws NO_IMPLEMENT, always (following the 1.4 specification).
   */
  public DomainManager[] get_domain_managers(org.omg.CORBA.Object target)
  {
    throw new NO_IMPLEMENT(WHY);
  }

  /**
   *
   * @param target the CORBA object, to that this operation must be applied.
   *
   * Get the <code>InterfaceDef</code> for this Object.
   */
  public abstract org.omg.CORBA.Object get_interface_def(org.omg.CORBA.Object target);

  /**
   * Returns the {@link Policy}, applying to this object.
   *
   * @param target the CORBA object, to that this operation must be applied.
   * @param a_policy_type a type of policy to be obtained.
   * @return a corresponding Policy object.
   *
   * @throws NO_IMPLEMENT, always (following the 1.4 specification).
   */
  public Policy get_policy(org.omg.CORBA.Object target, int a_policy_type)
                    throws BAD_PARAM
  {
    throw new NO_IMPLEMENT(WHY);
  }

  /**
   * Get the hashcode this object reference. The same hashcode still
   * does not means that the references are the same. From the other
   * side, two different references may still refer to the same CORBA
   * object. The returned value must not change during the object
   * lifetime.
   *
   * @param target the CORBA object, to that this operation must be applied.
   * @param maximum the maximal value to return.
   *
   * @return the hashcode.
   */
  public abstract int hash(org.omg.CORBA.Object target, int maximum);

  /**
   * Check if this object can be referenced by the given repository id.
   *
   * @param target the CORBA object, to that this operation must be applied.
   * @param repositoryIdentifer the repository id.
   *
   * @return true if the passed parameter is a repository id of this
   * CORBA object.
   */
  public abstract boolean is_a(org.omg.CORBA.Object target,
                               String repositoryIdentifer
                              );

  /**
   * Return true if the other object references are equivalent, so far as
   * it is possible to determine this easily.
   *
   * @param target the CORBA object, to that this operation must be applied.
   * @param other the other object reference.
   *
   * @return true if both references refer the same object, false
   * if they probably can refer different objects.
   *
   */
  public abstract boolean is_equivalent(org.omg.CORBA.Object target,
                                        org.omg.CORBA.Object other
                                       );

  /**
   * Returns true if the object is local.
   *
   * @param self the object to check.
   *
   * @return false, always (following 1.4 specs). Override to get
   * functionality.
   */
  public boolean is_local(org.omg.CORBA.Object self)
  {
    return false;
  }

  /**
   * Determines if the server object for this reference has already
   * been destroyed.
   *
   * @param target the CORBA object, to that this operation must be applied.
   *
   * @return true if the object has been destroyed, false otherwise.
   */
  public abstract boolean non_existent(org.omg.CORBA.Object target);

  /**
   * Compares two objects for equality. The default implementations
   * delegated call to {@link java.lang.Object#equals(java.lang.Object)}.
   *
   * @param self this CORBA object.
   * @param other the other CORBA object.
   *
   * @return true if the objects are equal.
   */
  public boolean equals(org.omg.CORBA.Object self, java.lang.Object other)
  {
    return self==other;
  }

  /**
   * Return the hashcode for this CORBA object. The default implementation
   * delegates call to {@link #hash(org.omg.CORBA.Object, int)}, passing Integer.MAX_VALUE as an
   * argument.
   *
   * @param target the object, for that the hash code must be computed.
   *
   * @return the hashcode.
   */
  public int hashCode(org.omg.CORBA.Object target)
  {
    return hash(target, Integer.MAX_VALUE);
  }

  /**
   * Invoke the operation.
   *
   * @param target the invocation target.
   * @param output the stream, containing the written arguments.
   *
   * @return the stream, from where the input parameters could be read.
   *
   * @throws ApplicationException if the application throws an exception,
   * defined as a part of its remote method definition.
   *
   * @throws RemarshalException if reading(remarshalling) fails.
   *
   * @throws NO_IMPLEMENT, always (following the 1.4 specification).
   */
  public InputStream invoke(org.omg.CORBA.Object target,
                            org.omg.CORBA.portable.OutputStream output
                           )
                     throws ApplicationException, RemarshalException
  {
    throw new NO_IMPLEMENT(WHY);
  }

  /**
   * Provides the reference to ORB.
   *
   * @param target the object reference.
   *
   * @return the associated ORB.
   *
   * @throws NO_IMPLEMENT, always (following the 1.4 specification).
   */
  public ORB orb(org.omg.CORBA.Object target)
  {
    throw new NO_IMPLEMENT(WHY);
  }

  /**
   * Free resoureces, occupied by this reference. The object implementation
   * is not notified, and the other references to the same object are not
   * affected.
   *
   * @param target the CORBA object, to that this operation must be applied.
   */
  public abstract void release(org.omg.CORBA.Object target);

  /**
   * Release the reply stream back to ORB after finishing reading the data
   * from it.
   *
   * @param target the CORBA object, to that this operation must be applied.
   * @param input the stream, normally returned by {@link #invoke} or
   * {@link ApplicationException#getInputStream()}, can be null.
   *
   * The default method returns without action.
   */
  public void releaseReply(org.omg.CORBA.Object target,
                           org.omg.CORBA.portable.InputStream input
                          )
  {
  }

  /**
   * Create a request to invoke the method of this CORBA object.
   *
   * @param target the CORBA object, to that this operation must be applied.
   * @param operation the name of the method to invoke.
   *
   * @return the request.
   */
  public abstract Request request(org.omg.CORBA.Object target, String operation);

  /**
   * Create a request to invoke the method of this CORBA object.
   *
   * @param target the CORBA object, to that this operation must be applied.
   * @param operation the name of the method to invoke.
   * @param response_expected specifies if this is one way message or the
   * response to the message is expected.
   *
   * @return the stream where the method arguments should be written.
   */
  public org.omg.CORBA.portable.OutputStream request(org.omg.CORBA.Object target,
                                                     String operation,
                                                     boolean response_expected
                                                    )
  {
    throw new NO_IMPLEMENT(WHY);
  }

  /**
   * This method is always called after invoking the operation on the
   * local servant.
   *
   * The default method returns without action.
   *
   * @param self the object.
   * @param servant the servant.
   */
  public void servant_postinvoke(org.omg.CORBA.Object self,
                                 ServantObject servant
                                )
  {
  }

  /**
   * Returns a servant that should be used for this request.
   * The servant can also be casted to the expected type, calling the
   * required method directly.
   *
   * @param self the object
   * @param operation the operation
   * @param expectedType the expected type of the servant.
   *
   * This implementation always returns null; override for different
   * behavior.
   *
   * @return the servant or null if the servant is not an expected type
   * of the method is not supported, for example, due security reasons.
   */
  @SuppressWarnings("unchecked") // Needed for API compatibility
  public ServantObject servant_preinvoke(org.omg.CORBA.Object self,
                                         String operation, Class expectedType
                                        )
  {
    return null;
  }

  /**
   * Returns a new object with the new policies either replacing or
   * extending the current policies, depending on the second parameter.
   *
   * @param target the CORBA object, to that this operation must be applied.
   * @param policies the policy additions or replacements.
   * @param how either {@link SetOverrideType#SET_OVERRIDE} to override the
   * current policies of {@link SetOverrideType#ADD_OVERRIDE} to replace
   * them.
   *
   * @throws NO_IMPLEMENT, always (following the 1.4 specification).
   *
   * @return the new reference with the changed policies.
   */
  public org.omg.CORBA.Object set_policy_override(org.omg.CORBA.Object target,
                                                  Policy[] policies,
                                                  SetOverrideType how
                                                 )
  {
    throw new NO_IMPLEMENT(WHY);
  }

  /**
   * Return a string representation of the passed object.
   *
   * @param self the CORBA object, to that the string representation must be
   * returned. By default, the call is delegated to
   * {@link java.lang.Object#toString()}.
   *
   * @return the string representation.
   */
  public String toString(org.omg.CORBA.Object self)
  {
    if (self instanceof ObjectImpl)
      {
        ObjectImpl x = (ObjectImpl) self;
        CPStringBuilder b = new CPStringBuilder(x.getClass().getName());
        b.append(": [");
        for (int i = 0; i < x._ids().length; i++)
          {
            b.append(x._ids() [ i ]);
            b.append(" ");
          }
        b.append("]");
        return b.toString();
      }
    else
      return self.getClass().getName();
  }
}
