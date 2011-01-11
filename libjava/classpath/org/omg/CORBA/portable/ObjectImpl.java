/* ObjectImpl.java --
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

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.Context;
import org.omg.CORBA.ContextList;
import org.omg.CORBA.DomainManager;
import org.omg.CORBA.ExceptionList;
import org.omg.CORBA.NVList;
import org.omg.CORBA.NamedValue;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Policy;
import org.omg.CORBA.Request;
import org.omg.CORBA.SetOverrideType;

/**
 * The basic implementation of the CORBA Object. The most of the methods
 * delegate the functionality to the {@link Delegate} that can be replaced
 * by {@link #_set_delegate(Delegate)}.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public abstract class ObjectImpl
  implements org.omg.CORBA.Object
{
  /**
   * The delegate, responsible for the method implementations.
   */
  transient Delegate delegate;

  /**
    * Create a request to invoke the method of this object, specifying
    * context list and the list of the expected exception.
    *
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
  public Request _create_request(Context context, String operation,
                                 NVList parameters, NamedValue returns,
                                 ExceptionList exceptions, ContextList ctx_list
                                )
  {
    return delegate.create_request(this, context, operation, parameters,
                                   returns, exceptions, ctx_list
                                  );
  }

  /**
   * Create a request to invoke the method of this object.
   *
   * @param context a list of additional properties.
   * @param operation the name of method to be invoked.
   * @param parameters the method parameters.
   * @param returns the container for tge method returned value.
   *
   * @return the created reaquest.
   */
  public Request _create_request(Context context, String operation,
                                 NVList parameters, NamedValue returns
                                )
  {
    return delegate.create_request(this, context, operation, parameters, returns);
  }

  /**
   * Duplicate the object reference. This does not make much sense for
   * java platform and is just included for the sake of compliance with
   * CORBA APIs.
   *
   * The method may return the object reference itself.
   *
   * @return as a rule, <code>this</code>.
   */
  public org.omg.CORBA.Object _duplicate()
  {
    return delegate.duplicate(this);
  }

  /**
   * Get vendor specific delegate, responsible for the implemented
   * functionality.
   */
  public Delegate _get_delegate()
  {
    return delegate;
  }

  /**
   * Retrieve the domain managers for this object.
   *
   * @return the domain managers.
   */
  public DomainManager[] _get_domain_managers()
  {
    return delegate.get_domain_managers(this);
  }

  /**
   * Get the <code>InterfaceDef</code> for this Object.
   */
  public org.omg.CORBA.Object _get_interface_def()
  {
    return delegate.get_interface_def(this);
  }

  /**
   * Returns the {@link Policy}, applying to this object.
   *
   * @param a_policy_type a type of policy to be obtained.
   * @return a corresponding Policy object.
   *
   * @throws BAD_PARAM if the policy of the given type is not
   * associated with this object, or if it is not supported by this ORB.
   */
  public Policy _get_policy(int a_policy_type)
  {
    return delegate.get_policy(this, a_policy_type);
  }

  /**
   * Get the array of interface repository ids, defining this object.
   */
  public abstract String[] _ids();

  /**
   * Get the hashcode this object reference. The same hashcode still
   * does not means that the references are the same. From the other
   * side, two different references may still refer to the same CORBA
   * object. The returned value must not change during the object
   * lifetime.
   *
   * @param max the maximal value to return.
   *
   * @return the hashcode.
   */
  public int _hash(int max)
  {
    return delegate.hash(this, max);
  }

  /**
   * Invoke the operation.
   *
   * @param output the stream, containing the written arguments.
   *
   * @return the stream, from where the input parameters could be read.
   *
   * @throws ApplicationException if the application throws an exception,
   * defined as a part of its remote method definition.
   *
   * @throws RemarshalException if reading(remarshalling) fails.
   */
  public InputStream _invoke(OutputStream output)
                      throws org.omg.CORBA.portable.ApplicationException,
                             org.omg.CORBA.portable.RemarshalException
  {
    return delegate.invoke(this, output);
  }

  /**
   * Check if this object can be referenced by the given repository id.
   *
   * @param idl_id the repository id.
   *
   * @return true if the passed parameter is a repository id of this
   * CORBA object.
   */
  public boolean _is_a(String idl_id)
  {
    return delegate.is_a(this, idl_id);
  }

  /**
   * Return true if the other object references are equivalent, so far as
   * it is possible to determine this easily.
   *
   * @param other the other object reference.
   *
   * @return true if both references refer the same object, false
   * if they probably can refer different objects. Uses direct
   * comparison if the delegate has not been set.
   */
  public boolean _is_equivalent(org.omg.CORBA.Object other)
  {
    return (delegate == null) ? this == other
           : delegate.is_equivalent(this, other);
  }

  /**
   * Returns true if the object is local.
   *
   * @return false, always (following 1.4 specs). Override to get
   * functionality.
   */
  public boolean _is_local()
  {
    return delegate.is_local(this);
  }

  /**
  * Determines if the server object for this reference has already
  * been destroyed.
  *
  * @return true if the object has been destroyed, false otherwise.
  */
  public boolean _non_existent()
  {
    return delegate.non_existent(this);
  }

  /**
   * Provides the reference to ORB.
   *
   * @return the associated ORB.
   */
  public ORB _orb()
  {
    return delegate.orb(this);
  }

  /**
   * Free resoureces, occupied by this reference. The object implementation
   * is not notified, and the other references to the same object are not
   * affected.
   */
  public void _release()
  {
    delegate.release(this);
  }

  /**
   * Release the reply stream back to ORB after finishing reading the data
   * from it.
   *
   * @param stream the stream, normally returned by {@link #_invoke} or
   * {@link ApplicationException#getInputStream()}, can be null.
   */
  public void _releaseReply(InputStream stream)
  {
    if (delegate != null)
      delegate.releaseReply(this, stream);
  }

  /**
   * Create a request to invoke the method of this CORBA object.
   *
   * @param method the name of the method to invoke.
   *
   * @return the request.
   */
  public Request _request(String method)
  {
    return delegate.request(this, method);
  }

  /**
   * Create a request to invoke the method of this CORBA object.
   *
   * @param method the name of the method to invoke.
   * @param response_expected specifies if this is one way message or the
   * response to the message is expected.
   *
   * @return the stream where the method arguments should be written.
   */
  public org.omg.CORBA.portable.OutputStream _request(String method,
                                                      boolean response_expected
                                                     )
  {
    return delegate.request(this, method, response_expected);
  }

  /**
   * This method is always called after invoking the operation on the
   * local servant.
   *
   * The default method returns without action.
   *
   * @param servant the servant.
   */
  public void _servant_postinvoke(ServantObject servant)
  {
    delegate.servant_postinvoke(this, servant);
  }

  /**
   * Returns a servant that should be used for this request.
   * The servant can also be casted to the expected type, calling the
   * required method directly.
   *
   * @param method the operation
   * @param expected_type the expected type of the servant.
   *
   * This implementation always returns null; override for different
   * behavior.
   *
   * @return the servant or null if the servant is not an expected type
   * of the method is not supported, for example, due security reasons.
   */
  @SuppressWarnings("unchecked") // Needed for API compatibility
  public ServantObject _servant_preinvoke(String method, Class expected_type)
  {
    return delegate.servant_preinvoke(this, method, expected_type);
  }

  /**
   * Set the delegate, responsible for the implemented functionality.
   *
   * @param a_delegate a delegate, responsible for the implemented
   * functionality.
   */
  public void _set_delegate(Delegate a_delegate)
  {
    delegate = a_delegate;
  }

  /**
   * Returns a new object with the new policies either replacing or
   * extending the current policies, depending on the second parameter.
   *
   * @param policies the policy additions or replacements.
   * @param how either {@link SetOverrideType#SET_OVERRIDE} to override the
   * current policies of {@link SetOverrideType#ADD_OVERRIDE} to replace
   * them.
   */
  public org.omg.CORBA.Object _set_policy_override(Policy[] policies,
                                                   SetOverrideType how
                                                  )
  {
    return delegate.set_policy_override(this, policies, how);
  }

  /**
   * Check if this object is equal to another object.
   *
   * @param other the other object to compare.
   *
   * @return true if the objects are equal.
   */
  public boolean equals(java.lang.Object other)
  {
    if (delegate == null)
      return this == other;
    else
      return delegate.equals(this, other);
  }

  /**
   * Return the string representation of the passed object.
   *
   * @return the string representation.
   */
  public String toString()
  {
    return delegate.toString(this);
  }
}
