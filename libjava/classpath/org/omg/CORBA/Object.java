/* Object.java --
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


package org.omg.CORBA;

/**
 * The CORBA object reference. The object can be either local or remote.
 * For the local object, the methods of the derived object are called
 * like on any other java object. For the remote object, the reference
 * points to the stup (proxy), responsible for the remote invocation.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public interface Object
{
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
  Request _create_request(Context context, String operation, NVList parameters,
                          NamedValue returns
                         );

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
  Request _create_request(Context context, String operation, NVList parameters,
                          NamedValue returns, ExceptionList exceptions,
                          ContextList ctx_list
                         );

  /**
   * Duplicate the object reference. This does not make much sense for
   * java platform and is just included for the sake of compliance with
   * CORBA APIs.
   *
   * The method may return the object reference itself.
   *
   * @return as a rule, <code>this</code>.
   */
  org.omg.CORBA.Object _duplicate();

  /**
   * Retrieve the domain managers for this object.
   *
   * @return the domain managers.
   */
  DomainManager[] _get_domain_managers();

  /**
   * Get the <code>InterfaceDef</code> for this Object.
   */
  org.omg.CORBA.Object _get_interface_def();

  /**
   * Returns the {@link Policy}, applying to this object.
   *
   * @param a_policy_type a type of policy to be obtained.
   * @return a corresponding Policy object.
   *
   * @throws BAD_PARAM if the policy of the given type is not
   * associated with this object, or if it is not supported by this ORB.
   */
  Policy _get_policy(int a_policy_type)
              throws BAD_PARAM;

  /**
   * Get the hashcode this object reference. The same hashcode still
   * does not means that the references are the same. From the other
   * side, two different references may still refer to the same CORBA
   * object. The returned value must not change during the object
   * lifetime.
   *
   * @param maximum the maximal value to return.
   *
   * @return the hashcode.
   */
  int _hash(int maximum);

  /**
   * Check if this object can be referenced by the given repository id.
   *
   * @param repositoryIdentifer the repository id.
   *
   * @return true if the passed parameter is a repository id of this
   * CORBA object.
   */
  boolean _is_a(String repositoryIdentifer);

  /**
   * Return true if the other object references are equivalent, so far as
   * it is possible to determine this easily.
   *
   * @param other the other object reference.
   *
   * @return true if both references refer the same object, false
   * if they probably can refer different objects.
   */
  boolean _is_equivalent(org.omg.CORBA.Object other);

  /**
   * Determines if the server object for this reference has already
   * been destroyed.
   *
   * @return true if the object has been destroyed, false otherwise.
   */
  boolean _non_existent();

  /**
   * Free resoureces, occupied by this reference. The object implementation
   * is not notified, and the other references to the same object are not
   * affected.
   */
  void _release();

  /**
   * Create a request to invoke the method of this CORBA object.
   *
   * @param operation the name of the method to invoke.
   *
   * @return the request.
   */
  Request _request(String operation);

  /**
   * Returns a new object with the new policies either replacing or
   * extending the current policies, depending on the second parameter.
   *
   * @param policies the policy additions or replacements.
   *
   * @param how either {@link SetOverrideType#SET_OVERRIDE} to override the
   * current policies of {@link SetOverrideType#ADD_OVERRIDE} to replace
   * them.
   *
   * @return the new reference with the changed policies.
   */
  Object _set_policy_override(Policy[] policies, SetOverrideType how);
}
