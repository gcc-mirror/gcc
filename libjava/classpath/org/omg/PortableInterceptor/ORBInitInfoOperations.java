/* ORBInitInfoOperations.java --
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


package org.omg.PortableInterceptor;

import org.omg.IOP.CodecFactory;
import org.omg.PortableInterceptor.ORBInitInfoPackage.DuplicateName;

/**
 * Defines operations, applicable to {@link ORBInitInfo}. The
 * {@link ORBInitInfo} is passed to the {@link ORBInitializer} that is
 * reponsible for registering an {@link Interceptor}.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface ORBInitInfoOperations
{
  /**
   * Register the client request interceptor.
   *
   * @param interceptor the interceptor to register.
   *
   * @throws DuplicateName if the interceptor name is not an empty string and an
   * interceptor with this name is already registered with the ORB being
   * created.
   */
  void add_client_request_interceptor(ClientRequestInterceptor interceptor)
    throws DuplicateName;

  /**
   * Register the IOR (object reference) interceptor. If the registered
   * interceptor implements the extended {@link IORInterceptor_3_0} interface,
   * ORB will call its additional methods, defined in the
   * {@link IORInterceptor_3_0Operations}.
   * 
   * @param interceptor the interceptor to register.
   * 
   * @throws DuplicateName if the interceptor name is not an empty string and an
   * interceptor with this name is already registered with the ORB being
   * created.
   */
  void add_ior_interceptor(IORInterceptor interceptor)
    throws DuplicateName;

  /**
   * Register the server request interceptor.
   *
   * @param interceptor the interceptor to register.
   *
   * @throws DuplicateName if the interceptor name is not an empty string and an
   * interceptor with this name is already registered with the ORB being
   * created.
   */
  void add_server_request_interceptor(ServerRequestInterceptor interceptor)
    throws DuplicateName;

  /**
   * Allocate a slot on a {@link PortableInterceptor.Current}. While slots can
   * be allocated by this method, they cannot be initialized.
   * {@link CurrentOperations#get_slot} and {@link CurrentOperations#set_slot}
   * throw {@link org.omg.CORBA.BAD_INV_ORDER} while called from the interceptor
   * initializer.
   *
   * @return the index to the slot that has been allocated.
   */
  int allocate_slot_id();

  /**
   * Returns the arguments passed to the ORB.init.
   *
   * @return the first parameter, passed to the method
   * {@link org.omg.CORBA.ORB#init}.
   */
  String[] arguments();

  /**
   * Get the CodecFactory that may be needed during the interceptor
   * initialization. The method ORB.resolve_initial_references ("CodecFactory")
   * cannot be used during ORB initialization.
   *
   * @return the CodecFactory.
   */
  CodecFactory codec_factory();

  /**
   * Returns the ID of the ORB being initialized.
   *
   * @return the ORB id that differs for each new ORB being created during the
   * current run of the java virtual machine.
   */
  String orb_id();

  /**
   * Register the initial reference. The registered object will be accessible by
   * the {@link ORB.resolve_initial_references} under the object_name.
   *
   * @param object_name the name of the object to register.
   * @param object the object to register.
   *
   * @throws org.omg.PortableInterceptor.ORBInitInfoPackage.InvalidName if the
   * name being registered is assumed to be invalid.
   */
  void register_initial_reference(String object_name,
    org.omg.CORBA.Object object
  ) throws org.omg.PortableInterceptor.ORBInitInfoPackage.InvalidName;

  /**
   * Identical to {@link org.omg.CORBA.ORB#resolve_initial_references}.
   *
   * This method can only be called from
   * {@link ORBInitializerOperations#post_init} and not during
   * {@link ORBInitializerOperations#pre_init}.
   *
   * @param object_name the name of the object to search.
   *
   * @return the object, accessible by the given name.
   *
   * @throws org.omg.PortableInterceptor.ORBInitInfoPackage.InvalidName if the
   * given name is not associated with the known object.
   *
   * @see org.omg.CORBA.ORB#resolve_initial_references
   */
  org.omg.CORBA.Object resolve_initial_references(String object_name)
    throws org.omg.PortableInterceptor.ORBInitInfoPackage.InvalidName;

  /**
   * Registers a PolicyFactory for the given PolicyType.
   *
   * @param policy_type the type of policy for that the factory is being
   * registered.
   * @param policy_factory the policy factory to register.
   *
   * @throws BAD_INV_ORDER minor 16 if the policy of the given type already has
   * the registered factory in this ORB.
   */
  void register_policy_factory(int policy_type, PolicyFactory policy_factory);
}