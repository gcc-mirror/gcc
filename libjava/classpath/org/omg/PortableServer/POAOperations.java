/* POAOperations.java --
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


package org.omg.PortableServer;

import org.omg.CORBA.BAD_INV_ORDER;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.OBJ_ADAPTER;
import org.omg.CORBA.Policy;
import org.omg.CORBA.TRANSIENT;
import org.omg.PortableServer.POAPackage.AdapterAlreadyExists;
import org.omg.PortableServer.POAPackage.AdapterNonExistent;
import org.omg.PortableServer.POAPackage.InvalidPolicy;
import org.omg.PortableServer.POAPackage.NoServant;
import org.omg.PortableServer.POAPackage.ObjectAlreadyActive;
import org.omg.PortableServer.POAPackage.ObjectNotActive;
import org.omg.PortableServer.POAPackage.ServantAlreadyActive;
import org.omg.PortableServer.POAPackage.ServantNotActive;
import org.omg.PortableServer.POAPackage.WrongAdapter;
import org.omg.PortableServer.POAPackage.WrongPolicy;

/**
 * Defines the operations, applicable to the POA.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface POAOperations
{
  /**
   * Creates a new POA as a child of the target POA.
   *
   * @param child_name the name of the child POA being created.
   * @param manager the manager that will control the new POA. If this parameter
   * is null, a new POA manager is created and associated with the new POA.
   *
   * @param policies the policies, applicable for the parent POA. Policies
   * are <i>not</i> inherited from the parent POA. If some policy type
   * is missing in the array (or the zero size array is passed), the missing
   * policies obtain the default values from the table, specified
   * in the {@link POA} documentation header.
   *
   * @return an newly created POA. The POA will be intially in the holding
   * state and must be activated to start processing requests.
   *
   * @throws AdapterAlreadyExists if the child with the given child_name
   * already exists for the current POA.
   * @throws InvalidPolicy if the policies conflict with each other or are
   * otherwise inappropriate.
   *
   * @see POA for the list of required policies.
   * @see #the_children()
   */
  POA create_POA(String child_name, POAManager manager, Policy[] policies)
          throws AdapterAlreadyExists, InvalidPolicy;

  /**
  * Find and optionally activate the child POA with the given name.
  *
  * @param poa_name the name of the POA to find.
  * @param activate_it if the child with the specified name is not found
  * or inactive and this parameter is true, the target POA activator is
  * invoked to activate that child. If this succeeds, that child POA
  * is returned.
  *
  * @throws AdapterNonExistent if no active child with the given name
  * is found and one of the following is true:
  * a) the target POA has no associated
  * {@link AdapterActivator}. b) that activator fails to activate the
  * child POA. c) <code>activate_id</code> = false.
  */
  POA find_POA(String poa_name, boolean activate_it)
        throws AdapterNonExistent;

  /**
   * Generate the Object Id for the given servant and add the servant to
   * the Active Object Map using this Id a a key. If the servant
   * activator is set, its incarnate method will be called. In this case,
   * the passed servant in this method can be null; in this case, the servant,
   * returned by {@link ServantActivatorOperations#incarnate} will
   * be used.
   *
   * @param a_servant a servant that would serve the object with the
   * returned Object Id.
   *
   * @return the generated objert Id for the given servant.
   *
   * @throws ServantAlreadyActive if this servant is already in the
   * Active Object Map and the UNIQUE_ID policy applies.
   *
   * @throws WrongPolicy if the required policies SYSTEM_ID and RETAIN
   * do not apply to this POA.
   */
  byte[] activate_object(Servant a_servant)
                  throws ServantAlreadyActive, WrongPolicy;

  /**
   * Add the given servant to the Active Object Map as a servant for the
   * object with the provided Object Id. If the servant activator is
   * set, its incarnate method will be called. In this case,
   * the passed servant in this method can be null; in this case, the servant,
   * returned by {@link ServantActivatorOperations#incarnate} will
   * be used.
   *
   * @param an_Object_Id an object id for the given object.
   * @param a_servant a servant that will serve the object with the given
   * Object Id.
   *
   * @throws ObjectAlreadyActive if the given object id is already in the
   * Active Object Map.
   * @throws WrongPolicy if the required RETAIN policy does not apply to
   * this POA.
   * @throws BAD_PARAM if the passed object id is invalid due any reason.
   */
  void activate_object_with_id(byte[] an_Object_Id, Servant a_servant)
                        throws ServantAlreadyActive, ObjectAlreadyActive,
                               WrongPolicy;

  /**
   * <p>Deactivate object with the given id. Client, trying to call
   * method on the deactivated object will either receive the remote
   * exception ({@link org.omg.CORBA.OBJECT_NOT_EXIST}, minor 0x535503ec),
   * incomplete) or the object will be reactivated and serve the request.
   * The object can be reactivated only if the implicit activation
   * policy applies and the servant activator is set.</p><p>
   * The deactivated object will continue to process requests that arrived
   * before decativation.
   * If this POA has the associated servant manager, a
   * {@link ServantActivatorOperations#etherealize} is <i>immediately</i>
   * invoked on the passed id. The deactivated object can be reactivated
   * by {@link #activate_object_with_id}.</p>
   * <p>The deactivation will not release thread, port or memory resources,
   * taken by that object. This is due requirement to make the
   * object reactivation possible at any time. To release the resources,
   * you must destroy the POA.
   * </p>
   *
   * @throws WrongPolicy if the required RETAIN policy does not apply to
   * this POA.
   */
  void deactivate_object(byte[] the_Object_Id)
                  throws ObjectNotActive, WrongPolicy;

  /**
  * Create the object reference, encapsulating the given repository Id and
  * the Object Id, generated by this POA. The returned object will not be
  * activated by default and may be activated on the first invocation by
  * the servant manager (if it is set and if policies are applicable).
  * The returned object can also be narrowed by helper and used locally.
  * In this case, the servant will be activated on the first local call of
  * any method. The methods on returned object can also be invoked by
  * name, using {@link org.omg.CORBA.Request}.
  *
  * @param a_repository_id the repository id for the given object. When
  * narrowing the returned object with some helper, it will be checked for
  * equality with value, returned by the the helper id().
  *
  * @throws WrongPolicy if the required SYSTEM_ID policy does not apply to
  * this POA.
  */
  org.omg.CORBA.Object create_reference(String a_repository_id)
                                 throws WrongPolicy;

  /**
  * <p> Create the object reference, encapsulating the given repository Id and
  * the given Object Id. The returned object will not be
  * activated by default and may be activated on the first invocation by
  * the servant manager (if it is set and if policies are applicable).
  * </p><p>
  * The returned object can also be narrowed by helper and used locally.
  * In this case, the servant will be activated on the first local call of
  * any method. The methods on returned object can also be invoked by
  * name, using {@link org.omg.CORBA.Request}.
  * </p>
  *
  * @param an_object_id the object id for the object being created.
  * If the POA uses the SYSTEM_ID policy, the portable application
  * must only supply ids, generated by that POA.
  *
  * @param a_repository_id the repository id for the given object. When
  * narrowing the returned object with some helper, it will be checked for
  * equality with value, returned by the the helper id().
  */
  org.omg.CORBA.Object create_reference_with_id(byte[] an_object_id,
                                                String a_repository_id
                                               );

  /**
   * Returns a default servant for this POA.
   *
   * @return a servant that will be used for requests for
   * which no servant is found in the Active Object Map.
   *
   * @throws NoServant if there is no default servant associated with this POA.
   * @throws WrongPolicy if the USE_DEFAULT_SERVANT policy is not active.
   */
  Servant get_servant()
               throws NoServant, WrongPolicy;

  /**
   * Sets the default servant for this POA.
   *
   * @param a_servant a servant that will be used for requests for
   * which no servant is found in the Active Object Map.
   *
   * @throws WrongPolicy if the USE_DEFAULT_SERVANT policy is not active.
   */
  void set_servant(Servant a_servant)
            throws WrongPolicy;

  /**
   * Set a servant manager for this POA.
   *
   * @param a_manager servant manager being set. If the RETAIN policy applies, the
   * manager must implement a {@link ServantActivator}. If the NON_RETAIN
   * policy applies, the manager must implement a {@link ServantLocator}.
   *
   * @throws WrongPolicy if the required USE_SERVANT_MANAGER policy does not
   * apply to this POA.
   *
   * @throws OBJ_ADAPTER minor code 4 if the passed manager does not
   * implement the required interface ({@link ServantActivator},
   * {@link ServantLocator}).
   *
   * @throws BAD_INV_ORDER minor code 6 if the method is called more than once
   * on the same POA. The manager can be set only once.
   */
  void set_servant_manager(ServantManager a_manager)
                    throws WrongPolicy;

  /**
   * Get the servant manager, associated with this POA.
   *
   * @return the associated servant manager or null if it has
   * been previously set.
   *
   * @throws WrongPolicy if the required USE_SERVANT_MANAGER policy does not
   * apply to this POA.
   */
  ServantManager get_servant_manager()
                              throws WrongPolicy;

  /**
   * Get the unique Id of the POA in the process in which it is created.
   * This Id is needed by portable interceptors. The id is unique
   * for the life span of the POA in the process. For persistent
   * POAs, if a POA is created in the same path with the same name as
   * another POA, these POAs are identical have the same id. All transient
   * POAs are assumed unique.
   */
  byte[] id();

  /**
   * Returns the reference to the active object with the given Id.
   *
   * @param the_Object_Id the object id.
   *
   * @throws ObjectNotActive if there is no active object with such Id.
   * @throws WrongPolicy if the required RETAIN policy does not apply to
   * this POA.
   */
  org.omg.CORBA.Object id_to_reference(byte[] the_Object_Id)
                                throws ObjectNotActive, WrongPolicy;

  /**
   * Returns the servant that serves the active object with the given Id.
   *
   * @param the_Object_Id the object id.
   *
   * @throws ObjectNotActive if there is no active object with such Id.
   * @throws WrongPolicy This method requires either RETAIN or
   * USE_DEFAULT_SERVANT policies and reaises the WrongPolicy if none of them
   * apply to this POA.
   */
  Servant id_to_servant(byte[] the_Object_Id)
                 throws ObjectNotActive, WrongPolicy;

  /**
   * Returns the Object Id, encapsulated in the given object reference.
   *
   * @param the_Object the object that has been previously created with this
   * POA. It need not be active.
   *
   * @throws WrongAdapter if the passed object has not been previously created
   * with this POA.
   * @throws WrongPolicy never (declared for the future extensions only).
   */
  byte[] reference_to_id(org.omg.CORBA.Object the_Object)
                  throws WrongAdapter, WrongPolicy;

  /**
   * Returns the servant that is serving this object.
   *
   * @return if the RETAIN policy applies and the object is in the Active
   * Object Map, the method returns the servant, associated with this object.
   * Otherwise, if the USE_DEFAULT_SERVANT policy applies, the method returns
   * the default servant (if one was set).
   *
   * @throws ObjectNotActive if none of the conditions above are satisfied.
   * @throws WrongAdapter if the object reference was not created with this POA.
   * @throws WrongPolicy This method requires either RETAIN or
   * USE_DEFAULT_SERVANT policies and reaises the WrongPolicy if none of them
   * apply to this POA.
   */
  Servant reference_to_servant(org.omg.CORBA.Object the_Object)
                        throws ObjectNotActive, WrongPolicy, WrongAdapter;

  /**
  * Returns the id of the object, served by the given servant. The id is found
  * in one of the following ways.
  * <ul>
  * <li>If the POA has both the RETAIN and the UNIQUE_ID policy and
  * the specified servant is active, the method return the Object Id associated
  * with that servant.
  * </li><li>
  * If the POA has both the RETAIN and the IMPLICIT_ACTIVATION policy and
  * either the POA has the MULTIPLE_ID policy or the specified servant is
  * inactive, the method activates the servant using a POA-generated Object Id
  * and the Interface Id associated with the servant, and returns that
  * Object Id.
  * </li>
  * <li>If the POA has the USE_DEFAULT_SERVANT policy, the servant specified
  * is the default servant, and the method is being invoked in the context o
  * f executing a request on the default servant, the method returns the
  * ObjectId associated with the current invocation.
  * </li>
  * </ul>
  * @throws ServantNotActive in all cases, not listed in the list above.
  * @throws WrongPolicy The method requres USE_DEFAULT_SERVANT policy or
  * a combination of the RETAIN policy and either the UNIQUE_ID or
  * IMPLICIT_ACTIVATION policies and throws the WrongPolicy if these conditions
  * are not satisfied.
  */
  byte[] servant_to_id(Servant the_Servant)
                throws ServantNotActive, WrongPolicy;

  /**
   * <p>Converts the given servant to the object reference.
   * The servant will serve all methods, invoked on the returned object.
   * The returned object reference can be passed to the remote client,
   * enabling remote invocations.
   * </p><p>
   * If the specified servant already serves some active object, that
   * object is returned. Otherwise,
   * if the POA has the IMPLICIT_ACTIVATION policy the method activates
   * the servant, creating an new object with the POA-generated Object Id.
   * In this case, if the servant activator is set, the
   * {@link ServantActivatorOperations#incarnate} method will be called.
   * </p>
   *
   * @throws ServantNotActive if the servant is inactive and no
   * IMPLICIT_ACTIVATION policy applies.
   * @throws WrongPolicy This method needs the RETAIN policy and either the
   * UNIQUE_ID or IMPLICIT_ACTIVATION policies.
   *
   * @return the object, exposing the given servant in the context of this POA.
   */
  org.omg.CORBA.Object servant_to_reference(Servant the_Servant)
                                     throws ServantNotActive, WrongPolicy;

  /**
   * Return the POA manager, associated with this POA.
   *
   * @return the associated POA manager (always available).
   */
  POAManager the_POAManager();

  /**
   * Returns the adapter activator, associated with this POA.
   * The newly created POA has no activator (null would be
   * returned). The ORB root POA also initially has no activator.
   *
   * @return tha adapter activator or null if this POA has no
   * associated adapter activator.
   */
  AdapterActivator the_activator();

  /**
  * Set the adapter activator for this POA.
  *
  * @param activator the activator being set.
  */
  void the_activator(AdapterActivator activator);

  /**
  * The children of this POA.
  *
  * @return the array of all childs for this POA.
  */
  POA[] the_children();

  /**
   * Return the name of this POA.
   *
   * @return the name of POA, relative to its parent.
   */
  String the_name();

  /**
   * Return the parent of this POA.
   *
   * @return the parent POA or <code>null</code> if this is a root POA.
   */
  POA the_parent();

  /**
   * <p> Destroy this POA and all descendant POAs. The destroyed POAs can be
   * later re-created via {@link AdapterActivator} or by invoking
   * {@link #create_POA}.
   * This differs from {@link POAManagerOperations#deactivate} that does
   * not allow recreation of the deactivated POAs. After deactivation,
   * recreation is only possible if the POAs were later destroyed.
   * </p><p>
   * The remote invocation on the target, belonging to the POA that is
   * currently destroyed return the remote exception ({@link TRANSIENT},
   * minor code 4).
   * </p>
   * @param etherealize_objects if true, and POA has RETAIN policy, and the
   * servant manager is available, the servant manager method
   * {@link ServantActivatorOperations#etherealize} is called for each
   *  <i>active</i> object in the Active Object Map. This method should not
   * try to access POA being destroyed. If <code>destroy</code> is called
   * multiple times before the destruction completes,
   * the etherialization should be invoked only once.
   *
   * @param wait_for_completion if true, the method waits till the POA being
   * destroyed completes all current requests and etherialization. If false,
   * the method returns immediately.
   */
  void destroy(boolean etherealize_objects, boolean wait_for_completion);

  /**
   * Create the IdUniquenessPolicy policy.
   *
   * @param a_value states which one Id uniqueness policy will apply.
   *
   * @return the created policy.
   */
  IdUniquenessPolicy create_id_uniqueness_policy(IdUniquenessPolicyValue a_value);

  /**
   * Create the ImplicitActivationPolicy policy.
   *
   * @param a_value states which one activation policy will apply.
   *
   * @return the created policy.
   */
  ImplicitActivationPolicy create_implicit_activation_policy(ImplicitActivationPolicyValue a_value);

  /**
   * Create the LifespanPolicy policy.
   *
   * @param a_value states which one object lifespan policy will apply.
   *
   * @return the created policy.
   */
  LifespanPolicy create_lifespan_policy(LifespanPolicyValue a_value);

  /**
   * Create the RequestProcessingPolicy policy.
   *
   * @param a_value states which one request processing policy will apply.
   *
   * @return the created policy.
   */
  RequestProcessingPolicy create_request_processing_policy(RequestProcessingPolicyValue a_value);

  /**
   * Create the ServantRetentionPolicy policy.
   *
   * @param a_value states which one servant retention policy will apply.
   *
   * @return the created policy.
   */
  ServantRetentionPolicy create_servant_retention_policy(ServantRetentionPolicyValue a_value);

  /**
   * Create the ThreadPolicy policy.
   *
   * @param a_value states which one thread policy will apply.
   *
   * @return the created policy.
   */
  ThreadPolicy create_thread_policy(ThreadPolicyValue a_value);

  /**
  * Create the ID assignment policy with the given value.
  *
  * @param value states which one ID assignment policy will apply.
  *
  * @return the created policy.
  */
  IdAssignmentPolicy create_id_assignment_policy(IdAssignmentPolicyValue value);

}
