/* gnuAbstractPOA.java --
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


package gnu.CORBA.Poa;

import gnu.java.lang.CPStringBuilder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;

import org.omg.CORBA.BAD_INV_ORDER;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.LocalObject;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.OBJ_ADAPTER;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;
import org.omg.CORBA.Policy;
import org.omg.CORBA.SetOverrideType;
import org.omg.CORBA.TRANSIENT;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.PortableInterceptor.NON_EXISTENT;
import org.omg.PortableInterceptor.ObjectReferenceFactory;
import org.omg.PortableInterceptor.ObjectReferenceTemplate;
import org.omg.PortableInterceptor.ObjectReferenceTemplateHelper;
import org.omg.PortableServer.AdapterActivator;
import org.omg.PortableServer.ForwardRequest;
import org.omg.PortableServer.IdAssignmentPolicy;
import org.omg.PortableServer.IdAssignmentPolicyValue;
import org.omg.PortableServer.IdUniquenessPolicy;
import org.omg.PortableServer.IdUniquenessPolicyValue;
import org.omg.PortableServer.ImplicitActivationPolicy;
import org.omg.PortableServer.ImplicitActivationPolicyValue;
import org.omg.PortableServer.LifespanPolicy;
import org.omg.PortableServer.LifespanPolicyValue;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAManager;
import org.omg.PortableServer.RequestProcessingPolicy;
import org.omg.PortableServer.RequestProcessingPolicyValue;
import org.omg.PortableServer.SERVANT_RETENTION_POLICY_ID;
import org.omg.PortableServer.Servant;
import org.omg.PortableServer.ServantActivator;
import org.omg.PortableServer.ServantLocator;
import org.omg.PortableServer.ServantManager;
import org.omg.PortableServer.ServantRetentionPolicy;
import org.omg.PortableServer.ServantRetentionPolicyValue;
import org.omg.PortableServer.ThreadPolicy;
import org.omg.PortableServer.ThreadPolicyValue;
import org.omg.PortableServer.POAManagerPackage.State;
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

import gnu.CORBA.OrbFunctional;
import gnu.CORBA.CDR.BufferredCdrInput;
import gnu.CORBA.CDR.BufferedCdrOutput;

/**
 * Our POA implementation.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuPOA
  extends LocalObject
  implements POA, ObjectReferenceFactory
{
  /**
   * The object reference template, associated with this POA.
   *
   * @since 1.5
   */
  class RefTemplate implements ObjectReferenceTemplate
  {
    /**
     * Use serialVersionUID for interoperability.
     */
    private static final long serialVersionUID = 1;

    RefTemplate()
    {
      // The adapter name is computed once.
      ArrayList names = new ArrayList();
      names.add(the_name());

      POA poa = the_parent();
      while (poa != null)
        {
          names.add(poa.the_name());
          poa = poa.the_parent();
        }

      // Fill in the string array in reverse (more natural) order,
      // root POA first.
      m_adapter_name = new String[names.size()];

      for (int i = 0; i < m_adapter_name.length; i++)
        m_adapter_name[i] = (String) names.get(m_adapter_name.length - i - 1);
    }

    /**
     * The adapter name
     */
    final String[] m_adapter_name;

    /**
     * Get the name of this POA.
     */
    public String[] adapter_name()
    {
      return (String[]) m_adapter_name.clone();
    }

    /**
     * Get the ORB id.
     */
    public String orb_id()
    {
      return m_orb.orb_id;
    }

    /**
     * Get the server id.
     */
    public String server_id()
    {
      return OrbFunctional.server_id;
    }

    /**
     * Create the object.
     */
    public Object make_object(String repositoryId, byte[] objectId)
    {
      return create_reference_with_id(objectId, repositoryId);
    }

    /**
     * Get the array of truncatible repository ids.
     */
    public String[] _truncatable_ids()
    {
      return ref_template_ids;
    }
  }

  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * The adapter reference template.
   */
  ObjectReferenceTemplate refTemplate;

  /**
   * The reference template repository ids. Defined outside the class as it
   * cannot have static members.
   */
  final static String[] ref_template_ids =
    new String[] { ObjectReferenceTemplateHelper.id() };

  /**
   * The active object map, mapping between object keys, objects and servants.
   */
  public final AOM aom = new AOM();

  /**
   * The children of this POA.
   */
  final ArrayList children = new ArrayList();

  /**
   * The name of this POA.
   */
  final String name;

  /**
   * The parent of this POA (null for the root POA).
   */
  final POA parent;

  /**
   * The ior key signature, indicating, that the ior key is encoded using
   * internal agreements of this implementation (0x'free').
   */
  static final int SIGNATURE = 0x66726565;

  /**
   * The adapter activator for this POA, null if no activator is set.
   */
  AdapterActivator m_activator;

  /**
   * The POA manager for this POA.
   */
  POAManager m_manager;

  /**
   * The servant manager (servant activator) for this POA.
   */
  ServantActivator servant_activator;

  /**
   * The servant manager (servant locator) for this POA.
   */
  ServantLocator servant_locator;

  /**
   * The default servant, if on is in use.
   */
  Servant default_servant;

  /**
   * The cached poa id value, computed once.
   */
  private byte[] m_poa_id;

  /**
   * The all policy values that apply to this POA.
   * The used policy values are singletons, unique between policies.
   */
  private final HashSet m_policies;

  /**
   * An array of the set policies.
   */
  Policy[] s_policies;

  /**
   * The ORB, where the POA is connected.
   */
  final ORB_1_4 m_orb;

  /**
   * When true, the POA is being destroyed or is destroyed.
   */
  boolean m_inDestruction;

  /**
   * True if the active object map is used by this POA.
   * The value is moved into separate boolean value due
   * necessity of the frequent checks.
   */
  public final boolean retain_servant;

  /**
   * The object reference factory, used to create the new object
   * references.
   */
  ObjectReferenceFactory m_object_factory = this;

  /**
   * Create a new abstract POA.
   *
   * @param a_parent the parent of this POA.
   * @param a_name a name for this POA.
   * @param a_manager a manager for this POA. If null, a new
   * {@link gnuPOAManager} will be instantiated.
   * @param a_policies an array of policies that apply to this POA.
   * @param an_orb an ORB for this POA.
   */
  public gnuPOA(gnuPOA a_parent, String a_name, POAManager a_manager,
                Policy[] a_policies, ORB_1_4 an_orb
               )
         throws InvalidPolicy
  {
    // Add default policies.
    Policy[] all_policies = StandardPolicies.withDefault(a_policies);

    name = a_name;
    parent = a_parent;
    m_orb = an_orb;

    if (a_manager != null)
      m_manager = a_manager;
    else
      m_manager = new gnuPOAManager();

    if (m_manager instanceof gnuPOAManager)
      {
        gnuPOAManager g = (gnuPOAManager) m_manager;
        g.addPoa(this);
      }

    m_policies = new HashSet(all_policies.length);

    s_policies = new Policy[ all_policies.length ];
    for (int i = 0; i < s_policies.length; i++)
      {
        s_policies [ i ] = all_policies [ i ].copy();
        m_policies.add(((AccessiblePolicy) s_policies [ i ]).getValue());
      }

    retain_servant = applies(ServantRetentionPolicyValue.RETAIN);

    validatePolicies(a_policies);

    refTemplate = new RefTemplate();
  }

  /**
   * Wait while at least one of the threads in this POA is actively
   * processing one of requests.
   */
  public void waitWhileRunning()
  {
    // First pause.
    long time = 1;

    // Maximal duration between checks.
    long max = 500;

    boolean runs;

    do
      {
        runs = m_orb.currents.has(this);

        if (runs)
          {
            // Avoid taking CPU resources
            // from the thread that is running.
            try
              {
                Thread.sleep(time);
                time = time * 2;
                if (time > max)
                  time = max;
              }
            catch (InterruptedException ex)
              {
              }
          }
      }
    while (runs);
  }

  /**
   * Etherealize all objects, associated with this POA. Invoked from the
   * {@link gnuPOAManager} only if it is known that the servant_activator
   * holds non-null value.
   */
  protected void etherealizeAll()
  {
    if (servant_activator == null)
      return;

    ArrayList keys = new ArrayList();
    keys.addAll(aom.keySet());

    byte[] key;
    AOM.Obj obj;
    boolean last;
    for (int i = 0; i < keys.size(); i++)
      {
        key = (byte[]) keys.get(i);
        obj = aom.get(key);

        if (obj.poa == this)
          {
            aom.remove(key);

            if (!obj.isDeactiveted())
              {
                // Check if the servant still stays under the other key.
                last = aom.findServant(obj.servant) == null;
                servant_activator.etherealize(obj.key, this, obj.servant, true,
                                              last
                                             );
              }
          }
      }
  }

  /**
   * Create an instance of the POA with the given features.
   * This method is not responsible for duplicate checking
   * or adding the returned instance to any possible table.
   *
   * @param child_name the name of the poa being created.
   * @param a_manager the poa manager (never null).
   * @param policies the array of policies.
   * @param an_orb the ORB for this POA.
   *
   * @return the created POA.
   *
   * @throws InvalidPolicy for conflicting or otherwise invalid policies.|
   */
  protected POA createPoaInstance(String child_name, POAManager a_manager,
                                  Policy[] policies, ORB_1_4 an_orb
                                 )
                           throws InvalidPolicy
  {
    POAManager some_manager =
      a_manager == null ? new gnuPOAManager() : a_manager;

    if (some_manager instanceof gnuPOAManager)
      {
        ((gnuPOAManager) some_manager).addPoa(this);
      }

    return new gnuPOA(this, child_name, some_manager, policies, an_orb);
  }

  /**
   * Check if the given policy value applies to this POA.
   *
   * @param policy_value a policy value to check. The policy values are
   * singletons and unique between the different policies, so the policy
   * type is not passed.
   *
   * @return true if the policy value applies, false otherwise.
   */
  public final boolean applies(java.lang.Object policy_value)
  {
    return m_policies.contains(policy_value);
  }

  /**
   * Check for the presence of the required policy.
   *
   * @param policy_value a policy value to check.
   *
   * @throws WrongPolicy if the required policy value is not applicable.
   */
  public final void required(java.lang.Object policy_value)
                      throws WrongPolicy
  {
    if (!applies(policy_value))
      throw new WrongPolicy(policy_value + " policy required.");
  }

  /**
   * Check for the absence of the given policy.
   *
   * @param policy_value a policy value to check.
   *
   * @throws WrongPolicy if the passed policy value is applicable.
   */
  public final void excluding(java.lang.Object policy_value)
                       throws WrongPolicy
  {
    if (applies(policy_value))
      throw new WrongPolicy(policy_value + " policy applies.");
  }

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
  public POA find_POA(String poa_name, boolean activate_it)
               throws AdapterNonExistent
  {
    POA child;
    for (int i = 0; i < children.size(); i++)
      {
        child = (POA) children.get(i);
        if (child.the_name().equals(poa_name))
          return child;
      }

    if (activate_it && m_activator != null)
      {
        boolean activated = m_activator.unknown_adapter(this, poa_name);
        if (!activated)
          throw new AdapterNonExistent(poa_name + " activation failed.");

        // Tha activator should add the child to the childrent table.
        for (int i = 0; i < children.size(); i++)
          {
            child = (POA) children.get(i);
            if (child.the_name().equals(poa_name))
              return child;
          }
        throw new AdapterNonExistent(poa_name + " not created. ");
      }
    else
      throw new AdapterNonExistent(poa_name);
  }

  /**
   * Generate the Object Id for the given servant and add the servant to the
   * Active Object Map using this Id a a key. If the servant activator is set,
   * its incarnate method will be called.
   *
   * @param a_servant a servant that would serve the object with the returned
   * Object Id. If null is passed, under apporoprate policies the servant
   * activator is invoked.
   *
   * @return the generated objert Id for the given servant.
   *
   * @throws ServantAlreadyActive if this servant is already in the Active
   * Object Map and the UNIQUE_ID policy applies.
   *
   * @throws WrongPolicy if the required policies SYSTEM_ID and RETAIN do not
   * apply to this POA.
   */
  public byte[] activate_object(Servant a_servant)
    throws ServantAlreadyActive, WrongPolicy
  {
    checkDiscarding();
    required(ServantRetentionPolicyValue.RETAIN);
    required(IdAssignmentPolicyValue.SYSTEM_ID);

    AOM.Obj exists = aom.findServant(a_servant);

    if (exists != null)
      {
        if (exists.isDeactiveted())
          {
            // If exists but deactivated, activate and return
            // the existing key.
            exists.setDeactivated(false);
            incarnate(exists, exists.key, a_servant, false);
            return exists.key;
          }
        else if (applies(IdUniquenessPolicyValue.UNIQUE_ID))
          throw new ServantAlreadyActive();

        // It multiple ids are allowed, exit block allowing repetetive
        // activations.
      }

    byte[] object_key = AOM.getFreeId();
    ServantDelegateImpl delegate = new ServantDelegateImpl(a_servant, this,
      object_key);
    create_and_connect(object_key,
      a_servant._all_interfaces(this, object_key)[0], delegate);
    return object_key;
  }

  /**
   * Add the given servant to the Active Object Map as a servant for the object
   * with the provided Object Id. If the servant activator is set, its incarnate
   * method will be called.
   *
   * @param an_Object_Id an object id for the given object.
   * @param a_servant a servant that will serve the object with the given Object
   * Id. If null is passed, under apporoprate policies the servant activator is
   * invoked.
   *
   * @throws ObjectAlreadyActive if the given object id is already in the Active
   * Object Map.
   * @throws ServantAlreadyActive if the UNIQUE_ID policy applies and this
   * servant is already in use.
   * @throws WrongPolicy if the required RETAIN policy does not apply to this
   * POA.
   * @throws BAD_PARAM if the passed object id is invalid due any reason.
   */
  public void activate_object_with_id(byte[] an_Object_Id, Servant a_servant)
                               throws ServantAlreadyActive, ObjectAlreadyActive,
                                      WrongPolicy
  {
    activate_object_with_id(an_Object_Id, a_servant, false);
  }

  /**
   * Same as activate_object_with_id, but permits gnuForwardRequest forwarding
   * exception. This is used when the activation is called from the remote
   * invocation context and we have possibility to return the forwarding
   * message.
   */
  public void activate_object_with_id(byte[] an_Object_Id, Servant a_servant,
    boolean use_forwarding)
    throws ServantAlreadyActive, ObjectAlreadyActive, WrongPolicy
  {
    checkDiscarding();
    required(ServantRetentionPolicyValue.RETAIN);

    // If the UNIQUE_ID applies, the servant being passed must not be
    // already active.
    if (applies(IdUniquenessPolicyValue.UNIQUE_ID))
      {
        AOM.Obj sx = aom.findServant(a_servant, false);
        if (sx != null)
          throw new ServantAlreadyActive();
      }

    AOM.Obj exists = aom.get(an_Object_Id);
    if (exists != null)
      {
        if (exists.servant == null)
          {
            locateServant(an_Object_Id, a_servant, exists, use_forwarding);
            exists.setDeactivated(false);
          }
        else if (exists.isDeactiveted())
          {
            exists.setDeactivated(false);
            incarnate(exists, an_Object_Id, a_servant, use_forwarding);
          }
        else
          throw new ObjectAlreadyActive();
      }
    else
      {
        ServantDelegateImpl delegate = new ServantDelegateImpl(a_servant, this,
          an_Object_Id);
        create_and_connect(an_Object_Id, a_servant._all_interfaces(this,
          an_Object_Id)[0], delegate);
      }
  }

  /**
   * Locate the servant for this object Id and connect it to ORB.
   *
   * @param an_Object_Id the object id.
   * @param a_servant the servant (may be null).
   * @param exists an existing active object map entry.
   * @param use_forwarding allow to throw the gnuForwardRequest if the activator
   * throws ForwardRequest.
   *
   * @throws OBJ_ADAPTER minor 4 if the servant cannot be located (the required
   * servant manager may be missing).
   */
  private void locateServant(byte[] an_Object_Id, Servant a_servant,
                             AOM.Obj exists, boolean use_forwarding
                            )
                      throws InternalError
  {
    // An object was created with create_reference.
    gnuServantObject object = (gnuServantObject) exists.object;
    if (servant_activator != null)
      {
        exists.setServant(incarnate(exists, an_Object_Id, a_servant,
                                    use_forwarding
                                   )
                         );
      }
    else if (default_servant != null)
      {
        exists.setServant(default_servant);
      }
    if (exists.servant == null)
      {
        exists.setServant(a_servant);
      }
    if (exists.servant == null)
      {
        throw new OBJ_ADAPTER("no servant", 4, CompletionStatus.COMPLETED_NO);
      }

    ServantDelegateImpl delegate =
      new ServantDelegateImpl(exists.servant, this, an_Object_Id);
    exists.servant._set_delegate(delegate);
    object.setServant(exists.servant);
    connect_to_orb(an_Object_Id, delegate.object);
  }

  /**
   * Deactivate object with the given id.
   *
   * The deactivated object will continue to process requests that arrived
   * before decativation. If this POA has the associated
   * servant manager, a {@link ServantActivatorOperations#etherealize} is
   * immediately invoked on the passed id.
   *
   * @throws WrongPolicy if the required RETAIN policy does not apply to
   * this POA.
   */
  public void deactivate_object(byte[] the_Object_Id)
                         throws ObjectNotActive, WrongPolicy
  {
    required(ServantRetentionPolicyValue.RETAIN);

    AOM.Obj exists = aom.get(the_Object_Id);

    if (exists == null || exists.isDeactiveted())
      throw new ObjectNotActive();

    exists.setDeactivated(true);

    // Check if this servant is serving something else.
    aom.remove(the_Object_Id);

    AOM.Obj other = aom.findServant(exists.servant, false);

    boolean remaining = other != null;

    aom.put(exists);

    if (servant_activator != null)
      servant_activator.etherealize(the_Object_Id, this, exists.servant, false,
                                    remaining
                                   );
  }

  /**
  * Create the object reference, encapsulating the given repository Id and
  * the Object Id, generated by this POA. The returned object will not be
  * activated by default and may be activated on the first invocation by
  * the servant manager (if it is set and if policies are applicable).
  *
  * @param a_repository_id the repository id for the given object, can
  * be null if to be requested from the servant later.
  *
  * @throws WrongPolicy if the required SYSTEM_ID policy does not apply to
  * this POA.
  */
  public org.omg.CORBA.Object create_reference(String a_repository_id)
                                        throws WrongPolicy
  {
    required(IdAssignmentPolicyValue.SYSTEM_ID);
    return create_reference_with_id(AOM.getFreeId(), a_repository_id);
  }

  /**
   * <p>
   * Create the object reference, encapsulating the given repository Id and
   * the given Object Id. The returned object will <i>not</i> be
   * activated by default and may be activated on the first invocation by
   * the servant manager (if the IMPLICIT_ACTIVATION policy applies).
   *
   * @param an_object_id the object id for the object being created. If this
   * POA uses the SYSTEM_ID policy, the portable application should only
   * pass the ids, generated by this POA.
   *
   * @param a_repository_id the repository id for the object being created,
   * can be null if this information should be later requested from the
   * servant.
   */
  public org.omg.CORBA.Object create_reference_with_id(byte[] an_object_id,
    String a_repository_id
   )
  {
    String[] ids;
    if (a_repository_id == null)
      ids = null;
    else
      ids = new String[] { a_repository_id };

    // Check maybe such object is already activated.
    AOM.Obj e = aom.get(an_object_id);

    Servant servant;
    if (e == null)
      {
        servant = null;
      }
    else
      {
        servant = e.servant;
        e.setDeactivated(false);
      }

    gnuServantObject object =
      new gnuServantObject(ids, an_object_id, this, m_orb);
    object._set_delegate(new LocalDelegate(object, this, an_object_id));
    aom.add(object.Id, object, servant, this);
    connect_to_orb(an_object_id, object);

    return object;
  }

  /**
   * Creates a new POA as a child of the target POA.
   *
   * @param child_name the name of the child POA being created.
   * @param manager the manager that will control the new POA. If this parameter
   * is null, a new POA manager is created and associated with the new POA.
   *
   * @param policies the policies, applicable for the parent POA. Policies
   * are <i>not</i> inherited from the parent POA.
   *
   * @return an newly created POA. The POA will be intially in the holding
   * state and must be activated to start processing requests.
   *
   * @throws AdapterAlreadyExists if the child with the given child_name
   * already exists for the current POA.
   * @throws InvalidPolicy if the policies conflict with each other or are
   * otherwise inappropriate.
   *
   * @see #the_children()
   */
  public POA create_POA(String child_name, POAManager manager, Policy[] policies)
                 throws AdapterAlreadyExists, InvalidPolicy
  {
    POA child;
    for (int i = 0; i < children.size(); i++)
      {
        child = (POA) children.get(i);
        if (child.the_name().equals(child_name))
          throw new AdapterAlreadyExists(name + "/" + child_name);
      }

    POA poa = createPoaInstance(child_name, manager, policies, m_orb);
    children.add(poa);
    return poa;
  }

  /**
   * Returns a default servant for this POA.
   *
   * @return a servant that will be used for requests for
   * which no servant is found in the Active Object Map.
   *
   * @throws NoServant if there is no default servant associated with this POA.
   * @throws WrongPolicy if the USE_DEFAULT_SERVANT policy is not active.
   */
  public Servant get_servant()
                      throws NoServant, WrongPolicy
  {
    required(RequestProcessingPolicyValue.USE_DEFAULT_SERVANT);
    if (default_servant == null)
      throw new NoServant();
    return default_servant;
  }

  /**
   * Sets the default servant for this POA.
   *
   * @param a_servant a servant that will be used for requests for
   * which no servant is found in the Active Object Map.
   *
   * @throws WrongPolicy if the USE_DEFAULT_SERVANT policy is not active.
   */
  public void set_servant(Servant a_servant)
                   throws WrongPolicy
  {
    required(RequestProcessingPolicyValue.USE_DEFAULT_SERVANT);
    default_servant = a_servant;
  }

  /**
   * Set a servant manager for this POA.
   *
   * @param a servant manager being set. If the RETAIN policy applies, the
   * manager must implement a {@link ServantActivator}. If the NON_RETAIN
   * policy applies, the manager must implement a {@link ServantLocator}.
   *
   * @throws WrongPolicy if the required USE_SERVANT_MANAGER policy does not
   * apply to this POA.
   *
   * @throws OBJ_ADAPTER minor code 4 if the passed manager does not
   * implement the required interface ({@link ServantActivator},
   * {@link ServantLocator}). The POA, that has the RETAIN policy uses
   * servant managers that are ServantActivators. When the POA has the
   * NON_RETAIN policy it uses servant managers that are ServantLoacators.
   *
   * @throws BAD_INV_ORDER minor code 6 if the method is called more than once
   * on the same POA. The manager can be set only once.
   */
  public void set_servant_manager(ServantManager a_manager)
                           throws WrongPolicy
  {
    required(RequestProcessingPolicyValue.USE_SERVANT_MANAGER);
    if (servant_activator != null || servant_locator != null)
      throw new BAD_INV_ORDER("Setting manager twice for " + name, 6,
                              CompletionStatus.COMPLETED_NO
                             );

    if (applies(ServantRetentionPolicyValue.RETAIN))
      {
        if (a_manager instanceof ServantActivator)
          servant_activator = (ServantActivator) a_manager;
        else
          throw new OBJ_ADAPTER("RETAIN requires ServantActivator", 4,
                                CompletionStatus.COMPLETED_NO
                               );
      }
    else if (applies(ServantRetentionPolicyValue.NON_RETAIN))
      {
        if (a_manager instanceof ServantLocator)
          servant_locator = (ServantLocator) a_manager;
        else
          throw new OBJ_ADAPTER("NON_RETAIN requires ServantLocator", 4,
                                CompletionStatus.COMPLETED_NO
                               );
      }
    else
      throw new WrongPolicy("No servant retention policy is specified.");
  }

  /**
   * Get the servant manager, associated with this POA.
   *
   * @return the associated servant manager or null if it has
   * been previously set.
   *
   * @throws WrongPolicy if the required USE_SERVANT_MANAGER policy does not
   * apply to this POA.
   */
  public ServantManager get_servant_manager()
                                     throws WrongPolicy
  {
    required(RequestProcessingPolicyValue.USE_SERVANT_MANAGER);

    if (servant_activator != null)
      return servant_activator;
    else
      return servant_locator;
  }

  /**
   * Get the unique Id of the POA in the process in which it is created.
   * This Id is needed by portable interceptors. The id is unique
   * for the life span of the POA in the process. For persistent
   * POAs, if a POA is created in the same path with the same name as
   * another POA, these POAs are identical have the same id. All transient
   * POAs are assumed unique.
   */
  public byte[] id()
  {
    if (m_poa_id != null)
      return m_poa_id;
    else
      {
        BufferedCdrOutput buffer = new BufferedCdrOutput();
        POA p = this;
        while (p != null)
          {
            buffer.write_string(p.the_name());
            p = p.the_parent();
          }
        m_poa_id = buffer.buffer.toByteArray();
        return m_poa_id;
      }
  }

  /**
   * Returns the reference to the active object with the given Id.
   *
   * @param the_Object_Id the object id.
   *
   * @throws ObjectNotActive if there is no active object with such Id
   * in the scope of this POA.
   * @throws WrongPolicy if the required RETAIN policy does not apply to
   * this POA.
   */
  public org.omg.CORBA.Object id_to_reference(byte[] the_Object_Id)
                                       throws ObjectNotActive, WrongPolicy
  {
    required(ServantRetentionPolicyValue.RETAIN);

    AOM.Obj ref = aom.get(the_Object_Id);
    if (ref == null)
      throw new ObjectNotActive();
    else
      return ref.object;
  }

  /**
   * Returns the servant that serves the active object with the given Id.
   *
   * @param the_Object_Id the object id.
   *
   * @throws ObjectNotActive if there is no active object with such Id or
   * it is not currently active.
   * @throws WrongPolicy. This method requires either RETAIN or
   * USE_DEFAULT_SERVANT policies and reaises the WrongPolicy if none of them
   * apply to this POA.
   */
  public Servant id_to_servant(byte[] the_Object_Id)
                        throws ObjectNotActive, WrongPolicy
  {
    if (applies(ServantRetentionPolicyValue.RETAIN))
      {
        AOM.Obj ref = aom.get(the_Object_Id);
        if (ref == null || ref.isDeactiveted())
          {
            if (default_servant != null)
              return default_servant;
            else
              throw new ObjectNotActive();
          }
        else if (ref.servant != null)
          return ref.servant;
        else if (default_servant != null)
          return default_servant;
        else
          throw new ObjectNotActive();
      }
    else if (default_servant != null)
      {
        return default_servant;
      }
    else
      throw new WrongPolicy("Either RETAIN or USE_DEFAULT_SERVANT required.");
  }

  /**
   * Returns the Object Id, encapsulated in the given object reference.
   *
   * @param the_Object the object that has been previously created with this
   * POA. It need not be active.
   *
   * @throws WrongAdapter if the passed object is not known for this POA.
   * @throws WrongPolicy never (declared for the future extensions only).
   */
  public byte[] reference_to_id(org.omg.CORBA.Object the_Object)
                         throws WrongAdapter, WrongPolicy
  {
    AOM.Obj ref = aom.findObject(the_Object);
    if (ref == null)
      throw new WrongAdapter();
    return ref.key;
  }

  /**
   * Returns the servant that is serving this object.
   *
   * @return if the RETAIN policy applies and the object is in the Active Object
   * Map, the method returns the servant, associated with this object.
   * Otherwise, if the USE_DEFAULT_SERVANT policy applies, the method returns
   * the default servant (if one was set).
   *
   * @throws ObjectNotActive if none of the conditions above are satisfied.
   * @throws WrongAdapter if the object reference was not created with this POA.
   * @throws WrongPolicy. This method requires either RETAIN or
   * USE_DEFAULT_SERVANT policies and reaises the WrongPolicy if none of them
   * apply to this POA.
   */
  public Servant reference_to_servant(org.omg.CORBA.Object the_Object)
    throws ObjectNotActive, WrongPolicy, WrongAdapter
  {
    if (applies(ServantRetentionPolicyValue.RETAIN))
      {
        AOM.Obj ref = aom.findObject(the_Object);
        if (ref == null)
          {
            String object;
            if (the_Object == null)
              object = "null passed";
            else if (the_Object instanceof gnuServantObject)
              {
                gnuServantObject gs = (gnuServantObject) the_Object;
                object = "Wrong owner POA " + gs.poa.the_name();
              }
            else
              object = "Unknown " + the_Object.getClass().getName();

            throw new WrongAdapter(object + " for '" + the_name() + "'");
          }
        else if (ref.isDeactiveted() || ref.servant == null)
          {
            if (default_servant != null)
              return default_servant;
            else
              throw new ObjectNotActive();
          }
        else
          return ref.servant;
      }
    else if (default_servant != null)
      {
        return default_servant;
      }
    else
      throw new WrongPolicy("Either RETAIN or USE_DEFAULT_SERVANT required.");
  }

  /**
   * Returns the id of the object, served by the given servant (assuming that
   * the servant serves only one object). The id is found in one of the
   * following ways.
   * <ul>
   * <li>If the POA has both the RETAIN and the UNIQUE_ID policy and the
   * specified servant is active, the method return the Object Id associated
   * with that servant. </li>
   * <li> If the POA has both the RETAIN and the IMPLICIT_ACTIVATION policy and
   * either the POA has the MULTIPLE_ID policy or the specified servant is
   * inactive, the method activates the servant using a POA-generated Object Id
   * and the Interface Id associated with the servant, and returns that Object
   * Id. </li>
   * <li>If the POA has the USE_DEFAULT_SERVANT policy, the servant specified
   * is the default servant, and the method is being invoked in the context of
   * executing a request on the default servant, the method returns the ObjectId
   * associated with the current invocation. </li>
   * </ul>
   *
   * @throws ServantNotActive in all cases, not listed in the list above.
   * @throws WrongPolicy The method requres USE_DEFAULT_SERVANT policy or a
   * combination of the RETAIN policy and either the UNIQUE_ID or
   * IMPLICIT_ACTIVATION policies and throws the WrongPolicy if these conditions
   * are not satisfied.
   */
  public byte[] servant_to_id(Servant the_Servant)
                       throws ServantNotActive, WrongPolicy
  {
    if (applies(RequestProcessingPolicyValue.USE_DEFAULT_SERVANT) ||
        applies(ServantRetentionPolicyValue.RETAIN) &&
        (
          applies(IdUniquenessPolicyValue.UNIQUE_ID) ||
          applies(ImplicitActivationPolicyValue.IMPLICIT_ACTIVATION)
        )
       )
      {
        AOM.Obj ref = null;
        if (!applies(IdUniquenessPolicyValue.MULTIPLE_ID))
          ref = aom.findServant(the_Servant);
        if (ref == null &&
            applies(ImplicitActivationPolicyValue.IMPLICIT_ACTIVATION)
           )
          {
            // Try to activate.
            try
              {
                return activate_object(the_Servant);
              }
            catch (ServantAlreadyActive ex)
              {
                // Either it shuld not be or the policy allows multiple ids.
                throw new InternalError();
              }
          }
        if (ref == null)
          throw new ServantNotActive();
        else
          return ref.key;
      }
    else
      throw new WrongPolicy("(RETAIN and UNIQUE ID) " +
                            "or USE_DEFAULT_SERVANT required."
                           );
  }

  /**
   * <p>
   * Converts the given servant to the object reference. The servant will serve
   * all methods, invoked on the returned object. The returned object reference
   * can be passed to the remote client, enabling remote invocations.
   * </p>
   * <p>
   * If the specified servant is active, it is returned. Otherwise, if the POA
   * has the IMPLICIT_ACTIVATION policy the method activates the servant. In
   * this case, if the servant activator is set, the
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
  public org.omg.CORBA.Object servant_to_reference(Servant the_Servant)
    throws ServantNotActive, WrongPolicy
  {
    required(ServantRetentionPolicyValue.RETAIN);

    AOM.Obj exists = null;

    if (!applies(IdUniquenessPolicyValue.MULTIPLE_ID))
      exists = aom.findServant(the_Servant);

    if (exists != null)
      {
        if (exists.isDeactiveted())
          {
            if (applies(ImplicitActivationPolicyValue.IMPLICIT_ACTIVATION))
              {
                checkDiscarding();
                exists.setDeactivated(false);
                incarnate(exists, exists.key, the_Servant, false);
              }
            else
              throw new ServantNotActive();
          }
        else
          return exists.object;
      }
    if (exists == null
      && applies(ImplicitActivationPolicyValue.IMPLICIT_ACTIVATION))
      {
        checkDiscarding();

        byte[] object_key = AOM.getFreeId();

        ServantDelegateImpl delegate = new ServantDelegateImpl(the_Servant,
          this, object_key);
        create_and_connect(object_key, the_Servant._all_interfaces(this,
          object_key)[0], delegate);

        return delegate.object;
      }
    else
      throw new ServantNotActive();
  }

  /**
   * Incarnate in cases when request forwarding is not expected because the
   * servant must be provided by the servant activator.
   *
   * @param x the aom entry, where the object is replaced by value, returned by
   * servant activator (if not null).
   *
   * @param object_key the object key.
   *
   * @param a_servant the servant that was passed as a parameter in the
   * activation method.
   *
   * @param use_forwarding if true, the gnuForwardRequest is throw under the
   * forwarding exception (for remote client). Otherwise, the request is
   * internally redirected (for local invocation).
   */
  private Servant incarnate(AOM.Obj x, byte[] object_key,
                            Servant a_servant, boolean use_forwarding
                           )
  {
    if (servant_activator != null)
      {
        Servant servant;
        try
          {
            servant = servant_activator.incarnate(object_key, this);
          }
        catch (ForwardRequest ex)
          {
            if (use_forwarding)
              throw new gnuForwardRequest(ex.forward_reference);
            else
              servant =
                ForwardedServant.create((ObjectImpl) ex.forward_reference);
          }
        if (servant != null && x != null)
          x.setServant(servant);
        if (servant == null && x != null)
          servant = x.servant;
        return servant;
      }
    else if (a_servant != null)
      {
        x.setServant(a_servant);
        return a_servant;
      }
    else if (x.servant != null)
      {
        return x.servant;
      }
    else if (default_servant != null)
      {
        x.setServant(default_servant);
        return x.servant;
      }
    else
      throw new BAD_INV_ORDER("No servant given and the servant activator not set");
  }

  /**
   * Return the POA manager, associated with this POA.
   *
   * @return the associated POA manager (always available).
   */
  public POAManager the_POAManager()
  {
    return m_manager;
  }

  /**
   * Returns the adapter activator, associated with this POA.
   * The newly created POA has no activator (null would be
   * returned). The ORB root POA also initially has no activator.
   *
   * @return tha adapter activator or null if this POA has no
   * associated adapter activator.
   */
  public AdapterActivator the_activator()
  {
    return m_activator;
  }

  /**
  * Set the adapter activator for this POA.
  *
  * @param an_activator the activator being set.
  */
  public void the_activator(AdapterActivator an_activator)
  {
    m_activator = an_activator;
  }

  /**
  * The children of this POA.
  *
  * @return the array of all childs for this POA.
  */
  public POA[] the_children()
  {
    POA[] poas = new POA[ children.size() ];
    for (int i = 0; i < poas.length; i++)
      {
        poas [ i ] = (POA) children.get(i);
      }
    return poas;
  }

  /**
   * Return the name of this POA.
   *
   * @return the name of POA, relative to its parent.
   */
  public String the_name()
  {
    return name;
  }

  /**
   * Return the parent of this POA.
   *
   * @return the parent POA or <code>null</code> if this is a root POA.
   */
  public POA the_parent()
  {
    return parent;
  }

  /** {@inheritDoc} */
  public IdAssignmentPolicy create_id_assignment_policy(IdAssignmentPolicyValue a_value)
  {
    return new gnuIdAssignmentPolicy(a_value);
  }

  /** {@inheritDoc} */
  public IdUniquenessPolicy create_id_uniqueness_policy(IdUniquenessPolicyValue a_value)
  {
    return new gnuIdUniquenessPolicy(a_value);
  }

  /** {@inheritDoc} */
  public ImplicitActivationPolicy create_implicit_activation_policy(ImplicitActivationPolicyValue a_value)
  {
    return new gnuImplicitActivationPolicy(a_value);
  }

  /** {@inheritDoc} */
  public LifespanPolicy create_lifespan_policy(LifespanPolicyValue a_value)
  {
    return new gnuLifespanPolicy(a_value);
  }

  /** {@inheritDoc} */
  public RequestProcessingPolicy create_request_processing_policy(RequestProcessingPolicyValue a_value)
  {
    return new gnuRequestProcessingPolicy(a_value);
  }

  /** {@inheritDoc} */
  public ServantRetentionPolicy create_servant_retention_policy(ServantRetentionPolicyValue a_value)
  {
    return new gnuServantRetentionPolicy(a_value);
  }

  /** {@inheritDoc} */
  public ThreadPolicy create_thread_policy(ThreadPolicyValue a_value)
  {
    return new gnuThreadPolicy(a_value);
  }

  /**
   * <p>
   * Destroy this POA and all descendant POAs. The destroyed POAs can be later
   * re-created via {@link AdapterActivator} or by invoking {@link #create_POA}.
   * This differs from {@link PoaManagerOperations#deactivate} that does not
   * allow recreation of the deactivated POAs. After deactivation, recreation is
   * only possible if the POAs were later destroyed.
   * </p>
   * <p>
   * The remote invocation on the target, belonging to the POA that is currently
   * destroyed return the remote exception ({@link TRANSIENT}, minor code 4).
   * </p>
   *
   * @param etherealize_objects if true, and POA has RETAIN policy, and the
   * servant manager is available, the servant manager method
   * {@link ServantActivatorOperations#etherealize} is called for each <i>active</i>
   * object in the Active Object Map. This method should not try to access POA
   * being destroyed. If <code>destroy</code> is called multiple times before
   * the destruction completes, the etherialization should be invoked only once.
   *
   * @param wait_for_completion if true, the method waits till the POA being
   * destroyed completes all current requests and etherialization. If false, the
   * method returns immediately.
   */
  public void destroy(boolean etherealize_objects, boolean wait_for_completion)
  {
    // Notify the IOR interceptors about that the POA is destroyed.
    if (m_orb.iIor != null)
      m_orb.iIor.adapter_state_changed(
        new ObjectReferenceTemplate[] { getReferenceTemplate() },
        NON_EXISTENT.value);

    if (wait_for_completion)
      waitWhileRunning();

    // Nofify the IOR interceptors that the POA is destroyed.
    if (m_manager instanceof gnuPOAManager)
      {
        ((gnuPOAManager) m_manager).poaDestroyed(this);
      }

    // Put the brake instead of manager, preventing the subsequent
    // requests.
    gnuPOAManager g = new gnuPOAManager();
    g.state = State.INACTIVE;
    m_manager = g;

    // Disconnect from parent.
    if (parent instanceof gnuPOA)
      {
        ((gnuPOA) parent).children.remove(this);
      }

    unregisterFromManager();

    // Disconnect from the ORB all objects, registered with this POA.
    ArrayList keys = new ArrayList();
    keys.addAll(aom.keySet());

    byte[] key;
    AOM.Obj obj;
    for (int i = 0; i < keys.size(); i++)
      {
        key = (byte[]) keys.get(i);
        obj = aom.get(key);
        if (obj.poa == this)
          m_orb.disconnect(obj.object);
      }

    m_orb.identityDestroyed(this);

    if (etherealize_objects && servant_activator != null && !m_inDestruction)
      {
        etherealizeAll();
      }
    m_inDestruction = true;

    POA[] ch = the_children();
    for (int i = 0; i < ch.length; i++)
      {
        ch[i].destroy(etherealize_objects, wait_for_completion);
      }
  }

  /**
   * Destroy this POA if it has not been destroyed, destroys it.
   */
  protected void finalize()
                   throws java.lang.Throwable
  {
    if (!m_inDestruction)
      destroy(false, false);
  }

  /**
   * Remove self from the manager list.
   */
  private void unregisterFromManager()
  {
    if (m_manager instanceof gnuPOAManager)
      {
        gnuPOAManager p = (gnuPOAManager) m_manager;
        p.removePOA(this);
      }
  }

  /**
   * Get the policy of the given type, associated with this POA.
   *
   * @param a_policy_type a type of the requested policy.
   * @return a policy of the given type, applyting to this POA.
   *
   * @throws org.omg.CORBA.BAD_PARAM if the policy of this type has not
   * been specified for this POA.
   */
  public Policy _get_policy(int a_policy_type)
                     throws org.omg.CORBA.BAD_PARAM
  {
    for (int i = 0; i < s_policies.length; i++)
      {
        if (s_policies [ i ].policy_type() == a_policy_type)
          return s_policies [ i ].copy();
      }
    throw new BAD_PARAM("No policy type " + a_policy_type);
  }

  /**
   * Get the copy of the policy array.
   */
  public Policy[] getPolicyArray()
  {
    Policy[] r = new Policy[ s_policies.length ];
    for (int i = 0; i < s_policies.length; i++)
      {
        r [ i ] = s_policies [ i ].copy();
      }
    return r;
  }

  /**
   * The POAs cannot be created by this method.
   *
   * @specnote this is also not possible in Suns jdk at least till 1.4.
   *
   * @throws NO_IMPLEMENT always.
   */
  public org.omg.CORBA.Object _set_policy_override(Policy[] policies,
                                                   SetOverrideType how
                                                  )
  {
    throw new NO_IMPLEMENT("Use createPOA instead.");
  }

  /**
   * Get the ORB, where this POA is connected.
   */
  public ORB orb()
  {
    return m_orb;
  }

  /**
   * Connect the given delegate under the given key, also calling incarnate.
   */
  private void create_and_connect(byte[] object_key, String repository_id,
    ServantDelegateImpl delegate)
  {
    aom.add(delegate);
    connect_to_orb(object_key, getReferenceFactory().make_object(repository_id,
      object_key));
    if (servant_activator != null)
      incarnate(null, object_key, delegate.servant, false);
  }

  /**
   * Check if the POA is not in a discarding mode. The activation
   * operations are forbidded in discarding mode.
   *
   * @throws TRANSIENT if the POA is in discarding mode.
   */
  void checkDiscarding()
                        throws TRANSIENT
  {
    if (m_manager.get_state() == State.DISCARDING)
      throw new TRANSIENT("Discarding mode", 1, CompletionStatus.COMPLETED_MAYBE);
  }

  /**
   * Connect the given delegate object to orb.
   */
  protected void connect_to_orb(byte[] an_Object_Id, org.omg.CORBA.Object object)
  {
    if (applies(ThreadPolicyValue.SINGLE_THREAD_MODEL))
      m_orb.connect_1_thread(object, toIORKey(an_Object_Id), this);
    else
      m_orb.connect(object, toIORKey(an_Object_Id));
  }

  /**
   * Returns the representation of this POA tree.
   */
  public String toString()
  {
    CPStringBuilder b = new CPStringBuilder(name);

    if (children.size() != 0)
      {
        b.append(" (");

        for (int i = 0; i < children.size(); i++)
          {
            b.append(children.get(i));
            if (i < children.size() - 2)
              b.append(", ");
          }
        b.append(")");
      }
    return b.toString();
  }

  /**
   * Check if the policy set is valid.
   */
  protected boolean validatePolicies(Policy[] a)
                              throws InvalidPolicy
  {
    if (applies(ServantRetentionPolicyValue.NON_RETAIN))
      {
        if (!applies(RequestProcessingPolicyValue.USE_DEFAULT_SERVANT) &&
            !applies(RequestProcessingPolicyValue.USE_SERVANT_MANAGER)
           )
          {
            short p = 0;
            for (short i = 0; i < a.length; i++)
              {
                if (a [ i ].policy_type() == SERVANT_RETENTION_POLICY_ID.value)
                  p = i;
              }
            throw new InvalidPolicy("NON_RETAIN requires either " +
                                    "USE_DEFAULT_SERVANT or USE_SERVANT_MANAGER",
                                    p
                                   );
          }
      }
    return true;
  }

  /**
   * Recursively searches for the given object in the POA tree.
   */
  public AOM.Obj findObject(org.omg.CORBA.Object object)
  {
    AOM.Obj h = aom.findObject(object);
    if (h != null)
      return h;
    else
      {
        for (int i = 0; i < children.size(); i++)
          {
            h = ((gnuPOA) children.get(i)).findObject(object);
            if (h != null)
              return h;
          }
      }
    return h;
  }

  /**
   * Recursively searches for the given key in the POA tree.
   * @param ior_key the key, ecapsulating both object
   * and poa ids.
   * @return
   */
  public AOM.Obj findKey(byte[] object_id, byte[] poa_id)
  {
    AOM.Obj h = null;
    if (Arrays.equals(poa_id, id()))
      h = aom.get(object_id);
    if (h != null)
      return h;
    else
      {
        for (int i = 0; i < children.size(); i++)
          {
            h = ((gnuPOA) children.get(i)).findKey(object_id, poa_id);
            if (h != null)
              return h;
          }
      }
    return h;
  }

  /**
   * Parses the given key, extracts poa and object id and searches
   * for such reference.
   */
  public AOM.Obj findIorKey(byte[] ior_key)
  {
    BufferredCdrInput in = new BufferredCdrInput(ior_key);
    int signature = in.read_long();
    if (signature != SIGNATURE)
      return null;

    byte[] id = in.read_sequence();
    byte[] poa = in.read_sequence();
    return findKey(id, poa);
  }

  /**
   * Converts the object Id into the IOR key. IOR key must be
   * unique in the scope of the ORB, and Ids only in the scope of POA.
   * Hence the IOR key includes the POA identifiers.
   */
  public byte[] toIORKey(byte[] object_id)
  {
    BufferedCdrOutput buffer = new BufferedCdrOutput();
    buffer.write_long(SIGNATURE);
    buffer.write_sequence(object_id);
    buffer.write_sequence(id());
    return buffer.buffer.toByteArray();
  }

  /**
   * Extracts the object id from the ior key.
   *
   * @param ior_key
   *
   * @return the encapsulated object ior key or null if
   * this ior key either refers a different POA or encoding signature
   * mismatch.
   */
  public byte[] idFormIor(byte[] ior_key)
  {
    BufferredCdrInput in = new BufferredCdrInput(ior_key);
    int signature = in.read_long();
    if (signature != SIGNATURE)
      return null;

    byte[] object_id = in.read_sequence();
    byte[] poa_id = in.read_sequence();
    if (Arrays.equals(poa_id, id()))
      return object_id;
    else
      return null;
  }

  /**
   * Recursively searches for the given servant in the POA tree.
   */
  public AOM.Obj findServant(Servant servant)
  {
    AOM.Obj h = aom.findServant(servant);
    if (h != null)
      return h;
    else
      {
        for (int i = 0; i < children.size(); i++)
          {
            h = ((gnuPOA) children.get(i)).findServant(servant);
            if (h != null)
              return h;
          }
      }
    return h;
  }

  /**
   * Get the object reference template of this POA.
   * Instantiate a singleton instance, if required.
   */
  public ObjectReferenceTemplate getReferenceTemplate()
  {
    if (refTemplate == null)
      refTemplate = new RefTemplate();

    return refTemplate;
  }

  public ObjectReferenceFactory getReferenceFactory()
  {
    return m_object_factory;
  }

  public void setReferenceFactory(ObjectReferenceFactory factory)
  {
    m_object_factory = factory;
  }

  /**
   * Create the object (needed by the factory interface).
   */
  public Object make_object(String a_repository_id, byte[] an_object_id)
  {
    AOM.Obj existing = aom.get(an_object_id);
    // The object may already exist. In this case, it is just returned.
    if (existing != null && existing.object != null)
      return existing.object;
    else
      {
        return new gnuServantObject(new String[] { a_repository_id },
          an_object_id, this, m_orb);
      }
  }

  /**
   * Required by object reference factory ops.
   */
  public String[] _truncatable_ids()
  {
    return ref_template_ids;
  }
}
