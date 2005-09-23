/* ORB_1_4.java --
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

import gnu.CORBA.Functional_ORB;
import gnu.CORBA.IOR;
import gnu.CORBA.Connected_objects.cObject;
import gnu.CORBA.DynAn.gnuDynAnyFactory;
import gnu.CORBA.Interceptor.ClientRequestInterceptors;
import gnu.CORBA.Interceptor.IORInterceptors;
import gnu.CORBA.Interceptor.Registrator;
import gnu.CORBA.Interceptor.ServerRequestInterceptors;
import gnu.CORBA.Interceptor.gnuIcCurrent;
import gnu.CORBA.Interceptor.gnuIorInfo;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.OBJECT_NOT_EXIST;
import org.omg.CORBA.Policy;
import org.omg.CORBA.PolicyError;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.PortableInterceptor.PolicyFactory;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAPackage.InvalidPolicy;

import java.applet.Applet;
import java.util.Properties;

/**
 * The ORB, supporting POAs that are the feature of jdk 1.4.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class ORB_1_4
  extends Functional_ORB
{
  /**
   * The root POA.
   */
  public final gnuPOA rootPOA;

  /**
   * Maps the active threads to the invocation data ("POA Current's").
   */
  public gnuPoaCurrent currents = new gnuPoaCurrent();

  /**
   * Maps the active threads to the interceptor data ("Interceptor Current's").
   */
  public gnuIcCurrent ic_current = new gnuIcCurrent(this);

  /**
   * Creates dynamic anys.
   */
  public gnuDynAnyFactory factory = new gnuDynAnyFactory(this);

  /**
   * Calls the parent constructor and additionally puts the "RootPOA",
   * "RootPOAManager", "POACurrent" and "DynAnyFactory" into initial references.
   */
  public ORB_1_4()
  {
    super();
    try
      {
        rootPOA = new gnuPOA(null, "RootPOA", null, policySets.rootPoa(), this);
      }
    catch (InvalidPolicy ex)
      {
        // Invalid default policy set.
        InternalError ierr = new InternalError();
        ierr.initCause(ex);
        throw ierr;
      }
    initial_references.put("RootPOA", rootPOA);
    initial_references.put("RootPOAManager", rootPOA.the_POAManager());
    initial_references.put("POACurrent", currents);
    initial_references.put("DynAnyFactory", factory);
    initial_references.put("PICurrent", ic_current);
  }

  /**
   * If the super method detects that the object is not connected to this ORB,
   * try to find and activate the object.
   */
  public String object_to_string(org.omg.CORBA.Object forObject)
  {
    try
      {
        return super.object_to_string(forObject);
      }
    catch (Exception ex)
      {
        try
          {
            activeObjectMap.Obj exists = rootPOA.findObject(forObject);
            if (exists == null)
              throw new OBJECT_NOT_EXIST(forObject == null ? "null"
                : forObject.toString());
            else if (exists.poa instanceof gnuPOA)
              ((gnuPOA) exists.poa).connect_to_orb(exists.key, forObject);
            else
              exists.poa.create_reference_with_id(exists.key,
                ((ObjectImpl) exists.object)._ids()[0]);
          }
        catch (Exception bex)
          {
            BAD_PARAM bad = new BAD_PARAM("Unable to activate " + forObject);
            bad.initCause(bex);
            throw bad;
          }

        return super.object_to_string(forObject);
      }
  }

  /**
   * Destroy all poas and then call the superclass method.
   */
  public void destroy()
  {
    // This will propagate through the whole POA tree.
    rootPOA.destroy(true, false);

    super.destroy();
  }

  /**
   * Do interceptor registration.
   *
   * @param properties the properties, between those names the agreed prefix
   * "org.omg.PortableInterceptor.ORBInitializerClass." is searched.
   *
   * @param args the string array, passed to the ORB.init
   */
  protected void registerInterceptors(Properties properties, String[] args)
  {
    Registrator registrator = new Registrator(this, properties, args);

    policyFactories = registrator.m_policyFactories;

    registrator.pre_init();
    initial_references.putAll(registrator.getRegisteredReferences());
    registrator.post_init();

    if (registrator.hasIorInterceptors())
      iIor = new IORInterceptors(registrator);

    if (registrator.hasServerRequestInterceptors())
      iServer = new ServerRequestInterceptors(registrator);

    if (registrator.hasClientRequestInterceptors())
      iClient = new ClientRequestInterceptors(registrator);

    policyFactories = registrator.m_policyFactories;
  }

  /**
   * Create IOR and allow registered interceptors to add additional components.
   */
  protected IOR createIOR(cObject ref)
    throws BAD_OPERATION
  {
    IOR ior = super.createIOR(ref);
    if (iIor != null)
      {
        activeObjectMap.Obj obj = rootPOA.findIorKey(ior.key);

        POA poa;

        // Null means that the object was connected to the ORB directly.
        if (obj == null)
          poa = rootPOA;
        else
          poa = obj.poa;

        gnuIorInfo info = new gnuIorInfo(this, poa, ior);

        // This may modify the ior.
        iIor.establish_components(info);
      }
    return ior;
  }

  /**
   * Create policy using the previously registered factory.
   */
  public Policy create_policy(int type, Any value)
    throws PolicyError
  {
    Integer policy = new Integer(type);

    PolicyFactory forge = (PolicyFactory) policyFactories.get(policy);
    if (forge == null)
      throw new PolicyError("No factory registered for policy " + type,
        (short) type);
    else
      return forge.create_policy(type, value);
  }

  /**
   * Set the parameters and then register interceptors.
   */
  protected void set_parameters(Applet app, Properties props)
  {
    super.set_parameters(app, props);
    registerInterceptors(props, new String[0]);
  }

  /**
   * Set the parameters and then register interceptors.
   */
  protected void set_parameters(String[] para, Properties props)
  {
    super.set_parameters(para, props);
    registerInterceptors(props, para);
  }

}