/* Registrator.java --
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


package gnu.CORBA.Interceptor;

import gnu.CORBA.Poa.ORB_1_4;
import gnu.CORBA.ObjectCreator;
import gnu.CORBA.gnuCodecFactory;

import org.omg.CORBA.BAD_INV_ORDER;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.LocalObject;
import org.omg.CORBA.Object;
import org.omg.IOP.CodecFactory;
import org.omg.PortableInterceptor.ClientRequestInterceptor;
import org.omg.PortableInterceptor.IORInterceptor;
import org.omg.PortableInterceptor.Interceptor;
import org.omg.PortableInterceptor.ORBInitInfo;
import org.omg.PortableInterceptor.ORBInitInfoPackage.DuplicateName;
import org.omg.PortableInterceptor.ORBInitInfoPackage.InvalidName;
import org.omg.PortableInterceptor.ORBInitializer;
import org.omg.PortableInterceptor.ORBInitializerOperations;
import org.omg.PortableInterceptor.PolicyFactory;
import org.omg.PortableInterceptor.ServerRequestInterceptor;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;

/**
 * Collects interceptors, references and factories into arrays during
 * registration. As the class is security sensitive, the most of the fields are
 * private.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class Registrator extends LocalObject implements ORBInitInfo
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * The agreed properties prefix.
   */
  public static final String m_prefix =
    "org.omg.PortableInterceptor.ORBInitializerClass.";

  /**
   * The initialization - time server request interceptors.
   */
  private ArrayList m_server = new ArrayList();

  /**
   * The initialization - time client request interceptors.
   */
  private ArrayList m_client = new ArrayList();

  /**
   * The initialization - time ior interceptors.
   */
  private ArrayList m_ior = new ArrayList();

  /**
   * The policy factories.
   */
  public Hashtable m_policyFactories = new Hashtable();

  /**
   * The registered references. To avoid exposing the ORB's references map, the
   * are added by ORB from inside the ORB code. The ORB is responsible for
   * taking them from this field between pre_init and post_init.
   */
  public TreeMap m_references = new TreeMap();

  /**
   * The initializers.
   */
  public ArrayList m_initializers = new ArrayList();

  /**
   * The ORB being intialised.
   */
  final ORB_1_4 orb;

  /**
   * The argument string array, passed to ORB.init.
   */
  final String[] m_args;

  /**
   * The codec factory.
   */
  final gnuCodecFactory m_codecFactory;

  /**
   * Create the interceptor collection from the given properties, using the
   * agreed naming convention.
   *
   * @param an_orb the ORB being initialised.
   * @param props the cumulated set of properties where the orb initializer
   * pattern is searched.
   * @param an_args the argument string array, passed to ORB.init.
   */
  public Registrator(ORB_1_4 an_orb, Properties props, String[] an_args)
  {
    orb = an_orb;
    m_args = an_args;
    m_codecFactory = new gnuCodecFactory(orb);
    checkProperties(props);
    checkProperties(System.getProperties());
    checkFile("user.home", null);
    checkFile("java.home", "lib");
  }

  /**
   * Scan the given properties for the possible interceptors.
   */
  private void checkProperties(Properties props)
  {
    if (props == null)
      {
        return;
      }

    Enumeration names = props.propertyNames();
    java.lang.Object key;
    String sk;

    while (names.hasMoreElements())
      {
        key = names.nextElement();
        if (key != null)
          {
            sk = key.toString();
            if (sk.startsWith(m_prefix))
              {
                try
                  {
                    String cn = sk.substring(m_prefix.length());
                    Class iClass = ObjectCreator.forName(cn);
                    
                    ORBInitializer initializer =
                      (ORBInitializer) iClass.newInstance();
                    m_initializers.add(initializer);
                  }
                catch (Exception exc)
                  {
                    // OMG states we should not throw an exception, but
                    // this will help the user to detect his error
                    // in initialiser properties. Should never print during
                    // normal run.
                    System.err.println(sk + " failed");
                  }
              }
          }
      }
  }

  /**
   * Check if the property is defined in the existsting file orb.properties.
   */
  private void checkFile(String dir, String subdir)
  {
    try
      {
        File f = new File(dir);
        if (!f.exists())
          {
            return;
          }

        if (subdir != null)
          {
            f = new File(f, subdir);
          }
        f = new File(f, "orb.properties");

        if (!f.exists())
          {
            return;
          }

        Properties p = new Properties();
        p.load(new BufferedInputStream(new FileInputStream(f)));

        checkProperties(p);
      }
    catch (IOException ex)
      {
      }
  }

  /**
   * Called by ORB as a pre_init for all initializers.
   */
  public void pre_init()
  {
    Iterator iter = m_initializers.iterator();
    while (iter.hasNext())
      {
        ORBInitializerOperations initializer =
          (ORBInitializerOperations) iter.next();
        initializer.pre_init(this);
      }
  }

  /**
   * Get the map of the registered references. The ORB calls this method to
   * import the references into its references map.
   */
  public Map getRegisteredReferences()
  {
    return m_references;
  }

  /**
   * Called by ORB as a post-init for all initializers. After this call, the
   * interceptor sets are fixed and redundant information is discarded.
   */
  public void post_init()
  {
    Iterator iter = m_initializers.iterator();
    while (iter.hasNext())
      {
        ORBInitializerOperations initializer =
          (ORBInitializerOperations) iter.next();
        initializer.post_init(this);
      }
  }

  public ServerRequestInterceptor[] getServerRequestInterceptors()
  {
    ServerRequestInterceptor[] iServer =
      new ServerRequestInterceptor[ m_server.size() ];
    for (int i = 0; i < iServer.length; i++)
      {
        iServer [ i ] = (ServerRequestInterceptor) m_server.get(i);
      }
    return iServer;
  }

  public ClientRequestInterceptor[] getClientRequestInterceptors()
  {
    ClientRequestInterceptor[] iClient =
      new ClientRequestInterceptor[ m_client.size() ];
    for (int i = 0; i < iClient.length; i++)
      {
        iClient [ i ] = (ClientRequestInterceptor) m_client.get(i);
      }
    return iClient;
  }

  public IORInterceptor[] getIORInterceptors()
  {
    IORInterceptor[] iIor = new IORInterceptor[ m_ior.size() ];
    for (int i = 0; i < iIor.length; i++)
      {
        iIor [ i ] = (IORInterceptor) m_ior.get(i);
      }
    return iIor;
  }

  public void add_client_request_interceptor(
    ClientRequestInterceptor interceptor
  ) throws DuplicateName
  {
    add(m_client, interceptor);
  }

  public void add_ior_interceptor(IORInterceptor interceptor)
    throws DuplicateName
  {
    add(m_ior, interceptor);
  }

  public void add_server_request_interceptor(
    ServerRequestInterceptor interceptor
  ) throws DuplicateName
  {
    add(m_server, interceptor);
  }

  /**
   * Allocate a new slot for request - specific records.
   */
  public int allocate_slot_id()
  {
    return orb.icSlotSize++;
  }

  /**
   * Add the interceptor to the given collection.
   *
   * @param list the collection to add.
   * @param interceptor the interceptor to add.
   */
  private void add(ArrayList list, Interceptor interceptor)
    throws DuplicateName
  {
    if (interceptor.name().length() > 0)
      {
        Iterator iter = list.iterator();
        Interceptor ic;

        while (iter.hasNext())
          {
            ic = (Interceptor) iter.next();
            if (ic.name().equals(interceptor.name()))
              {
                throw new DuplicateName(interceptor.name());
              }
          }
      }
    list.add(interceptor);
  }

  /**
   * Get string array, passed to ORB.init.
   */
  public String[] arguments()
  {
    return m_args;
  }

  /**
   * Get the codec factory.
   */
  public CodecFactory codec_factory()
  {
    return m_codecFactory;
  }

  /**
   * Get the ORB's id, currently using .toString.
   */
  public String orb_id()
  {
    return "orb_" + orb;
  }

  /**
   * Register reference.
   */
  public void register_initial_reference(String object_name, Object object)
    throws InvalidName
  {
    if (object_name == null)
      {
        throw new InvalidName("null");
      }
    else if (object_name.length() == 0)
      {
        throw new InvalidName("Empty string");
      }
    else if (m_references.containsKey(object_name))
      {
        throw new InvalidName(object_name);
      }
    else
      {
        m_references.put(object_name, object);
      }
  }

  /**
   * Accumulates the policy factory map.
   */
  public void register_policy_factory(int policy_type,
    PolicyFactory policy_factory
  )
  {
    Integer it = new Integer(policy_type);
    if (m_policyFactories.containsKey(it))
      {
        throw new BAD_INV_ORDER(
          "Repetetive registration of the policy factory for type " +
          policy_type,
          16,
          CompletionStatus.COMPLETED_NO
        );
      }
    m_policyFactories.put(it, policy_factory);
  }

  /**
   * Delegates to ORB.
   */
  public org.omg.CORBA.Object resolve_initial_references(String object_name)
    throws InvalidName
  {
    try
      {
        return orb.resolve_initial_references(object_name);
      }
    catch (org.omg.CORBA.ORBPackage.InvalidName e)
      {
        InvalidName in = new InvalidName(e.getMessage());
        in.initCause(e);
        throw in;
      }
  }

  /**
   * Check if any interceptors of this type were registered.
   */
  public boolean hasClientRequestInterceptors()
  {
    return m_client.size() > 0;
  }

  /**
   * Check if any interceptors of this type were registered.
   */
  public boolean hasServerRequestInterceptors()
  {
    return m_server.size() > 0;
  }

  /**
   * Check if any interceptors of this type were registered.
   */
  public boolean hasIorInterceptors()
  {
    return m_ior.size() > 0;
  }
}