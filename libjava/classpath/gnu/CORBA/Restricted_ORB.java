/* RestrictedORB.java --
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

import gnu.CORBA.CDR.cdrBufOutput;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.ContextList;
import org.omg.CORBA.Environment;
import org.omg.CORBA.ExceptionList;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.NVList;
import org.omg.CORBA.NamedValue;
import org.omg.CORBA.ORB;
import org.omg.CORBA.ORBPackage.InvalidName;
import org.omg.CORBA.Request;
import org.omg.CORBA.StructMember;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.CORBA.UnionMember;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.ValueFactory;
import org.omg.PortableInterceptor.ClientRequestInterceptorOperations;
import org.omg.PortableInterceptor.IORInterceptorOperations;
import org.omg.PortableInterceptor.ServerRequestInterceptorOperations;

import java.applet.Applet;

import java.util.Hashtable;
import java.util.Properties;

/**
 * This class implements so-called Singleton ORB, a highly restricted version
 * that cannot communicate over network. This ORB is provided for the
 * potentially malicious applets with heavy security restrictions. It, however,
 * supports some basic features that might be needed even when the network
 * access is not granted.
 *
 * This ORB can only create typecodes, {@link Any}, {@link ContextList},
 * {@link NVList} and {@link org.omg.CORBA.portable.OutputStream} that writes to
 * an internal buffer.
 *
 * All other methods throw the {@link NO_IMPLEMENT} exception.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class Restricted_ORB extends org.omg.CORBA_2_3.ORB
{
  /**
   * The singleton instance of this ORB.
   */
  public static final ORB Singleton = new Restricted_ORB();

  /**
   * The cumulated listener for all IOR interceptors. Interceptors are used by
   * {@link gnu.CORBA.Poa.ORB_1_4}.
   */
  public IORInterceptorOperations iIor;

  /**
   * The cumulated listener for all server request interceptors. Interceptors
   * are used by {@link gnu.CORBA.Poa.ORB_1_4}.
   */
  public ServerRequestInterceptorOperations iServer;

  /**
   * The cumulated listener for all client request interceptros. Interceptors
   * are used by {@link gnu.CORBA.Poa.ORB_1_4}.
   */
  public ClientRequestInterceptorOperations iClient;

  /**
   * The required size of the interceptor slot array.
   */
  public int icSlotSize = 0;

  /**
   * The value factories.
   */
  protected Hashtable factories = new Hashtable();

  /**
   * The policy factories.
   */
  protected Hashtable policyFactories = new Hashtable();

  /**
       * Create a new instance of the RestrictedORB. This is used in derived classes
   * only.
   */
  protected Restricted_ORB()
  {
  }

  /** {@inheritDoc} */
  public TypeCode create_alias_tc(String id, String name, TypeCode typecode)
  {
    return new aliasTypeCode(typecode, id, name);
  }

  /** {@inheritDoc} */
  public Any create_any()
  {
    gnuAny any = new gnuAny();
    any.setOrb(this);
    return any;
  }

  /** {@inheritDoc} */
  public TypeCode create_array_tc(int length, TypeCode element_type)
  {
    primitiveArrayTypeCode p =
      new primitiveArrayTypeCode(TCKind.tk_array, element_type);
    p.setLength(length);
    return p;
  }

  /** {@inheritDoc} */
  public ContextList create_context_list()
  {
    return new gnuContextList();
  }

  /** {@inheritDoc} */
  public TypeCode create_enum_tc(String id, String name, String[] values)
  {
    recordTypeCode r = new recordTypeCode(TCKind.tk_enum);
    for (int i = 0; i < values.length; i++)
      {
        r.field().name = values [ i ];
      }

    r.setId(id);
    r.setName(name);

    return r;
  }

  /** {@inheritDoc} */
  public Environment create_environment()
  {
    return new gnuEnvironment();
  }

  /** {@inheritDoc} */
  public ExceptionList create_exception_list()
  {
    return new gnuExceptionList();
  }

  /** {@inheritDoc} */
  public TypeCode create_exception_tc(String id, String name,
    StructMember[] members
  )
  {
    recordTypeCode r = new recordTypeCode(TCKind.tk_except);
    r.setId(id);
    r.setName(name);

    for (int i = 0; i < members.length; i++)
      {
        r.add(members [ i ]);
      }

    return r;
  }

  /**
   * This method is not allowed for a RestrictedORB.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public TypeCode create_interface_tc(String id, String name)
  {
    no();
    return null;
  }

  /** {@inheritDoc} */
  public NVList create_list(int count)
  {
    return new gnuNVList(count);
  }

  /** {@inheritDoc} */
  public NamedValue create_named_value(String s, Any any, int flags)
  {
    return new gnuNamedValue();
  }

  /** {@inheritDoc} */
  public OutputStream create_output_stream()
  {
    cdrBufOutput stream = new cdrBufOutput();
    stream.setOrb(this);
    return stream;
  }

  /** {@inheritDoc} */
  public TypeCode create_sequence_tc(int bound, TypeCode element_type)
  {
    primitiveArrayTypeCode p =
      new primitiveArrayTypeCode(TCKind.tk_sequence, element_type);
    p.setLength(bound);
    return p;
  }

  /** {@inheritDoc} */
  public TypeCode create_string_tc(int bound)
  {
    stringTypeCode p = new stringTypeCode(TCKind.tk_string);
    p.setLength(bound);
    return p;
  }

  /** {@inheritDoc} */
  public TypeCode create_struct_tc(String id, String name,
    StructMember[] members
  )
  {
    recordTypeCode r = new recordTypeCode(TCKind.tk_struct);
    r.setId(id);
    r.setName(name);

    for (int i = 0; i < members.length; i++)
      {
        r.add(members [ i ]);
      }

    return r;
  }

  /** {@inheritDoc} */
  public TypeCode create_union_tc(String id, String name,
    TypeCode discriminator_type, UnionMember[] members
  )
  {
    recordTypeCode r = new recordTypeCode(TCKind.tk_union);
    r.setId(id);
    r.setName(name);
    r.setDiscriminator_type(discriminator_type);
    r.setDefaultIndex(0);

    for (int i = 0; i < members.length; i++)
      {
        r.add(members [ i ]);
      }

    return r;
  }

  /** {@inheritDoc} */
  public TypeCode create_wstring_tc(int bound)
  {
    stringTypeCode p = new stringTypeCode(TCKind.tk_wstring);
    p.setLength(bound);
    return p;
  }

  /** {@inheritDoc} */
  public TypeCode get_primitive_tc(TCKind tcKind)
  {
    try
      {
        return typeNamer.getPrimitveTC(tcKind);
      }
    catch (BadKind ex)
      {
        throw new BAD_PARAM("This is not a primitive type code: " +
          tcKind.value()
        );
      }
  }

  /**
   * This method is not allowed for a RestrictedORB.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public String[] list_initial_services()
  {
    no();
    throw new InternalError();
  }

  /**
   * This method is not allowed for a RestrictedORB.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public String object_to_string(org.omg.CORBA.Object forObject)
  {
    no();
    throw new InternalError();
  }

  /**
   * This method is not allowed for a RestrictedORB.
   *
   * @throws InvalidName never in this class, but it is thrown in the derived
   * classes.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public org.omg.CORBA.Object resolve_initial_references(String name)
    throws InvalidName
  {
    no();
    throw new InternalError();
  }

  /**
   * Shutdown the ORB server.
   *
   * For RestrictedORB, returns witout action.
   */
  public void run()
  {
  }

  /**
   * Shutdown the ORB server.
   *
   * For RestrictedORB, returns witout action.
   */
  public void shutdown(boolean wait_for_completion)
  {
  }

  /**
   * This method is not allowed for a RestrictedORB.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public org.omg.CORBA.Object string_to_object(String IOR)
  {
    no();
    throw new InternalError();
  }

  /**
   * This method is not allowed for a RestrictedORB.
   *
   * @throws NO_IMPLEMENT, always.
   */
  protected void set_parameters(Applet app, Properties props)
  {
    no();
  }

  /**
   * This method is not allowed for a RestrictedORB.
   *
   * @throws NO_IMPLEMENT, always.
   */
  protected void set_parameters(String[] args, Properties props)
  {
    no();
  }

  /**
   * Throws an exception, stating that the given method is not supported by the
   * Restricted ORB.
   */
  private final void no()
  {
    // Apart the programming errors, this can only happen if the
    // malicious code is trying to do that it is not allowed.
    throw new NO_IMPLEMENT("Use init(args, props) for the functional version.");
  }

  /**
   * This method is not allowed for a RestrictedORB.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public Request get_next_response() throws org.omg.CORBA.WrongTransaction
  {
    no();
    throw new InternalError();
  }

  /**
   * This method is not allowed for a RestrictedORB.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public boolean poll_next_response()
  {
    no();
    throw new InternalError();
  }

  /**
   * This method is not allowed for a RestrictedORB.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public void send_multiple_requests_deferred(Request[] requests)
  {
    no();
  }

  /**
   * This method is not allowed for a RestrictedORB.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public void send_multiple_requests_oneway(Request[] requests)
  {
    no();
  }

  /**
   * Register the value factory under the given repository id.
   */
  public ValueFactory register_value_factory(String repository_id,
    ValueFactory factory
  )
  {
    factories.put(repository_id, factory);
    return factory;
  }

  /**
   * Unregister the value factroy.
   */
  public void unregister_value_factory(String id)
  {
    factories.remove(id);
  }

  /**
   * Look for the value factory for the value, having the given repository id.
       * The implementation checks for the registered value factories first. If none
       * found, it tries to load and instantiate the class, mathing the given naming
   * convention. If this faild, null is returned.
   *
   * @param repository_id a repository id.
   *
   * @return a found value factory, null if none.
   */
  public ValueFactory lookup_value_factory(String repository_id)
  {
    ValueFactory f = (ValueFactory) factories.get(repository_id);
    if (f != null)
      {
        return f;
      }

    f = (ValueFactory) ObjectCreator.createObject(repository_id,
        "DefaultFactory"
      );
    if (f != null)
      {
        factories.put(repository_id, f);
      }
    return f;
  }

  /**
   * Destroy the interceptors, if they are present.
   */
  public void destroy()
  {
    if (iIor != null)
      {
        iIor.destroy();
        iIor = null;
      }

    if (iServer != null)
      {
        iServer.destroy();
        iServer = null;
      }

    if (iClient != null)
      {
        iClient.destroy();
        iClient = null;
      }

    super.destroy();
  }
}