/* _IORInterceptor_3_0Stub.java --
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

import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.portable.ApplicationException;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.RemarshalException;

import java.io.Serializable;

/**
 * The IORInterceptor_3_0 stub (proxy), used on the client side. The
 * {@link IORInterceptor_3_0} methods contain the code for remote invocaton. The
 * stub is required by {@link IORInterceptor_3_0Helper} .read, .narrow and
 * .unchecked_narrow methods.
 * 
 * @specnote Being not specified in 1.5 API, this class is package private.
 * From that happened to some other stubs, it will likely to appear in the 1.6
 * or later. Because of this, it is placed here. 
 * 
 * @specnote The stub and the helper support the existence of the interceptor
 * on the remote side only. To support the corresponding support on the side 
 * where the ORB is registered with this interceptor, you also need
 * _IORInfoStub, IORInfoHelper and either servants or implementation bases
 * for both POA and IORInfo. These classes are not defined in the 1.5 API,
 * hence they are not included. You may need to generate the manually from
 * the IDL descriptions, available from 
 * http://www.omg.org/docs/formal/04-03-12.pdf.
 * 
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
class _IORInterceptor_3_0Stub
  extends ObjectImpl
  implements IORInterceptor_3_0, Serializable
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * Create the IORInterceptor_3_0 stub. To get the stub working, you must later
   * set the delegate with {@link ObjectImpl#_set_delegate(Delegate)}.
   */
  public _IORInterceptor_3_0Stub()
  {
  }

  /**
   * Create the naming context stub with the given delegate.
   */
  public _IORInterceptor_3_0Stub(Delegate delegate)
  {
    _set_delegate(delegate);
  }

  /**
   * Return the array of repository ids for this object.
   */
  public String[] _ids()
  {
    return new String[] { IORInterceptor_3_0Helper.id() };
  }

  /** {@inheritDoc} */
  public void adapter_manager_state_changed(int adapterManagerId,
    short adapterState)
  {
    InputStream input = null;
    try
      {
        OutputStream output = _request("adapter_manager_state_changed", true);
        output.write_long(adapterManagerId);
        output.write_short(adapterState);
        input = _invoke(output);

      }
    catch (ApplicationException ex)
      {
        input = ex.getInputStream();
        String id = ex.getId();
        throw new MARSHAL(id);
      }
    catch (RemarshalException remarsh)
      {
        adapter_manager_state_changed(adapterManagerId, adapterState);
      }
    finally
      {
        _releaseReply(input);
      }
  }

  /** {@inheritDoc} */
  public void adapter_state_changed(ObjectReferenceTemplate[] adapters,
    short adaptersState)
  {
    InputStream input = null;
    try
      {
        OutputStream output = _request("adapter_state_changed", true);
        output.write_long(adapters.length);
        for (int i0 = 0; i0 < adapters.length; i0++)
          ObjectReferenceTemplateHelper.write(output, adapters[i0]);
        output.write_short(adaptersState);
        input = _invoke(output);

      }
    catch (ApplicationException ex)
      {
        input = ex.getInputStream();
        String id = ex.getId();
        throw new MARSHAL(id);
      }
    catch (RemarshalException remarsh)
      {
        adapter_state_changed(adapters, adaptersState);
      }
    finally
      {
        _releaseReply(input);
      }
  }

  /** {@inheritDoc} */
  public void components_established(IORInfo info)
  {
    InputStream input = null;
    try
      {
        OutputStream output = _request("components_established", true);
        output.write_Object(info);
        input = _invoke(output);

      }
    catch (ApplicationException ex)
      {
        input = ex.getInputStream();
        String id = ex.getId();
        throw new MARSHAL(id);
      }
    catch (RemarshalException remarsh)
      {
        components_established(info);
      }
    finally
      {
        _releaseReply(input);
      }
  }

  /** {@inheritDoc} */
  public void establish_components(IORInfo info)
  {
    InputStream input = null;
    try
      {
        OutputStream output = _request("establish_components", true);
        output.write_Object(info);
        input = _invoke(output);

      }
    catch (ApplicationException ex)
      {
        input = ex.getInputStream();
        String id = ex.getId();
        throw new MARSHAL(id);
      }
    catch (RemarshalException remarsh)
      {
        establish_components(info);
      }
    finally
      {
        _releaseReply(input);
      }
  }

  /** {@inheritDoc} */
  public String name()
  {
    InputStream input = null;
    try
      {
        OutputStream output = _request("name", true);
        input = _invoke(output);
        String returns = input.read_string();

        return returns;
      }
    catch (ApplicationException ex)
      {
        input = ex.getInputStream();
        String id = ex.getId();
        throw new MARSHAL(id);
      }
    catch (RemarshalException remarsh)
      {
        return name();
      }
    finally
      {
        _releaseReply(input);
      }
  }

  /** {@inheritDoc} */
  public void destroy()
  {
    InputStream input = null;
    try
      {
        OutputStream output = _request("destroy", true);
        input = _invoke(output);

      }
    catch (ApplicationException ex)
      {
        input = ex.getInputStream();
        String id = ex.getId();
        throw new MARSHAL(id);
      }
    catch (RemarshalException remarsh)
      {
        destroy();
      }
    finally
      {
        _releaseReply(input);
      }
  }
}