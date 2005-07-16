/* _NamingContextExtImplBase.java --
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


package org.omg.CosNaming;

import org.omg.CORBA.ObjectHolder;
import org.omg.CORBA.ServerRequest;
import org.omg.CORBA.StringHolder;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.InvokeHandler;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.ResponseHandler;
import org.omg.CORBA.portable.Streamable;
import org.omg.CosNaming.NamingContextExtPackage.InvalidAddress;
import org.omg.CosNaming.NamingContextExtPackage.InvalidAddressHelper;
import org.omg.CosNaming.NamingContextPackage.CannotProceed;
import org.omg.CosNaming.NamingContextPackage.CannotProceedHelper;
import org.omg.CosNaming.NamingContextPackage.InvalidName;
import org.omg.CosNaming.NamingContextPackage.InvalidNameHelper;
import org.omg.CosNaming.NamingContextPackage.NotFound;
import org.omg.CosNaming.NamingContextPackage.NotFoundHelper;

import java.util.Hashtable;

/**
 * The extended naming context implementation base.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class _NamingContextExtImplBase
  extends _NamingContextImplBase
  implements NamingContextExt, InvokeHandler
{
  private static Hashtable _methods = new Hashtable();

  static
  {
    _methods.put("to_string", new java.lang.Integer(0));
    _methods.put("to_name", new java.lang.Integer(1));
    _methods.put("to_url", new java.lang.Integer(2));
    _methods.put("resolve_str", new java.lang.Integer(3));
  }

  /**
   * This stub can be the base of the two CORBA objects, so it
   * has two repository ids.
   */
  private static String[] __ids =
    { NamingContextExtHelper.id(), NamingContextHelper.id() };

  /**
   * Return the array of repository ids for this object.
   * This stub can be the base of the two CORBA objects, so it
   * has two repository ids, for {@link NamingContext} and
   * for {@link NamingContextExt}.
   */
  public String[] _ids()
  {
    return __ids;
  }

  public OutputStream _invoke(String method, InputStream in, ResponseHandler rh)
  {
    Integer call_method = (Integer) _methods.get(method);

    if (call_method == null)

      // The older methods are handled by the parent class.
      return super._invoke(method, in, rh);

    OutputStream out = null;

    switch (call_method.intValue())
      {
        case 0 : // to_string
        {
          try
            {
              NameComponent[] a_name = NameHelper.read(in);
              String result = null;
              result = this.to_string(a_name);
              out = rh.createReply();
              out.write_string(result);
            }
          catch (InvalidName ex)
            {
              out = rh.createExceptionReply();
              InvalidNameHelper.write(out, ex);
            }
          break;
        }

        case 1 : // to_name
        {
          try
            {
              String a_name_string = in.read_string();
              NameComponent[] result = to_name(a_name_string);
              out = rh.createReply();
              NameHelper.write(out, result);
            }
          catch (InvalidName ex)
            {
              out = rh.createExceptionReply();
              InvalidNameHelper.write(out, ex);
            }
          break;
        }

        case 2 : // to_url
        {
          try
            {
              String an_address = in.read_string();
              String a_name_string = in.read_string();
              String result = to_url(an_address, a_name_string);
              out = rh.createReply();
              out.write_string(result);
            }
          catch (InvalidAddress ex)
            {
              out = rh.createExceptionReply();
              InvalidAddressHelper.write(out, ex);
            }
          catch (InvalidName ex)
            {
              out = rh.createExceptionReply();
              InvalidNameHelper.write(out, ex);
            }
          break;
        }

        case 3 : // resolve_str
        {
          try
            {
              String a_name_string = in.read_string();
              org.omg.CORBA.Object result = resolve_str(a_name_string);
              out = rh.createReply();
              org.omg.CORBA.ObjectHelper.write(out, result);
            }
          catch (NotFound ex)
            {
              out = rh.createExceptionReply();
              NotFoundHelper.write(out, ex);
            }
          catch (CannotProceed ex)
            {
              out = rh.createExceptionReply();
              CannotProceedHelper.write(out, ex);
            }
          catch (InvalidName ex)
            {
              out = rh.createExceptionReply();
              InvalidNameHelper.write(out, ex);
            }
          break;
        }
      }
    return out;
  }

  /**
   * The obsolete invocation using server request. Implemented for
   * compatibility reasons, but is it more effectinve to use
   * {@link #_invoke}.
   *
   * @param request a server request.
   */
  public void invoke(ServerRequest request)
  {
    Streamable result = null;

    Integer call_method = (Integer) _methods.get(request.operation());

    if (call_method == null)
      {
        super.invoke(request);
        return;
      }

    switch (call_method.intValue())
      {
        case 0 : // to_string, String
          result = new StringHolder();
          break;

        case 1 : // to_name, Name
          result = new NameHolder();
          break;

        case 2 : // to_url, String
          result = new StringHolder();
          break;

        case 3 : // resolve_str, Object
          result = new ObjectHolder();
          break;
      }
    gnu.CORBA.ServiceRequestAdapter.invoke(request, this, result);
  }
}