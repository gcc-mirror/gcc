/* _NamingContextImplBase.java --
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

import gnu.CORBA.Minor;

import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.DynamicImplementation;
import org.omg.CORBA.ObjectHelper;
import org.omg.CORBA.ObjectHolder;
import org.omg.CORBA.ServerRequest;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.InvokeHandler;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.ResponseHandler;
import org.omg.CORBA.portable.Streamable;
import org.omg.CosNaming.NamingContextPackage.AlreadyBound;
import org.omg.CosNaming.NamingContextPackage.AlreadyBoundHelper;
import org.omg.CosNaming.NamingContextPackage.CannotProceed;
import org.omg.CosNaming.NamingContextPackage.CannotProceedHelper;
import org.omg.CosNaming.NamingContextPackage.InvalidName;
import org.omg.CosNaming.NamingContextPackage.InvalidNameHelper;
import org.omg.CosNaming.NamingContextPackage.NotEmpty;
import org.omg.CosNaming.NamingContextPackage.NotEmptyHelper;
import org.omg.CosNaming.NamingContextPackage.NotFound;
import org.omg.CosNaming.NamingContextPackage.NotFoundHelper;

import java.util.Hashtable;

/**
 * The naming context implementation base.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class _NamingContextImplBase
  extends DynamicImplementation
  implements NamingContext, InvokeHandler
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = -114280294134561035L;

  /**
   * As there are quite many methods, it may be sensible to use the hashtable.
   * This field is also reused in NamingContextPOA.
   */
  static Hashtable methods = new Hashtable();

  /**
   * Put all methods into the table.
   */
  static
  {
    methods.put("bind", new Integer(0));
    methods.put("rebind", new Integer(1));
    methods.put("bind_context", new Integer(2));
    methods.put("rebind_context", new Integer(3));
    methods.put("resolve", new Integer(4));
    methods.put("unbind", new Integer(5));
    methods.put("new_context", new Integer(6));
    methods.put("bind_new_context", new Integer(7));
    methods.put("destroy", new Integer(8));
    methods.put("list", new Integer(9));
  }

  /**
   * Return the array of repository ids.
   */
  public String[] _ids()
  {
    return new String[] { NamingContextHelper.id() };
  }

  /**
   * The server calls this method after receiving the request message
   * from client. The implementation base calls one of its abstract
   * methods to perform the requested operation.
   *
   * @param method the method being invoked.
   * @param in the stream to read parameters from.
   * @param rh the handler to get a stream for writing a response.
   *
   * @return the stream, returned by the handler.
   */
  public OutputStream _invoke(String method, InputStream in, ResponseHandler rh)
  {
    OutputStream out = null;
    Integer call_method = (Integer) methods.get(method);
    if (call_method == null)
      throw new BAD_OPERATION(Minor.Method, CompletionStatus.COMPLETED_MAYBE);

    switch (call_method.intValue())
      {
        case 0 : // bind
        {
          try
            {
              NameComponent[] a_name = NameHelper.read(in);
              org.omg.CORBA.Object an_object = ObjectHelper.read(in);
              bind(a_name, an_object);
              out = rh.createReply();
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
          catch (AlreadyBound ex)
            {
              out = rh.createExceptionReply();
              AlreadyBoundHelper.write(out, ex);
            }
          break;
        }

        case 1 : // rebind
        {
          try
            {
              NameComponent[] a_name = NameHelper.read(in);
              org.omg.CORBA.Object an_object = ObjectHelper.read(in);
              rebind(a_name, an_object);
              out = rh.createReply();
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

        case 2 : // bind_context
        {
          try
            {
              NameComponent[] a_name = NameHelper.read(in);
              NamingContext a_context = NamingContextHelper.read(in);
              bind_context(a_name, a_context);
              out = rh.createReply();
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
          catch (AlreadyBound ex)
            {
              out = rh.createExceptionReply();
              AlreadyBoundHelper.write(out, ex);
            }
          break;
        }

        case 3 : // rebind_context
        {
          try
            {
              NameComponent[] a_name = NameHelper.read(in);
              NamingContext a_context = NamingContextHelper.read(in);
              rebind_context(a_name, a_context);
              out = rh.createReply();
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

        case 4 : // resolve
        {
          try
            {
              NameComponent[] a_name = NameHelper.read(in);
              org.omg.CORBA.Object __result = null;
              __result = resolve(a_name);
              out = rh.createReply();
              ObjectHelper.write(out, __result);
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

        case 5 : // unbind
        {
          try
            {
              NameComponent[] a_name = NameHelper.read(in);
              unbind(a_name);
              out = rh.createReply();
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

        case 6 : // new_context
        {
          NamingContext __result = null;
          __result = new_context();
          out = rh.createReply();
          NamingContextHelper.write(out, __result);
          break;
        }

        case 7 : // bind_new_context
        {
          try
            {
              NameComponent[] a_name = NameHelper.read(in);
              NamingContext __result = null;
              __result = bind_new_context(a_name);
              out = rh.createReply();
              NamingContextHelper.write(out, __result);
            }
          catch (NotFound ex)
            {
              out = rh.createExceptionReply();
              NotFoundHelper.write(out, ex);
            }
          catch (AlreadyBound ex)
            {
              out = rh.createExceptionReply();
              AlreadyBoundHelper.write(out, ex);
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

        case 8 : // destroy
        {
          try
            {
              destroy();
              out = rh.createReply();
            }
          catch (NotEmpty ex)
            {
              out = rh.createExceptionReply();
              NotEmptyHelper.write(out, ex);
            }
          break;
        }

        case 9 : // list
        {
          int amount = in.read_ulong();
          BindingListHolder a_list = new BindingListHolder();
          BindingIteratorHolder an_iter = new BindingIteratorHolder();
          list(amount, a_list, an_iter);
          out = rh.createReply();
          BindingListHelper.write(out, a_list.value);
          BindingIteratorHelper.write(out, an_iter.value);
          break;
        }

        default :
          throw new BAD_OPERATION(0, CompletionStatus.COMPLETED_MAYBE);
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

    // The server request contains no required result type.
    Integer call_method = (Integer) methods.get(request.operation());
    if (call_method == null)
      throw new BAD_OPERATION(Minor.Method, CompletionStatus.COMPLETED_MAYBE);

    switch (call_method.intValue())
      {
        case 4 : // resolve, object
          result = new ObjectHolder();
          break;

        case 6 : // new_context, NamingContext
        case 7 : // bind_new_context, NamingContext
        {
          result = new NamingContextHolder();
          break;
        }

        default : // void for others.
          result = null;
      }

    gnu.CORBA.ServiceRequestAdapter.invoke(request, this, result);
  }
}