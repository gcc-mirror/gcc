/* NamingContextExtPOA.java --
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
import org.omg.CORBA.ObjectHelper;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.InvokeHandler;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.ResponseHandler;
import org.omg.CosNaming.NamingContextExtPackage.InvalidAddress;
import org.omg.CosNaming.NamingContextExtPackage.InvalidAddressHelper;
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
import org.omg.PortableServer.POA;
import org.omg.PortableServer.Servant;

/**
 * The extended naming service servant. After implementing the abstract methods the
 * instance of this class can be connected to an ORB using POA.
 * 
 * @since 1.4
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class NamingContextExtPOA
  extends Servant
  implements NamingContextExtOperations, InvokeHandler

{
  /** @inheritDoc */
  public String[] _all_interfaces(POA poa, byte[] object_ID)
  {
    return new String[] { NamingContextExtHelper.id(), NamingContextHelper.id() };
  }

  /** @inheritDoc */
  public OutputStream _invoke(String method, InputStream in, ResponseHandler rh)
  {
    Integer call_method = _NamingContextExtImplBase._methods.get(method);

    if (call_method == null)
      // The older methods are handled separately.
      return super_invoke(method, in, rh);

    OutputStream out = null;

    switch (call_method.intValue())
      {
        case 0: // to_string
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

        case 1: // to_name
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

        case 2: // to_url
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

        case 3: // resolve_str
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
   * Handles calls to the methods from the NamingContext. The classes cannot be
   * directly derived from each other; new public methods would appear.
   */
  OutputStream super_invoke(String method, InputStream in, ResponseHandler rh)
  {
    OutputStream out = null;
    Integer call_method = _NamingContextImplBase.methods.get(method);
    if (call_method == null)
      throw new BAD_OPERATION(Minor.Method, CompletionStatus.COMPLETED_MAYBE);

    switch (call_method.intValue())
      {
        case 0: // bind
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

        case 1: // rebind
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

        case 2: // bind_context
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

        case 3: // rebind_context
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

        case 4: // resolve
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

        case 5: // unbind
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

        case 6: // new_context
        {
          NamingContext __result = null;
          __result = new_context();
          out = rh.createReply();
          NamingContextHelper.write(out, __result);
          break;
        }

        case 7: // bind_new_context
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

        case 8: // destroy
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

        case 9: // list
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

        default:
          throw new BAD_OPERATION(0, CompletionStatus.COMPLETED_MAYBE);
      }

    return out;
  }

  /**
   * Get the CORBA object that delegates calls to this servant. The servant must
   * be already connected to an ORB.
   */
  public NamingContextExt _this()
  {
    return NamingContextExtHelper.narrow(super._this_object());
  }

  /**
   * Get the CORBA object that delegates calls to this servant. Connect to the
   * given ORB, if needed.
   */
  public NamingContextExt _this(org.omg.CORBA.ORB orb)
  {
    return NamingContextExtHelper.narrow(super._this_object(orb));
  }
}
