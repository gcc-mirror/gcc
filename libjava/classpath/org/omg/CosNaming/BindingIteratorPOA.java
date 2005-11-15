/* BindingIteratorPOA.java --
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
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.InvokeHandler;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.ResponseHandler;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.Servant;

/**
 * The binding iterator servant, used in POA-based naming service
 * implementations.
 * 
 * @since 1.4 
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class BindingIteratorPOA
  extends Servant
  implements BindingIteratorOperations, InvokeHandler
{
  /** @inheritDoc */
  public String[] _all_interfaces(POA poa, byte[] object_ID)
  {
    return new String[] { BindingIteratorHelper.id() };
  }

  /**
   * Call the required method.
   */
  public OutputStream _invoke(String method, InputStream in, ResponseHandler rh)
  {
    OutputStream out = null;

    // We suppose that the next_n should be the most popular.
    if (method.equals("next_n"))
      {
        // The next_n has been invoked.
        int amount = in.read_ulong();
        BindingListHolder a_list = new BindingListHolder();

        boolean result = next_n(amount, a_list);

        out = rh.createReply();
        out.write_boolean(result);
        BindingListHelper.write(out, a_list.value);
      }
    else if (method.equals("next_one"))
      {
        // The next_one has been invoked.
        BindingHolder a_binding = new BindingHolder();

        boolean result = next_one(a_binding);

        out = rh.createReply();
        out.write_boolean(result);
        BindingHelper.write(out, a_binding.value);
      }
    else if (method.equals("destroy"))
      {
        // The destroy has been invoked.
        destroy();
        out = rh.createReply();
      }
    else
      throw new BAD_OPERATION(method, Minor.Method, 
        CompletionStatus.COMPLETED_MAYBE);

    return out;
  }

  /**
   * Get the CORBA object that delegates calls to this servant. The servant must
   * be already connected to an ORB.
   */
  public BindingIterator _this()
  {
    return BindingIteratorHelper.narrow(super._this_object());
  }

  /**
   * Get the CORBA object that delegates calls to this servant. Connect to the
   * given ORB, if needed.
   */
  public BindingIterator _this(org.omg.CORBA.ORB orb)
  {
    return BindingIteratorHelper.narrow(super._this_object(orb));
  }
}
