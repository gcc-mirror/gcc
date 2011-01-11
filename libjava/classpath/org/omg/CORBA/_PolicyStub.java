/* _PolicyStub.java --
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


package org.omg.CORBA;

import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.portable.ApplicationException;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.RemarshalException;

import java.io.Serializable;

/**
 * The Policy stub (proxy), used on the client side.
 * The {@link Policy} methods contain the code for remote
 * invocaton.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class _PolicyStub
  extends ObjectImpl
  implements Policy, Serializable
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = 2453656196708903849L;

  /**
   * Create the Policy stub. To get the stub working,
   * you must later set the delegate with
   * {@link ObjectImpl#_set_delegate(Delegate)}.
   */
  public _PolicyStub()
  {
  }

  /**
   * Create the naming context stub with the given delegate.
   */
  public _PolicyStub(Delegate delegate)
  {
    _set_delegate(delegate);
  }

  /**
   * Return the array of repository ids for this object.
   */
  public String[] _ids()
  {
    return new String[] { PolicyHelper.id() };
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

  /** {@inheritDoc} */
  public Policy copy()
  {
    InputStream input = null;
    try
      {
        OutputStream output = _request("copy", true);
        input = _invoke(output);
        return PolicyHelper.read(input);
      }
    catch (ApplicationException ex)
      {
        input = ex.getInputStream();

        String id = ex.getId();
        throw new MARSHAL(id);
      }
    catch (RemarshalException remarsh)
      {
        return copy();
      }
    finally
      {
        _releaseReply(input);
      }
  }

  /** {@inheritDoc} */
  public int policy_type()
  {
    InputStream input = null;
    try
      {
        OutputStream output = _request("policy_type", true);
        input = _invoke(output);

        int returns = input.read_long();

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
        return policy_type();
      }
    finally
      {
        _releaseReply(input);
      }
  }
}
