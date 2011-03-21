/* _BindingIteratorStub.java --
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


package org.omg.CosNaming;

import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.portable.ApplicationException;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.RemarshalException;

/**
 * The binding interator stub (proxy), used on the client side.
 * The BindingIterator methods contains the code for remote invocaton.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class _BindingIteratorStub
  extends ObjectImpl
  implements BindingIterator
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = 8969257760771186704L;

  /**
   * The object can be destroyed only once.
   */
  private boolean destroyed;

  /**
   * Create the stub.
   */
  public _BindingIteratorStub()
  {
    super();
  }

  /**
   * Create the stub that used the given delegate.
   */
  _BindingIteratorStub(Delegate delegate)
  {
    super();
    _set_delegate(delegate);
  }

  /**
   * Get an array of repository ids for this object.
   */
  public String[] _ids()
  {
    return new String[] { BindingIteratorHelper.id() };
  }

  /**
   * Returns true if the object has been destroyed.
   */
  public boolean _non_existent()
  {
    return destroyed;
  }

  /**
   * Destroys the object on the server side.
   * The destruction message is sent only once, even if the method is
   * called repeatedly.
   */
  public void destroy()
  {
    if (destroyed)
      return;

    InputStream _in = null;
    try
      {
        OutputStream _out = _request("destroy", true);
        _in = _invoke(_out);
        destroyed = true;
      }
    catch (ApplicationException _ex)
      {
        _in = _ex.getInputStream();
        throw new MARSHAL(_ex.getId());
      }
    catch (RemarshalException _rm)
      {
        destroy();
      }
    finally
      {
        _releaseReply(_in);
      }
  }

  /** {@inheritDoc} */
  public boolean next_n(int amount, BindingListHolder a_list)
  {
    InputStream _in = null;
    try
      {
        OutputStream _out = _request("next_n", true);
        _out.write_ulong(amount);
        _in = _invoke(_out);

        boolean result = _in.read_boolean();
        a_list.value = BindingListHelper.read(_in);
        return result;
      }
    catch (ApplicationException _ex)
      {
        _in = _ex.getInputStream();
        throw new MARSHAL(_ex.getId());
      }
    catch (RemarshalException _rm)
      {
        return next_n(amount, a_list);
      }
    finally
      {
        _releaseReply(_in);
      }
  }

  /** {@inheritDoc} */
  public boolean next_one(BindingHolder a_binding)
  {
    InputStream _in = null;
    try
      {
        OutputStream _out = _request("next_one", true);
        _in = _invoke(_out);

        boolean result = _in.read_boolean();
        a_binding.value = BindingHelper.read(_in);
        return result;
      }
    catch (ApplicationException _ex)
      {
        _in = _ex.getInputStream();
        throw new MARSHAL(_ex.getId());
      }
    catch (RemarshalException _rm)
      {
        return next_one(a_binding);
      }
    finally
      {
        _releaseReply(_in);
      }
  }

  /**
   * Destroys the iterator instance on the server side, calling
   * {@link #destroy()}.
   *
   * @throws java.lang.Throwable
   */
  protected void finalize()
                   throws java.lang.Throwable
  {
    destroy();
    super.finalize();
  }
}
