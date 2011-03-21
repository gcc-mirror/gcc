/* _NamingContextStub.java --
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
import org.omg.CORBA.ObjectHelper;
import org.omg.CORBA.portable.ApplicationException;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.RemarshalException;
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

/**
 * The naming context stub (proxy), used on the client side.
 * The {@link NamingContext} methods contain the code for remote
 * invocaton.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class _NamingContextStub
  extends ObjectImpl
  implements NamingContext
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = 6835430958405349379L;

  /**
   * Create the naming context stub.
   */
  public _NamingContextStub()
  {
    super();
  }

  /**
   * Create the naming context stub with the given delegate.
   */
  _NamingContextStub(Delegate delegate)
  {
    super();
    _set_delegate(delegate);
  }

  /**
   * Return the array of repository ids for this object.
   */
  public String[] _ids()
  {
    return new String[] { NamingContextHelper.id() };
  }

  /** {@inheritDoc} */
  public void bind(NameComponent[] a_name, org.omg.CORBA.Object an_object)
            throws NotFound, CannotProceed, InvalidName, AlreadyBound
  {
    InputStream in = null;
    try
      {
        OutputStream out = _request("bind", true);
        NameHelper.write(out, a_name);
        ObjectHelper.write(out, an_object);
        in = _invoke(out);
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();

        String id = ex.getId();
        throw5(in, id);
      }
    catch (RemarshalException remarsh)
      {
        bind(a_name, an_object);
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /** {@inheritDoc} */
  public void bind_context(NameComponent[] a_name, NamingContext a_context)
                    throws NotFound, CannotProceed, InvalidName, AlreadyBound
  {
    InputStream in = null;
    try
      {
        OutputStream out = _request("bind_context", true);
        NameHelper.write(out, a_name);
        NamingContextHelper.write(out, a_context);
        in = _invoke(out);
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();

        String id = ex.getId();
        throw5(in, id);
      }
    catch (RemarshalException remarsh)
      {
        bind_context(a_name, a_context);
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /** {@inheritDoc} */
  public NamingContext bind_new_context(NameComponent[] a_name)
                                 throws NotFound, AlreadyBound, CannotProceed,
                                        InvalidName
  {
    InputStream in = null;
    try
      {
        OutputStream out = _request("bind_new_context", true);
        NameHelper.write(out, a_name);
        in = _invoke(out);

        NamingContext __result = NamingContextHelper.read(in);
        return __result;
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();

        String id = ex.getId();
        throw5(in, id);
        throw new InternalError();
      }
    catch (RemarshalException remarsh)
      {
        return bind_new_context(a_name);
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /** {@inheritDoc} */
  public void destroy()
               throws NotEmpty
  {
    InputStream in = null;
    try
      {
        OutputStream out = _request("destroy", true);
        in = _invoke(out);
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();

        String id = ex.getId();
        if (id.equals(NotEmptyHelper.id()))
          throw NotEmptyHelper.read(in);
        else
          throw new MARSHAL(id);
      }
    catch (RemarshalException remarsh)
      {
        destroy();
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /** {@inheritDoc} */
  public void list(int amount, BindingListHolder a_list,
                   BindingIteratorHolder an_iter
                  )
  {
    InputStream in = null;
    try
      {
        OutputStream out = _request("list", true);
        out.write_ulong(amount);
        in = _invoke(out);
        a_list.value = BindingListHelper.read(in);
        an_iter.value = BindingIteratorHelper.read(in);
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();
        throw new MARSHAL(ex.getId());
      }
    catch (RemarshalException remarsh)
      {
        list(amount, a_list, an_iter);
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /** {@inheritDoc} */
  public NamingContext new_context()
  {
    InputStream in = null;
    try
      {
        OutputStream out = _request("new_context", true);
        in = _invoke(out);

        NamingContext __result = NamingContextHelper.read(in);
        return __result;
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();
        throw new MARSHAL(ex.getId());
      }
    catch (RemarshalException remarsh)
      {
        return new_context();
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /** {@inheritDoc} */
  public void rebind(NameComponent[] a_name, org.omg.CORBA.Object an_object)
              throws NotFound, CannotProceed, InvalidName
  {
    InputStream in = null;
    try
      {
        OutputStream out = _request("rebind", true);
        NameHelper.write(out, a_name);
        ObjectHelper.write(out, an_object);
        in = _invoke(out);
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();

        String id = ex.getId();
        throw4(in, id);
      }
    catch (RemarshalException remarsh)
      {
        rebind(a_name, an_object);
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /** {@inheritDoc} */
  public void rebind_context(NameComponent[] a_name, NamingContext a_context)
                      throws NotFound, CannotProceed, InvalidName
  {
    InputStream in = null;
    try
      {
        OutputStream out = _request("rebind_context", true);
        NameHelper.write(out, a_name);
        NamingContextHelper.write(out, a_context);
        in = _invoke(out);
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();

        String id = ex.getId();
        throw4(in, id);
      }
    catch (RemarshalException remarsh)
      {
        rebind_context(a_name, a_context);
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /** {@inheritDoc} */
  public org.omg.CORBA.Object resolve(NameComponent[] a_name)
                               throws NotFound, CannotProceed, InvalidName
  {
    InputStream in = null;
    try
      {
        OutputStream out = _request("resolve", true);
        NameHelper.write(out, a_name);
        in = _invoke(out);

        org.omg.CORBA.Object __result = ObjectHelper.read(in);
        return __result;
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();

        String id = ex.getId();
        throw4(in, id);
        throw new InternalError();
      }
    catch (RemarshalException remarsh)
      {
        return resolve(a_name);
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /** {@inheritDoc} */
  public void unbind(NameComponent[] a_name)
              throws NotFound, CannotProceed, InvalidName
  {
    InputStream in = null;
    try
      {
        OutputStream out = _request("unbind", true);
        NameHelper.write(out, a_name);
        in = _invoke(out);
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();

        String id = ex.getId();
        if (id.equals(NotFoundHelper.id()))
          throw NotFoundHelper.read(in);
        else if (id.equals(CannotProceedHelper.id()))
          throw CannotProceedHelper.read(in);
        else if (id.equals(InvalidNameHelper.id()))
          throw InvalidNameHelper.read(in);
        else
          throw new MARSHAL(id);
      }
    catch (RemarshalException remarsh)
      {
        unbind(a_name);
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /**
   * Throw one of the three possible exceptions, as specified in
   * the passed exception repository id.
   *
   * This method should never return normally.
   *
   * @param in the stream to read the exception from.
   * @param id the exception id.
   *
   * @throws InvalidName if the id matches.
   * @throws CannotProceed if the id matches.
   * @throws NotFound if the id matches.
   * @throws MARSHAL if the id does not match any of the previous 4 exceptions.
   */
  void throw4(InputStream in, String id)
                 throws MARSHAL, InvalidName, CannotProceed, NotFound
  {
    if (id.equals(NotFoundHelper.id()))
      throw NotFoundHelper.read(in);
    else if (id.equals(CannotProceedHelper.id()))
      throw CannotProceedHelper.read(in);
    else if (id.equals(InvalidNameHelper.id()))
      throw InvalidNameHelper.read(in);
    else
      throw new MARSHAL(id);
  }

  /**
   * Throw one of the five possible exceptions, as specified in
   * the passed exception repository id.
   *
   * This method should never return normally.
   *
   * @param in the stream to read the exception from.
   * @param id the exception id.
   *
   * @throws AlreadyBound if the id matches.
   * @throws InvalidName if the id matches.
   * @throws CannotProceed if the id matches.
   * @throws NotFound if the id matches.
   * @throws MARSHAL if the id does not match any of the previous 4 exceptions.
   */
  void throw5(InputStream in, String id)
                 throws MARSHAL, AlreadyBound, InvalidName, CannotProceed,
                        NotFound
  {
    if (id.equals(AlreadyBoundHelper.id()))
      throw AlreadyBoundHelper.read(in);
    else
      throw4(in, id);
  }
}
