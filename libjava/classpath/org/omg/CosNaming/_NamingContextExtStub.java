/* _NamingContextExtStub.java --
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

import gnu.CORBA.NamingService.NameTransformer;

import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.ObjectHelper;
import org.omg.CORBA.portable.ApplicationException;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.RemarshalException;
import org.omg.CosNaming.NamingContextExtPackage.AddressHelper;
import org.omg.CosNaming.NamingContextExtPackage.InvalidAddress;
import org.omg.CosNaming.NamingContextExtPackage.InvalidAddressHelper;
import org.omg.CosNaming.NamingContextExtPackage.StringNameHelper;
import org.omg.CosNaming.NamingContextExtPackage.URLStringHelper;
import org.omg.CosNaming.NamingContextPackage.CannotProceed;
import org.omg.CosNaming.NamingContextPackage.InvalidName;
import org.omg.CosNaming.NamingContextPackage.InvalidNameHelper;
import org.omg.CosNaming.NamingContextPackage.NotFound;

/**
 * The extended naming context stub (proxy), used on the client side.
 * The most of the {@link NamingContextExt} methods contain the code
 * for remote invocaton. However as remote invocation is potencially an
 * expensive step, some trivial methods, not requiring access to the
 * naming database, are handled locally (see the method descriptions for
 * details).
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class _NamingContextExtStub
  extends _NamingContextStub
  implements NamingContextExt
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = 6333293895664182866L;

  /**
   * This stub can be the base of the two CORBA objects, so it
   * has two repository ids.
   */
  private static String[] __ids =
    { NamingContextExtHelper.id(), NamingContextHelper.id() };

  /**
   * The local name form converter.
   */
  private NameTransformer converter = new NameTransformer();

  /**
   * Create the naming context stub.
   */
  public _NamingContextExtStub()
  {
    super();
  }

  /**
   * Create the naming context stub with the given delegate.
   */
  _NamingContextExtStub(Delegate delegate)
  {
    super(delegate);
  }

  /**
   * Return the array of repository ids for this object.
   * This stub can be the base of the two CORBA objects, so it
   * has two repository ids, for {@link NamingContext} and
   * for {@link NamingContextExt}.
   */
  public String[] _ids()
  {
    return (String[]) __ids.clone();
  }

  /** {@inheritDoc} */
  public org.omg.CORBA.Object resolve_str(String a_name_string)
                                   throws NotFound, CannotProceed, InvalidName
  {
    InputStream in = null;
    try
      {
        OutputStream _out = _request("resolve_str", true);
        StringNameHelper.write(_out, a_name_string);
        in = _invoke(_out);

        return ObjectHelper.read(in);
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();

        String id = ex.getId();
        throw4(in, id);

        // Should never happen.
        throw new InternalError();
      }
    catch (RemarshalException _rm)
      {
        return resolve_str(a_name_string);
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /**
   * Converts the string name representation into the array
   * name representation.
   *
   * This method is handled locally.
   */
  public NameComponent[] to_name(String a_name_string)
                          throws InvalidName
  {
    return converter.toName(a_name_string);
  }

  /**
   * Convert the name array representation to the name string
   * representation.
   *
   * This method is handled locally.
   */
  public String to_string(NameComponent[] a_name)
                   throws InvalidName
  {
    return converter.toString(a_name);
  }

  /** {@inheritDoc} */
  public String to_url(String an_address, String a_name_string)
                throws InvalidAddress, InvalidName
  {
    InputStream in = null;
    try
      {
        OutputStream _out = _request("to_url", true);
        AddressHelper.write(_out, an_address);
        StringNameHelper.write(_out, a_name_string);
        in = _invoke(_out);

        return URLStringHelper.read(in);
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();

        String id = ex.getId();
        if (id.equals(InvalidAddressHelper.id()))
          throw InvalidAddressHelper.read(in);
        else if (id.equals(InvalidNameHelper.id()))
          throw InvalidNameHelper.read(in);
        else
          throw new MARSHAL(id);
      }
    catch (RemarshalException _rm)
      {
        return to_url(an_address, a_name_string);
      }
    finally
      {
        _releaseReply(in);
      }
  }
}