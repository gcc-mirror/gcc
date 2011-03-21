/* _IDLTypeStub.java --
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

import gnu.CORBA.Minor;
import gnu.CORBA.TypeCodeHelper;

import org.omg.CORBA.portable.ApplicationException;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.RemarshalException;

import java.io.Serializable;

/**
 * The stub for the IDL type. This stub can be used to access the
 * remote IDL type object, if its IOR is known. To create the
 * working instance with the known IOR, pass {@link gnu.CORBA.IorDelegate}
 * to the constructor.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class _IDLTypeStub
  extends ObjectImpl
  implements IDLType, Serializable
{
  /**
  * Use serialVersionUID (v1.4) for interoperability.
  */
  private static final long serialVersionUID = 9150293942452453626L;

  /**
   * Create the instance of the IDL type stub without
   * the set delegate. The delegate must be set anyway before calling
   * any remote method.
   */
  public _IDLTypeStub()
  {
  }

  /**
   * Create an instance with the given delegate.
   *
   * @see gnu.CORBA.IorDelegate
   */
  public _IDLTypeStub(Delegate delegate)
  {
    _set_delegate(delegate);
  }

  /**
   * Get the typecode of the remote IDL type object. The method is
   * written following OMG specification, treating the typecode
   * as a read only attribute rather than a method. This means,
   * the operation name is "_get_type".
   *
   * @return a typecode, returned by the remote IDL type object.
   */
  public TypeCode type()
  {
    InputStream in = null;
    try
      {
        OutputStream out = _request("_get_type", true);
        in = _invoke(out);
        return TypeCodeHelper.read(in);
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();
        throw new org.omg.CORBA.MARSHAL(ex.getId());
      }
    catch (RemarshalException rex)
      {
        return type();
      }
    catch (UserException ex)
      {
        MARSHAL m = new MARSHAL();
        m.minor = Minor.UserException;
        m.initCause(ex);
        throw m;
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /**
   * Get the definition kind of the remote IDL type object. The method is
   * written following OMG specification, treating the typecode
   * as a read only attribute rather than a method. This means,
   * the operation name is "_get_def_kind".
   *
   * @return a definition kind, returned by remote IDL type object.
   */
  public DefinitionKind def_kind()
  {
    InputStream in = null;
    try
      {
        OutputStream out = _request("_get_def_kind", true);
        in = _invoke(out);
        return DefinitionKindHelper.read(in);
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();
        throw new org.omg.CORBA.MARSHAL(ex.getId());
      }
    catch (RemarshalException rex)
      {
        return def_kind();
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /**
   * Destroy the remote IDL type object.
   */
  public void destroy()
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
        throw new org.omg.CORBA.MARSHAL(ex.getId());
      }
    catch (RemarshalException rex)
      {
        destroy();
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /**
   * Return the array of repository ids of the IDL type.
   *
   * @return "IDL:omg.org/CORBA/IDLType:1.0" and
   *  "IDL:omg.org/CORBA/IRObject:1.0", always.
   */
  public String[] _ids()
  {
    return new String[]
           {
             "IDL:omg.org/CORBA/IDLType:1.0", "IDL:omg.org/CORBA/IRObject:1.0"
           };
  }
}
