/* BindingIteratorHelper.java --
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

import gnu.CORBA.Minor;
import gnu.CORBA.OrbRestricted;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;

/**
 * The helper operations for {@link BindingIterator}
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class BindingIteratorHelper
{
  /**
   * The {@link BindingIterator} repository id.
   */
  private static String _id = "IDL:omg.org/CosNaming/BindingIterator:1.0";

  /**
   * Extract the binding iterator from the given {@link Any}.
   */
  public static BindingIterator extract(Any a)
  {
    try
      {
        return ((BindingIteratorHolder) a.extract_Streamable()).value;
      }
    catch (ClassCastException ex)
      {
        BAD_OPERATION bad = new BAD_OPERATION("Binding iterator expected");
        bad.initCause(ex);
        bad.minor = Minor.Any;
        throw bad;
      }
  }

  /**
   * Return the binding iterator repository id.
   */
  public static String id()
  {
    return _id;
  }

  /**
   * Insert the binding iterator into the given {@link Any}.
   */
  public static void insert(Any a, BindingIterator that)
  {
    a.insert_Streamable(new BindingIteratorHolder(that));
  }

  /**
   * Narrow the given object to the BindingIterator. The narrowing
   * means either direct casting or re-instantiating with the same
   * delegate.
   *
   * @param obj the object to cast.
   *
   * @return the casted binding iterator.
   */
  public static BindingIterator narrow(org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof BindingIterator)
      return (BindingIterator) obj;
    else if (!obj._is_a(id()))
      throw new BAD_PARAM();
    else
      {
        Delegate delegate = ((ObjectImpl) obj)._get_delegate();
        return new _BindingIteratorStub(delegate);
      }
  }

  /**
   * Narrow the given object to the BindingIterator. No type-checking is
   * performed to verify that the object actually supports the requested type.
   * The {@link BAD_OPERATION} will be thrown if unsupported operations are
   * invoked on the new returned reference, but no failure is expected at the
   * time of the unchecked_narrow. See OMG issue 4158.
   *
   * @param obj the object to cast.
   *
   * @return the casted binding iterator.
   *
   * @since 1.5
   */
  public static BindingIterator unchecked_narrow(org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof BindingIterator)
      return (BindingIterator) obj;
    else
      {
        // Do not call the _is_a(..).
        Delegate delegate = ((ObjectImpl) obj)._get_delegate();
        return new _BindingIteratorStub(delegate);
      }
  }

  /**
   * Read the exception from the given CDR stream.
   */
  public static BindingIterator read(InputStream istream)
  {
    return narrow(istream.read_Object(_BindingIteratorStub.class));
  }

  /**
   * Create the type code for the BindingIterator.
   */
  public static TypeCode type()
  {
    return OrbRestricted.Singleton.create_interface_tc(id(), "BindingIterator");
  }

  /**
   * Write the exception to the CDR output stream.
   */
  public static void write(OutputStream ostream, BindingIterator value)
  {
    ostream.write_Object((org.omg.CORBA.Object) value);
  }
}
