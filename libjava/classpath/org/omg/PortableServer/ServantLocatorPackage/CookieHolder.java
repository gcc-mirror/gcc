/* CookieHolder.java --
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


package org.omg.PortableServer.ServantLocatorPackage;

import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.Streamable;

/**
 * The CookieHolder is used together with the
 * {@link org.omg.PortableServer.ServantLocator}, allowing the
 * {@link org.omg.PortableServer.ServantLocatorOperations#preinvoke}
 * to return an arbitrary java.lang.Object that will be later passed to
 * {@link org.omg.PortableServer.ServantLocatorOperations#postinvoke}.
 *
 * @see org.omg.PortableServer.ServantLocatorOperations
 * @see org.omg.PortableServer.ServantManager
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class CookieHolder
  implements Streamable
{
  /**
   * The wrapped java.lang.Object.
   */
  public java.lang.Object value;

  /**
   * Create the unitialised instance of this holder.
   */
  public CookieHolder()
  {
  }

  /**
   * Create the instance, intialising the {@link #value} field to the passed
   * value.
   */
  public CookieHolder(java.lang.Object initial)
  {
    value = initial;
  }

  /**
   * java.lang.Object is outside the scope of the CORBA standards and the read
   * operation cannot be supported in a language-neutral way.
   *
   * @throws NO_IMPLEMENT always.
   */
  public void _read(InputStream input)
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * There is no CORBA typecode that would specifically match a java.lang.Object.
   *
   * @throws NO_IMPLEMENT always.
   */
  public TypeCode _type()
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * java.lang.Object is outside the scope of the CORBA standards and the write
   * operation cannot be supported in a language-neutral way.
   *
   * @throws NO_IMPLEMENT always.
   */
  public void _write(OutputStream output)
  {
    throw new NO_IMPLEMENT();
  }
}