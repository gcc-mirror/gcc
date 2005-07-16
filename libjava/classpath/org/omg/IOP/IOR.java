/* IOR.java --
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


package org.omg.IOP;

import org.omg.CORBA.portable.IDLEntity;

import java.io.Serializable;

/**
 * <p>
 * The object IOR contains sufficient information for finding local or
 * remote CORBA object. It also contains additional data like the object
 * native and supported char sets, URLs to download the required additional
 * java classes and so on. IOR can define multiple alternative addresses
 * for the same object or indicate that the object is equal to null.
 * </p><p>
 * The IOR is a standard structure, transferred when sending an object with
 * {@link org.omg.CORBA.portable.OutputStream#write_Object} and receiving with
 * {@link org.omg.CORBA.portable.InputStream#read_Object}. The stringified
 * object references, managed by {@link org.omg.CORBA.ORB#string_to_object}
 * and {@link org.omg.CORBA.ORB#object_to_string} are also IORs, where the
 * initially binary data are encoded as strings using hexadecimal notation.
 * </p><p>
 * The IOR is represented as the object repository id, followed
 * by the sequence of the abstract profiles, each having the integer type
 * identifier and the followed array of binary data. The empty sequence of
 * profiles represents a null object, written, for instance, in response
 * to the call of write_Object(null).
 *
 * @specnote GNU Classpath has its own implementation of IOR machinery at
 * gnu.CORBA.IOR. The reason is that IORs are required from 1.2, but only
 * in 1.4 the associated classes appear in the public API.
 */
public class IOR
  implements IDLEntity, Serializable
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = 1901439890645554948L;

  /**
   * The profiles, associated with this IOR reference. The possible
   * profiles are listed in {@link TaggedProfile} description.
   */
  public TaggedProfile[] profiles;

  /**
   * The object repository Id.
   */
  public String type_id;

  /**
   * Create an unitialised instance of IOR profile.
   *
   * @specnote The profile will be intialised to the IOR, representing
   * a null object.
   */
  public IOR()
  {
    type_id = "";
    profiles = new TaggedProfile[ 0 ];
  }

  /**
   * Create the IOR, initialised with the passed data.
   *
   * @param _type_id the repository id for this IOR object.
   * @param _profiles the array of profiles for this IOR.
   */
  public IOR(String _type_id, TaggedProfile[] _profiles)
  {
    type_id = _type_id;
    profiles = _profiles;
  }
}