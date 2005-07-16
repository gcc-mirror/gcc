/* CodecFactoryHelper.java --
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

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
* The helper operations for the
* CORBA object {@link CodecFactory}.
*
* @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
*/
public abstract class CodecFactoryHelper
{
  /**
   * The cached {@link CodecFactory} typecode, computed once.
   */
  private static TypeCode typeCode;

  /**
   * Get the type code of the {@link CodecFactory}.
   */
  public static TypeCode type()
  {
    if (typeCode == null)
      typeCode = ORB.init().create_interface_tc(id(), "CodecFactory");
    return typeCode;
  }

  /**
   * Insert the CodecFactory into the given Any.
   *
   * @param any the Any to insert into.
   * @param that the CodecFactory to insert.
   */
  public static void insert(Any any, CodecFactory that)
  {
    any.insert_Object(that);
  }

  /**
   * Extract the CodecFactory from given Any.
   *
   * @throws BAD_OPERATION if the passed Any does not contain CodecFactory.
   */
  public static CodecFactory extract(Any any)
  {
    return narrow(any.extract_Object());
  }

  /**
   * Get the CodecFactory repository id.
   *
   * @return "IDL:omg.org/IOP/CodecFactory:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/IOP/CodecFactory:1.0";
  }

  /**
   * Cast the passed object into the CodecFactory. As the CodecFactory
   * is a local object, this is not different from the java type cast.
   *
   * @throws BAD_PARAM if the passed object is not a CodecFactory.
   */
  public static CodecFactory narrow(org.omg.CORBA.Object obj)
  {
    try
      {
        return (CodecFactory) obj;
      }
    catch (ClassCastException ex)
      {
        BAD_PARAM bad = new BAD_PARAM("CodecFactory expected");
        bad.initCause(ex);
        throw bad;
      }
  }

  /**
   * This should read the CodecFactory from the CDR intput stream,
   * but this is not possible as CodecFactory is a local object.
   *
   * @specnote Suns implementation (1.4) throws this exception either.
   *
   * @throws {@link MARSHAL}, minor code 0 and incomplete, always.
   */
  public static CodecFactory read(InputStream input)
  {
    throw new MARSHAL(UNSUPPORTED, 0, CompletionStatus.COMPLETED_NO);
  }

  /**
   * This should write the CodecFactory from the CDR intput stream,
   * but this is not possible as CodecFactory is a local object.
   *
   * @specnote Suns implementation (1.4) throws this exception either.
   *
   * @throws {@link MARSHAL}, minor code 0 and incomplete, always.
   */
  public static void write(OutputStream output, CodecFactory value)
  {
    throw new MARSHAL(UNSUPPORTED, 0, CompletionStatus.COMPLETED_NO);
  }

  private static String UNSUPPORTED =
    "The operation is unsupported for CodecFactory because it is a local object";
}