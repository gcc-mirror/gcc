/* SyncScopeHelper.java --
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


package org.omg.Messaging;

import gnu.CORBA.primitiveTypeCode;
import gnu.CORBA.recordTypeCode;

import org.omg.CORBA.Any;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * A helper operations for synchronization scope as an alias of
 * <code>short</code>.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 *
 * @see SYNC_WITH_TRANSPORT
 */
public class SyncScopeHelper
{
  /**
   * A cached typecode value, computed only once.
   */
  private static TypeCode typecode;

  /**
   * Delegates call to {@link Any.extract_short()}.
   */
  public static short extract(Any a)
  {
    return a.extract_short();
  }

  /**
   * Returns synchronization scope repository id.
   *
   * @return "IDL:omg.org/Messaging/SyncScope:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/Messaging/SyncScope:1.0";
  }

  /**
   * Delegates call to {@link Any.insert_short(short)}.
   */
  public static void insert(Any a, short that)
  {
    a.insert_short(that);
  }

  /**
   * Delegates call to {@link InputStream.read_short()}.
   */
  public static short read(InputStream istream)
  {
    return istream.read_short();
  }

  /**
   * Returns a typecode of the synchronization scope, stating it
   * is an alias of <code>short</code>, named "SyncScope".
   *
   * @return a typecode of synchronization scope.
   */
  public static TypeCode type()
  {
    recordTypeCode r = new recordTypeCode(TCKind.tk_alias);
    r.setName("SyncScope");
    r.setId(id());
    r.setContentType(new primitiveTypeCode(TCKind.tk_short));
    return r;
  }

  /**
   * Delegates call to {@link OutputStream.write_short()}.
   */
  public static void write(OutputStream ostream, short value)
  {
    ostream.write_short(value);
  }
}