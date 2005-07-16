/* ObjectImpl.java --
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


package org.omg.CORBA_2_3.portable;

import org.omg.CORBA.BAD_OPERATION;

/**
 * Extends the previous version of the CORBA object by one additional method,
 * defined in CORBA 2_3 . See ancestor for details about the CORBA object.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class ObjectImpl
  extends org.omg.CORBA.portable.ObjectImpl
{
  /**
   * Returns the codebase for the provided object reference.
   * A codebase is a location information (for instance, URL), specifying,
   * where the missing parts of the object code can be downloaded. This
   * is important for the value type objects that handle they method calls
   * locally and hence must have the local copy of the implementing code.
   *
   * This implementation expects that the object delegate is set and is
   * an instance of the org.omg.CORBA_2_3.portable.Delegate. If casts
   * the set delegate to the given type and invokes
   * {@link org.omg.CORBA_2_3.portable.Delegate#get_codebase}.
   *
   * @return normally, an agreed location information, specifying, where
   * the supporting code (like java classes) can be found.
   *
   * @throws BAD_OPERATION if the object delegate is not an instance
   * of org.omg.CORBA_2_3.portable.Delegate.
   */
  public String _get_codebase()
  {
    Object delegate = _get_delegate();
    if (delegate instanceof org.omg.CORBA_2_3.portable.Delegate)
      return ((org.omg.CORBA_2_3.portable.Delegate) delegate).get_codebase(this);
    else if (delegate != null)
      throw new BAD_OPERATION(delegate.getClass().getName() +
                              " is not a org.omg.CORBA_2_3.portable.Delegate"
                             );
    else
      throw new BAD_OPERATION("The delegate not set.");
  }
}