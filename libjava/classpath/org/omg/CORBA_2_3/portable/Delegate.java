/* Delegate.java --
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

/**
 * Extends the previous version of the Delegate by one additional method,
 * defined in CORBA 2_3 .
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class Delegate
  extends org.omg.CORBA.portable.Delegate
{
  /**
   * Returns the codebase for the provided object reference.
   * A codebase is a location information (for instance, URL), specifying,
   * where the missing parts of the object code can be downloaded. This
   * is important for the value type objects that handle they method calls
   * locally and hence must have the local copy of the implementing code.
   *
   * This method must be overridden to get functionality, the default method
   * always returns an empty string.
   *
   * @return normally, an agreed location information, specifying, where
   * the supporting code (like java classes) can be found.
   */
  public String get_codebase(org.omg.CORBA.Object self)
  {
    return "";
  }
}
