/* NameComponent.java --
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


package org.omg.CosNaming;

import org.omg.CORBA.portable.IDLEntity;

/**
 * The name component, a node in the multi-comonent name.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public final class NameComponent
  implements IDLEntity
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = -1052538183391762390L;

  /**
   * The name component identifier.
   */
  public String id;

  /**
  * The name component kind (this conception is similar to
  * the file type conception.
  */
  public String kind;

  /**
   * Create the empty name component.
   */
  public NameComponent()
  {
  }

  /**
   * Create the initialised name component.
   */
  public NameComponent(String _id, String _kind)
  {
    id = _id;
    kind = _kind;
  }

  /**
   * Returns true if both name and kind are equal.
   */
  public boolean equals(Object other)
  {
    if (other instanceof NameComponent)
      {
        NameComponent n = (NameComponent) other;

        boolean ieq;
        boolean keq;

        if (id == null || n.id == null)
          ieq = id == n.id;
        else
          ieq = id.equals(n.id);

        if (kind == null || n.kind == null)
          keq = kind == n.kind;
        else
          keq = kind.equals(n.kind);

        return keq && ieq;
      }
    else
      return false;
  }

  /**
   * Get a string representation (id.kind).
   */
  public String toString()
  {
    return id + "." + kind;
  }
}