/* ORB.java --
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


package org.omg.CORBA_2_3;

import javax.rmi.CORBA.Tie;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.portable.ValueFactory;

/**
 * This class should provide the {@link org.omg.CORBA.ORB}) ORB extensions,
 * defined in the OMG CORBA version 2.3 specification. However in the
 * Sun's API specification is written that this functionality is not
 * implemented at least at least till 1.4 inclusive.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public abstract class ORB
  extends org.omg.CORBA.ORB
{
  /**
   * Should return a defintion of the value type as described
   * in the interface repository.
   *
   * @param repository_id a value type repository id.
   *
   * @return never
   * @throws BAD_PARAM never
   * @throws NO_IMPLEMENT, always.
   *
   * @see org.omg.CORBA.portable.ValueBase
   */
  public org.omg.CORBA.Object get_value_def(String repository_id)
                                     throws BAD_PARAM
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * This should register the given value factory under the given
   * repository id.
   *
   * @param repository_id a repository id
   *
   * @return never
   * @throws NO_IMPLEMENT, always.
   */
  public ValueFactory register_value_factory(String repository_id,
                                             ValueFactory factory
                                            )
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * This should unregister the given value factory under the given
   * repository id.
   *
   * @param repository_id a repository id
   *
   * @throws NO_IMPLEMENT, always.
   */
  public void unregister_value_factory(String repository_id)
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * This should find a suitable value factory to create an instance
   * of the value type when is being read (unmarshaled) from the
   * stream.
   *
   * @param repository_id a repository id
   *
   * @return never
   * @throws NO_IMPLEMENT, always.
   */
  public ValueFactory lookup_value_factory(String repository_id)
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * This method is called by RMI-IIOP {@link Tie#orb(ORB)},
   * passing <code>this</code> as parameter. The ORB will try to connect
   * that tie as one of its objects.
   */
  public void set_delegate(java.lang.Object wrapper)
  {
    throw new NO_IMPLEMENT();
  }
}
