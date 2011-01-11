/* CurrentOperations.java --
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


package org.omg.PortableInterceptor;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_INV_ORDER;

/**
 * Defines the operations, applicable to the portable interceptor Current.
 *
 * Portable Interceptors Current (also known as PICurrent) is a slot table. Each
 * slot has an integer identifier, can hold a CORBA {@link Any} and is used by
 * some service to transfer data between thread and request contexts. Each
 * service which wishes to use PICurrent reserves a slot or slots at
 * initialization time and uses those slots during the processing of requests
 * and replies.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface CurrentOperations
  extends org.omg.CORBA.CurrentOperations
{
  /**
   * Get data from the slot with the given slot_id.
   *
   * @param slot_id the slot slot_id.
   *
   * @return the Any that was stored in the slot. If the given slot has not been
   * set, the returned Any contains a type code with a TCKind value of tk_null
   * and has no value.
   *
   * @throws InvalidSlot for the unknown slot.
   * @throws BAD_INV_ORDER minor 10 if called from the {@link ORBInitializer}
   * methods.
   */
  Any get_slot(int slot_id) throws InvalidSlot, BAD_INV_ORDER;

  /**
   * Sets data for the slot with the given slot_id.
   *
   * @param slot_id the slot slot_id.
   *
   * @param data the Any that will be stored into the slot.
   *
   * @throws InvalidSlot for the unknown slot.
   * @throws BAD_INV_ORDER minor 10 if called from the {@link ORBInitializer}
   * methods.
   *
   */
  void set_slot(int slot_id, Any data) throws InvalidSlot, BAD_INV_ORDER;
}
