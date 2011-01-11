/* AdapterActivatorOperations.java --
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


package org.omg.PortableServer;

import org.omg.CORBA.OBJECT_NOT_EXIST;


/**
 * Defines the operations, applicable to the AdapterActivator.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface AdapterActivatorOperations
{
  /**
  * This method is invoked when the ORB receives a request for an object
  * reference that identifies a non-existing target POA, to create it.
  * The ORB invokes this operation once for each POA that must be created
  * in order for the target POA to exist. The process remebers creating a
  * nested folder structure, starting from the ancestor POA closest to the
  * root POA. The operation is invoked on the adapter activator of
  * POA that is the parent of the POA that needs to be created.
  *
  * @param parent the parent POA, for that the child POA must be created.
  * @param child_name the name of the child POA that must be created.
  *
  * @return true if the operation has successfully created the needed POA,
  * false that POA cannot be created. In this case, the client will receive
  * the remote exception ({@link OBJECT_NOT_EXIST}, minor code 2).
  */
  boolean unknown_adapter(POA parent, String child_name);
}
