/* OMGVMCID.java -- 
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


package org.omg.CORBA;

/**
 * </p>
 * The higher 20 bits of any CORBA exception hold "Vendor Minor Codeset ID"
 * (VMCID), for instance 0x4F4D0000 (OMG standard), 0x54410000 (TAO), 0x4A430000
 * (JacORB), 0x49540000 (IONA), 0x53550000 (Sun).
 * </p>
 * <p>
 * GNU Classpath official vendor minor code set id is 0x47430000 ("GC\x00\x00"),
 * and the reserved space spans till 0x47430FFF, allowing to use up to 4096
 * Classpath specific exceptions. It was assigned 30/09/2005 by OMG Vice President
 * Andrew Watson.
 * </p>
 * <p>
 * The standard minor codes for the standard system exceptions are prefaced by
 * the VMCID assigned to OMG, defined as 0x4F4D0000 (the code of the minor field
 * for the standard exception with minor code 1 is 0x4F4D0001). Within a vendor
 * assigned space, the assignment of values to minor codes is left to the
 * vendor.
 * </p>
 * 
 * <p>
 * The VMCID 0 and 0xFFFFF0000 are reserved for experimental use.
 * </p>
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public interface OMGVMCID
{
  /**
   * The OMG vendor minor code ID.
   */
  int value = 0x4F4D0000;
}
