/* ServiceDetail.java --
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

import org.omg.CORBA.portable.IDLEntity;

/**
 * An object, representing one of the additional details about the
 * particular ORB service.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public final class ServiceDetail
  implements IDLEntity
{
  /**
   * The type of the information, defined by this detail.
   */
  public int service_detail_type;

  /**
   * The binary data, defining the information.
   */
  public byte[] service_detail;

  /**
   * Create an unitialised instance.
   */
  public ServiceDetail()
  {
  }

  /**
   * Create the instance of the service detail, initialising its fields
   * to the given values.
   *
   * @param _service_detail_type the service detail type.
   * @param _service_detail the service detail.
   */
  public ServiceDetail(int _service_detail_type, byte[] _service_detail)
  {
    service_detail_type = _service_detail_type;
    service_detail = _service_detail;
  }
}
