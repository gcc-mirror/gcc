/* gnuForwardRequest.java --
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


package gnu.CORBA.Poa;

import gnu.CORBA.GIOP.ReplyHeader;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.portable.ObjectImpl;

/**
 * The class, indicating that the request should be forwarded to another
 * target. We cannot use ForwardRequest because the exception is throws
 * from methods that does not declare throwing it. Hence must be
 * RuntimeException.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuForwardRequest
  extends RuntimeException
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = -1L;

  /**
   * The object reference, indicating the new location of the invocation target.
   */
  public ObjectImpl forward_reference;

  /**
   * This information shows if we use LOCATION_FORWARD or
   * LOCATION_FORWARD_PERM in request. By defalult, LOCATION_FORWARD
   * is always used. To use LOCATION_FORWARD_PERM, this exception should
   * be thrown from the servant manager instead of ForwardRequest,
   * with this field set to  ReplyHeader.LOCATION_FORWARD_PERM.
   */
  public byte forwarding_code = ReplyHeader.LOCATION_FORWARD;

  /**
   * Create the ForwardRequest with explaining message and
   * initialising the object reference to the given value.
   *
   * @param why a string, explaining, why this exception has been thrown.
   * @param a_forward_reference a value for forward_reference.
   */
  public gnuForwardRequest(org.omg.CORBA.Object a_forward_reference)
  {
    if (a_forward_reference instanceof ObjectImpl)
      this.forward_reference = (ObjectImpl) a_forward_reference;
    else
      throw new BAD_PARAM("ObjectImpl expected");
  }
}
