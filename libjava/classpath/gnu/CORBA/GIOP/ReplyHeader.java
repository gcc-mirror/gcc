/* ReplyHeader.java --
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


package gnu.CORBA.GIOP;

import gnu.CORBA.CDR.cdrInput;
import gnu.CORBA.CDR.cdrOutput;

/**
 * The header of the standard reply.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public abstract class ReplyHeader
  extends contextSupportingHeader
{
  /**
   * Reply status, if no exception occured.
   */
  public static final int NO_EXCEPTION = 0;

  /**
   * Reply status, user exception.
   */
  public static final int USER_EXCEPTION = 1;

  /**
   * Reply status, system exception.
   */
  public static final int SYSTEM_EXCEPTION = 2;

  /**
   * Reply status, if the client ORB must re - send the request to another
   * destination. The body contains IOR.
   */
  public static final int LOCATION_FORWARD = 3;

  /**
   * Reply status, indicating that the target has permanently changed the
   * address to the supplied IOR.
   */
  public static final int LOCATION_FORWARD_PERM = 4;

  /**
   * Reply status, indicating, that the ORB requires to resend the object
   * address in the required addressing mode, contained as the reply body.
   */
  public static final int NEEDS_ADDRESSING_MODE = 5;

  /**
   * The status of this reply, holds one of the reply status constants.
   */
  public int reply_status;

  /**
   * The Id of request into response of which this reply has been sent.
   */
  public int request_id;

  /**
   * Return the message status as a string.
   */
  public String getStatusString()
  {
    switch (reply_status)
      {
        case NO_EXCEPTION:
          return "ok";

        case USER_EXCEPTION:
          return "user exception";

        case SYSTEM_EXCEPTION:
          return "system exception";

        case LOCATION_FORWARD:
          return "moved";

        default:
          return null;
      }
  }

  /**
   * Reads the header from the stream.
   *
   * @param in a stream to read from.
   */
  public abstract void read(cdrInput in);

  /**
   * Returns a short string representation.
   *
   * @return a string representation.
   */
  public String toString()
  {
    String status = getStatusString();
    if (status == null)
      status = "status " + reply_status;
    return request_id + ", " + status;
  }

  /**
   * Writes the header to the stream.
   *
   * @param out a stream to write into.
   */
  public abstract void write(cdrOutput out);
}
