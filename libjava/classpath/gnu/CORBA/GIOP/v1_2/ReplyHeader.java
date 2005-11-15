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


package gnu.CORBA.GIOP.v1_2;

import gnu.CORBA.CDR.AbstractCdrInput;
import gnu.CORBA.CDR.AbstractCdrOutput;
import gnu.CORBA.GIOP.ServiceContext;
import gnu.CORBA.GIOP.CodeSetServiceContext;

/**
 * GIOP 1.2 reply header.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class ReplyHeader
  extends gnu.CORBA.GIOP.v1_0.ReplyHeader
{
  /**
   * Adds the standard encoding context.
   */
  public ReplyHeader()
  {
    service_context = new ServiceContext[] { CodeSetServiceContext.STANDARD };
  }

  /**
   * Return the message status as a string.
   */
  public String getStatusString()
  {
    String s = super.getStatusString();
    if (s != null)
      return s;
    switch (reply_status)
      {
        case LOCATION_FORWARD_PERM :
          return "moved permanently";

        case NEEDS_ADDRESSING_MODE :
          return "the alternative addressing mode required";

        default :
          return null;
      }
  }

  /**
   * Reads the header from the stream.
   * The fields go in different order than in the previous GIOP versions.
   *
   * Sets the code set of this stream to
   * the code set, specfied in the header.
   *
   * @param in a stream to read from.
   */
  public void read(AbstractCdrInput in)
  {
    request_id = in.read_ulong();
    reply_status = in.read_ulong();
    service_context = gnu.CORBA.GIOP.ServiceContext.readSequence(in);

    in.setCodeSet(CodeSetServiceContext.find(service_context));
  }

  /**
   * Writes the header to the stream.
   * The fields go in different order than in the previous GIOP versions.
   *
   * Sets the code set of this stream to
   * the code set, specfied in the header.
   *
   * @param out a stream to write into.
   */
  public void write(AbstractCdrOutput out)
  {
    out.write_ulong(request_id);
    out.write_ulong(reply_status);
    gnu.CORBA.GIOP.ServiceContext.writeSequence(out, service_context);

    out.setCodeSet(CodeSetServiceContext.find(service_context));
  }
}
