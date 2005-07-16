/* RequestHeader.java -- The GIOP 1.0 request message.
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


package gnu.CORBA.GIOP.v1_0;

import gnu.CORBA.CDR.cdrInput;
import gnu.CORBA.CDR.cdrOutput;

import org.omg.CORBA.portable.IDLEntity;
import gnu.CORBA.GIOP.ServiceContext;
import gnu.CORBA.GIOP.cxCodeSet;

/**
 * The GIOP 1.0 request message.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class RequestHeader
  extends gnu.CORBA.GIOP.RequestHeader
  implements IDLEntity
{
  /**
   * Creates an empty request header, setting requesting principal
   * to byte[] { 'P' }.
   */
  public RequestHeader()
  {
    requesting_principal = new byte[] { 'P' };
  }

  /**
   * Set if the sender expects any response to this message.
   */
  public void setResponseExpected(boolean expected)
  {
    response_expected = expected;
  }

  /**
   * Return true if response is expected.
   */
  public boolean isResponseExpected()
  {
    return response_expected;
  }

  public String bytes(byte[] array)
  {
    StringBuffer b = new StringBuffer();
    for (int i = 0; i < array.length; i++)
      {
        b.append(Integer.toHexString(array [ i ] & 0xFF));
        b.append(" ");
      }
    return b.toString();
  }

  /**
   * Get the string representation of all included contexts.
   */
  public String contexts()
  {
    StringBuffer b = new StringBuffer();
    for (int i = 0; i < service_context.length; i++)
      {
        b.append(service_context [ i ].toString());
        b.append(' ');
      }
    return b.toString();
  }

  /**
   * Reads the header from the stream.
   *
   * Sets the code set of this stream to
   * the code set, specfied in the header.
   *
   * @param in a stream to read from.
   */
  public void read(cdrInput in)
  {
    service_context = ServiceContext.readSequence(in);
    request_id = in.read_ulong();
    response_expected = in.read_boolean();
    object_key = in.read_sequence();
    operation = in.read_string();
    requesting_principal = in.read_sequence();

    in.setCodeSet(cxCodeSet.find(service_context));
  }

  /**
   * Return a string representation.
   */
  public String toString()
  {
    return "Request " + request_id + ", call '" + operation + "' on " +
           bytes(object_key) + ", " +
           (response_expected ? "wait response" : "one way") + ", from " +
           bytes(requesting_principal) + contexts();
  }

  /**
   * Writes the header to the stream.
   *
   * Sets the code set of this stream to
   * the code set, specfied in the header.
   *
   * @param out a stream to write into.
   */
  public void write(cdrOutput out)
  {
    ServiceContext.writeSequence(out, service_context);
    out.write_ulong(request_id);
    out.write_boolean(response_expected);
    out.write_sequence(object_key);
    out.write_string(operation);
    out.write_sequence(requesting_principal);

    out.setCodeSet(cxCodeSet.find(service_context));
  }
}
