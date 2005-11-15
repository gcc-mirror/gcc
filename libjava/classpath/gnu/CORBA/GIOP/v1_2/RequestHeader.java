/* RequestHeader.java --
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

import gnu.CORBA.Minor;
import gnu.CORBA.CDR.AbstractCdrInput;
import gnu.CORBA.CDR.AbstractCdrOutput;
import gnu.CORBA.GIOP.ServiceContext;
import gnu.CORBA.GIOP.CodeSetServiceContext;

import java.io.IOException;

import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.NO_IMPLEMENT;

/**
 * The GIOP 1.2 request header. The GIOP 1.1 request header
 * is the same as GIOP 1.0 request header, if taking the
 * alignment into consideration.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class RequestHeader
  extends gnu.CORBA.GIOP.v1_0.RequestHeader
{
  /** 
   * Use serialVersionUID for interoperability. 
   */
  private static final long serialVersionUID = 1;
  
  /**
   * Indicates that the object is addressed by the object key.
   */
  public static final short KeyAddr = 0;

  /**
   * Indicates that the object is addressed by the IOP tagged profile.
   */
  public static final short ProfileAddr = 1;

  /**
   * Indicates that the objec is addressed by IOR addressing info.
   */
  public static final short ReferenceAddr = 2;

  /**
   * The response flags of the header. By default, the flags are initialised
   * by value 0x3 (response expected).
   */
  public byte response_flags = 3;

  /**
   * The used addressing method.
   */
  public short AddressingDisposition;

  /**
   * Adds the standard encoding context.
   */
  public RequestHeader()
  {
    service_context = new ServiceContext[] { CodeSetServiceContext.STANDARD };
  }

  /**
   * Set if the sender expects any response to this message.
   * Clears or sets the 2 lower bits of flags
   * (0 - not expected, 0x3 - expected).
   */
  public void setResponseExpected(boolean expected)
  {
    response_expected = expected;

    if (expected)
      response_flags = (byte) (response_flags | 0x3);
    else
      response_flags = (byte) (response_flags & (~0x3));
  }

  /**
   * Return true if response is expected.
   *
   * @return true if the two lowest bits of the flags are set or
   * the response expected is explicitly set to true.
   */
  public boolean isResponseExpected()
  {
    return response_expected || ((response_flags & 0x3) == 0x3);
  }

  /**
   * Read the header from the given stream.
   *
   * @param in a stream to read from.
   */
  public void read(AbstractCdrInput in)
  {
    try
      {
        request_id = in.read_ulong();
        response_flags = (byte) in.read();

        // Skip 3 reserved octets:
        in.skip(3);

        // Read target address.
        AddressingDisposition = in.read_ushort();

        switch (AddressingDisposition)
          {
            case KeyAddr :
              object_key = in.read_sequence();
              break;

            // TODO FIXME add other addressing methods.
            case ProfileAddr :
              throw new NO_IMPLEMENT("Object addressing by IOP tagged profile");

            case ReferenceAddr :
              throw new NO_IMPLEMENT("Object addressing by by IOR addressing info");

            default :
              MARSHAL m = new MARSHAL("Unknow addressing method in request, " +
                                AddressingDisposition
                               );
              m.minor = Minor.UnsupportedAddressing;
              throw m;
          }

        operation = in.read_string();
        service_context = gnu.CORBA.GIOP.ServiceContext.readSequence(in);

        // No requesting principal in this new format.
        in.setCodeSet(CodeSetServiceContext.find(service_context));
      }
    catch (IOException ex)
      {
        MARSHAL t = new MARSHAL();
        t.minor = Minor.Header;
        t.initCause(ex);
        throw t;
      }
  }

  /**
   * Return a string representation.
   */
  public String toString()
  {
    return "Request " + request_id + ", call '" + operation + "' on " +
           bytes(object_key) + ", " +
           (response_expected ? "wait response" : "one way") +
           " addressed by " + " method " + AddressingDisposition + "." +
           contexts();
  }

  /**
   * Write the header to the given stream.
   *
   * @param out a stream to write into.
   */
  public void write(AbstractCdrOutput out)
  {
    out.write_ulong(request_id);

    out.write(response_flags);

    // Skip 3 reserved octets:
    out.write(0);
    out.write(0);
    out.write(0);

    // Write addressing disposition from IOR.
    // TODO FIXME add other addressing methods.
    out.write_ushort(KeyAddr);

    out.write_sequence(object_key);

    out.write_string(operation);

    ServiceContext.writeSequence(out, service_context);

    // No requesting principal in this new format.
    out.setCodeSet(CodeSetServiceContext.find(service_context));
  }
}
