/* ServiceContext.java --
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


import org.omg.CORBA.portable.IDLEntity;

/**
 * Contains the ORB service data being passed.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class ServiceContext
  implements IDLEntity
{
  /**
   * The context data.
   */
  public byte[] context_data;

  /**
   * The context id.
   */
  public int context_id;

  /**
   * Read the context values from the stream.
   *
   * @param istream a stream to read from.
   */
  public static ServiceContext read(cdrInput istream)
  {
    int id = istream.read_ulong();

    switch (id)
      {
        case cxCodeSet.ID :

          cxCodeSet codeset = new cxCodeSet();
          codeset.readContext(istream);
          return codeset;

        default :

          ServiceContext ctx = new ServiceContext();
          ctx.context_id = id;
          ctx.context_data = istream.read_sequence();
          return ctx;
      }
  }

  /**
   * Read a sequence of contexts from the input stream.
   */
  public static ServiceContext[] readSequence(cdrInput istream)
  {
    int size = istream.read_long();
    ServiceContext[] value = new gnu.CORBA.GIOP.ServiceContext[ size ];
    for (int i = 0; i < value.length; i++)
      value [ i ] = read(istream);
    return value;
  }

  /**
   * Write the context values into the stream.
   *
   * @param ostream a stream to write the data to.
   */
  public void write(cdrOutput ostream)
  {
    ostream.write_ulong(context_id);
    ostream.write_sequence(context_data);
  }

  /**
   * Write the sequence of contexts into the input stream.
   */
  public static void writeSequence(cdrOutput ostream, ServiceContext[] value)
  {
    ostream.write_long(value.length);
    for (int i = 0; i < value.length; i++)
      value [ i ].write(ostream);
  }

  /**
   * Return a string representation.
   */
  public String toString()
  {
    return "ctx "+context_id+", size "+context_data.length;
  }
}
