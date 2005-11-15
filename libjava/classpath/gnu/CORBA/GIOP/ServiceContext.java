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

import gnu.CORBA.CDR.AbstractCdrInput;
import gnu.CORBA.CDR.AbstractCdrOutput;

import org.omg.CORBA.BAD_INV_ORDER;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.CompletionStatus;
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
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /* Standard values for the context_id. */
  public static final int TransactionService = 0;

  /**
   * Defines code sets, used to encode wide and narrow characters. Required for
   * messages with data structures, involving wide characters.
   */
  public static final int CodeSets = 1;

  public static final int ChainBypassCheck = 2;

  public static final int ChainBypassInfo = 3;

  public static final int LogicalThreadId = 4;

  public static final int BI_DIR_IIOP = 5;

  public static final int SendingContextRunTime = 6;

  public static final int INVOCATION_POLICIES = 7;

  public static final int FORWARDED_IDENTITY = 8;

  /**
   * Contains exception details if exception being transferred is other than
   * System or User exception. javax.rmi uses this context to transfer arbitrary
   * java exceptions as CORBA value types.
   */
  public static final int UnknownExceptionInfo = 9;

  public static final int RTCorbaPriority = 10;

  public static final int RTCorbaPriorityRange = 11;

  public static final int FT_GROUP_VERSION = 12;

  public static final int FT_REQUEST = 13;

  public static final int ExceptionDetailMessage = 14;

  public static final int SecurityAttributeService = 15;

  public static final int ActivityService = 16;

  /**
   * The context id (for instance, 0x1 for code sets context). At the moment of
   * writing, the OMG defines 16 standard values and provides rules to register
   * the vendor specific context ids. The range 0-4095 is reserved for the
   * future standard OMG contexts.
   */
  public int context_id;

  /**
   * The context_data.
   */
  public byte[] context_data;

  /**
   * Crete unitialised instance.
   */
  public ServiceContext()
  {
  }

  /**
   * Create from omg context.
   */
  public ServiceContext(org.omg.IOP.ServiceContext from)
  {
    context_id = from.context_id;
    context_data = from.context_data;
  }

  /**
   * Read the context values from the stream.
   * 
   * @param istream a stream to read from.
   */
  public static ServiceContext read(AbstractCdrInput istream)
  {
    int id = istream.read_ulong();

    switch (id)
      {
        case CodeSetServiceContext.ID:

          CodeSetServiceContext codeset = new CodeSetServiceContext();
          codeset.readContext(istream);
          return codeset;

        default:

          ServiceContext ctx = new ServiceContext();
          ctx.context_id = id;
          ctx.context_data = istream.read_sequence();
          return ctx;
      }
  }

  /**
   * Read a sequence of contexts from the input stream.
   */
  public static ServiceContext[] readSequence(AbstractCdrInput istream)
  {
    int size = istream.read_long();
    ServiceContext[] value = new gnu.CORBA.GIOP.ServiceContext[size];
    for (int i = 0; i < value.length; i++)
      value[i] = read(istream);
    return value;
  }

  /**
   * Write the context values into the stream.
   * 
   * @param ostream a stream to write the data to.
   */
  public void write(AbstractCdrOutput ostream)
  {
    ostream.write_ulong(context_id);
    ostream.write_sequence(context_data);
  }

  /**
   * Write the sequence of contexts into the input stream.
   */
  public static void writeSequence(AbstractCdrOutput ostream, ServiceContext[] value)
  {
    ostream.write_long(value.length);
    for (int i = 0; i < value.length; i++)
      value[i].write(ostream);
  }

  /**
   * Add context to the given array of contexts.
   */
  public static void add(org.omg.IOP.ServiceContext[] cx,
    org.omg.IOP.ServiceContext service_context, boolean replace)
  {
    int exists = -1;

    for (int i = 0; i < cx.length; i++)
      if (cx[i].context_id == service_context.context_id)
        exists = i;

    if (exists < 0)
      {
        // Add context.
        org.omg.IOP.ServiceContext[] n = new org.omg.IOP.ServiceContext[cx.length + 1];
        for (int i = 0; i < cx.length; i++)
          n[i] = cx[i];
        n[cx.length] = service_context;
      }
    else
      {
        // Replace context.
        if (!replace)
          throw new BAD_INV_ORDER("Repetetive setting of the context "
            + service_context.context_id, 15, CompletionStatus.COMPLETED_NO);
        else
          cx[exists] = service_context;
      }
  }

  /**
   * Add context to the given array of contexts.
   */
  public static ServiceContext[] add(ServiceContext[] cx,
    org.omg.IOP.ServiceContext service_context, boolean replace)
  {
    int exists = -1;

    for (int i = 0; i < cx.length; i++)
      if (cx[i].context_id == service_context.context_id)
        exists = i;

    if (exists < 0)
      {
        // Add context.
        ServiceContext[] n = new ServiceContext[cx.length + 1];
        for (int i = 0; i < cx.length; i++)
          n[i] = cx[i];
        n[cx.length] = new ServiceContext(service_context);
        return n;
      }
    else
      {
        // Replace context.
        if (!replace)
          throw new BAD_INV_ORDER("Repetetive setting of the context "
            + service_context.context_id, 15, CompletionStatus.COMPLETED_NO);
        else
          cx[exists] = new ServiceContext(service_context);
        return cx;
      }
  }

  /**
   * Find context with the given name in the context array.
   */
  public static org.omg.IOP.ServiceContext findContext(int ctx_name,
    org.omg.IOP.ServiceContext[] cx)
  {
    for (int i = 0; i < cx.length; i++)
      if (cx[i].context_id == ctx_name)
        return cx[i];
    throw new BAD_PARAM("No context with id " + ctx_name);
  }

  /**
   * Find context with the given name in the context array, converting into
   * org.omg.IOP.ServiceContext.
   */
  public static org.omg.IOP.ServiceContext findContext(int ctx_name,
    ServiceContext[] cx)
  {
    for (int i = 0; i < cx.length; i++)
      if (cx[i].context_id == ctx_name)
        return new org.omg.IOP.ServiceContext(ctx_name, cx[i].context_data);
    throw new BAD_PARAM("No context with id " + ctx_name);
  }

  /**
   * Find context with the given name in the context array without conversions.
   */
  public static ServiceContext find(int ctx_name, ServiceContext[] cx)
  {
    for (int i = 0; i < cx.length; i++)
      if (cx[i].context_id == ctx_name)
        return cx[i];
    return null;
  }

  /**
   * Return a string representation.
   */
  public String toString()
  {
    return "ctx " + context_id + ", size " + context_data.length;
  }
}
