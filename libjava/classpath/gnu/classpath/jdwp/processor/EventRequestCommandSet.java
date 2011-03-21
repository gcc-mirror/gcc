/* EventRequestCommandSet.java -- class to implement the EventRequest Command
   Set
   Copyright (C) 2005, 2007 Free Software Foundation

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


package gnu.classpath.jdwp.processor;

import gnu.classpath.jdwp.JdwpConstants;
import gnu.classpath.jdwp.VMVirtualMachine;
import gnu.classpath.jdwp.event.EventManager;
import gnu.classpath.jdwp.event.EventRequest;
import gnu.classpath.jdwp.event.filters.ClassExcludeFilter;
import gnu.classpath.jdwp.event.filters.ClassMatchFilter;
import gnu.classpath.jdwp.event.filters.ClassOnlyFilter;
import gnu.classpath.jdwp.event.filters.ConditionalFilter;
import gnu.classpath.jdwp.event.filters.CountFilter;
import gnu.classpath.jdwp.event.filters.ExceptionOnlyFilter;
import gnu.classpath.jdwp.event.filters.FieldOnlyFilter;
import gnu.classpath.jdwp.event.filters.IEventFilter;
import gnu.classpath.jdwp.event.filters.InstanceOnlyFilter;
import gnu.classpath.jdwp.event.filters.LocationOnlyFilter;
import gnu.classpath.jdwp.event.filters.StepFilter;
import gnu.classpath.jdwp.event.filters.ThreadOnlyFilter;
import gnu.classpath.jdwp.exception.JdwpException;
import gnu.classpath.jdwp.exception.JdwpInternalErrorException;
import gnu.classpath.jdwp.exception.NotImplementedException;
import gnu.classpath.jdwp.id.ObjectId;
import gnu.classpath.jdwp.id.ReferenceTypeId;
import gnu.classpath.jdwp.id.ThreadId;
import gnu.classpath.jdwp.util.JdwpString;
import gnu.classpath.jdwp.util.Location;

import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;

/**
 * A class representing the EventRequest Command Set.
 *
 * @author Aaron Luchko <aluchko@redhat.com>
 */
public class EventRequestCommandSet
  extends CommandSet
{
  public boolean runCommand(ByteBuffer bb, DataOutputStream os, byte command)
      throws JdwpException
  {
    try
      {
        switch (command)
          {
          case JdwpConstants.CommandSet.EventRequest.SET:
            executeSet(bb, os);
            break;
          case JdwpConstants.CommandSet.EventRequest.CLEAR:
            executeClear(bb, os);
            break;
          case JdwpConstants.CommandSet.EventRequest.CLEAR_ALL_BREAKPOINTS:
            executeClearAllBreakpoints(bb, os);
            break;
          default:
            throw new NotImplementedException("Command " + command +
              " not found in EventRequest Reference Command Set.");
          }
      }
    catch (IOException ex)
      {
        // The DataOutputStream we're using isn't talking to a socket at all
        // So if we throw an IOException we're in serious trouble
        throw new JdwpInternalErrorException(ex);
      }

    return false;
  }

  private void executeSet(ByteBuffer bb, DataOutputStream os)
      throws JdwpException, IOException
  {
    byte eventKind = bb.get();
    byte suspendPolicy = bb.get();
    int modifiers = bb.getInt();

    switch (eventKind)
      {
        case JdwpConstants.EventKind.FIELD_ACCESS:
        if (!VMVirtualMachine.canWatchFieldAccess)
          {
            String msg = "watching field accesses is not supported";
            throw new NotImplementedException(msg);
          }
        break;

        case JdwpConstants.EventKind.FIELD_MODIFICATION:
        if (!VMVirtualMachine.canWatchFieldModification)
          {
            String msg = "watching field modifications is not supported";
            throw new NotImplementedException(msg);
          }
        break;

      default:
        // okay
      }

    EventRequest eventReq = new EventRequest(eventKind, suspendPolicy);
    IEventFilter filter = null;
    ReferenceTypeId refId;
    for (int i = 0; i < modifiers; i++)
      {
        byte modKind = bb.get();
        switch (modKind)
          {
          case JdwpConstants.ModKind.COUNT:
            filter = new CountFilter(bb.getInt());
            break;
          case JdwpConstants.ModKind.CONDITIONAL:
            filter = new ConditionalFilter(idMan.readObjectId(bb));
            break;
          case JdwpConstants.ModKind.THREAD_ONLY:
            filter = new ThreadOnlyFilter((ThreadId) idMan.readObjectId(bb));
            break;
          case JdwpConstants.ModKind.CLASS_ONLY:
            filter = new ClassOnlyFilter(idMan.readReferenceTypeId(bb));
            break;
          case JdwpConstants.ModKind.CLASS_MATCH:
            filter = new ClassMatchFilter(JdwpString.readString(bb));
            break;
          case JdwpConstants.ModKind.CLASS_EXCLUDE:
            filter = new ClassExcludeFilter(JdwpString.readString(bb));
            break;
          case JdwpConstants.ModKind.LOCATION_ONLY:
            filter = new LocationOnlyFilter(new Location(bb));
            break;
          case JdwpConstants.ModKind.EXCEPTION_ONLY:
            long id = bb.getLong();
            if (id == 0)
              refId = null;
            else
              refId = idMan.getReferenceType(id);
            boolean caught = (bb.get() == 0) ? false : true;
            boolean unCaught = (bb.get() == 0) ? false : true;
            filter = new ExceptionOnlyFilter(refId, caught, unCaught);
            break;
          case JdwpConstants.ModKind.FIELD_ONLY:
            refId = idMan.readReferenceTypeId(bb);
            ReferenceTypeId fieldId = idMan.readReferenceTypeId(bb);
            filter = new FieldOnlyFilter(refId, fieldId);
            break;
          case JdwpConstants.ModKind.STEP:
            ThreadId tid = (ThreadId) idMan.readObjectId(bb);
            int size = bb.getInt();
            int depth = bb.getInt();
            filter = new StepFilter(tid, size, depth);
            break;
          case JdwpConstants.ModKind.INSTANCE_ONLY:
            ObjectId oid = idMan.readObjectId(bb);
            filter = new InstanceOnlyFilter(oid);
            break;
          default:
            throw new NotImplementedException("modKind " + modKind
                                              + " is not implemented.");
          }
        eventReq.addFilter(filter);
      }

    EventManager.getDefault().requestEvent(eventReq);
    os.writeInt(eventReq.getId());

  }

  private void executeClear(ByteBuffer bb, DataOutputStream os)
      throws JdwpException, IOException
  {
    byte eventKind = bb.get();
    int requestId = bb.getInt();
    EventManager.getDefault().deleteRequest(eventKind, requestId);
  }

  private void executeClearAllBreakpoints(ByteBuffer bb, DataOutputStream os)
      throws JdwpException, IOException
  {
    byte eventKind = bb.get ();
    EventManager.getDefault().clearRequests (eventKind);
  }

}
