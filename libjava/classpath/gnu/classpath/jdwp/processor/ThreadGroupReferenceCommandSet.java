/* ThreadGroupReferenceCommandSet.java -- class to implement the 
   ThreadGroupReference Command Set
   Copyright (C) 2005 Free Software Foundation
 
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
import gnu.classpath.jdwp.exception.JdwpException;
import gnu.classpath.jdwp.exception.JdwpInternalErrorException;
import gnu.classpath.jdwp.exception.NotImplementedException;
import gnu.classpath.jdwp.id.ObjectId;
import gnu.classpath.jdwp.util.JdwpString;

import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;

/**
 * A class representing the ThreadGroupReference Command Set.
 * 
 * @author Aaron Luchko <aluchko@redhat.com>
 */
public class ThreadGroupReferenceCommandSet
  extends CommandSet
{
  public boolean runCommand(ByteBuffer bb, DataOutputStream os, byte command)
    throws JdwpException
  {
    try
      {
        switch (command)
          {
          case JdwpConstants.CommandSet.ThreadGroupReference.NAME:
            executeName(bb, os);
            break;
          case JdwpConstants.CommandSet.ThreadGroupReference.PARENT:
            executeParent(bb, os);
            break;
          case JdwpConstants.CommandSet.ThreadGroupReference.CHILDREN:
            executeChildren(bb, os);
            break;
          default:
            throw new NotImplementedException("Command " + command +
              " not found in ThreadGroupReference Command Set.");
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

  private void executeName(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ObjectId oid = idMan.readObjectId(bb);
    ThreadGroup group = (ThreadGroup) oid.getObject();
    JdwpString.writeString(os, group.getName());
  }

  private void executeParent(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ObjectId oid = idMan.readObjectId(bb);
    ThreadGroup group = (ThreadGroup) oid.getObject();
    ThreadGroup parent = group.getParent();
    if (parent == null) {
    	os.writeLong(0L);
    } else {
    	ObjectId parentId = idMan.getObjectId(parent);
    	parentId.write(os);   	
    }
  }

  private void executeChildren(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ObjectId oid = idMan.readObjectId(bb);
    ThreadGroup group = (ThreadGroup) oid.getObject();

    ThreadGroup jdwpGroup = Thread.currentThread().getThreadGroup();
    int numThreads = group.activeCount();
    Thread allThreads[] = new Thread[numThreads];

    group.enumerate(allThreads, false);

    // We need to loop through for the true count since some threads may have
    // been destroyed since we got activeCount so those spots in the array will
    // be null. As well we must ignore any threads that belong to jdwp
    numThreads = 0;
    for (int i = 0; i < allThreads.length; i++)
      {
        Thread thread = allThreads[i];
        if (thread == null)
          break; // No threads after this point
        if (!thread.getThreadGroup().equals(jdwpGroup))
          numThreads++;
      }

    os.writeInt(numThreads);

    for (int i = 0; i < allThreads.length; i++)
      {
        Thread thread = allThreads[i];
        if (thread == null)
          break; // No threads after this point
        if (!thread.getThreadGroup().equals(jdwpGroup))
          idMan.getObjectId(thread).write(os);
      }

    int numGroups = group.activeCount();
    ThreadGroup allGroups[] = new ThreadGroup[numGroups];

    group.enumerate(allGroups, false);

    // We need to loop through for the true count since some ThreadGroups may
    // have been destroyed since we got activeCount so those spots in the array
    // will be null. As well we must ignore any threads that belong to jdwp.
    numGroups = 0;
    for (int i = 0; i < allGroups.length; i++)
      {
        ThreadGroup tgroup = allGroups[i];
        if (tgroup == null)
          break; // No ThreadGroups after this point
        if (!tgroup.equals(jdwpGroup))
          numGroups++;
      }

    os.writeInt(numGroups);

    for (int i = 0; i < allGroups.length; i++)
      {
        ThreadGroup tgroup = allGroups[i];
        if (tgroup == null)
          break; // No ThreadGroups after this point
        if (!tgroup.equals(jdwpGroup))
          idMan.getObjectId(tgroup).write(os);
      }
  }
}
