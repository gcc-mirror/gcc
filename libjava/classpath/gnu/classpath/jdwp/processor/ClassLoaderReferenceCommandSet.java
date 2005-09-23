/* ClassLoaderReferenceCommandSet.java -- class to implement the 
   ClassLoaderReference Command Set
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
import gnu.classpath.jdwp.exception.JdwpException;
import gnu.classpath.jdwp.exception.JdwpInternalErrorException;
import gnu.classpath.jdwp.exception.NotImplementedException;
import gnu.classpath.jdwp.id.ObjectId;
import gnu.classpath.jdwp.id.ReferenceTypeId;

import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * A class representing the ClassLoaderReference Command Set.
 * 
 * @author Aaron Luchko <aluchko@redhat.com>
 */
public class ClassLoaderReferenceCommandSet
  extends CommandSet
{
  public boolean runCommand(ByteBuffer bb, DataOutputStream os, byte command)
      throws JdwpException
  {

    // Although there's only a single command to choose from we still use
    // a switch to maintain consistency with the rest of the CommandSets
   try
      {
        switch (command)
          {
          case JdwpConstants.CommandSet.ClassLoaderReference.VISIBLE_CLASSES:
            executeVisibleClasses(bb, os);
            break;
          default:
            throw new NotImplementedException("Command " + command +
            " not found in ClassLoaderReference Command Set.");
          }
      }
    catch (IOException ex)
      {
        // The DataOutputStream we're using isn't talking to a socket at all
        // So if we throw an IOException we're in serious trouble
        throw new JdwpInternalErrorException(ex);
      }
    return true;
  }

  public void executeVisibleClasses(ByteBuffer bb, DataOutputStream os)
      throws JdwpException, IOException
  {
    ObjectId oId = idMan.readObjectId(bb);
    ClassLoader cl = (ClassLoader) oId.getObject();
    ArrayList loadRequests = VMVirtualMachine.getLoadRequests(cl);
    os.writeInt(loadRequests.size());
    for (Iterator iter = loadRequests.iterator(); iter.hasNext();)
      {
        Class clazz = (Class)iter.next();
        ReferenceTypeId refId = idMan.getReferenceTypeId(clazz);
        refId.writeTagged(os);
      }
  }

}
