/* MonitorInfo.java -- class used to return monitor information
   for JDWP.

   Copyright (C) 2007 Free Software Foundation

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


package gnu.classpath.jdwp.util;

import gnu.classpath.jdwp.VMIdManager;
import gnu.classpath.jdwp.id.ObjectId;

import java.io.DataOutputStream;
import java.io.IOException;

/**
 * This class is used to pass monitor information between
 * the JDWP back-end and the virtual machine.
 *
 * @author Keith Seitz  (keiths@redhat.com)
 */
public class MonitorInfo
{
  public int entryCount;
  public Thread owner;
  public Thread[] waiters;

  public void write(DataOutputStream os)
    throws IOException
  {
    VMIdManager idm = VMIdManager.getDefault();
    ObjectId id = idm.getObjectId(owner);
    id.write(os);
    os.write(entryCount);
    os.write(waiters.length);
    for (int i = 0; i < waiters.length; ++i)
      {
	id = idm.getObjectId(waiters[i]);
	id.write(os);
      }
  }
}
