/* PersistentBidiHasthable.java -- Constants for the persistent tables.
   Copyright (C) 2006 Free Software Foundation, Inc.

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

package gnu.classpath.tools.common;

import java.util.Timer;
import java.util.TimerTask;

/**
 * The static fields, shared by the multiple classes, implementing the
 * persistent work.
 * 
 * @author Audrius Meskauskas (audriusa@bioinformatics.org)
 */
public interface Persistent
{
  /**
   * Sheduled termination task.
   */
  static class ExitTask extends TimerTask
  {
    public void run()
    {
      System.exit(0);
    }
  }
  
  /**
   * The timer, sheduling all disk database update events, shared by all
   * instances.
   */
  static Timer timer = new Timer(true);

  /**
   * The longest time, in ms, after that the database content on the disk must
   * be updated. The algorithm is written to avoid the very frequent writings to
   * the disk.
   */
  static long SAVE_AT_MOST_AFTER = 5000;

  /**
   * States how long the database may stay not updated during the intensive
   * operations, in ms. Otherwise the intensively used structure may never
   * be stored to the disk.
   */
  static long ALWAYS_UPDATE = 300000;
  
  /**
   * Write the database content to the disk.
   */
  void writeContent();  
  
}
