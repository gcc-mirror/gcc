/* GtkClipboardNotifier.java -- Helper for announcing GtkSelection changes.
   Copyright (C) 2005  Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import java.awt.datatransfer.*;

class GtkClipboardNotifier extends Thread
{
  /** Whether or not to announce a GtkSelection change. */
  private static boolean announceChange;

  /**
   * The one and only instance. All operations are synchronized on
   * this.
   */
  private static GtkClipboardNotifier notifier = new GtkClipboardNotifier();

  /**
   * Creates a deamon thread that monitors this for change
   * announcements.
   */
  private GtkClipboardNotifier()
  {
    super("GtkClipBoardNotifier");
    setDaemon(true);
    start();
  }

  /**
   * Notifies that a new GtkSelection has to be announced.
   */
  static void announce()
  {
    synchronized (notifier)
      {
	announceChange = true;
	notifier.notifyAll();
      }
  }

  public void run()
  {
    final GtkClipboard clipboard = GtkClipboard.getInstance();
    while (true)
      {
	synchronized (this)
	  {
	    while (!announceChange)
	      {
		try
		  {
		    this.wait();
		  }
		catch (InterruptedException ie)
		  {
		    // ignore
		  }
	      }
	    announceChange = false;
	  }

	// Do the actual announcement without the lock held.  We will
	// notice a new change after this notification has finished.
	try
	  {
	    clipboard.setContents(new GtkSelection(), null);
	  }
	catch (Throwable t)
	  {
	    // should never happen, but might if we have some faulty
	    // listener.
	    t.printStackTrace();
	  }
      }
  }
}
