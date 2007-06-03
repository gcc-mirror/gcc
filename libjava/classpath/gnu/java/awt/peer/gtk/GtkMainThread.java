/* GtkMainThread.java -- Wrapper for the GTK main thread, and some utilities.
   Copyright (C) 2006  Free Software Foundation, Inc.

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

import gnu.java.awt.peer.NativeEventLoopRunningEvent;

/**
 * The Java thread representing the native GTK main loop, that is,
 * GtkMainThread.mainThread, terminates when GtkToolkit.gtkMain()
 * returns.  That happens in response to the last window peer being
 * disposed (see GtkWindowPeer.dispose).
 *
 * When GtkMainThread.destroyWindow is called for the last window, it
 * in turn calls GtkMainThread.endMainThread, which calls gtk_quit.
 * gtk_quit signals gtk_main to return, which causes GtkMainThread.run
 * to return.
 *
 * There should only be one native GTK main loop running at any given
 * time.  In order to safely start and stop the GTK main loop, we use
 * a running flag and corresponding runningLock.  startMainThread will
 * not return until the native GTK main loop has started, as confirmed
 * by the native set_running_flag callback setting the running flag to
 * true.  Without this protection, gtk_quit could be called before the
 * main loop has actually started, which causes GTK assertion
 * failures.  Likewise endMainThread will not return until the native
 * GTK main loop has ended.
 *
 * post_running_flag_callback is called during gtk_main initialization
 * and no window can be created before startMainThread returns.  This
 * ensures that calling post_running_flag_callback is the first action
 * taken by the native GTK main loop.
 *
 * GtkMainThread.mainThread is started when the window count goes from
 * zero to one.
 *
 * GtkMainThread keeps the AWT event queue informed of its status by
 * posting NativeEventLoopRunningEvents.  The AWT event queue uses
 * this status to determine whether or not the AWT exit conditions
 * have been met (see EventQueue.isShutdown).
 */
public class GtkMainThread extends Thread
{
  /** Count of the number of open windows */
  private static int numberOfWindows = 0;

  /** Lock for the above */
  private static Object nWindowsLock = new Object();

  /** Indicates whether or not the GTK main loop is running. */
  private static boolean running = false;

  /** Lock for the above. */
  private static Object runningLock = new Object();

  /** The main thread instance (singleton) */
  public static GtkMainThread mainThread;

  /** Constructs a main thread */
  private GtkMainThread()
  {
    super("GTK main thread");
  }

  public void run ()
  {
    GtkToolkit.gtkMain ();
  }

  private static void setRunning(boolean running)
  {
    synchronized (runningLock)
      {
        GtkMainThread.running = running;
        runningLock.notifyAll();
      }
  }

  private static void startMainThread()
  {
    synchronized (runningLock)
      {
        if (!running)
          {
            mainThread = new GtkMainThread();
            mainThread.start();

            while (!running)
              {
                try
                  {
                    runningLock.wait();
                  }
                catch (InterruptedException e)
                  {
                    System.err.println ("GtkMainThread.startMainThread:"
                                        + " interrupted while waiting "
                                        + " for GTK main loop to start");
                  }
              }
            GtkGenericPeer.q()
              .postEvent(new NativeEventLoopRunningEvent(new Boolean(true)));
          }
      }
  }

  private static void endMainThread()
  {
    synchronized (runningLock)
      {
        if (running)
          {
            GtkToolkit.gtkQuit();

            while (running)
              {
                try
                  {
                    runningLock.wait();
                  }
                catch (InterruptedException e)
                  {
                    System.err.println ("GtkMainThread.endMainThread:"
                                        + " interrupted while waiting "
                                        + " for GTK main loop to stop");
                  }
              }
            GtkGenericPeer.q()
              .postEvent(new NativeEventLoopRunningEvent(new Boolean(false)));
            }
      }
  }

  public static void createWindow()
  {
    synchronized (nWindowsLock)
      {
        if (numberOfWindows == 0)
          startMainThread();
        numberOfWindows++;
      }
  }

  public static void destroyWindow()
  {
    synchronized (nWindowsLock)
      {
        numberOfWindows--;
        if (numberOfWindows == 0)
          endMainThread();
      }
  }
}
