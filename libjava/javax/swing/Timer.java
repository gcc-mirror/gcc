/* Timer.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

package javax.swing;

import java.awt.event.*;
import java.util.*;


public class Timer
{
  int ticks;
  static boolean verbose;
  boolean running;
  boolean repeat_ticks = true;
  long interval, init_delay;
  Vector actions = new Vector();
    
  class Waker extends Thread
  {
    public void run()
    {
      running = true;
      try {
	sleep(init_delay);
		
	while (running)
	  {
	    sleep(interval);

	    if (verbose)
	      {
		System.out.println("javax.swing.Timer -> clocktick");
	      }

	    ticks++;
	    fireActionPerformed();
  
	    if (! repeat_ticks)
	      break;
	  }
	running = false;
      } catch (Exception e) {
	System.out.println("swing.Timer::" + e);
      }
    }
  }

  public void addActionListener(ActionListener listener)
  {
    actions.addElement(listener);
  }
  public void removeActionListener(ActionListener listener)
  {
    actions.removeElement(listener);
  }

  void fireActionPerformed()
  {
    for (int i=0;i<actions.size();i++)
      {
	ActionListener a = (ActionListener) actions.elementAt(i);
	a.actionPerformed(new ActionEvent(this, ticks, "Timer"));
      }
  }
  


  public static void setLogTimers(boolean flag)
  {
    verbose = flag;
  }

  public static boolean getLogTimers()
  {
    return verbose;
  }
    

  public void setDelay(int delay)
  {
    interval = delay;
  }

  public int getDelay()
  {
    return (int)interval;
  }


  public void setInitialDelay(int initialDelay)
  {
    init_delay = initialDelay;
  }

  public void setRepeats(boolean flag)
  {
    repeat_ticks = flag;
  }

  boolean isRunning()
  {
    return running;
  }

  void start()
  {
    if (isRunning())
      {
	System.err.println("attempt to start a running timer");
	return;
      }
    new Waker().start();
  }

  void stop()
  {
    running = false;
  }
}
