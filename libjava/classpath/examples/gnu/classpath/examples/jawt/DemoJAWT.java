/* DemoJAWT.java -- AWT Native Interface demo
   Copyright (C) 2005  Free Software Foundation, Inc.

This file is part of GNU Classpath examples.

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
02110-1301 USA. */

package gnu.classpath.examples.jawt;

import java.awt.*;
import java.awt.event.*;

public class DemoJAWT extends Canvas
{
  static
  {
    System.loadLibrary ("DemoJAWT");
  }

  public native void paintIt (Graphics g, boolean on);

  public void paint (Graphics g)
  {
    paintIt (g, on);
  }

  private boolean on;

  public static void main (String[] args)
  {
    Frame f = new Frame ("GNU Classpath JAWT Demo");

    f.setBounds (0, 0, 300, 300);

    f.setResizable (false);

    DemoJAWT jawtDemo = new DemoJAWT ();
    f.add (jawtDemo);

    f.addWindowListener (new WindowAdapter ()
      {
	public void windowClosing (WindowEvent evt)
	{
	  System.exit (0);
	}
      });

    f.show ();

    while (true)
    {
      try
	{
	  Thread.sleep (500);
	}
      catch (InterruptedException ie)
	{
	  // ignored
	}
      jawtDemo.on = ! jawtDemo.on;
      f.repaint();
    }
  }
}
