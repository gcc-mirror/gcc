/* Demo.java -- And example of MIDI support
   Copyright (C) 2005 Free Software Foundation, Inc.

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

package gnu.classpath.examples.midi;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.sound.midi.*;

/**
 * An example how javax.sound.midi facilities work.
 */
public class Demo extends Frame implements ItemListener
{
  Choice midiInChoice = new Choice();
  Choice midiOutChoice = new Choice();

  MidiDevice inDevice = null;
  MidiDevice outDevice = null;
  
  ArrayList inDevices = new ArrayList();
  ArrayList outDevices = new ArrayList();

  public Demo () throws Exception
  {
    MenuBar mb = new MenuBar ();
    Menu menu = new Menu ("File");
    MenuItem quit = new MenuItem("Quit", new MenuShortcut('Q'));
    quit.addActionListener(new ActionListener()
      {
	public void actionPerformed(ActionEvent e)
	{
	  System.exit(0);
	}
      });
    menu.add (quit);
    mb.add(menu);
    
    setTitle("synthcity: the GNU Classpath MIDI Demo");
    setLayout(new FlowLayout());
    
    MidiDevice.Info[] infos = MidiSystem.getMidiDeviceInfo();

    for (int i = 0; i < infos.length; i++)
      {
	MidiDevice device = MidiSystem.getMidiDevice(infos[i]);
	if (device.getMaxReceivers() > 0)
	  {
	    midiOutChoice.addItem(infos[i].getDescription());
	    outDevices.add(device);
	  }
	if (device.getMaxTransmitters() > 0)
	  {
	    midiInChoice.addItem(infos[i].getDescription());
	    inDevices.add(device);
	  }
      }

    setMenuBar (mb);
    add(new Label("MIDI IN: "));
    add(midiInChoice);
    add(new Label("   MIDI OUT: "));
    add(midiOutChoice);

    midiInChoice.addItemListener(this);
    midiOutChoice.addItemListener(this);

    pack();
    show();
  }
  
  public void itemStateChanged (ItemEvent e)
  {
    try
      {
	if (e.getItemSelectable() == midiInChoice)
	  {
	    if (inDevice != null)
	      inDevice.close();
	    inDevice =  (MidiDevice) 
	      inDevices.get(midiInChoice.getSelectedIndex());
	  }
	
	if (e.getItemSelectable() == midiOutChoice)
	  {
	    if (outDevice != null)
	      outDevice.close();
	    outDevice = (MidiDevice)
	      outDevices.get(midiOutChoice.getSelectedIndex());
	  }
	
	if (inDevice != null && outDevice != null)
	  {
	    if (! inDevice.isOpen())
	      inDevice.open();
	    if (! outDevice.isOpen())
	      outDevice.open();
	    Transmitter t = inDevice.getTransmitter();
	    if (t == null)
	      System.err.println (inDevice + ".getTransmitter() == null");
	    Receiver r = outDevice.getReceiver();
	    if (r == null)
	      System.err.println (outDevice + ".getReceiver() == null");
	    
	    if (t != null && r != null)
	      t.setReceiver (r);
	  }
      }
    catch (Exception ex)
      {
	ex.printStackTrace();
      }
  }

  public static void main (String args[]) throws Exception
    {
      new Demo();
    }
}
