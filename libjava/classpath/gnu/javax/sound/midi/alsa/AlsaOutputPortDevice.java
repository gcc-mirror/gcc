/* AlsaOutputPortDevice.java -- ALSA MIDI Output Port Device
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.javax.sound.midi.alsa;

import gnu.javax.sound.midi.alsa.AlsaMidiDeviceProvider.AlsaPortInfo;

import javax.sound.midi.MidiUnavailableException;
import javax.sound.midi.Receiver;
import javax.sound.midi.Transmitter;

/**
 * ALSA MIDI Out Device
 * 
 * @author Anthony Green (green@redhat.com)
 *
 */
public class AlsaOutputPortDevice extends AlsaPortDevice
{
  AlsaOutputPortDevice (AlsaPortInfo info)
  {
    super(info);
  }
  /* (non-Javadoc)
   * @see javax.sound.midi.MidiDevice#open()
   */
  public void open() throws MidiUnavailableException
  {
    // TODO Auto-generated method stub

  }

  /* (non-Javadoc)
   * @see javax.sound.midi.MidiDevice#close()
   */
  public void close()
  {
    // TODO Auto-generated method stub

  }

  /* (non-Javadoc)
   * @see javax.sound.midi.MidiDevice#isOpen()
   */
  public boolean isOpen()
  {
    // TODO Auto-generated method stub
    return false;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.MidiDevice#getMicrosecondPosition()
   */
  public long getMicrosecondPosition()
  {
    // TODO Auto-generated method stub
    return 0;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.MidiDevice#getMaxReceivers()
   */
  public int getMaxReceivers()
  {
    // TODO Auto-generated method stub
    return 1;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.MidiDevice#getMaxTransmitters()
   */
  public int getMaxTransmitters()
  {
    // TODO Auto-generated method stub
    return 0;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.MidiDevice#getReceiver()
   */
  public Receiver getReceiver() throws MidiUnavailableException
  {
    // TODO Auto-generated method stub
    return null;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.MidiDevice#getTransmitter()
   */
  public Transmitter getTransmitter() throws MidiUnavailableException
  {
    // TODO Auto-generated method stub
    return null;
  }

}
