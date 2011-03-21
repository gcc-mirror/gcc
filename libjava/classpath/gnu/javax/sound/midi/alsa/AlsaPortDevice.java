/* AlsaPortDevice.java -- ALSA MIDI Port Devices
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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

import javax.sound.midi.MidiDevice;
import javax.sound.midi.MidiMessage;
import javax.sound.midi.Receiver;
import javax.sound.midi.Transmitter;

import gnu.javax.sound.midi.alsa.AlsaMidiDeviceProvider.AlsaPortInfo;

/**
 * ALSA Port Device
 *
 * @author Anthony Green (green@redhat.com)
 *
 */
public abstract class AlsaPortDevice implements MidiDevice
{
  /**
   * The ALSA Receiver class.
   *
   * @author Anthony Green (green@redhat.com)
   *
   */
  public class AlsaReceiver implements Receiver
  {
    /* (non-Javadoc)
     * @see javax.sound.midi.Receiver#send(javax.sound.midi.MidiMessage, long)
     */
    public void send(MidiMessage message, long timeStamp)
        throws IllegalStateException
    {
      // TODO Auto-generated method stub

    }

    /* (non-Javadoc)
     * @see javax.sound.midi.Receiver#close()
     */
    public void close()
    {
      // TODO Auto-generated method stub

    }
  }

  AlsaMidiDeviceProvider.AlsaPortInfo info;

  public AlsaPortDevice (AlsaPortInfo info)
  {
    this.info = info;
  }

  public Info getDeviceInfo()
  {
    return info;
  }

  native void run_receiver_thread_ (long client, long port, Receiver receiver);

  /**
   * The ALSA Transmitter class.
   *
   * @author Anthony Green (green@redhat.com)
   *
   */
  protected class AlsaTransmitter implements Transmitter, Runnable
  {
    private Receiver receiver;

    public void run()
    {
      run_receiver_thread_ (info.client, info.port, receiver);
    }

    /* (non-Javadoc)
     * @see javax.sound.midi.Transmitter#setReceiver(javax.sound.midi.Receiver)
     */
    public void setReceiver(Receiver receiver)
    {
      synchronized (this)
      {
        this.receiver = receiver;
      }

      // Create the processing thread
      new Thread(this).start();
    }

    /* (non-Javadoc)
     * @see javax.sound.midi.Transmitter#getReceiver()
     */
    public Receiver getReceiver()
    {
      synchronized (this)
      {
        return receiver;
      }
    }

    /* (non-Javadoc)
     * @see javax.sound.midi.Transmitter#close()
     */
    public void close()
    {
      synchronized (this)
      {
        receiver.close();
        receiver = null;
      }
    }
  }
}
