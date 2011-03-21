/* AlsaMidiDeviceProvider.java -- The ALSA MIDI Device Provider
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

import gnu.classpath.Configuration;

import javax.sound.midi.MidiDevice;
import javax.sound.midi.MidiDevice.Info;
import javax.sound.midi.spi.MidiDeviceProvider;

/**
 * Provide ALSA MIDI devices.
 *
 * @author Anthony Green (green@redhat.com)
 *
 */
public class AlsaMidiDeviceProvider extends MidiDeviceProvider
{
  /**
   * Abstract base for ALSA specific MIDI device info.
   *
   * @author Anthony Green (green@redhat.com)
   *
   */
  private static abstract class AlsaInfo extends Info
  {
    /**
     * Create an ALSA specific MIDI device info object.
     *
     * @param name the device name
     * @param description the device description
     */
    public AlsaInfo(String name, String description)
    {
      super(name, "Alsa", description, "0.0");
    }

    abstract MidiDevice getDevice ();
  }

  /**
   * ALSA MIDI Port.
   *
   * @author Anthony Green (green@redhat.com)
   *
   */
  public static abstract class AlsaPortInfo extends AlsaInfo
  {
    long client;
    long port;

    /**
     * Create ALSA MIDI In Port.
     *
     * @param name the device name
     * @param description the device description
     * @param client the client ID
     * @param port the port ID
     */
    public AlsaPortInfo(String name, String description, long client, long port)
    {
      super(name, description);
      this.client = client;
      this.port = port;
    }
  }

  /**
   * ALSA Sequencer specific info.
   *
   * @author Anthony Green (green@redhat.com)
   *
   */
  private static class AlsaSequencerInfo extends AlsaInfo
  {
    public AlsaSequencerInfo(String name, String description)
    {
      super(name, description);
    }

    MidiDevice getDevice()
    {
      return AlsaMidiSequencerDevice.getInstance();
    }
  }

  /**
   * ALSA MIDI In Port.
   *
   * @author Anthony Green (green@redhat.com)
   *
   */
  private static class AlsaInputPortInfo extends AlsaPortInfo
  {
    public AlsaInputPortInfo(String name, String description, long client, long port)
    {
      super(name, description, client, port);
    }

    MidiDevice getDevice()
    {
      return new AlsaInputPortDevice(this);
    }
  }

  /**
   * ALSA MIDI Out Port.
   *
   * @author Anthony Green (green@redhat.com)
   *
   */
  private static class AlsaOutputPortInfo extends AlsaPortInfo
  {
    public AlsaOutputPortInfo(String name, String description, long client, long port)
    {
      super(name, description, client, port);
    }

    MidiDevice getDevice()
    {
      return new AlsaOutputPortDevice(this);
    }
  }

  private static AlsaInfo[] infos;

  private static native AlsaInfo[] getInputDeviceInfo_();
  private static native AlsaInfo[] getOutputDeviceInfo_();

  /**
   * Initialize the ALSA system
   */
  private static native void init_();

  static
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
        System.loadLibrary("gjsmalsa");
      }

    init_();

    AlsaInfo inputs[] = getInputDeviceInfo_();
    AlsaInfo outputs[] = getOutputDeviceInfo_();

    infos = new AlsaInfo[inputs.length + outputs.length + 1];
    infos[0] = new AlsaSequencerInfo ("/dev/snd/seq", "ALSA Sequencer");
    System.arraycopy(inputs, 0, infos, 1, inputs.length);
    System.arraycopy(outputs, 0, infos, 1 + inputs.length, outputs.length);
  }

  public AlsaMidiDeviceProvider()
  {
    // Nothing.
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.spi.MidiDeviceProvider#getDeviceInfo()
   */
  public Info[] getDeviceInfo()
  {
    return infos;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.spi.MidiDeviceProvider#getDevice(javax.sound.midi.MidiDevice.Info)
   */
  public MidiDevice getDevice(Info info)
  {
    for (int i = 0; i < infos.length; i++)
    {
      if (info.equals(infos[i]))
      {
        return infos[i].getDevice();
      }
    }
    throw new IllegalArgumentException("Don't recognize MIDI device " + info);
  }
}
