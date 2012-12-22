/* DSSISynthesizer.java -- DSSI Synthesizer Provider
   Copyright (C) 2005, 2006, 2012 Free Software Foundation, Inc.

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


package gnu.javax.sound.midi.dssi;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.sound.midi.Instrument;
import javax.sound.midi.MidiChannel;
import javax.sound.midi.MidiMessage;
import javax.sound.midi.MidiUnavailableException;
import javax.sound.midi.Patch;
import javax.sound.midi.Receiver;
import javax.sound.midi.ShortMessage;
import javax.sound.midi.Soundbank;
import javax.sound.midi.SoundbankResource;
import javax.sound.midi.Synthesizer;
import javax.sound.midi.Transmitter;
import javax.sound.midi.VoiceStatus;

/**
 * DSSI soft-synth support.
 *
 * All DSSI soft-synths are expected to be installed in /usr/lib/dssi.
 *
 * @author Anthony Green (green@redhat.com)
 *
 */
public class DSSISynthesizer implements Synthesizer
{
  /**
   * The DSSI Instrument class.
   *
   * @author Anthony Green (green@redhat.com)
   *
   */
  class DSSIInstrument extends Instrument
  {
    DSSIInstrument (Soundbank soundbank, Patch patch, String name)
    {
      super (soundbank, patch, name, null);
    }

    /* @see javax.sound.midi.SoundbankResource#getData()
     */
    public Object getData()
    {
      return null;
    }

  }

/**
   * DSSISoundbank holds all instruments.
   *
   * @author Anthony Green (green@redhat.com)
   *
   */
  class DSSISoundbank implements Soundbank
  {
    private String name;
    private String description;
    private List<Instrument> instruments = new ArrayList<Instrument>();
    private List<SoundbankResource> resources = new ArrayList<SoundbankResource>();
    private String vendor;
    private String version;

    public DSSISoundbank(String name, String description, String vendor, String version)
    {
      this.name = name;
      this.description = description;
      this.vendor = vendor;
      this.version = version;
    }

    void add(Instrument instrument)
    {
      instruments.add(instrument);
    }

    /* @see javax.sound.midi.Soundbank#getName()
     */
    public String getName()
    {
      return name;
    }

    /* @see javax.sound.midi.Soundbank#getVersion()
     */
    public String getVersion()
    {
      return version;
    }

    /* @see javax.sound.midi.Soundbank#getVendor()
     */
    public String getVendor()
    {
      return vendor;
    }

    /* @see javax.sound.midi.Soundbank#getDescription()
     */
    public String getDescription()
    {
      return description;
    }

    /* @see javax.sound.midi.Soundbank#getResources()
     */
    public SoundbankResource[] getResources()
    {
      return resources.toArray(new SoundbankResource[resources.size()]);
    }

    /* @see javax.sound.midi.Soundbank#getInstruments()
     */
    public Instrument[] getInstruments()
    {
      return instruments.toArray(new Instrument[instruments.size()]);
    }

    /* @see javax.sound.midi.Soundbank#getInstrument(javax.sound.midi.Patch)
     */
    public Instrument getInstrument(Patch patch)
    {
      Iterator<Instrument> itr = instruments.iterator();

      while (itr.hasNext())
      {
        Instrument i = itr.next();
        if (i.getPatch().equals(patch))
          return i;
      }

      return null;
    }
  }

/**
   * The Receiver class receives all MIDI messages from a connected
   * Transmitter.
   *
   * @author Anthony Green (green@redhat.com)
   *
   */
  class DSSIReceiver implements Receiver
  {
    /* (non-Javadoc)
     * @see javax.sound.midi.Receiver#send(javax.sound.midi.MidiMessage, long)
     */
    public void send(MidiMessage message, long timeStamp)
        throws IllegalStateException
    {
      if (message instanceof ShortMessage)
      {
        ShortMessage smessage = (ShortMessage) message;

        switch (message.getStatus())
        {
        case ShortMessage.NOTE_ON:
          int velocity = smessage.getData2();
          if (velocity > 0)
            channels[smessage.getChannel()].noteOn(smessage.getData1(),
                                                   smessage.getData2());
          else
            channels[smessage.getChannel()].noteOff(smessage.getData1());
          break;
        case ShortMessage.CONTROL_CHANGE:
          channels[smessage.getChannel()].controlChange(smessage.getData1(),
                                                        smessage.getData2());
          break;
        default:
          System.out.println ("Unhandled message: " + message.getStatus());
          break;
        }
      }
    }

    /* (non-Javadoc)
     * @see javax.sound.midi.Receiver#close()
     */
    public void close()
    {
      // TODO Auto-generated method stub
    }

  }

  static native void noteOn_(long handle, int channel, int noteNumber, int velocity);
  static native void noteOff_(long handle, int channel, int noteNumber, int velocity);
  static native void setPolyPressure_(long handle, int channel, int noteNumber, int pressure);
  static native int getPolyPressure_(long handle, int channel, int noteNumber);
  static native void controlChange_(long handle, int channel, int control, int value);
  static native void open_(long handle);
  static native void close_(long handle);
  static native String getProgramName_(long handle, int index);
  static native int getProgramBank_(long handle, int index);
  static native int getProgramProgram_(long handle, int index);
  static native void selectProgram_(long handle, int bank, int program);

  /**
   * @author Anthony Green (green@redhat.com)
   *
   */
  public class DSSIMidiChannel implements MidiChannel
  {
    int channel = 0;

    /**
     * Default contructor.
     */
    public DSSIMidiChannel(int channel)
    {
      super();
      this.channel = channel;
    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#noteOn(int, int)
     */
    public void noteOn(int noteNumber, int velocity)
    {
      noteOn_(sohandle, channel, noteNumber, velocity);
    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#noteOff(int, int)
     */
    public void noteOff(int noteNumber, int velocity)
    {
      noteOff_(sohandle, channel, noteNumber, velocity);
    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#noteOff(int)
     */
    public void noteOff(int noteNumber)
    {
      noteOff_(sohandle, channel, noteNumber, -1);
    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#setPolyPressure(int, int)
     */
    public void setPolyPressure(int noteNumber, int pressure)
    {
      setPolyPressure_(sohandle, channel, noteNumber, pressure);
    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#getPolyPressure(int)
     */
    public int getPolyPressure(int noteNumber)
    {
      return getPolyPressure_(sohandle, channel, noteNumber);
    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#setChannelPressure(int)
     */
    public void setChannelPressure(int pressure)
    {
      // TODO Auto-generated method stub

    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#getChannelPressure()
     */
    public int getChannelPressure()
    {
      // TODO Auto-generated method stub
      return 0;
    }

    /* @see javax.sound.midi.MidiChannel#controlChange(int, int)  */
    public void controlChange(int controller, int value)
    {
      controlChange_(sohandle, channel, controller, value);
    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#getController(int)
     */
    public int getController(int controller)
    {
      // TODO Auto-generated method stub
      return 0;
    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#programChange(int)
     */
    public void programChange(int program)
    {
      // TODO Auto-generated method stub

    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#programChange(int, int)
     */
    public void programChange(int bank, int program)
    {
      // TODO Auto-generated method stub

    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#getProgram()
     */
    public int getProgram()
    {
      // TODO Auto-generated method stub
      return 0;
    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#setPitchBend(int)
     */
    public void setPitchBend(int bend)
    {
      // TODO Auto-generated method stub

    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#getPitchBend()
     */
    public int getPitchBend()
    {
      // TODO Auto-generated method stub
      return 0;
    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#resetAllControllers()
     */
    public void resetAllControllers()
    {
      // TODO Auto-generated method stub

    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#allNotesOff()
     */
    public void allNotesOff()
    {
      // TODO Auto-generated method stub

    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#allSoundOff()
     */
    public void allSoundOff()
    {
      // TODO Auto-generated method stub

    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#localControl(boolean)
     */
    public boolean localControl(boolean on)
    {
      // TODO Auto-generated method stub
      return false;
    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#setMono(boolean)
     */
    public void setMono(boolean on)
    {
      // TODO Auto-generated method stub

    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#getMono()
     */
    public boolean getMono()
    {
      // TODO Auto-generated method stub
      return false;
    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#setOmni(boolean)
     */
    public void setOmni(boolean on)
    {
      // TODO Auto-generated method stub

    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#getOmni()
     */
    public boolean getOmni()
    {
      // TODO Auto-generated method stub
      return false;
    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#setMute(boolean)
     */
    public void setMute(boolean mute)
    {
      // TODO Auto-generated method stub

    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#getMute()
     */
    public boolean getMute()
    {
      // TODO Auto-generated method stub
      return false;
    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#setSolo(boolean)
     */
    public void setSolo(boolean solo)
    {
      // TODO Auto-generated method stub

    }

    /* (non-Javadoc)
     * @see javax.sound.midi.MidiChannel#getSolo()
     */
    public boolean getSolo()
    {
      // TODO Auto-generated method stub
      return false;
    }

  }

  long sohandle;
  long handle;
  private Info info;

  MidiChannel channels[] = new MidiChannel[16];

  // The list of known soundbanks, and the default one.
  List<Soundbank> soundbanks = new ArrayList<Soundbank>();
  DSSISoundbank defaultSoundbank;

  /**
   * Create a DSSI Synthesizer.
   *
   * @param info the DSSIInfo for this soft-synth
   * @param soname the name of the .so file for this DSSI synth
   * @param index the DSSI index for this soft-synth
   */
  public DSSISynthesizer(Info info, String soname, long index)
  {
    super();
    this.info = info;
    sohandle = DSSIMidiDeviceProvider.dlopen_(soname);
    handle = DSSIMidiDeviceProvider.getDSSIHandle_(sohandle, index);
    channels[0] = new DSSIMidiChannel(0);
    defaultSoundbank = new DSSISoundbank("name", "description",
                                         "vendor", "version");
    soundbanks.add(defaultSoundbank);

    int i = 0;
    String name;
    do
    {
      name = getProgramName_(sohandle, i);
      if (name != null)
      {
        defaultSoundbank.
          add(new DSSIInstrument(defaultSoundbank,
                                 new Patch(getProgramBank_(sohandle, i),
                                           getProgramProgram_(sohandle, i)),
                                 name));
        i++;
      }
    } while (name != null);
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Synthesizer#getMaxPolyphony()
   */
  public int getMaxPolyphony()
  {
    // TODO Auto-generated method stub
    return 0;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Synthesizer#getLatency()
   */
  public long getLatency()
  {
    // DSSI and LADSPA provide no way to determine the latency.
    // Let's just return 0 for now.
    return 0;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Synthesizer#getChannels()
   */
  public MidiChannel[] getChannels()
  {
    return channels;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Synthesizer#getVoiceStatus()
   */
  public VoiceStatus[] getVoiceStatus()
  {
    // TODO Auto-generated method stub
    return null;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Synthesizer#isSoundbankSupported(javax.sound.midi.Soundbank)
   */
  public boolean isSoundbankSupported(Soundbank soundbank)
  {
    // TODO Auto-generated method stub
    return false;
  }

  /* @see javax.sound.midi.Synthesizer#loadInstrument(javax.sound.midi.Instrument)
   */
  public boolean loadInstrument(Instrument instrument)
  {
    // FIXME: perhaps this isn't quite right.  It can probably
    // be in any soundbank.
    if (instrument.getSoundbank() != defaultSoundbank)
      throw new IllegalArgumentException ("Synthesizer doesn't support this instrument's soundbank");

    Patch patch = instrument.getPatch();
    selectProgram_(sohandle, patch.getBank(), patch.getProgram());
    return true;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Synthesizer#unloadInstrument(javax.sound.midi.Instrument)
   */
  public void unloadInstrument(Instrument instrument)
  {
    // TODO Auto-generated method stub

  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Synthesizer#remapInstrument(javax.sound.midi.Instrument, javax.sound.midi.Instrument)
   */
  public boolean remapInstrument(Instrument from, Instrument to)
  {
    // TODO Auto-generated method stub
    return false;
  }

  /* @see javax.sound.midi.Synthesizer#getDefaultSoundbank()
   */
  public Soundbank getDefaultSoundbank()
  {
    return defaultSoundbank;
  }

  /* @see javax.sound.midi.Synthesizer#getAvailableInstruments()
   */
  public Instrument[] getAvailableInstruments()
  {
    List<Instrument> instruments = new ArrayList<Instrument>();
    Iterator<Soundbank> itr = soundbanks.iterator();
    while (itr.hasNext())
    {
      Soundbank sb = itr.next();
      Instrument ins[] = sb.getInstruments();
      for (int i = 0; i < ins.length; i++)
        instruments.add(ins[i]);
    }
    return instruments.toArray(new Instrument[instruments.size()]);
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Synthesizer#getLoadedInstruments()
   */
  public Instrument[] getLoadedInstruments()
  {
    // TODO Auto-generated method stub
    return null;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Synthesizer#loadAllInstruments(javax.sound.midi.Soundbank)
   */
  public boolean loadAllInstruments(Soundbank soundbank)
  {
    // TODO Auto-generated method stub
    return false;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Synthesizer#unloadAllInstruments(javax.sound.midi.Soundbank)
   */
  public void unloadAllInstruments(Soundbank soundbank)
  {
    // TODO Auto-generated method stub
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Synthesizer#loadInstruments(javax.sound.midi.Soundbank, javax.sound.midi.Patch[])
   */
  public boolean loadInstruments(Soundbank soundbank, Patch[] patchList)
  {
    // TODO Auto-generated method stub
    return false;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Synthesizer#unloadInstruments(javax.sound.midi.Soundbank, javax.sound.midi.Patch[])
   */
  public void unloadInstruments(Soundbank soundbank, Patch[] patchList)
  {
    // TODO Auto-generated method stub

  }

  /* @see javax.sound.midi.MidiDevice#getDeviceInfo()
   */
  public Info getDeviceInfo()
  {
    return info;
  }

  /* @see javax.sound.midi.MidiDevice#open()
   */
  public void open() throws MidiUnavailableException
  {
    open_(sohandle);
  }

  /* @see javax.sound.midi.MidiDevice#close()
   */
  public void close()
  {
    close_(sohandle);
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

  /* @see javax.sound.midi.MidiDevice#getMaxReceivers()
   */
  public int getMaxReceivers()
  {
    return 1;
  }

  /* @see javax.sound.midi.MidiDevice#getMaxTransmitters()
   */
  public int getMaxTransmitters()
  {
    return 0;
  }

  /* @see javax.sound.midi.MidiDevice#getReceiver()
   */
  public Receiver getReceiver() throws MidiUnavailableException
  {
    return new DSSIReceiver();
  }

  /* @see javax.sound.midi.MidiDevice#getTransmitter()
   */
  public Transmitter getTransmitter() throws MidiUnavailableException
  {
    return null;
  }
}
