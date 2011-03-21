/* AlsaMidiSequencerDevice.java -- The ALSA MIDI sequencer device
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

import java.io.IOException;
import java.io.InputStream;

import javax.sound.midi.ControllerEventListener;
import javax.sound.midi.InvalidMidiDataException;
import javax.sound.midi.MetaEventListener;
import javax.sound.midi.MidiUnavailableException;
import javax.sound.midi.Receiver;
import javax.sound.midi.Sequence;
import javax.sound.midi.Sequencer;
import javax.sound.midi.Track;
import javax.sound.midi.Transmitter;

/**
 * The ALSA MIDI sequencer device.  This is a singleton device.
 *
 * @author green@redhat.com
 *
 */
public class AlsaMidiSequencerDevice implements Sequencer
{
  // The singleton instance.
  public final static AlsaMidiSequencerDevice instance = new AlsaMidiSequencerDevice();

  // A pointer to a native chunk of memory
  private long nativeState;

  // The sequence to process
  private Sequence sequence;

  /**
   * A private constructor.  There should only be one instance of this
   * device.
   */
  private AlsaMidiSequencerDevice()
  {
    super();
  }

  /**
   * Return the sequencer singleton.
   *
   * @return the sequencer singleton
   */
  public static AlsaMidiSequencerDevice getInstance()
  {
    return instance;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#setSequence(javax.sound.midi.Sequence)
   */
  public void setSequence(Sequence seq) throws InvalidMidiDataException
  {
    sequence = seq;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#setSequence(java.io.InputStream)
   */
  public void setSequence(InputStream istream) throws IOException,
      InvalidMidiDataException
  {
    // TODO Auto-generated method stub
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#getSequence()
   */
  public Sequence getSequence()
  {
    return sequence;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#start()
   */
  public void start()
  {
    // TODO Auto-generated method stub
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#stop()
   */
  public void stop()
  {
    // TODO Auto-generated method stub

  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#isRunning()
   */
  public boolean isRunning()
  {
    // TODO Auto-generated method stub
    return false;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#startRecording()
   */
  public void startRecording()
  {
    // TODO Auto-generated method stub

  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#stopRecording()
   */
  public void stopRecording()
  {
    // TODO Auto-generated method stub

  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#isRecording()
   */
  public boolean isRecording()
  {
    // TODO Auto-generated method stub
    return false;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#recordEnable(javax.sound.midi.Track, int)
   */
  public void recordEnable(Track track, int channel)
  {
    // TODO Auto-generated method stub

  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#recordDisable(javax.sound.midi.Track)
   */
  public void recordDisable(Track track)
  {
    // TODO Auto-generated method stub

  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#getTempoInBPM()
   */
  public float getTempoInBPM()
  {
    // TODO Auto-generated method stub
    return 0;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#setTempoInBPM(float)
   */
  public void setTempoInBPM(float bpm)
  {
    // TODO Auto-generated method stub

  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#getTempoInMPQ()
   */
  public float getTempoInMPQ()
  {
    // TODO Auto-generated method stub
    return 0;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#setTempoInMPQ(float)
   */
  public void setTempoInMPQ(float mpq)
  {
    // TODO Auto-generated method stub

  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#setTempoFactor(float)
   */
  public void setTempoFactor(float factor)
  {
    // TODO Auto-generated method stub

  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#getTempoFactor()
   */
  public float getTempoFactor()
  {
    // TODO Auto-generated method stub
    return 0;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#getTickLength()
   */
  public long getTickLength()
  {
    // TODO Auto-generated method stub
    return 0;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#getTickPosition()
   */
  public long getTickPosition()
  {
    // TODO Auto-generated method stub
    return 0;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#setTickPosition(long)
   */
  public void setTickPosition(long tick)
  {
    // TODO Auto-generated method stub

  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#getMicrosecondLength()
   */
  public long getMicrosecondLength()
  {
    // TODO Auto-generated method stub
    return 0;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#getMicrosecondPosition()
   */
  public long getMicrosecondPosition()
  {
    // TODO Auto-generated method stub
    return 0;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#setMicrosecondPosition(long)
   */
  public void setMicrosecondPosition(long microsecond)
  {
    // TODO Auto-generated method stub

  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#setMasterSyncMode(javax.sound.midi.Sequencer.SyncMode)
   */
  public void setMasterSyncMode(SyncMode sync)
  {
    // TODO Auto-generated method stub

  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#getMasterSyncMode()
   */
  public SyncMode getMasterSyncMode()
  {
    // TODO Auto-generated method stub
    return null;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#getMasterSyncModes()
   */
  public SyncMode[] getMasterSyncModes()
  {
    // TODO Auto-generated method stub
    return null;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#setSlaveSyncMode(javax.sound.midi.Sequencer.SyncMode)
   */
  public void setSlaveSyncMode(SyncMode sync)
  {
    // TODO Auto-generated method stub

  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#getSlaveSyncMode()
   */
  public SyncMode getSlaveSyncMode()
  {
    // TODO Auto-generated method stub
    return null;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#getSlaveSyncModes()
   */
  public SyncMode[] getSlaveSyncModes()
  {
    // TODO Auto-generated method stub
    return null;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#setTrackMute(int, boolean)
   */
  public void setTrackMute(int track, boolean mute)
  {
    // TODO Auto-generated method stub

  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#getTrackMute(int)
   */
  public boolean getTrackMute(int track)
  {
    // TODO Auto-generated method stub
    return false;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#setTrackSolo(int, boolean)
   */
  public void setTrackSolo(int track, boolean solo)
  {
    // TODO Auto-generated method stub

  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#getTrackSolo(int)
   */
  public boolean getTrackSolo(int track)
  {
    // TODO Auto-generated method stub
    return false;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#addMetaEventListener(javax.sound.midi.MetaEventListener)
   */
  public boolean addMetaEventListener(MetaEventListener listener)
  {
    // TODO Auto-generated method stub
    return false;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#removeMetaEventListener(javax.sound.midi.MetaEventListener)
   */
  public void removeMetaEventListener(MetaEventListener listener)
  {
    // TODO Auto-generated method stub

  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#addControllerEventListener(javax.sound.midi.ControllerEventListener, int[])
   */
  public int[] addControllerEventListener(ControllerEventListener listener,
                                          int[] controllers)
  {
    // TODO Auto-generated method stub
    return null;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.Sequencer#removeControllerEventListener(javax.sound.midi.ControllerEventListener, int[])
   */
  public int[] removeControllerEventListener(ControllerEventListener listener,
                                             int[] controllers)
  {
    // TODO Auto-generated method stub
    return null;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.MidiDevice#getDeviceInfo()
   */
  public Info getDeviceInfo()
  {
    // TODO Auto-generated method stub
    return null;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.MidiDevice#open()
   */
  public void open() throws MidiUnavailableException
  {
    synchronized(this)
    {
      // Check to see if we're open already.
      if (nativeState != 0)
        return;

      nativeState = open_();
    }
  }

  /**
   * Allocate the native state object, and open the sequencer.
   *
   * @return a long representation of a pointer to the nativeState.
   */
  private native long open_();

  /**
   * Close the sequencer and free the native state object.
   */
  private native void close_(long nativeState);

  /* (non-Javadoc)
   * @see javax.sound.midi.MidiDevice#close()
   */
  public void close()
  {
    synchronized(this)
    {
      close_(nativeState);
      nativeState = 0;
    }
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.MidiDevice#isOpen()
   */
  public boolean isOpen()
  {
    synchronized(this)
    {
      return (nativeState != 0);
    }
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.MidiDevice#getMaxReceivers()
   */
  public int getMaxReceivers()
  {
    return -1;
  }

  /* (non-Javadoc)
   * @see javax.sound.midi.MidiDevice#getMaxTransmitters()
   */
  public int getMaxTransmitters()
  {
    return -1;
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
