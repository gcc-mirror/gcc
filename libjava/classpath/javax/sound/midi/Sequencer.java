/* Sequencer.java -- A MIDI sequencer object
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


package javax.sound.midi;

import java.io.IOException;
import java.io.InputStream;

/**
 * A Sequencer object plays MIDI sequences described as Sequence objects.
 * This class provides methods for loading and unloading sequences, as well
 * as basic transport controls.
 * 
 * @author Anthony Green (green@redhat.com)
 * @since 1.3
 *
 */
public interface Sequencer extends MidiDevice
{
  /**
   * Set the Sequence object for this sequencer.
   * 
   * @param seq the Sequence to process
   * @throws InvalidMidiDataException if the sequence is invalid for any reason
   */
  public void setSequence(Sequence seq) throws InvalidMidiDataException;
  
  /**
   * Set the sequence for this sequencer.  istream reads on a valid MIDI file.
   * 
   * @param istream an input stream for a valid MIDI file
   * @throws IOException if an I/O exception happens
   * @throws InvalidMidiDataException if the MIDI file contains bad data
   */
  public void setSequence(InputStream istream) 
      throws IOException, InvalidMidiDataException;
  
  /**
   * Get the current sequence object for this sequencer.
   * 
   * @return the current sequence object.  May be null.
   */
  public Sequence getSequence();
  
  /**
   * Start playback of the current sequence.
   */
  public void start();
  
  /**
   * Stop playback of the current sequence.
   */
  public void stop();
  
  /**
   * Returns true if the sequence is playing.
   * 
   * @return true if the sequence is playing and false otherwise
   */
  public boolean isRunning();
  
  /**
   * Start playback and record of MIDI events.
   * Any tracks enabled for recording will have their events replaced.
   * Any newly recorded events, and all events from non-recording tracks
   * will be sent to the sequencer's transmitter.
   */
  public void startRecording();
  
  /**
   * Stop recording, although continue playing.
   */
  public void stopRecording();
  
  /**
   * Returns true if sequence is recording.
   * 
   * @return true if the sequence is recording and false otherwise
   */
  public boolean isRecording();
  
  /**
   * Enable recording for a specific track using data from a specific channel.
   * 
   * @param track the track to enable for recording 
   * @param channel the channel from which to record
   */
  public void recordEnable(Track track, int channel);
  
  /**
   * Disable recording for a specific track.
   * 
   * @param track the track to disable recording for
   */
  public void recordDisable(Track track);
  
  /**
   * Get the current tempo in beats per minute.
   * 
   * @return the current tempo in beats per minute
   */
  public float getTempoInBPM();
  
  /**
   * Sets the current tempo in beats per minute.
   * 
   * @param bpm the new tempo in bears per minutes
   */
  public void setTempoInBPM(float bpm);
  
  /**
   * Get the current tempo in microseconds per quarter note.
   * 
   * @return the current tempo in microseconds per quarter note.
   */
  public float getTempoInMPQ();
  
  /**
   * Sets the current tempo in microseconds per quarter note.
   * 
   * @param mpq the new tempo in microseconds per quarter note.
   */
  public void setTempoInMPQ(float mpq);
  
  /**
   * Set a scaling factor for the playback tempo, which is 1.0 by default.
   * 
   * @param factor the new tempo scaling factor
   */
  public void setTempoFactor(float factor);
  
  /**
   * Get the current scaling factor for the playback tempo.
   * 
   * @return the current tempo scaling factor 
   */
  public float getTempoFactor();
  
  /**
   * Get the length of the current sequence in MIDI ticks.
   *  
   * @return the length of the current sequence in MIDI ticks
   */
  public long getTickLength();
  
  /**
   * Get the current playback position of the sequencer in MIDI ticks.
   * 
   * @return the current playback position of the sequencer in MIDI ticks
   */
  public long getTickPosition();
  
  /**
   * Set the current playback position of the sequencer in MIDI ticks.
   * 
   * @param tick the new playback position of the sequencer in MIDI ticks
   */
  public void setTickPosition(long tick);

  /**
   * Get the length of the current sequence in microseconds.
   *  
   * @return the length of the current sequence in microseconds
   */
  public long getMicrosecondLength();
  
  /**
   * Get the current playback position of the sequencer in microseconds.
   * 
   * @return the current playback position of the sequencer in microseconds
   */
  public long getMicrosecondPosition();
  
  /**
   * Set the current playback position of the sequencer in microseconds.
   * 
   * @param microsecond the new playback position of the sequencer in microseconds
   */
  public void setMicrosecondPosition(long microsecond);

  /**
   * Set the source of timing information.  sync must be found in the array
   * returned by getMasterSyncModes().
   * FIXME: What happens if it isn't?
   * 
   * @param sync the new source of timing information
   */
  public void setMasterSyncMode(SyncMode sync);
  
  /**
   * Get the source of timing information.
   * 
   * @return the current source of timing information
   */
  public SyncMode getMasterSyncMode();
  
  /**
   * Get an array of timing sources supported by this sequencer.
   * 
   * @return an array of timing sources supported by this sequencer
   */
  public SyncMode[] getMasterSyncModes();
  
  /**
   * Set the slave synchronization mode for this sequencer.  sync must be
   * found in the array returned by getSlaveSyncModes().
   * FIXME: What happens if it isn't?
   * 
   * @param sync the new slave sync mode for this sequencer
   */
  public void setSlaveSyncMode(SyncMode sync);
  
  /**
   * Get the current slave synchronization mode.
   * 
   * @return the current slave synchronization mode
   */
  public SyncMode getSlaveSyncMode();
  
  /**
   * Get an array of slave sync modes supported by this sequencer.
   * 
   * @return an array of slave sync modes supported by this sequencer
   */
  public SyncMode[] getSlaveSyncModes();
  
  /**
   * Sets the mute state for a specific track.
   * 
   * @param track the track to modify
   * @param mute the new mute state
   */
  public void setTrackMute(int track, boolean mute);
  
  /**
   * Get the mute state of a specific track.
   * 
   * @param track the track to query
   * @return the mute state for track
   */
  public boolean getTrackMute(int track);
  
  /**
   * Sets the solo state for a specific track.
   * 
   * @param track the track to modify
   * @param solo the new solo state
   */
  public void setTrackSolo(int track, boolean solo);
  
  /**
   * Get the solo state for a specific track.
   * 
   * @param track the track to query
   * @return the solo state for track
   */
  public boolean getTrackSolo(int track);
  
  /**
   * Add a meta event listening object to this sequencer.  It will receive
   * notification whenever the sequencer processes a meta event.
   * A listener may fail to get added if this sequencer doesn't support
   * meta events.
   * 
   * @param listener the listener to add
   * @return true if listener was added, false othewise
   */
  public boolean addMetaEventListener(MetaEventListener listener);
  
  /**
   * Remove a meta event listener from this sequencer.
   * 
   * @param listener the listener to remove
   */
  public void removeMetaEventListener(MetaEventListener listener);
  
  /**
   * Add a controller event listening object to this sequencer.  It will 
   * receive notification whenever the sequencer processes a controller 
   * event for a specified controller number..
   * 
   * @param listener the listener to add
   * @param controllers the conroller numbers to listen to
   * @return the controller numbers being listened to
   */
  public int[] addControllerEventListener(ControllerEventListener listener, 
                                          int controllers[]);
  
  /**
   * Remove a controller listener from this sequencer for the specified
   * controller numbers.
   * 
   * @param listener the listener to remove
   * @param controllers the controllers to unlisten
   * @return the controller numbers being unlistened
   */
  public int[] removeControllerEventListener(ControllerEventListener listener,
                                             int controllers[]);
  
  /**
   * A SyncMode object represents the mechanism by which a MIDI sequencer
   * synchronizes time with a master or slave device.
   * 
   * @author green@redhat.com
   *
   */
  public static class SyncMode
  {
    /**
     * A master sync mode indicating the use of an internal sequencer clock.
     */
    public static final SyncMode INTERNAL_CLOCK = new SyncMode("Internal Clock");
    
    /**
     * A master or slave sync mode indicating the use of MIDI clock messages.
     */
    public static final SyncMode MIDI_SYNC = new SyncMode("MIDI Sync");
    
    /**
     * A master or slave sync mode indicating the use of MIDI Time Code 
     * messages.
     */
    public static final SyncMode MIDI_TIME_CODE = new SyncMode("MIDI Time Code");
    
    /**
     * A slave sync mode indicating that no timing info will be transmitted.
     */
    public static final SyncMode NO_SYNC = new SyncMode("No Timing");

    // The name
    private String name;
    
    /**
     * Create a new SyncMode object
     * @param name the SyncMode name
     */
    protected SyncMode(String name)
    {
      this.name = name;
    }
    
    /**
     * SyncMode objects are only equal when identical.
     */
    public final boolean equals(Object o)
    {
      return super.equals(o);
    }
    
    /**
     * SyncMode objects use the Object hashCode.
     */
    public final int hashCode()
    {
      return super.hashCode();
    }
    
    /**
     * Use the SyncMode name as the string representation.
     * @see java.lang.Object#toString()
     */
    public final String toString()
    {
      return name;
    }
  }
}
