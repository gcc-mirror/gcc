/* Sequence.java -- A sequence of MIDI events
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

import java.util.Iterator;
import java.util.Vector;

/**
 * Objects of this type represent sequences of MIDI messages that can be 
 * played back by a Sequencer.
 * 
 * @author Anthony Green (green@redhat.com)
 * @since 1.3
 *
 */
public class Sequence
{
  /**
   * The timing division type for this sequence (PPQ or SMPTE*) 
   */
  protected float divisionType;
  
  /**
   * The timing resolution in ticks/beat or ticks/frame, depending on the 
   * division type.  
   */
  protected int resolution;
  
  /**
   * The MIDI tracks used by this sequence. 
   */
  protected Vector<Track> tracks;
  
  /**
   * Tempo-based timing.  Resolution is specified in ticks per beat.
   */
  public static final float PPQ = 0.0f;
  
  /**
   * 24 frames/second timing.  Resolution is specific in ticks per frame.
   */
  public static final float SMPTE_24 = 24.0f;
  
  /**
   * 25 frames/second timing.  Resolution is specific in ticks per frame.
   */
  public static final float SMPTE_25 = 25.0f;
  
  /**
   * 30 frames/second timing.  Resolution is specific in ticks per frame.
   */
  public static final float SMPTE_30 = 30.0f;
  
  /**
   * 29.97 frames/second timing.  Resolution is specific in ticks per frame.
   */
  public static final float SMPTE_30DROP = 29.97f;
  
  // Private helper class
  private void init(float divisionType, int resolution, int numTracks)
    throws InvalidMidiDataException
  {
    if (divisionType != PPQ
        && divisionType != SMPTE_24 
        && divisionType != SMPTE_25
        && divisionType != SMPTE_30
        && divisionType != SMPTE_30DROP)
      throw new InvalidMidiDataException("Invalid division type (" 
                                         + divisionType + ")");

    this.divisionType = divisionType;
    this.resolution = resolution;
    
    tracks = new Vector<Track>(numTracks);
    while (numTracks > 0)
      tracks.set(--numTracks, new Track());
  }
  
  /**
   * Create a MIDI sequence object with no initial tracks.
   * 
   * @param divisionType the division type (must be one of PPQ or SMPTE_*)
   * @param resolution the timing resolution
   * @throws InvalidMidiDataException if the division type is invalid
   */
  public Sequence(float divisionType, int resolution)
    throws InvalidMidiDataException
  {
    init(divisionType, resolution, 0);
  }

  /**
   * Create a MIDI seqence object.
   * 
   * @param divisionType the division type (must be one of PPQ or SMPTE_*)
   * @param resolution the timing resolution
   * @param numTracks the number of initial tracks
   * @throws InvalidMidiDataException if the division type is invalid
   */
  public Sequence(float divisionType, int resolution, int numTracks)
    throws InvalidMidiDataException
  {
    init(divisionType, resolution, 0);
  }
  
  /**
   * The division type of this sequence.
   * 
   * @return division type of this sequence
   */
  public float getDivisionType()
  {
    return divisionType;
  }
  
  /**
   * The timing resolution for this sequence, relative to the division type.
   * 
   * @return the timing resolution for this sequence
   */
  public int getResolution()
  {
    return resolution;
  }
  
  /**
   * Create a new empty MIDI track and add it to this sequence.
   * 
   * @return the newly create MIDI track
   */
  public Track createTrack()
  {
    Track track = new Track();
    tracks.add(track);
    return track;
  }
  
  /**
   * Remove the specified MIDI track from this sequence.
   * 
   * @param track the track to remove
   * @return true if track was removed and false othewise
   */
  public boolean deleteTrack(Track track)
  {
    return tracks.remove(track);
  }
  
  /**
   * Get an array of MIDI tracks used in this sequence.
   * 
   * @return a possibly empty array of tracks
   */
  public Track[] getTracks()
  {
    return tracks.toArray(new Track[tracks.size()]);
  }
  
  /**
   * The length of this sequence in microseconds.
   * 
   * @return the length of this sequence in microseconds
   */
  public long getMicrosecondLength()
  {
    long tickLength = getTickLength();
    
    if (divisionType == PPQ)
    {
      // FIXME
      // How can this possible be computed?  PPQ is pulses per quarter-note,
      // which is dependent on the tempo of the Sequencer.
      throw new 
	  UnsupportedOperationException("Can't compute PPQ based lengths yet");
    }
    else
    {
      // This is a fixed tick per frame computation
      return (long) ((tickLength * 1000000) / (divisionType * resolution));
    }
  }
  
  /**
   * The length of this sequence in MIDI ticks.
   * 
   * @return the length of this sequence in MIDI ticks
   */
  public long getTickLength()
  {
    long length = 0;
    Iterator<Track> itr = tracks.iterator();
    while (itr.hasNext())
    {
      Track track = itr.next();
      long trackTicks = track.ticks();
      if (trackTicks > length)
        length = trackTicks;
    }
    return length;
  }
  
  /**
   * Get an array of patches used in this sequence.
   * 
   * @return an array of patches used in this sequence
   */
  public Patch[] getPatchList()
  {
    // FIXE: not quite sure how to do this yet. 
    throw new UnsupportedOperationException("Can't get patch list yet");
  }
}
