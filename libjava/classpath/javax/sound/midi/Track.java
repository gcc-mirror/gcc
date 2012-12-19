/* Track.java -- A track of MIDI events
   Copyright (C) 2005, 2012 Free Software Foundation, Inc.

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

import java.util.HashSet;
import java.util.Vector;

/**
 * A Track contains a list of timecoded MIDI events for processing
 * by a Sequencer.
 *
 * @author Anthony Green (green@redhat.com)
 * @since 1.3
 *
 */
public class Track
{
  /**
   * The list of MidiEvents for this track.
   */
  Vector<MidiEvent> events = new Vector<MidiEvent>();

  // A HashSet to speed processing
  private HashSet<MidiEvent> eventSet = new HashSet<MidiEvent>();

  // This is only instantiable within this package.
  Track()
  {
  }

  /**
   * Add a new event to this track.  Specific events may only be added once.
   * The event will be inserted into the appropriate spot in the event list
   * based on its timecode.
   *
   * @param event the event to add
   * @return true if the event was added, false otherwise
   */
  public boolean add(MidiEvent event)
  {
    synchronized (events)
    {
      if (eventSet.contains(event))
        return false;

      eventSet.add(event);

      long targetTick = event.getTick();
      int i = events.size() - 1;
      while (i >= 0 && (events.get(i).getTick() > targetTick))
        i--;
      events.add(i+1, event);
      return true;
    }
  }

  /**
   * Remove an event from this track.
   *
   * @param event the event to remove
   * @return true if the event was removed, false otherwise
   */
  public boolean remove(MidiEvent event)
  {
    synchronized (events)
    {
      if (! eventSet.remove(event))
        return false;

      int i = events.indexOf(event);
      if (i >= 0)
        {
          events.remove(i);
          return true;
        }

      throw new InternalError("event in set but not list");
    }
  }

  /**
   * Get an event idetified by its order index
   *
   * @param index the location of the event to get
   * @return the event at index
   * @throws ArrayIndexOutOfBoundsException if index is out of bounds
   */
  public MidiEvent get(int index) throws ArrayIndexOutOfBoundsException
  {
    synchronized (events)
    {
      try
      {
        return events.get(index);
      }
      catch (IndexOutOfBoundsException e)
      {
        throw (ArrayIndexOutOfBoundsException)
          new ArrayIndexOutOfBoundsException().initCause(e);
      }
    }
  }


  /**
   * Get the number events in this track.
   *
   * @return the number of events in this track
   */
  public int size()
  {
    return events.size();
  }

  /**
   * Get the length of the track in MIDI ticks.
   *
   * @return the length of the track in MIDI ticks
   */
  public long ticks()
  {
    synchronized (events)
    {
      int size = events.size();
      return events.get(size - 1).getTick();
    }
  }
 }
