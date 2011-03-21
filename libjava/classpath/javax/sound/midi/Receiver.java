/* Receiver.java -- An interface for objects receiving MIDI data
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

/**
 * This interface describes the methods required by objects receiving MIDI
 * messages.
 *
 * @author Anthony Green (green@redhat.com)
 * @since 1.3
 *
 */
public interface Receiver
{
  /**
   * Send a MIDI message and timestamp.  Some receivers don't support
   * timestamps, in which case timeStamp should be -1.
   *
   * @param message the MIDI message to send
   * @param timeStamp time timestamp for this message in microseconds (or -1)
   * @throws IllegalStateException if the receiver is closed
   */
  public void send(MidiMessage message, long timeStamp)
    throws IllegalStateException;

  /**
   * Close this receiver, possibly freeing system resources.
   */
  public void close();
}
