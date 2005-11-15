/* MidiDeviceProvider.java -- Abstract parent for a MIDI device provider.
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


package javax.sound.midi.spi;

import javax.sound.midi.*;

/**
 * The abstract base class for all MidiDeviceProvider types.
 * 
 * @author Anthony Green (green@redhat.com)
 * @since 1.3
 *
 */
public abstract class MidiDeviceProvider
{
  /**
   * Returns true if this provider supports a specific MIDI device.
   * 
   * @param info the MIDI device descriptor
   * @return true if this provider supports info
   */
  public boolean isDeviceSupported(MidiDevice.Info info)
  {
    MidiDevice.Info infos[] = getDeviceInfo();
    
    int i = infos.length;
    
    while (i > 0)
    {
      if (info.equals(infos[--i]))
        return true;
    }
    
    return false;
  }
  
  /**
   * Get the list descriptors for all MIDI devices supported by
   * this provider.
   * 
   * @return an array of descriptors for all supported MIDI devices.
   */
  public abstract MidiDevice.Info[] getDeviceInfo();
  
  /**
   * Get the MidiDevice for the MIDI device described by info
   * 
   * @param info the descriptor for the MIDI device we want
   * @return the MidiDevice we're looking for
   * @throws IllegalArgumentException is this provider doesn't support info
   */
  public abstract MidiDevice getDevice(MidiDevice.Info info)
    throws IllegalArgumentException;
}
