/* MidiChannel.java -- A MIDI channel
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
 * A MIDI channel.
 * 
 * @author Anthony Green (green@redhat.com)
 * @since 1.3
 *
 */
public interface MidiChannel
{
  
  /**
   * Start playing a note.
   * 
   * @param noteNumber the MIDI note number
   * @param velocity the velocity at which the key was pressed
   */
  public void noteOn(int noteNumber, int velocity);
  
  /**
   * Stop playing a note.
   *
   * @param noteNumber the MIDI note number
   * @param velocity the volcity at which the ket was released
   */
  public void noteOff(int noteNumber, int velocity);
  
  /**
   * Stop playing a note.
   * 
   * @param noteNumber the MIDI note number
   */
  public void noteOff(int noteNumber);
  
  /**
   * Change in a key pressure for a note.
   * 
   * @param noteNumber the MIDI note number
   * @param pressure the key pressure
   */
  public void setPolyPressure(int noteNumber, int pressure);
  
  /**
   * Get the key pressure for a note.
   * 
   * @param noteNumber the MIDI note number
   * @return the key pressure
   */
  public int getPolyPressure(int noteNumber);
  
  /**
   * Set the key pressure for the channel.
   * 
   * @param pressure the key pressure
   */
  public void setChannelPressure(int pressure);
  
  /**
   * Get the key pressure for the channel.
   * 
   * @return the key pressure
   */
  public int getChannelPressure();
  
  /**
   * Set a change in a controller's value.
   * 
   * @param controller the MIDI controller number (0 to 127)
   * @param value the new value (0 to 127)
   */
  public void controlChange(int controller, int value);
  
  /**
   * Get a controller's value.
   * 
   * @param controller the MIDI controller number (0 to 127)
   * @return the controller's value (0 to 127)
   */
  public int getController(int controller);
  
  /**
   * Change the patch for this channel.
   * 
   * @param program the patch number to switch to (0 to 127)
   */
  public void programChange(int program);
  
  /**
   * Change the bank and patch for this channel.
   * 
   * @param bank the bank to switch to (0 to 16383)
   * @param program the patch to switch to (0 to 127)
   */
  public void programChange(int bank, int program);
  
  /**
   * Get the current patch for this channel.
   * 
   * @return current patch (0 to 127)
   */
  public int getProgram();
  
  /**
   * Change the pitch bend for this channel using a positive 14-bit value.
   * 
   * @param bend the new pitch bend value
   */
  public void setPitchBend(int bend);
  
  /**
   * Get the pitch bend for this channel as a positive 14-bit value.
   * 
   * @return the current patch bend value
   */
  public int getPitchBend();
  
  /**
   * Reset all MIDI controllers to their default values.
   */
  public void resetAllControllers();
  
  /**
   * Stop playing all notes.  Sound may not stop.
   */
  public void allNotesOff();
  
  /**
   * Stop all sound.
   */
  public void allSoundOff();
  
  /**
   * Set whether or not local controls are on or off.  They are on by
   * default.
   * 
   * @param on true to enable local controls, false to disable
   * @return the new value
   */
  public boolean localControl(boolean on);
  
  /**
   * Turns mono mode on or off.
   * 
   * @param on true to enable mono mode, false to disable 
   */
  public void setMono(boolean on);
  
  /**
   * Get the current mono mode.
   * 
   * @return true if mono is enabled, false otherwise
   */
  public boolean getMono();
  
  /**
   * Turns omni mode on or off.
   * 
   * @param on true to enable omni mode, false to disable
   */
  public void setOmni(boolean on);
  
  /**
   * Get the current omni mode.
   * 
   * @return true if omni is enabled, false otherwise
   */
  public boolean getOmni();
  
  /**
   * Turns mute mode on or off.
   * 
   * @param mute true to enable mute mode, false to disable
   */
  public void setMute(boolean mute);
  
  /**
   * Get the current mute mode.
   * 
   * @return true if mute is enabled, false otherwise
   */
  public boolean getMute();
  
  /**
   * Turns solo mode on or off.  If any channels are soloed, then only those
   * channels make sounds, otherwise all channels will make sound.
   * 
   * @param solo true to enable solo mode, false to disable
   */
  public void setSolo(boolean solo);
  
  /**
   * Get the current solo mode.
   * 
   * @return true is solo is enabled, false otherwise.
   */
  public boolean getSolo();
}
