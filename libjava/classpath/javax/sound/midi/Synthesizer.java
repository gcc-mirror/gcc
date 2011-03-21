/* Synthesizer.java -- A MIDI audio synthesizer interface
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
 * Interface for MIDI audio synthesizer devices.
 *
 * @author Anthony Green (green@redhat.com)
 * @since 1.3
 *
 */
public interface Synthesizer extends MidiDevice
{
  /**
   * Get the maximum number of notes that the synth can play at once.
   *
   * @return the maximum number of notes that the synth can play at once
   */
  public int getMaxPolyphony();

  /**
   * The processing latency for this synth in microseconds.
   *
   * @return the processing latency for this synth in microseconds
   */
  public long getLatency();

  /**
   * Get the set of MIDI channels controlled by this synth.
   *
   * @return an array of MIDI channels controlled by this synth
   */
  public MidiChannel[] getChannels();

  /**
   * Get the current status for the voices produced by this synth.
   *
   * @return an array of VoiceStatus objects, getMaxPolyphony() in length
   */
  public VoiceStatus[] getVoiceStatus();

  /**
   * Returns true is this synth is capable of loading soundbank.
   *
   * @param soundbank the Soundbank to examine
   * @return true if soundbank can be loaded, false otherwise
   */
  public boolean isSoundbankSupported(Soundbank soundbank);

  /**
   * Load an instrument into this synth.  The instrument must be part of a
   * supported soundbank.
   *
   * @param instrument the Instrument to load
   * @return true if the instrument was loaded and false otherwise
   * @throws IllegalArgumentException if this synth doesn't support instrument
   */
  public boolean loadInstrument(Instrument instrument);

  /**
   * Unload an instrument from this synth.
   *
   * @param instrument the Instrument to unload
   * @throws IllegalArgumentException if this synth doesn't support instrument
   */
  public void unloadInstrument(Instrument instrument);

  /**
   * Move an intrument from one place to another.  The instrument at the
   * target location is unloaded.
   *
   * @param from the instrument source
   * @param to the instrument target
   * @return if from was remapped
   * @throws IllegalArgumentException
   */
  public boolean remapInstrument(Instrument from, Instrument to);

  /**
   * Get the default Soundbank for this synth.  Return null if there is no
   * default.
   *
   * @return the default Soundbank for this synth, possibly null.
   */
  public Soundbank getDefaultSoundbank();

  /**
   * Get an array containing all instruments in this synthesizer.
   *
   * @return an array containing all instruments in this synthesizer
   */
  public Instrument[] getAvailableInstruments();

  /**
   * Get an array containing all instruments loaded in this synthesizer.
   *
   * @return an array containing all instruments loaded in this synthesizer
   */
  public Instrument[] getLoadedInstruments();

  /**
   * Load all soundbank instruments into this synthesizer.
   *
   * @param soundbank the Soundbank from which to load instruments
   * @return true if all instruments were loaded, false othewise
   * @throws IllegalArgumentException if the soundbank isn't supported by this
   */
  public boolean loadAllInstruments(Soundbank soundbank);

  /**
   * Unload all soundbank instruments from this synthesizer.
   *
   * @param soundbank the Soundbank containing the instruments to unload
   * @throws IllegalArgumentException if the soundbank isn't supported by this
   */
  public void unloadAllInstruments(Soundbank soundbank);

  /**
   * Load a subset of soundbank instruments into this synthesizer.  The
   * subset is defined by an array of Patch objects.
   *
   * @param soundbank the Soundbank from which to load instruments
   * @param patchList the array of patches identifying instruments to load
   * @return true if instruments were loaded, false otherwise
   * @throws IllegalArgumentException if the soundbank isn't supported by this
   */
  public boolean loadInstruments(Soundbank soundbank, Patch[] patchList);

  /**
   * Unload a subset of soundbank instruments from this synthesizer.
   *
   * @param soundbank the Soundbank containing the instruments to unload
   * @param patchList the array of patches identifying instruments to unload
   * @throws IllegalArgumentException if the soundbank isn't supported by this
   */
  public void unloadInstruments(Soundbank soundbank, Patch[] patchList);
}
