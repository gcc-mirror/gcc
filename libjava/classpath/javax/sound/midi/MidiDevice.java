/* MidiDevice.java -- Interface for MIDI devices
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
 * Interface for all MIDI devices.
 *
 * @author Anthony Green (green@redhat.com)
 * @since 1.3
 *
 */
public interface MidiDevice
  extends AutoCloseable
{
  /**
   * Get the Info object describing this device.
   * @return the Info object describing this device
   */
  public Info getDeviceInfo();

  /**
   * Open this MIDI device and allocate any system resource we need.
   *
   * @throws MidiUnavailableException if we're not able to open for some reason
   */
  public void open() throws MidiUnavailableException;

  /**
   * Close this MIDI device, and release any system resources we're using.
   */
  public void close();

  /**
   * Returns true if this MIDI device is open and false otherwise.
   *
   * @return true if this is open, false otherwise
   */
  public boolean isOpen();

  /**
   * If this device supports time-stamps, then it will return the number
   * of microseconds since this device has been open, and -1 otherwise.
   *
   * @return -1 or the number of microseconds since this was opened
   */
  public long getMicrosecondPosition();

  /**
   * The maximum number of MIDI IN connections we can get as Receivers,
   * or -1 if there is no maximum.
   *
   * @return -1 or the maximum number of Receivers we can get
   */
  public int getMaxReceivers();

  /**
   * The maximum number of MIDI OUT connections we can get as Transmitters,
   * or -1 if there is no maximum.
   *
   * @return -1 or the maximum number of Transmitters we can get
   */
  public int getMaxTransmitters();

  /**
   * Get a MIDI IN Receiver for this device.
   *
   * @return a MIDI IN Receiver for this device
   * @throws MidiUnavailableException if we can't get a Receiver
   */
  public Receiver getReceiver() throws MidiUnavailableException;

  /**
   * Get a MIDI OUT Transmitter for this device.
   *
   * @return a MIDI OUT Transmitter for this device
   * @throws MidiUnavailableException if we can't get a Transmitter
   */
  public Transmitter getTransmitter() throws MidiUnavailableException;

  /**
   * A MIDI device descriptor object.
   *
   * @author green@redhat.com
   *
   */
  public static class Info
  {
    // Private data describing this device
    private String name;
    private String vendor;
    private String description;
    private String version;

    /**
     * Create an Info object for a MIDI device
     *
     * @param name the device name
     * @param vendor the vendor name
     * @param description the device description
     * @param version the device version string
     */
    protected Info(String name, String vendor, String description, String version)
    {
      this.name = name;
      this.vendor = vendor;
      this.description = description;
      this.version = version;
    }

    /**
     * This equals method only returns true if this object
     * is the same as obj.
     *
     * @param obj the object we're comparing to
     * @return true if this is the same object
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public final boolean equals(Object obj)
    {
      return super.equals(obj);
    }

    /**
     * A hash code for this object.
     *
     * @return the hash code for this object
     * @see java.lang.Object#hashCode()
     */
    public final int hashCode()
    {
      return super.hashCode();
    }

    /**
     * Get the device name.
     *
     * @return the device name
     */
    public final String getName()
    {
      return name;
    }

    /**
     * Get the device vendor.
     *
     * @return the device vendor
     */
    public final String getVendor()
    {
      return vendor;
    }

    /**
     * Get the device description
     *
     * @return the device description
     */
    public final String getDescription()
    {
      return description;
    }

    /**
     * get the device version
     *
     * @return the device version
     */
    public final String getVersion()
    {
      return version;
    }

    /**
     * Simple return the name of the device.
     *
     * @return the device name
     * @see java.lang.Object#toString()
     */
    public final String toString()
    {
      return name;
    }
  }
}
