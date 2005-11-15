/* Mixers
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


package javax.sound.sampled;

/**
 * A Mixer is a Line which itself holds multiple lines.
 * @since 1.3
 */
public interface Mixer extends Line
{
  /**
   * An Info object describes a mixer.
   * @since 1.3
   */
  class Info
  {
    private String name;
    private String description;
    private String vendor;
    private String version;

    /**
     * Create a new mixer description.
     * @param name the name of the mixer
     * @param vendor the vendor
     * @param desc a descriptive string
     * @param vers the mixer's version
     */
    public Info(String name, String vendor, String desc, String vers)
    {
      this.name = name;
      this.description = desc;
      this.vendor = vendor;
      this.version = vers;
    }

    public final boolean equals(Object o)
    {
      return super.equals(o);
    }

    public final int hashCode()
    {
      return super.hashCode();
    }

    /**
     * Return the name of the mixer.
     */
    public String getName()
    {
      return name;
    }

    /**
     * Return the mixer's description.
     */
    public String getDescription()
    {
      return description;
    }

    /**
     * Return the mixer's vendor.
     */
    public String getVendor()
    {
      return vendor;
    }

    /**
     * Return the mixer's version.
     */
    public String getVersion()
    {
      return version;
    }

    public String toString()
    {
      return ("name=" + name + "; description=" + description
	      + "; vendor=" + vendor + "; version=" + version);
    }
  }

  /**
   * Return a Line associated with this Mixer, given its description.
   * @param info the description of the line to find
   * @return the corresponding Line
   * @throws LineUnavailableException if no Line matching the description
   * exists in this Mixer
   */
  Line getLine(Line.Info info) throws LineUnavailableException;

  /**
   * Return the number of lines matching this description.
   * @param info the description of the lines to find.
   */
  int getMaxLines(Line.Info info);

  /**
   * Return an Info object describing this Mixer.
   */
  Info getMixerInfo();

  /**
   * Return an array of Info objects describing all the source lines
   * available in this Mixer.
   */
  Line.Info[] getSourceLineInfo();

  /**
   * Return an array of Info objects describing all the source lines
   * available in this Mixer, which match the provided decsription.
   * @param info the description of the source lines to find 
   */
  Line.Info[] getSourceLineInfo(Line.Info info);

  /**
   * Return an array of all the source lines available in this Mixer.
   */
  Line[] getSourceLines();

  /**
   * Return an array of Info objects describing all the target lines
   * available in this Mixer.
   */
  Line.Info[] getTargetLineInfo();

  /**
   * Return an array of Info objects describing all the target lines
   * available in this Mixer, which match the provided decsription.
   * @param info the description of the target lines to find 
   */
  Line.Info[] getTargetLineInfo(Line.Info info);

  /**
   * Return an array of all the target lines available in this Mixer.
   */
  Line[] getTargetLines();

  /**
   * Return true if a Line matching the given description is supported
   * by this Mixer, false otherwise.
   * @param info the description of the line to find
   */
  boolean isLineSupported(Line.Info info);

  /**
   * Return true if this Mixer supports synchronization of the given set
   * of lines.
   * @param lines the lines to check
   * @param sync true if the synchronization must be accurate at all times
   */
  boolean isSynchronizationSupported(Line[] lines, boolean sync);

  /**
   * Start synchronization on the given set of lines.
   * @param lines the lines to synchronize, or null for all the lines
   * @param sync true if the synchronization must be accurate at all times
   * @throws IllegalArgumentException if the lines cannot be synchronized
   */
  void synchronize(Line[] lines, boolean sync);

  /**
   * Stop synchronization for the given set of lines.
   * @param lines the lines to unsynchronize, or null for all the lines
   */
  void unsynchronize(Line[] lines);
}
