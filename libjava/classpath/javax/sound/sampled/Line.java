/* An input or output line
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
 * A Line represents a single input or output audio line.
 * @since 1.3
 */
public interface Line
{
  /**
   * An object of this type holds information about a Line.
   * @since 1.3
   */
  class Info
  {
    private Class klass;

    /**
     * Create a new Info object.  The argument is the class of the line,
     * for instance TargetDataLine.class.
     * @param klass the class of the line
     */
    public Info(Class<?> klass)
    {
      this.klass = klass;
    }

    /**
     * Return the line's class.
     */
    public Class<?> getLineClass()
    {
      return klass;
    }

    /**
     * Return true if this Info object matches the given object.
     * @param other the object to match
     * @return true if they match, false otherwise
     */
    public boolean matches(Info other)
    {
      return klass.equals(other.klass);
    }

    /**
     * Return a description of this Info object.
     */
    public String toString()
    {
      return klass.toString();
    }
  }

  /**
   * Add a listener which will be notified whenever this Line changes state.
   * @param listener the listener to notify
   */
  void addLineListener(LineListener listener);
  
  /**
   * Close this line.
   */
  void close();

  /**
   * Return the control associated with this Line that matches the
   * argument.
   * @param what the type of the control to match
   * @return the associated control
   * @throws IllegalArgumentException if a control of this type is not
   * available for this line
   */
  Control getControl(Control.Type what);

  /**
   * Return an array of controls associated with this Line.  Note that
   * this method will not return null -- if there are no controls, it
   * will return a zero-length array.
   */
  Control[] getControls();

  /**
   * Return the Info object associated with this Line.
   */
  Info getLineInfo();

  /**
   * Return true if a Control matching the argument is available for this
   * Line, false otherwise.
   * @param what the type of the control to match
   */
  boolean isControlSupported(Control.Type what);

  /**
   * Return true if this line is open, false otherwise.
   */
  boolean isOpen();

  /**
   * Open this line.
   * @throws LineUnavailableException if the line is unavailable for some
   * reason
   */
  void open() throws LineUnavailableException;

  /**
   * Remove the listener from this Line; after this call the listener will
   * no longer be notified when this Line changes state.
   * @param listener the listener to remove
   */
  void removeLineListener(LineListener listener);
}
