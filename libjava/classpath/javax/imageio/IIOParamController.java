/* IIOParamController.java --
   Copyright (C) 2004  Free Software Foundation, Inc.

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


package javax.imageio;

/**
 * An interface to set image parameters.  An IIOParamController may be
 * a GUI component, a database reader, command-line parser or any
 * other means of getting parameter settings.  For exampe, a dialog
 * box could implement IIOParamController to allow a user to adjust
 * JPEG compression levels.
 *
 * The activate method should always behave modally; it should only
 * return when the action has been either cancelled or completed.
 *
 * @author Michael Koch (konqueror@gmx.de)
 */
public interface IIOParamController
{
  /**
   * Activates the controller.  A return value of false should mean
   * that no changes were made to param.  A return value of true
   * should mean that the image is ready to be read or written.
   *
   * @param param the <code>IIOParam</code> to be modified
   *
   * @return true if the <code>IIOParam</code> has been modified,
   * false otherwise
   *
   * @exception IllegalArgumentException if param is null or is not an instance
   * of the correct class
   */
  boolean activate(IIOParam param);
}
