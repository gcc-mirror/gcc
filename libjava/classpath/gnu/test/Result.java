/* Result.java -- Abstract base class for all Result types.
   Copyright (c) 1998 Free Software Foundation, Inc.

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

package gnu.test;

/**
 * Class which all usable Result objects extend.
 */
public abstract class Result
{
  String name, msg;

  /**
   * Create a result of a given type, with a given message.
   *
   * @param name name of type
   * @param msg message
   */
  public Result(String name, String msg) {
    this.name = name;
    this.msg = msg;
  }

  /**
   * Create a result of a given type.
   *
   * @param name name of type
   */
  public Result(String name) {
    this(name, "");
  }

  /**
   * Returns the name of the type.
   */
  public String getName() {
    return name;
  }

  /**
   * Returns the message associated with this instance of Result, or
   * the empty string if no message exists.
   */
  public String getMsg() {
    return (msg != null) ? msg : "";
  }

  public String toString() {
    return getName() + ":" + getMsg();
  }
}
