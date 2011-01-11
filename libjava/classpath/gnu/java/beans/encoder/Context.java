/* Context.java -- Provides calling context information to ScannerStates.
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


package gnu.java.beans.encoder;

/** A <code>Contect</code> object describes the current state
 * and the call number while processing the original object
 * tree in the {@link ScanEngine}.
 *
 * <p>The class allows to distinguish the different calling states
 * and is neccessary for the child element skipping feature of
 * the {@link GenericScannerState}.</p>
 *
 * @author Robert Schuster (robertschuster@fsfe.org)
 *
 */
public class Context
{
  private String state;

  private int call;

  Context(String newState, int newCall)
  {
    state = newState;
    call = newCall;
  }

  public int hashCode()
  {
    int hc = 7;
    hc = 31 * hc + state.hashCode();
    hc = 31 * hc + call;

    return hc;
  }

  public boolean equals(Object o)
  {
    if (!(o instanceof Context))
      return false;

    Context that = (Context) o;

    return state.equals(that.state)
      && call == that.call;
  }

  public String toString()
  {
    return "Context [state=" + state + ", call=" + call + "]";
  }
}
