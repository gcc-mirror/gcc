/* VMPipe.java -- Reference implementation for VM hooks used by PipeImpl
   Copyright (C) 2004, 2010  Free Software Foundation, Inc.

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

package gnu.java.nio;

import java.io.IOException;
import gnu.classpath.Configuration;

/**
 * This class contains the native methods for gnu.java.nio.PipeImpl
 * As such, it needs help from the VM.
 *
 * @author Patrik Reali
 */
final class VMPipe
{

  static
  {
    // load the shared library needed for native methods.
    if (Configuration.INIT_LOAD_LIBRARY)
      {
        System.loadLibrary ("javanio");
      }
  }

  private VMPipe() {} // Prohibits instantiation.

  /**
   * Create a pipe, consisting of a readable VMChannel and a writable
   * VMChannel. The readable channel is returned is the first element
   * of the array, and the writable in the second.
   *
   * @return A pair of VMChannels; the first readable, the second
   *  writable.
   * @throws IOException If the pipe cannot be created.
   */
  static VMChannel[] pipe() throws IOException
  {
    VMChannel[] pipe = new VMChannel[2];
    int[] fds = pipe0();
    pipe[0] = new VMChannel(fds[0]);
    pipe[1] = new VMChannel(fds[1]);
    return pipe;
  }

  private static native int[] pipe0() throws IOException;
}
