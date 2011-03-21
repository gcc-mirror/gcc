/* VMChannels.java --
   Copyright (C) 2005  Free Software Foundation, Inc.

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


package java.nio.channels;

import gnu.java.nio.ChannelInputStream;
import gnu.java.nio.ChannelOutputStream;
import gnu.java.nio.FileChannelImpl;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

final class VMChannels
{
  /**
   * This class isn't intended to be instantiated.
   */
  private VMChannels()
  {
    // Do nothing here.
  }

  private static Object createStream(Class streamClass, Channel ch)
  {
    try
      {
        Class[] argTypes = new Class[1];
        argTypes[0] = FileChannelImpl.class;
        Constructor constructor =
          streamClass.getDeclaredConstructor(argTypes);
        constructor.setAccessible(true);
        Object[] args = new Object[1];
        args[0] = ch;
        return constructor.newInstance(args);
      }
    catch (IllegalAccessException e)
      {
        // Ignored.
      }
    catch (InstantiationException e)
      {
        // Ignored.
      }
    catch (InvocationTargetException e)
      {
        // Ignored.
      }
    catch (NoSuchMethodException e)
      {
        // Ignored.
      }

    return null;
  }

  /**
   * Constructs a stream that reads bytes from the given channel.
   */
  static InputStream newInputStream(ReadableByteChannel ch)
  {
    if (ch instanceof FileChannelImpl)
      return (FileInputStream) createStream(FileInputStream.class, ch);

    return  new ChannelInputStream(ch);
  }

  /**
   * Constructs a stream that writes bytes to the given channel.
   */
  static OutputStream newOutputStream(WritableByteChannel ch)
  {
    if (ch instanceof FileChannelImpl)
      return (FileOutputStream) createStream(FileOutputStream.class, ch);

    return new ChannelOutputStream(ch);
  }
}
