/* RecordingInputStream.java -- Input stream that records data.
   Copyright (C) 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.net.ssl.provider;

import java.io.ByteArrayOutputStream;
import java.io.FilterInputStream;
import java.io.InputStream;
import java.io.IOException;

/**
 * A filter input stream that records every byte read from the underlying
 * input stream. This class is useful for protocols that require portions
 * of the communication to be saved, such as the handshake and key
 * derivation in SSL.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
class RecordingInputStream extends FilterInputStream
{

  // Fields.
  // -------------------------------------------------------------------------

  protected ByteArrayOutputStream sink;

  // Constructors.
  // -------------------------------------------------------------------------

  RecordingInputStream(InputStream in)
  {
    this(in, new ByteArrayOutputStream());
  }

  RecordingInputStream(InputStream in, ByteArrayOutputStream sink)
  {
    super(in);
    this.sink = sink;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public synchronized int read() throws IOException
  {
    int i = in.read();
    sink.write(i);
    return i;
  }

  public synchronized int read(byte[] buf, int off, int len) throws IOException
  {
    int l = in.read(buf, off, len);
    sink.write(buf, off, l);
    return l;
  }

  public synchronized int read(byte[] buf) throws IOException
  {
    return read(buf, 0, buf.length);
  }

  public synchronized long skip(long len) throws IOException
  {
    long l = 0;
    int i = 0;
    byte[] buf = new byte[1024];
    while (l < len)
      {
        i = read(buf, 0, (int) Math.min((long) buf.length, len - l));
        if (i == -1)
          break;
        l += i;
      }
    return l;
  }

  /**
   * Returns all bytes recorded after this instance was created, or the last
   * call to {@link resetSink()}.
   *
   * @return The recorded bytes.
   */
  byte[] getBytes()
  {
    return sink.toByteArray();
  }

  /**
   * Clears the recording buffer off all previously-recorded bytes.
   */
  void resetSink()
  {
    sink.reset();
  }
}
