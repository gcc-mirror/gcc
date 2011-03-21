/*
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

import java.io.IOException;
import java.io.InputStream;

/**
 * This is an InputStream which is specialized for reading audio files.
 * In particular it only allows operations to act on a multiple of
 * the audio stream's frame size.
 * @since 1.3
 */
public class AudioInputStream extends InputStream
{
  /** The format of the audio stream.  */
  protected AudioFormat format;

  /** The length of the audio stream in frames.  */
  protected long frameLength;

  /** The current frame position, starting from frame zero.  */
  protected long framePos;

  /** The size of a frame in bytes.  */
  protected int frameSize;

  // I wonder why this class doesn't inherit from FilterInputStream.
  private InputStream input;

  // The saved frame position, used for mark/reset.
  private long markedFramePos;

  /**
   * Create a new AudioInputStream given an underlying InputStream,
   * the audio format, and the length of the data in frames.  The
   * frame size is taken from the format.
   * @param is the underlying input stream
   * @param fmt the format of the data
   * @param length the length of the data in frames
   */
  public AudioInputStream(InputStream is, AudioFormat fmt, long length)
  {
    this.format = fmt;
    this.frameLength = length;
    this.framePos = 0;
    this.frameSize = fmt.getFrameSize();
    this.input = is;
  }

  /**
   * Create a new AudioInputStream given a TargetDataLine.  The audio
   * format and the frame size are taken from the line.
   * @param line the TargetDataLine
   */
  public AudioInputStream(TargetDataLine line)
  {
    this(new TargetInputStream(line), line.getFormat(),
         AudioSystem.NOT_SPECIFIED);
  }

  /**
   * Return the number of bytes available to be read from the
   * underlying stream.  This wrapper method ensures that the result
   * is always a multiple of the frame size.
   */
  public int available() throws IOException
  {
    int result = input.available();
    // Ensure result is a multiple of the frame size.
    if (frameSize != AudioSystem.NOT_SPECIFIED)
      result -= result % frameSize;
    return result;
  }

  /**
   * Close the stream.
   */
  public void close() throws IOException
  {
    input.close();
  }

  /**
   * Get the format associated with this stream.
   * @return the AudioFormat
   */
  public AudioFormat getFormat()
  {
    return format;
  }

  /**
   * Get the length of this stream in frames.  Note that this
   * may be AudioSystem#NOT_SPECIFIED.
   * @return the length of the stream in frames
   */
  public long getFrameLength()
  {
    return frameLength;
  }

  public void mark(int limit)
  {
    input.mark(limit);
    markedFramePos = framePos;
  }

  /**
   * Return true if the underlying stream supports mark and reset,
   * false otherwise.
   */
  public boolean markSupported()
  {
    return input.markSupported();
  }

  /**
   * Read a single byte from the underlying stream.  If the frame
   * size is set, and is not one byte, an IOException will be thrown.
   */
  public int read() throws IOException
  {
    if (frameSize != 1)
      throw new IOException("frame size must be 1 for read()");
    int result;
    if (framePos == frameLength)
      result = -1;
    else
      result = input.read();
    if (result != -1)
      ++framePos;
    return result;
  }

  public int read(byte[] buf) throws IOException
  {
    return read(buf, 0, buf.length);
  }

  public int read(byte[] buf, int offset, int length) throws IOException
  {
    int result;
    if (framePos == frameLength)
      result = -1;
    else
      {
        int myFrameSize = (frameSize == AudioSystem.NOT_SPECIFIED
                           ? 1 : frameSize);
        // Ensure length is a multiple of frame size.
        length -= length % myFrameSize;

        result = 0;
        while (result == 0 || result % myFrameSize != 0)
          {
            int val = input.read(buf, offset, length);
            if (val < 0)
              {
                // This is a weird situation as we might have read a
                // frame already.  It isn't clear at all what to do if
                // we only found a partial frame.  For now we just
                // return whatever we did find.
                if (result == 0)
                  return -1;
                result -= result % myFrameSize;
                break;
              }
            result += val;
          }
        // assert result % myFrameSize == 0;
        framePos += result / myFrameSize;
      }
    return result;
  }

  public void reset() throws IOException
  {
    input.reset();
    framePos = markedFramePos;
  }

  public long skip(long n) throws IOException
  {
    if (frameSize != AudioSystem.NOT_SPECIFIED)
      n -= n % frameSize;
    long actual = input.skip(n);
    if (frameSize != AudioSystem.NOT_SPECIFIED)
      framePos += actual / frameSize;
    return actual;
  }

  private static class TargetInputStream extends InputStream
  {
    private TargetDataLine line;
    private byte[] buf;

    /**
     * Create a new TargetInputStream.
     * @param line the line to wrap
     */
    public TargetInputStream(TargetDataLine line)
    {
      this.line = line;
      // FIXME: do we have to call line.open()?
    }

    public synchronized int read() throws IOException
    {
      if (buf == null)
        buf = new byte[1];
      int count = read(buf, 0, 1);
      if (count < 0)
        return -1;
      return buf[0];
    }

    public int read(byte[] buf, int offset, int length) throws IOException
    {
      return line.read(buf, offset, length);
    }
  }
}
