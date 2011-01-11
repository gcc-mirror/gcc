/* GstSourceDataLine.java -- SourceDataLine implementation.
 Copyright (C) 2007 Free Software Foundation, Inc.

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

package gnu.javax.sound.sampled.gstreamer.lines;

import gnu.javax.sound.AudioSecurityManager;
import gnu.javax.sound.sampled.gstreamer.lines.GstPipeline.State;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.SourceDataLine;

import static gnu.javax.sound.AudioSecurityManager.Permission;

public class GstSourceDataLine
    extends GstDataLine implements SourceDataLine
{
  private GstPipeline pipeline = null;
  private boolean open = false;

  public GstSourceDataLine(AudioFormat format)
  {
    super(format);
  }

  public void open() throws LineUnavailableException
  {
    AudioSecurityManager.checkPermissions(Permission.PLAY);

    if (open)
      throw new IllegalStateException("Line already opened");

    // create the pipeline
    pipeline = GstNativeDataLine.createSourcePipeline(getBufferSize());

    this.open = true;
  }

  public void open(AudioFormat fmt) throws LineUnavailableException
  {
    AudioSecurityManager.checkPermissions(Permission.PLAY);

    setFormat(fmt);
    this.open();
  }

  public void open(AudioFormat fmt, int size) throws LineUnavailableException
  {
    AudioSecurityManager.checkPermissions(Permission.PLAY);

    setBufferSize(size);
    this.open(fmt);
  }

  public int write(byte[] buf, int offset, int length)
  {
    return this.pipeline.write(buf, offset, length);
  }

  public int available()
  {
    return this.pipeline.available();
  }

  public void drain()
  {
    this.pipeline.drain();
  }

  public void flush()
  {
    this.pipeline.flush();
  }

  public int getFramePosition()
  {
    System.out.println("getFramePosition -: IMPLEMENT ME!!");
    return 0;
  }

  public long getLongFramePosition()
  {
    System.out.println("getLongFramePosition -: IMPLEMENT ME!!");
    return 0;
  }

  public long getMicrosecondPosition()
  {
    System.out.println("getMicrosecondPosition -: IMPLEMENT ME!!");
    return 0;
  }

  public boolean isActive()
  {
    State state = pipeline.getState();
    return (state == State.PLAY || state == State.PAUSE);
  }

  public void start()
  {
    pipeline.setState(State.PLAY);
  }

  public void stop()
  {
    pipeline.setState(State.PAUSE);
  }

  public void close()
  {
    pipeline.close();
    this.open = false;
  }

  public boolean isRunning()
  {
    return (pipeline.getState() == State.PLAY);
  }
}
