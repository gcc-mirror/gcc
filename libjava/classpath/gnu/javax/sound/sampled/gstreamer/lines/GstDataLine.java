/* GstDataLine.java -- Abstract DataLine.
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

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.Control;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.LineListener;
import javax.sound.sampled.Control.Type;

public abstract class GstDataLine
    implements DataLine
{
  public static final int DEFAULT_BUFFER_SIZE = 1024;

  /** Represents if this Line is opened or not.  */
  protected Boolean open = false;

  private AudioFormat format = null;
  private int bufferSize = 0;

  public GstDataLine(AudioFormat format)
  {
    this.format = format;
    this.bufferSize = DEFAULT_BUFFER_SIZE;
  }

  public GstDataLine(AudioFormat format, int bufferSize)
  {
    this.format = format;
    this.bufferSize = bufferSize;
  }

  public int getBufferSize()
  {
    return this.bufferSize;
  }

  public AudioFormat getFormat()
  {
    return this.format;
  }

  public float getLevel()
  {
    // TODO Auto-generated method stub
    return 0;
  }

  public void addLineListener(LineListener listener)
  {
    // TODO Auto-generated method stub

  }

  public Control getControl(Type what)
  {
    // TODO Auto-generated method stub
    return null;
  }

  public Control[] getControls()
  {
    // TODO Auto-generated method stub
    return null;
  }

  public javax.sound.sampled.Line.Info getLineInfo()
  {
    // TODO Auto-generated method stub
    return null;
  }

  public boolean isControlSupported(Type what)
  {
    return false;
  }

  public boolean isOpen()
  {
    // TODO Auto-generated method stub
    return false;
  }

  public void removeLineListener(LineListener listener)
  {
    // TODO Auto-generated method stub

  }

  /* protected methods for subclasses */

  /**
   * @param open the open to set
   */
  protected void setOpen(Boolean open)
  {
    this.open = open;
  }

  /**
   * @param bufferSize the bufferSize to set
   */
  protected void setBufferSize(int bufferSize)
  {
    this.bufferSize = bufferSize;
  }

  /**
   * @param format the format to set
   */
  protected void setFormat(AudioFormat format)
  {
    this.format = format;
  }
}
