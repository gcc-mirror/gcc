/* GStreamerMixer.java -- Mixer implementation.
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

package gnu.javax.sound.sampled.gstreamer;

import gnu.javax.sound.sampled.gstreamer.lines.GstSourceDataLine;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.Control;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.Line;
import javax.sound.sampled.LineListener;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.Mixer;
import javax.sound.sampled.SourceDataLine;
import javax.sound.sampled.Control.Type;

/**
 * @author Mario Torre <neugens@limasoftware.net>
 */
public class GStreamerMixer
    implements Mixer
{
  public static class GstInfo extends Info
  {
    /* Mixer Properties */

    /** Name */
    private static final String name = "Classpath GStreamer Sound Audio Engine";

    /** Vendor */
    private static final String vendor = "GNU Classpath";

    /** Description */
    private static final String desc = "GStreamer-based software mixer";

    /** Version */
    private static final String vers = "0.0.1";

    protected GstInfo()
    {
      super(name, vendor, desc, vers);
    }
  }

  public static final String GST_BACKEND = GstInfo.name;
  public static final String GST_DECODER = "decoder";
  public static final String GST_TYPE_NAME = "type";
  public static final String GST_FILE_EXTENSION = "ext";

  /** Mixer Info */
  private static final Mixer.Info INFO = new GStreamerMixer.GstInfo();

  public Line getLine(Line.Info info)
      throws LineUnavailableException
  {
    // get all the lines formats supported by this mixer and
    // and see if there is one matching the given line
    // if the format comes from the gstreamer backend
    // gstreamer will be able to deal with it
    Class clazz = info.getLineClass();
    DataLine.Info _info = (DataLine.Info) info;

    if (clazz == SourceDataLine.class)
      {
        for (AudioFormat format : _info.getFormats())
          {
            // see if we are a gstreamer child :)
            if (format.properties().containsKey(GST_BACKEND));
              {
                // we got it
                return new GstSourceDataLine(format);
              }
          }
      }

    // TODO: we also support basic PCM

    throw new LineUnavailableException("Cannot open a line");
  }

  public int getMaxLines(Line.Info info)
  {
    // TODO
    return 1;
  }

  public Info getMixerInfo()
  {
    return INFO;
  }

  public javax.sound.sampled.Line.Info[] getSourceLineInfo()
  {
    // TODO Auto-generated method stub
    return null;
  }

  public Line.Info[] getSourceLineInfo(Line.Info info)
  {
    // TODO Auto-generated method stub
    return null;
  }

  public Line[] getSourceLines()
  {
    // TODO Auto-generated method stub
    return null;
  }

  public javax.sound.sampled.Line.Info[] getTargetLineInfo()
  {
    // TODO Auto-generated method stub
    return null;
  }

  public Line.Info[] getTargetLineInfo(Line.Info info)
  {
    // TODO Auto-generated method stub
    return null;
  }

  public Line[] getTargetLines()
  {
    // TODO Auto-generated method stub
    return null;
  }

  public boolean isLineSupported(Line.Info info)
  {
    // We support any kind of mixer that comes
    // from our gstreamer backend.
    // In addition, we support PCM based audio streams for
    // direct playback.
    if (info instanceof DataLine.Info)
      {
        DataLine.Info _dinfo = (DataLine.Info) info;
        _dinfo.getFormats();
      }

    return true;
  }

  public boolean isSynchronizationSupported(Line[] lines, boolean sync)
  {
    // TODO Auto-generated method stub
    return false;
  }

  public void synchronize(Line[] lines, boolean sync)
  {
    // TODO Auto-generated method stub

  }

  public void unsynchronize(Line[] lines)
  {
    // TODO Auto-generated method stub

  }

  public void addLineListener(LineListener listener)
  {
    // TODO Auto-generated method stub

  }

  public void close()
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
    // TODO Auto-generated method stub
    return false;
  }

  public boolean isOpen()
  {
    // TODO Auto-generated method stub
    return false;
  }

  public void open() throws LineUnavailableException
  {
    // TODO Auto-generated method stub

  }

  public void removeLineListener(LineListener listener)
  {
    // TODO Auto-generated method stub
  }
}
