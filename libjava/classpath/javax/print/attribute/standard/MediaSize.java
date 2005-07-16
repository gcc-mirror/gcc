/* MediaSize.java -- 
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


package javax.print.attribute.standard;

import java.util.ArrayList;

import javax.print.attribute.Attribute;
import javax.print.attribute.Size2DSyntax;

public class MediaSize extends Size2DSyntax
  implements Attribute
{
  private static final long serialVersionUID = -1967958664615414771L;

  private static ArrayList mediaCache = new ArrayList();
  
  private MediaSizeName media;
  
  public MediaSize(float x, float y, int units)
  {
    super(x, y, units);
  }
  
  public MediaSize(float x, float y, int units, MediaSizeName media)
  {
    super(x, y, units);
    this.media = media;
  }
  
  public MediaSize(int x, int y, int units)
  {
    super(x, y, units);
  }
  
  public MediaSize(int x, int y, int units, MediaSizeName media)
  {
    super(x, y, units);
    this.media = media;
  }
  
  /**
   * Returns category of this class.
   *
   * @return the class <code>MediaSize</code> itself
   */
  public Class getCategory()
  {
    return MediaSize.class;
  }

  public static MediaSize getMediaSizeForName(MediaSizeName media)
  {
    for (int i = 0; i < mediaCache.size(); i++)
      {
	MediaSize size = (MediaSize) mediaCache.get(i);
	
	if (size.getMediaSizeName().equals(media))
	  return size;
      }

    return null;
  }
  
  public MediaSizeName getMediaSizeName()
  {
    return media;
  }

  /**
   * Returns name of this class.
   *
   * @return the string "media-size"
   */
  public String getName()
  {
    return "media-size";
  }
}
