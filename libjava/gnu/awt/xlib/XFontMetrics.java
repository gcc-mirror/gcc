/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.awt.xlib;

import java.awt.FontMetrics;

public class XFontMetrics extends FontMetrics
{
  gnu.gcj.xlib.Font xfont;
  
  public XFontMetrics(gnu.gcj.xlib.Font xfont, java.awt.Font awtFont)
  {
    super(awtFont);
    this.xfont = xfont;
  }

  public int getAscent()
  {
    return xfont.getAscent();
  }

  public int getDescent()
  {
    return xfont.getDescent();
  }

  public int getMaxAscent()
  {
    return xfont.getMaxAscent();
  }
 
  public int getMaxDescent()
  {
    return xfont.getMaxDescent();
  }

  public int stringWidth(String str)
  {
    return xfont.getStringWidth(str);
  }
}
