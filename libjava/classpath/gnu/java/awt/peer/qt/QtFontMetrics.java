/* QtFontMetrics.java --
   Copyright (C)  2005  Free Software Foundation, Inc.

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

package gnu.java.awt.peer.qt;

import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.geom.Rectangle2D;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.font.LineMetrics;

public class QtFontMetrics extends FontMetrics
{

  private long nativeObject;
  private QtFontPeer peer;

  public QtFontMetrics( Font f )
  {
    super( f );
    if(f.getPeer() == null || !(f.getPeer() instanceof QtFontPeer))
      throw new IllegalArgumentException("Invalid Font object.");
    peer = (QtFontPeer) f.getPeer();
    init( peer );
  }

  public QtFontMetrics( Font f, Graphics g )
  {
    super( f );
    if(f.getPeer() == null || !(f.getPeer() instanceof QtFontPeer))
      throw new IllegalArgumentException("Invalid Font object.");
    if( !(g instanceof QtGraphics) )
      throw new IllegalArgumentException("Invalid graphics object.");
    peer = (QtFontPeer) f.getPeer();
    initGraphics(peer, (QtGraphics)g );
  }

  QtFontMetrics( QtFontPeer f, Graphics g )
  {
    super( null );
    if( !(g instanceof QtGraphics) )
      throw new IllegalArgumentException("Invalid graphics object.");
    peer = f;
    initGraphics(peer, (QtGraphics)g );
  }

  public QtFontMetrics( QtFontPeer fp )
  {
    super( (Font)null );
    peer = fp;
    init( peer );
  }

  private native void init(QtFontPeer fp);

  private native void initGraphics(QtFontPeer fp, QtGraphics g);

  private native void dispose();

  native Rectangle2D getStringBounds(String s);

  // ****************** Package private ***************************
  
  native boolean canDisplay( char c );

  // ****************** Public methods ****************************

  public native int getAscent();

  public native int getDescent();

  public native int getHeight();

  public native int getLeading();

  public native int getMaxAdvance();

  public native int charWidth(char c);

  public int charsWidth(char[] chars, int off, int len)
  {
    return stringWidth( new String(chars, off, len) );
  }

  public native int stringWidth(String str);

  public Rectangle2D getStringBounds(String str, Graphics context)
  {
    QtFontMetrics fm = new QtFontMetrics(peer, context);
    return fm.getStringBounds( str );
  }
}
