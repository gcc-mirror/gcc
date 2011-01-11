/* gnu/regexp/CharIndexedInputStream.java
   Copyright (C) 1998-2001, 2004, 2006 Free Software Foundation, Inc.

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

package gnu.java.util.regex;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;

// TODO: move(x) shouldn't rely on calling next() x times

class CharIndexedInputStream implements CharIndexed
{
  private static final int BUFFER_INCREMENT = 1024;
  private static final int UNKNOWN = Integer.MAX_VALUE; // value for end

  private BufferedInputStream br;

  // so that we don't try to reset() right away
  private int index = -1;

  private int bufsize = BUFFER_INCREMENT;

  private int end = UNKNOWN;

  private char cached = OUT_OF_BOUNDS;

  // Big enough for a \r\n pair
  // lookBehind[0] = most recent
  // lookBehind[1] = second most recent
  private char[] lookBehind = new char[]{ OUT_OF_BOUNDS, OUT_OF_BOUNDS };

    CharIndexedInputStream (InputStream str, int index)
  {
    if (str instanceof BufferedInputStream)
      br = (BufferedInputStream) str;
    else
      br = new BufferedInputStream (str, BUFFER_INCREMENT);
    next ();
    if (index > 0)
      move (index);
  }

  private boolean next ()
  {
    if (end == 1)
      return false;
    end--;                      // closer to end

    try
    {
      if (index != -1)
        {
          br.reset ();
        }
      int i = br.read ();
      br.mark (bufsize);
      if (i == -1)
        {
          end = 1;
          cached = OUT_OF_BOUNDS;
          return false;
        }
      cached = (char) i;
      index = 1;
    } catch (IOException e)
    {
      e.printStackTrace ();
      cached = OUT_OF_BOUNDS;
      return false;
    }
    return true;
  }

  public char charAt (int index)
  {
    if (index == 0)
      {
        return cached;
      }
    else if (index >= end)
      {
        return OUT_OF_BOUNDS;
      }
    else if (index == -1)
      {
        return lookBehind[0];
      }
    else if (index == -2)
      {
        return lookBehind[1];
      }
    else if (index < -2)
      {
        return OUT_OF_BOUNDS;
      }
    else if (index >= bufsize)
      {
        // Allocate more space in the buffer.
        try
        {
          while (bufsize <= index)
            bufsize += BUFFER_INCREMENT;
          br.reset ();
          br.mark (bufsize);
          br.skip (index - 1);
        }
        catch (IOException e)
        {
        }
      }
    else if (this.index != index)
      {
        try
        {
          br.reset ();
          br.skip (index - 1);
        }
        catch (IOException e)
        {
        }
      }
    char ch = OUT_OF_BOUNDS;

    try
    {
      int i = br.read ();
      this.index = index + 1;   // this.index is index of next pos relative to charAt(0)
      if (i == -1)
        {
          // set flag that next should fail next time?
          end = index;
          return ch;
        }
      ch = (char) i;
    } catch (IOException ie)
    {
    }

    return ch;
  }

  public boolean move (int index)
  {
    // move read position [index] clicks from 'charAt(0)'
    boolean retval = true;
    while (retval && (index-- > 0))
      retval = next ();
    return retval;
  }

  public boolean isValid ()
  {
    return (cached != OUT_OF_BOUNDS);
  }

  public CharIndexed lookBehind (int index, int length)
  {
    throw new
      UnsupportedOperationException
      ("difficult to look behind for an input stream");
  }

  public int length ()
  {
    throw new
      UnsupportedOperationException
      ("difficult to tell the length for an input stream");
  }

  public void setLastMatch (REMatch match)
  {
    throw new
      UnsupportedOperationException
      ("difficult to support setLastMatch for an input stream");
  }

  public REMatch getLastMatch ()
  {
    throw new
      UnsupportedOperationException
      ("difficult to support getLastMatch for an input stream");
  }

  public void setHitEnd (REMatch match)
  {
    throw new
      UnsupportedOperationException
      ("difficult to support setHitEnd for an input stream");
  }

  public boolean hitEnd ()
  {
    throw new
      UnsupportedOperationException
      ("difficult to support hitEnd for an input stream");
  }

  public int getAnchor ()
  {
    throw new
      UnsupportedOperationException
      ("difficult to support getAnchor for an input stream");
  }

  public void setAnchor (int anchor)
  {
    throw new
      UnsupportedOperationException
      ("difficult to support setAnchor for an input stream");
  }

  public boolean move1 (int index)
  {
    throw new
      UnsupportedOperationException
      ("difficult to support move1 for an input stream");
  }

}
