/* MediaEntry.java -- An entry in a MediaTracker
   Copyright (C) 1999 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package java.awt;

/**
  * This is an entry in the media tracker
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
abstract class MediaEntry implements java.io.Serializable
{

protected static final int LOADING = 1;
protected static final int ABORTED = 2;
protected static final int ERRORED = 4;
protected static final int COMPLETE = 8;
protected static final int LOADSTARTED = 16;
protected static final int DONE = 32;

private MediaTracker tracker;
private int ID;
private int status;
private boolean cancelled;
private MediaEntry next;

static MediaEntry
insert(MediaEntry a, MediaEntry b)
{
  while (a.next != null)
    a = a.next;

  a.next = b;
  return(b);
}

MediaEntry(MediaTracker tracker, int ID)
{
  this.tracker = tracker;
  this.ID = ID;
}

public int
getID()
{
  return(ID);
}

public int
getStatus()
{
  return(status);
}

public void
setStatus(int status)
{
  this.status = status;
}

public MediaEntry
getNext()
{
  return(next);
}

public void
cancel()
{
  cancelled = true;
  if ((status == LOADING) || (status == LOADSTARTED))
    setStatus(ABORTED);
}

abstract void
startLoad();

abstract Object
getMedia();

} // class MediaEntry 

