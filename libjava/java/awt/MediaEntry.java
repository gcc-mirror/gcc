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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


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

