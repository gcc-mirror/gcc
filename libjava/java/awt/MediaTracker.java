/* MediaTracker.java -- Class used for keeping track of images
   Copyright (C) 1999, 2002 Free Software Foundation, Inc.

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

import java.util.ArrayList;
import java.awt.image.ImageObserver;

/**
  * This class is used for keeping track of the status of various media
  * objects.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Bryce McKinlay
  */
public class MediaTracker implements java.io.Serializable
{
  public static final int LOADING = 1 << 0;
  public static final int ABORTED = 1 << 1;
  public static final int ERRORED = 1 << 2;
  public static final int COMPLETE = 1 << 3;
  
  Component target;
  MediaEntry head;

  static final long serialVersionUID = -483174189758638095L;

  // FIXME: The serialized form documentation says MediaEntry is a 
  // serializable field, but the serialized form of MediaEntry itself
  // doesn't appear to be documented.
  class MediaEntry implements ImageObserver
  {
    int id;
    Image image;
    MediaEntry next;
    int status;
    int width;
    int height;
    
    public boolean imageUpdate(Image img, int flags, int x, int y, 
			       int width, int height)
    {
      if ((flags & ABORT) != 0)
        status = ABORTED | COMPLETE;
      else if ((flags & ERROR) != 0)
        status = ERRORED | COMPLETE;
      else if ((flags & ALLBITS) != 0)
        status = COMPLETE;
      else
        status = LOADING;
      
      synchronized (MediaTracker.this)
      {
	MediaTracker.this.notifyAll();
      }
      // If status is not COMPLETE then we need more updates.
      return (status & COMPLETE) == 0;
    }
  }

  public MediaTracker(Component c)
  {
    target = c;
  }

  public void addImage(Image image, int id)
  {
    MediaEntry e = new MediaEntry();
    e.id = id;
    e.image = image;
    e.next = head;
    head = e;
    // Start tracking image status.
    target.checkImage(image, e);
  }

  public void addImage(Image image, int id, int width, int height)
  {
    MediaEntry e = new MediaEntry();
    e.id = id;
    e.image = image;
    e.next = head;
    e.width = width;
    e.height = height;
    head = e;
    // Start tracking image status.
    target.checkImage(image, width, height, e);
  }

  public boolean checkAll()
  {
    return checkAll(false);
  }

  public boolean checkAll(boolean load)
  {
    MediaEntry e = head;
    boolean result = true;
    
    while (e != null)
      {
	if ((e.status & COMPLETE) == 0)
	  {
	    if (load)
	      {
		result = false;
	        if (e.status == 0)
		  {
		    target.prepareImage(e.image, e);
		    e.status = LOADING;
		  }
	      }
	    else
	      return false;
	  }
	e = e.next;
      }
    return result;
  }

  public boolean isErrorAny()
  {
    MediaEntry e = head;    
    while (e != null)
      {
        if ((e.status & ERRORED) != 0)
	  return true;
        e = e.next;
      }
    return false;
  }

  public Object[] getErrorsAny()
  {
    MediaEntry e = head;
    ArrayList result = null;
    while (e != null)
      {
        if ((e.status & ERRORED) != 0)
	  {
	    if (result == null)
	      result = new ArrayList();
	    result.add(e.image);
	  }
        e = e.next;
      }
    if (result == null)
      return null;
    else
      return result.toArray();
  }

  public void waitForAll() throws InterruptedException
  {
    synchronized (this)
    {
      while (checkAll(true) == false)
        wait();
    }
  }

  public boolean waitForAll(long ms) throws InterruptedException
  {
    long start = System.currentTimeMillis();
    synchronized (this)
    {
      while (!checkAll(true))
        wait(ms);
    }
    if ((System.currentTimeMillis() - start) < ms)
      return true;
    else
      return false;
  }

  public int statusAll(boolean load)
  {
    int result = 0;
    MediaEntry e = head;
    while (e != null)
      {
        if (load && e.status == 0)
	  {
	    target.prepareImage(e.image, e);
	    e.status = LOADING;
	  }
        result |= e.status;
	e = e.next;
      }
    return result;
  }

  public boolean checkID(int id)
  {
    return checkID(id, false);
  }

  public boolean checkID(int id, boolean load)
  {
    MediaEntry e = head;
    boolean result = true;
    
    while (e != null)
      {
	if (e.id == id && ((e.status & COMPLETE) == 0))
	  {
	    if (load)
	      {
		result = false;
	        if (e.status == 0)
		  {
		    target.prepareImage(e.image, e);
		    e.status = LOADING;
		  }
	      }
	    else
	      return false;
	  }
	e = e.next;
      }
    return result;
  }

  public boolean isErrorID(int id)
  {
    MediaEntry e = head;    
    while (e != null)
      {
        if (e.id == id && ((e.status & ERRORED) != 0))
	  return true;
        e = e.next;
      }
    return false;
  }

  public Object[] getErrorsID(int id)
  {
    MediaEntry e = head;
    ArrayList result = null;
    while (e != null)
      {
        if (e.id == id && ((e.status & ERRORED) != 0))
	  {
	    if (result == null)
	      result = new ArrayList();
	    result.add(e.image);
	  }
        e = e.next;
      }
    if (result == null)
      return null;
    else
      return result.toArray();
  }

  public void waitForID(int id) throws InterruptedException
  {
    MediaEntry e = head;
    synchronized (this)
    {
      while (checkID (id, true) == false)
        wait();
    }
  }

  public boolean waitForID(int id, long ms) throws InterruptedException
  {
    MediaEntry e = head;
    long start = System.currentTimeMillis();
    synchronized (this)
    {
      while (checkID (id, true) == false)
        wait(ms);
    }  
    if ((System.currentTimeMillis() - start) < ms)
      return true;
    else
      return false;
  }

  public int statusID(int id, boolean load)
  {
    int result = 0;
    MediaEntry e = head;
    while (e != null)
      {
        if (e.id == id)
	  {
            if (load && e.status == 0)
	      {
		target.prepareImage(e.image, e);
		e.status = LOADING;
	      }
            result |= e.status;
	  }
	e = e.next;
      }
    return result;
  }

  public void removeImage(Image image)
  {
    MediaEntry e = head;
    MediaEntry prev = null;
    while (e != null)
      {
        if (e.image == image)
	  {
	    if (prev == null)
	      head = e.next;
	    else
	      prev.next = e.next;
	  }
	prev = e;
	e = e.next;
      }
  }

  public void removeImage(Image image, int id)
  {
    MediaEntry e = head;
    MediaEntry prev = null;
    while (e != null)
      {
        if (e.id == id && e.image == image)
	  {
	    if (prev == null)
	      head = e.next;
	    else
	      prev.next = e.next;
	  }
	else
	  prev = e;
	e = e.next;
      }  
  }

  public void removeImage(Image image, int id, int width, int height)
  {
    MediaEntry e = head;
    MediaEntry prev = null;
    while (e != null)
      {
        if (e.id == id && e.image == image
	    && e.width == width && e.height == height)
	  {
	    if (prev == null)
	      head = e.next;
	    else
	      prev.next = e.next;
	  }
	else
	  prev = e;
	e = e.next;
      }
  }
}
