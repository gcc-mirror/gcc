/* MediaTracker.java -- Class used for keeping track of images
   Copyright (C) 1999, 2002, 2004, 2005  Free Software Foundation, Inc.

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


package java.awt;

import java.awt.image.ImageObserver;
import java.util.ArrayList;

/**
  * This class is used for keeping track of the status of various media
  * objects.
  *
  * Media objects are tracked by assigning them an ID. It is possible
  * to assign the same ID to mutliple objects, effectivly grouping them
  * together. In this case the status flags ({@link #statusID}) and error flag
  * (@link #isErrorID} and {@link #getErrorsID}) are ORed together. This
  * means that you cannot say exactly which media object has which status,
  * at most you can say that there <em>are</em> certain media objects with
  * some certain status.
  * 
  * At the moment only images are supported by this class.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Bryce McKinlay
  */
public class MediaTracker implements java.io.Serializable
{
  /** Indicates that the media is still loading. */
  public static final int LOADING = 1 << 0;

  /** Indicates that the loading operation has been aborted. */
  public static final int ABORTED = 1 << 1;

  /** Indicates that an error has occured during loading of the media. */
  public static final int ERRORED = 1 << 2;

  /** Indicates that the media has been successfully and completely loaded. */
  public static final int COMPLETE = 1 << 3;

  /** The component on which the media is eventually been drawn. */
  Component target;

  /** The head of the linked list of tracked media objects. */
  MediaEntry head;

  /** Our serialVersionUID for serialization. */
  static final long serialVersionUID = -483174189758638095L;

  /**
   * This represents a media object that is tracked by a MediaTracker.
   * It also implements a simple linked list.
   */
  // FIXME: The serialized form documentation says MediaEntry is a 
  // serializable field, but the serialized form of MediaEntry itself
  // doesn't appear to be documented.
  class MediaEntry implements ImageObserver
  {
    /** The ID of the media object. */
    int id;

    /** The media object. (only images are supported ATM). */
    Image image;

    /** The link to the next entry in the list. */
    MediaEntry next;

    /** The tracking status. */
    int status;

    /** The width of the image. */
    int width;

    /** The height of the image. */
    int height;
    
    /**
     * Receives notification from an {@link java.awt.image.ImageProducer}
     * that more data of the image is available.
     *
     * @param img the image that is updated
     * @param flags flags from the ImageProducer that indicate the status
     *        of the loading process
     * @param x the X coordinate of the upper left corner of the image
     * @param y the Y coordinate of the upper left corner of the image
     * @param width the width of the image
     * @param height the height of the image
     *
     * @return <code>true</code> if more data is needed, <code>false</code>
     *         otherwise
     *
     * @see java.awt.image.ImageObserver
     */
    public boolean imageUpdate(Image img, int flags, int x, int y, 
                               int width, int height)
    {
      if ((flags & ABORT) != 0)
        status = ABORTED;
      else if ((flags & ERROR) != 0)
        status = ERRORED;
      else if ((flags & ALLBITS) != 0)
        status = COMPLETE;
      else
        status = 0;

      synchronized (MediaTracker.this)
        {
          MediaTracker.this.notifyAll();
        }

      // If status is not COMPLETE then we need more updates.
      return ((status & (COMPLETE | ERRORED | ABORTED)) == 0);
    }
  }

  /**
   * Constructs a new MediaTracker for the component <code>c</code>. The
   * component should be the component that uses the media (i.e. draws it).
   *
   * @param c the Component that wants to use the media
   */
  public MediaTracker(Component c)
  {
    target = c;
  }

  /**
   * Adds an image to the tracker with the specified <code>ID</code>.
   *
   * @param image the image to be added
   * @param id the ID of the tracker list to which the image is added
   */
  public void addImage(Image image, int id)
  {
    MediaEntry e = new MediaEntry();
    e.id = id;
    e.image = image;
    synchronized(this)
      {
        e.next = head;
        head = e;
      }
  }

  /**
   * Adds an image to the tracker with the specified <code>ID</code>.
   * The image is expected to be rendered with the specified width and
   * height.
   *
   * @param image the image to be added
   * @param id the ID of the tracker list to which the image is added
   * @param width the width of the image
   * @param height the height of the image
   */
  public void addImage(Image image, int id, int width, int height)
  {
    MediaEntry e = new MediaEntry();
    e.id = id;
    e.image = image;
    e.width = width;
    e.height = height;
    synchronized(this)
      {
        e.next = head;
        head = e;
      }
  }

  /**
   * Checks if all media objects have finished loading, i.e. are
   * {@link #COMPLETE}, {@link #ABORTED} or {@link #ERRORED}.
   *
   * If the media objects are not already loading, a call to this
   * method does <em>not</em> start loading. This is equivalent to
   * a call to <code>checkAll(false)</code>.
   *
   * @return if all media objects have finished loading either by beeing
   *         complete, have been aborted or errored.
   */
  public boolean checkAll()
  {
    return checkAll(false);
  }

  /**
   * Checks if all media objects have finished loading, i.e. are
   * {@link #COMPLETE}, {@link #ABORTED} or {@link #ERRORED}.
   *
   * If the media objects are not already loading, and <code>load</code>
   * is <code>true</code> then a call to this
   * method starts loading the media objects.
   *
   * @param load if <code>true</code> this method starts loading objects
   *        that are not already loading
   *
   * @return if all media objects have finished loading either by beeing
   *         complete, have been aborted or errored.
   */
  public boolean checkAll(boolean load)
  {
    MediaEntry e = head;
    boolean result = true;
    
    while (e != null)
      {
        if ((e.status & (COMPLETE | ERRORED | ABORTED)) == 0)
          {
            if (load && ((e.status & LOADING) == 0))
              {
		if (target.prepareImage(e.image, e))
		  e.status = COMPLETE;
		else
		  {
		    e.status = LOADING;
		    int flags = target.checkImage(e.image, e);
		    if ((flags & ImageObserver.ABORT) != 0)
		      e.status = ABORTED;
		    else if ((flags & ImageObserver.ERROR) != 0)
		      e.status = ERRORED;
		    else if ((flags & ImageObserver.ALLBITS) != 0)
		      e.status = COMPLETE;
		  }
		boolean complete = (e.status
				    & (COMPLETE | ABORTED | ERRORED)) != 0;
		if (!complete)
		  result = false;
	      }
            else
              result = false;
          }
        e = e.next;
      }
    return result;
  }

  /**
   * Checks if any of the registered media objects has encountered an error
   * during loading.
   *
   * @return <code>true</code> if at least one media object has encountered
   *         an error during loading, <code>false</code> otherwise
   *
   */
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

  /**
   * Returns all media objects that have encountered errors during loading.
   *
   * @return an array of all media objects that have encountered errors
   *         or <code>null</code> if there were no errors at all
   */
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

  /**
   * Waits for all media objects to finish loading, either by completing
   * successfully or by aborting or encountering an error.
   *
   * @throws InterruptedException if another thread interrupted the
   *         current thread while waiting
   */
  public void waitForAll() throws InterruptedException
  {
    synchronized (this)
    {
      while (checkAll(true) == false)
        wait();
    }
  }

  /**
   * Waits for all media objects to finish loading, either by completing
   * successfully or by aborting or encountering an error.
   *
   * This method waits at most <code>ms</code> milliseconds. If the
   * media objects have not completed loading within this timeframe, this
   * method returns <code>false</code>, otherwise <code>true</code>.
   *
   * @param ms timeframe in milliseconds to wait for the media objects to
   *        finish
   *
   * @return <code>true</code> if all media objects have successfully loaded
   *         within the timeframe, <code>false</code> otherwise
   *
   * @throws InterruptedException if another thread interrupted the
   *         current thread while waiting
   */
  public boolean waitForAll(long ms) throws InterruptedException
  {
    long start = System.currentTimeMillis();
    boolean result = checkAll(true);
    synchronized (this)
    {
      while (result == false)
        {
          wait(ms);
          result = checkAll(true);
          if ((System.currentTimeMillis() - start) > ms)
            break;
        }
    }

    return result;
  }

  /**
   * Returns the status flags of all registered media objects ORed together.
   * If <code>load</code> is <code>true</code> then media objects that
   * are not already loading will be started to load.
   *
   * @param load if set to <code>true</code> then media objects that are
   *        not already loading are started
   *
   * @return the status flags of all tracked media objects ORed together
   */
  public int statusAll(boolean load)
  {
    int result = 0;
    MediaEntry e = head;
    while (e != null)
      {
        if (load && e.status == 0)
          {
            if (target.prepareImage(e.image, e))
              e.status = COMPLETE;
            else
	      {
                e.status = LOADING;
                int flags = target.checkImage(e.image, e);
		if ((flags & ImageObserver.ABORT) != 0)
		  e.status = ABORTED;
		else if ((flags & ImageObserver.ERROR) != 0)
		  e.status = ERRORED;
		else if ((flags & ImageObserver.ALLBITS) != 0)
		  e.status = COMPLETE;
	      }
          }
        result |= e.status;
        e = e.next;
      }
    return result;
  }

  /**
   * Checks if the media objects with <code>ID</code> have completed loading.
   *
   * @param id the ID of the media objects to check
   *
   * @return <code>true</code> if all media objects with <code>ID</code>
   *         have successfully finished
   */
  public boolean checkID(int id)
  {
    return checkID(id, false);
  }

  /**
   * Checks if the media objects with <code>ID</code> have completed loading.
   * If <code>load</code> is <code>true</code> then media objects that
   * are not already loading will be started to load.
   *
   * @param id the ID of the media objects to check
   * @param load if set to <code>true</code> then media objects that are
   *        not already loading are started
   *
   * @return <code>true</code> if all media objects with <code>ID</code>
   *         have successfully finished
   */
  public boolean checkID(int id, boolean load)
  {
    MediaEntry e = head;
    boolean result = true;
    
    while (e != null)
      {
        if (e.id == id && ((e.status & (COMPLETE | ABORTED | ERRORED)) == 0))
          {
            if (load && ((e.status & LOADING) == 0))
              {
                e.status = LOADING;
		if (target.prepareImage(e.image, e))
		  e.status = COMPLETE;
		else
		  {
		    int flags = target.checkImage(e.image, e);
		    if ((flags & ImageObserver.ABORT) != 0)
		      e.status = ABORTED;
		    else if ((flags & ImageObserver.ERROR) != 0)
		      e.status = ERRORED;
		    else if ((flags & ImageObserver.ALLBITS) != 0)
		      e.status = COMPLETE;
		  }
		boolean complete = (e.status
				    & (COMPLETE | ABORTED | ERRORED)) != 0;
		if (!complete)
		  result = false;
              }
            else
              result = false;
          }
        e = e.next;
      }
    return result;
  }

  /**
   * Returns <code>true</code> if any of the media objects with <code>ID</code>
   * have encountered errors during loading, false otherwise.
   *
   * @param id the ID of the media objects to check
   *
   * @return <code>true</code> if any of the media objects with <code>ID</code>
   *         have encountered errors during loading, false otherwise
   */
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

  /**
   * Returns all media objects with the specified ID that have encountered
   * an error.
   *
   * @param id the ID of the media objects to check
   *
   * @return an array of all media objects  with the specified ID that
   *         have encountered an error
   */
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

  /**
   * Waits for all media objects with the specified ID to finish loading,
   * either by completing successfully or by aborting or encountering an error.
   *
   * @param id the ID of the media objects to wait for
   *
   * @throws InterruptedException if another thread interrupted the
   *         current thread while waiting
   */
  public void waitForID(int id) throws InterruptedException
  {
    MediaEntry e = head;
    synchronized (this)
    {
      while (checkID (id, true) == false)
        wait();
    }
  }

  /**
   * Waits for all media objects with the specified ID to finish loading,
   * either by completing successfully or by aborting or encountering an error.
   *
   * This method waits at most <code>ms</code> milliseconds. If the
   * media objects have not completed loading within this timeframe, this
   * method returns <code>false</code>, otherwise <code>true</code>.
   *
   * @param id the ID of the media objects to wait for
   * @param ms timeframe in milliseconds to wait for the media objects to
   *        finish
   *
   * @return <code>true</code> if all media objects have successfully loaded
   *         within the timeframe, <code>false</code> otherwise
   *
   * @throws InterruptedException if another thread interrupted the
   *         current thread while waiting
   */
  public boolean waitForID(int id, long ms) throws InterruptedException
  {
    MediaEntry e = head;
    long start = System.currentTimeMillis();
    boolean result = checkID(id, true);

    synchronized (this)
    {
      while (result == false)
        {
          wait(ms);
          result = checkID(id, true);
          if ((System.currentTimeMillis() - start) > ms)
            break;
        }
    }

    return result;
  }

  /**
   * Returns the status flags of the media objects with the specified ID
   * ORed together.
   *
   * If <code>load</code> is <code>true</code> then media objects that
   * are not already loading will be started to load.
   *
   * @param load if set to <code>true</code> then media objects that are
   *        not already loading are started
   *
   * @return the status flags of all tracked media objects ORed together
   */
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
		if (target.prepareImage(e.image, e))
                  e.status = COMPLETE;
		else
		  {
		    e.status = LOADING;
		    int flags = target.checkImage(e.image, e);
		    if ((flags & ImageObserver.ABORT) != 0)
		      e.status = ABORTED;
		    else if ((flags & ImageObserver.ERROR) != 0)
		      e.status = ERRORED;
		    else if ((flags & ImageObserver.ALLBITS) != 0)
		      e.status = COMPLETE;
		  }
              }
            result |= e.status;
          }
        e = e.next;
      }
    return result;
  }

  /**
   * Removes an image from this MediaTracker.
   *
   * @param image the image to be removed
   */
  public void removeImage(Image image)
  {
    synchronized (this)
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
  }

  /**
   * Removes an image with the specified ID from this MediaTracker.
   *
   * @param image the image to be removed
   */
  public void removeImage(Image image, int id)
  {
    synchronized (this)
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
  }

  /**
   * Removes an image with the specified ID and scale from this MediaTracker.
   *
   * @param image the image to be removed
   */
  public void removeImage(Image image, int id, int width, int height)
  {
    synchronized (this)
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
}
