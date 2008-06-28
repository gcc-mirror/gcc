/* GstPipeline.java -- Represents a Gstreamer Pipeline.
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

import java.io.FileOutputStream;
import java.io.IOException;
import java.util.prefs.Preferences;

import javax.sound.sampled.LineUnavailableException;

import gnu.classpath.Pointer;

/**
 * This class represent a GStreamer pipeline and is resposible to handle the
 * flow of data to and from the GStreamer native backend.
 * 
 * @author Mario Torre <neugens@limasoftware.net>
 */
public class GstPipeline
{
  /*
   * Implementation note:
   * This class is at first a bit confusing as it serves as a gateway
   * to a real filesystem named pipe.
   * The pipelines is shared by the gstreamer backend and by the java code.
   * If the operation we are performing is to play a given stream of bytes,
   * we need to open the java side of the pipeline for writing, which is done
   * in the prepareWrite method. At the same time, the native side of the code
   * need to open the pipeline in read mode, to get access to the data,
   * and hence, act as a source element. This is why you will see terms
   * like "read" or "source" in methods that are used to write in the pipeline,
   * in other words, each the native operation is the opposite of the java
   * side operation.
   * Opening the pipe to record audio data from the sound card works the same
   * except that all the operation are inverted.
   */
  
  // These enums are used in the native code also, changes here must reflect
  // changes in the native code.
  public static enum State
  {
    PLAY, PAUSE, STOP, CLOSE
  }
  
  private static final int READ = 0;
  private static final int WRITE = 1;
  private static final int QUEUED = 1;
  
  private static final String CAPACITY_KEY = "Capacity"; 
  
  private static final Object [] lock = new Object[0];
  
  /*
   * Preference subsystem. We use this to store some system specific settings.
   */
  protected Preferences prefs =
    Preferences.userNodeForPackage(GstPipeline.class).node("GStreamer");
  
  // used by the native code, stores the size of the named pipeline
  // created by the operating system.
  private long capacity = -1;
  
  /** Represents the playing state of this Line. */
  private State state = State.STOP;
  
  /** The name of the named pipe. */
  // Will be setup and filled in the native code. See the native library
  // for details.
  private String name = null;
  
  /** This is the named pipe that will be read by the gstreamer backend. */
  private FileOutputStream output = null;
  
  /**
   * Defines if we are getting data from a sink pipe
   * or writing to a source pipe.
   */
  private boolean source = true;
  
  /** Indicate that we are ready to process audio data to/from the pipe. */
  private boolean ready = false;
  
  /**
   * This is the native GStreamer Pipeline.
   */
  // This field is used by the native code, so any change to it must be
  // followed by similar changes in the native peer.
  private Pointer pipeline = null;

  /**
   * Creates a new GstPipeline with a capacity of
   * {@link GstDataLine#DEFAULT_BUFFER_SIZE}.
   * 
   * @see GstDataLine#DEFAULT_BUFFER_SIZE
   */
  public GstPipeline()
  {
    this(GstDataLine.DEFAULT_BUFFER_SIZE);
  }
  
  /**
   * Creates a new GstPipeline with a capacity of bufferSize.
   * @see GstDataLine#DEFAULT_BUFFER_SIZE
   */
  public GstPipeline(int bufferSize)
  { 
    // see if we need to detect the size of the named pipe or we can use
    // an already computet default for this system.
    // Note that this is very different from the bufferSize parameter,
    // see below.
    capacity = prefs.getLong(CAPACITY_KEY, -1);
    if (capacity == -1)
      {
        synchronized (lock)
          {
            capacity = detect_pipe_size();
          }
        
        prefs.putLong(CAPACITY_KEY, capacity);
      }
    
    // FIXME: bufferSize actually not used nor needed by the backend.
    // Applications that expects a buffer of different size will be a
    // bit disappointed by that..
    init_instance();
    
    // need to remove the named pipe in case of abnormal termination
    Runtime.getRuntime().addShutdownHook(new CleanPipeline());
  }
  
  /**
   * Creates a source pipeline. A source pipeline is a pipe you send data for
   * processing using the write method.
   */
  public void createForWrite() throws LineUnavailableException
  { 
    // create the named pipe
    if (!create_named_pipe(this.pipeline))
      throw new LineUnavailableException("Unable to create filesystem pipe");
   
    open_native_pipe(this.pipeline, READ);
    prepareWrite();
    
    this.source = true;
  }
  
  /**
   * @return the state
   */
  public State getState()
  {
    return this.state;
  }

  /**
   * Closes this pipeline.
   * Short hand for #setState(State.STOP).
   */
  public void close()
  {
    setState(State.STOP);
  }
  
  /**
   * @param state the state to set
   */
  public void setState(final State state)
  {
    int _state = -1;
    switch (state)
      {
        case PLAY:
          _state = 0;
          break;

        case PAUSE:
          _state = 1;
          break;
        
        case STOP: case CLOSE:
          _state = 2;
          closePipe();
          break;
      }

    if (set_state(pipeline, _state))
      GstPipeline.this.state = state;
  }
   
  /**
   * Return a reference to the GstPipeline native class as a Pointer object.
   * This method is intended as an helper accessor and the returned pointer
   * needs to be casted and used in the native code only. 
   *  
   * @return Pointer to the native GstPipeline class.
   */
  public Pointer getNativeClass()
  {
    return this.pipeline;
  }
  
  /**
   * Write length bytes from the given buffer into this pipeline,
   * starting at offset.
   * This method block if the pipeline can't accept more data. 
   * 
   * @param buffer
   * @param offset
   * @param length
   * @return
   */
  public int write(byte[] buffer, int offset, int length)
  { 
    if (this.state == State.STOP)
      return -1;
    else if (this.state == State.PAUSE)
      return 0;
    else if (!ready)
      return -1;
    
    try
      {
        if (output != null)
          {
            output.write(buffer, offset, length);
            return length;
          }
        return 0;
      }
    catch (Exception e)
      {
        /* nothing to do */
      }
    
    return -1;
  }
  
  public int read(byte[] buffer, int offset, int length)
  {
    return 0;
  }
  
  public int available()
  {
    if (this.source)
      return available(this.pipeline, READ);
    else
      return available(this.pipeline, WRITE);
  }
  
  /**
   * Wait for remaining data to be enqueued in the pipeline.
   */
  public void drain()
  {
    if (this.state == State.STOP)
      return;
    
    try
      {
        // wait untill there is anymore data in the pipe
        while (available(this.pipeline, QUEUED) > 0)
          Thread.sleep(3000);
        
        // plus a bit to allow data to be processed
        Thread.sleep(1000);
      }
    catch (InterruptedException e)
      {
        /* nothing to do*/
      }
  }
  
  /**
   * Flush all the data currently waiting to be processed.
   */
  public void flush()
  {
    try
      {
        if (source)
          this.output.flush();
      }
    catch (IOException e)
      {
        /* nothing */
      }
  }
  
  private void closePipe()
  {
    try
      {
        GstPipeline.this.flush();
        if (source)
          GstPipeline.this.output.close();
      }
    catch (IOException e)
      {
        /* nothing to do */
      }
  }
  
  private void prepareWrite()
  {
    try
      {
        // if this is not completed for some reason, we will catch
        // in the write method. As this call can block, we assume we will
        // succeed and that the dataline can get data.
        GstPipeline.this.ready = true;
        GstPipeline.this.output = new FileOutputStream(name);
      }
    catch (Exception e)
      {
        GstPipeline.this.ready = false;
      }
  }
  
  /* ***** native ***** */
  
  /**
   * Initialize the native peer and enables the object cache.
   * It is meant to be used by the static initializer.
   */
  native private static final void init_id_cache();
  
  /**
   * Set the playing state of this pipeline.
   */
  native private static final boolean set_state(Pointer pipeline, int state);
  
  /**
   * Get the number of bytes currently available for reading or writing
   * from the pipeline.
   */
  native private static final int available(Pointer pipeline, int mode);
  
  /**
   * Open the native pipeline with the given mode.
   */
  native private static final void open_native_pipe(Pointer jpipeline,
                                                    int mode);
  
  /**
   * Close the native pipeline.
   */
  native private static final void close_native_pipe(Pointer jpipeline);
  
  /**
   * Initialize the native peer and enables the object cache.
   * It is meant to be used by the class constructor.
   */
  native private final void init_instance();
  
  /**
   * Crates the named pipe used to pass data between the application code
   * and gstreamer.
   */
  native private final boolean create_named_pipe(Pointer jpipeline);
  
  /**
   * Detect and return the size of the filesystem named pipe.
   */
  native private final long detect_pipe_size();
  
  private class CleanPipeline extends Thread
  {
    public void run()
    {
      GstPipeline.close_native_pipe(GstPipeline.this.pipeline);
    }
  }
  
  static
  {
    System.loadLibrary("gstreamerpeer"); //$NON-NLS-1$
    init_id_cache();
  }
}
