/* AudioPlayerSample.java -- Simple Java Audio Player
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
package gnu.classpath.examples.sound;

import java.io.File;
import java.io.IOException;
import java.util.Map;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.SourceDataLine;
import javax.sound.sampled.UnsupportedAudioFileException;

/**
 * A simple demo to show the use of the Java Sound API.
 * It plays the given file (up to the end, so don't pass the 26 minutes long
 * Pink Floyd's Echoes unless you really want!!).
 *
 * See: http://jsresources.org/examples/SimpleAudioPlayer.java.html
 *
 * @author Mario Torre <neugens@limasoftware.net>
 */
public class AudioPlayerSample
{
  private static final int EXTERNAL_BUFFER_SIZE = 128000;

  /**
   * @param args
   */
  public static void main(String[] args)
  {
    if (args.length < 1)
      {
        System.out.println("Radio Classpath -: Usage: " +
                           "AudioPlayerSample [file]");
        return;
      }

    String file = args[0];

    System.out.println("Welcome to Radio Classpath, only great music for you!");
    System.out.println("Today's DJ Tap The WaterDroplet");

    // now create the AudioInputStream
    AudioInputStream audioInputStream = null;
    try
      {
        audioInputStream = AudioSystem.getAudioInputStream(new File(file));
      }
    catch (UnsupportedAudioFileException e)
      {
        // This happen when the subsystem is unable to parse the kind of
        // audio file we are submitting
        // See the README for supported audio file types under Classpath
        // for the version you are using.
        e.printStackTrace();
        return;
      }
    catch (IOException e)
      {
        e.printStackTrace();
        return;
      }

    // get informations about the kind of file we are about to play
    AudioFormat audioFormat = audioInputStream.getFormat();

    System.out.println("Playing file: " + file);
    System.out.println("format: " + audioFormat.toString());

    System.out.print("Additional properties: ");

    // now, we try to get all the properties we have in this AudioFormat
    // and display them
    Map<String, Object> properties = audioFormat.properties();
    if (properties.size() < 0)
      {
        System.out.println("none");
      }
    else
      {
        System.out.println("found #" + properties.size() + " properties");
        for (String key : properties.keySet())
          {
            System.out.println(key + ": " + properties.get(key));
          }
      }

    // let's setup things for playing
    // first, we require a Line. As we are doing playing, we will ask for a
    // SourceDataLine
    SourceDataLine line = null;

    // To get the source line, we first need to build an Info object
    // this is done in one line:
    DataLine.Info info = new DataLine.Info(SourceDataLine.class, audioFormat);

    System.out.println("searching line...");

    // usually, if a backend can parse a file type, it can also
    // create a line to handle it, but that's not guaranteed
    // so we need to take care and to handle a possible
    // LineUnavailableException
    try
      {
        line = (SourceDataLine) AudioSystem.getLine(info);

        System.out.println("line found, opening...");

        // once created, a line must be opened to let data flow
        // though it.
        line.open(audioFormat);
      }
    catch (LineUnavailableException e)
      {
        // in a real application you should signal that in a kindly way to
        // your users
        e.printStackTrace();
        return;
      }
    catch (Exception e)
      {
        e.printStackTrace();
        return;
      }

    // an open line pass data to the backend only when it is in
    // a state called "started" ("playing" or "play" in some other
    // framework)
    System.out.print("starting line... ");

    line.start();
    System.out.println("done");

    // now we can start reading data from the AudioStream and writing
    // data to the pipeline. The Java Sound API is rather low level
    // so let you pass up to one byte of data at a time
    // (with some constraints, refer to the API documentation to know more)
    // We will do some buffering. You may want to check the frame size
    // to allow a better buffering, also.

    System.out.println("now playing...");

    int nBytesRead = 0;
    byte[] abData = new byte[EXTERNAL_BUFFER_SIZE];
    while (nBytesRead != - 1)
      {
        try
          {
            nBytesRead = audioInputStream.read(abData, 0, abData.length);
          }
        catch (IOException e)
          {
            e.printStackTrace();
          }

        if (nBytesRead >= 0)
          {
            // this method returns the number of bytes actuall written
            // to the line. You may want to use this number to check
            // for events, display the current position (give also a
            // look to the API for other ways of doing that) etc..
            line.write(abData, 0, nBytesRead);
          }
      }

    System.out.print("stream finished, draining line... ");

    // call this method to ensure that all the data in the internal buffer
    // reach the audio backend, otherwise your application will
    // cut the last frames of audio data (and users will not enjoy the last
    // seconds of their precious music)
    line.drain();

    // Once done, we can close the line. Note that a line, once closed
    // may not be reopened (depends on the backend, in some cases a "reopen",
    // if allowed, really opens a new line, reallocating all the resources)

    System.out.println("line drained, now exiting");
    line.close();

    System.out.println("We hope you enjoyed Radio Classpath!");
  }

}
