/* AudioClip.java -- Play an audio clip.
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


package java.applet;

/**
  * This interface provides a simple mechanism for playing audio clips.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public interface AudioClip
{
  /**
    * Plays the audio clip starting from the beginning.
    */
  void play();

  /**
    * Stops playing this audio clip.  There is no mechanism for restarting
    * at the point where the clip is stopped.
    */
  void stop();

  /**
    * Plays this audio clip in a continuous loop.
    */
  void loop();

} // interface AudioClip
