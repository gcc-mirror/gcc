/* DSSIMidiDeviceProvider.java -- DSSI Device Provider
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.javax.sound.midi.dssi;

import java.util.ArrayList;
import java.util.List;
import java.io.File;
import java.io.FilenameFilter;

import gnu.classpath.Configuration;
import gnu.javax.sound.midi.alsa.AlsaMidiSequencerDevice;

import javax.sound.midi.MidiDevice;
import javax.sound.midi.MidiDevice.Info;
import javax.sound.midi.spi.MidiDeviceProvider;

/**
 * A DSSI MIDI device provider.
 * 
 * DSSI (pronounced "dizzy") is an API for audio plugins, with particular 
 * application for software synthesis plugins with native user interfaces.
 * 
 * Read about DSSI at http://dssi.sourceforge.net
 * 
 * @author Anthony Green (green@redhat.com)
 *
 */
public class DSSIMidiDeviceProvider extends MidiDeviceProvider
{
  /**
   * The MidiDevice.Info specialized for DSSI synthesizers.
   * 
   * @author Anthony Green (green@redhat.com)
   *
   */
  private static class DSSIInfo extends Info
  {
    String soname;
    long index;
    
    public DSSIInfo(String name, String vendor, String description,
                    String version, String soname, long index)
    {
      super(name, vendor, description, version);
      this.soname = soname;
      this.index = index;
    }
  }

  static native long dlopen_(String soname);
  static native void dlclose_(long sohandle);
  static native long getDSSIHandle_(long sohandle, long index);
  static native String getDSSIName_(long handle);
  static native String getDSSICopyright_(long handle);
  static native String getDSSIVendor_(long handle);
  static native String getDSSILabel_(long handle);
  
  private static List examineLibrary(String soname)
  {
     List list = new ArrayList();
     long index = 0;
     long handle;
     
     long sohandle = dlopen_(soname);
     if (sohandle == 0)
       return list;
     do
     {
       handle = getDSSIHandle_(sohandle, index);
       if (handle == 0)
         break;
       String name = getDSSILabel_(handle);
       String copyright = getDSSICopyright_(handle);
       String label = getDSSIName_(handle);
       String vendor = getDSSIVendor_(handle);
       list.add(new DSSIInfo(name, vendor, label, 
                             "DSSI-1", soname, index));
       index++;
     } while (true);
     
     // Close the library and free memory
     dlclose_(sohandle);
     
     return list;
  }
  
  private static DSSIInfo[] infos;
  
  static
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      System.loadLibrary("gjsmdssi");

    File dssidir = new File("/usr/lib/dssi/");
    String sofiles[] = dssidir.list(new FilenameFilter()
                                    {
                                      public boolean accept(File dir, String n)
                                      {
                                        return n.endsWith(".so");
                                      }   
                                    });
    List ilist = new ArrayList();
    for (int i = 0; i < sofiles.length; i++)
      ilist.addAll(examineLibrary(new File(dssidir, sofiles[i]).getAbsolutePath()));
    infos = (DSSIInfo[]) ilist.toArray(new DSSIInfo[ilist.size()]);
  }
  
  public DSSIMidiDeviceProvider()
  {
    // Empty.
  }
  
  /* Return the Info array.
   * @see javax.sound.midi.spi.MidiDeviceProvider#getDeviceInfo()
   */
  public Info[] getDeviceInfo()
  {
    return infos;
  }

  /* Get a MIDI Device for info.
   * @see javax.sound.midi.spi.MidiDeviceProvider#getDevice(javax.sound.midi.MidiDevice.Info)
   */
  public MidiDevice getDevice(Info info)
  {
    for (int i = 0; i < infos.length; i++)
    {
      if (info.equals(infos[i]))
      {
          return new DSSISynthesizer(infos[i],
                                     infos[i].soname, 
                                     infos[i].index);
      }
    }
    throw new IllegalArgumentException("Don't recognize MIDI device " + info);
  }
}
