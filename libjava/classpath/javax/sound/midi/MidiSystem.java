/* MidiSystem.java -- Access system MIDI resources
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


package javax.sound.midi;

import gnu.classpath.ServiceFactory;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;

import javax.sound.midi.spi.MidiDeviceProvider;
import javax.sound.midi.spi.MidiFileReader;
import javax.sound.midi.spi.MidiFileWriter;
import javax.sound.midi.spi.SoundbankReader;

/**
 * MidiSystem provides access to the computer system's MIDI resources,
 * as well as utility routines for reading MIDI files and more.
 *
 * @author Anthony Green (green@redhat.com)
 * @since 1.3
 *
 */
public class MidiSystem
{
  private MidiSystem()
  {
    // Not instantiable.
  }

  /**
   * Get an array of all available MIDI devices.
   *
   * @return a possibly empty array of all available MIDI devices
   */
  public static MidiDevice.Info[] getMidiDeviceInfo()
  {
    Iterator deviceProviders =
        ServiceFactory.lookupProviders(MidiDeviceProvider.class);
    List infoList = new ArrayList();

    while (deviceProviders.hasNext())
    {
      MidiDeviceProvider provider = (MidiDeviceProvider) deviceProviders.next();
      MidiDevice.Info[] infos = provider.getDeviceInfo();
      for (int i = infos.length; i > 0; )
        infoList.add(infos[--i]);
    }

    return (MidiDevice.Info[])
        infoList.toArray(new MidiDevice.Info[infoList.size()]);
  }

  /**
   * Get the specified MIDI device.
   *
   * @param info a description of the device we're looking for
   * @return the requested MIDI device
   * @throws MidiUnavailableException if no MIDI devices are configured or found
   * @throws IllegalArgumentException if the device described by info is not found
   */
  public static MidiDevice getMidiDevice(MidiDevice.Info info)
    throws MidiUnavailableException
  {
    Iterator deviceProviders =
        ServiceFactory.lookupProviders(MidiDeviceProvider.class);

    if (! deviceProviders.hasNext())
      throw new MidiUnavailableException("No MIDI device providers available.");

    do
    {
      MidiDeviceProvider provider =
        (MidiDeviceProvider) deviceProviders.next();
      if (provider.isDeviceSupported(info))
        return provider.getDevice(info);
    } while (deviceProviders.hasNext());

    throw new IllegalArgumentException("MIDI device "
                                       + info + " not available.");
  }

  /**
   * Get the default Receiver instance.  This just picks the first one
   * it finds for now.
   *
   * @return the default Receiver instance
   * @throws MidiUnavailableException if no Receiver is found
   */
  public static Receiver getReceiver() throws MidiUnavailableException
  {
    // TODO: The 1.5 spec has a fancy mechanism to specify the default
    // receiver device.  For now, well just return the first one we find.
    MidiDevice.Info[] infos = getMidiDeviceInfo();
    for (int i = 0; i < infos.length; i++)
    {
      MidiDevice device = getMidiDevice(infos[i]);
      if (device instanceof Receiver)
        return (Receiver) device;
    }
    throw new MidiUnavailableException("No Receiver device available");
  }

  /**
   * Get the default Transmitter instance.  This just picks the first one
   * it finds for now.
   *
   * @return the default Transmitter instance
   * @throws MidiUnavailableException if no Transmitter is found
   */
  public static Transmitter getTransmitter() throws MidiUnavailableException
  {
    // TODO: The 1.5 spec has a fancy mechanism to specify the default
    // Transmitter device.  For now, well just return the first one we find.
    MidiDevice.Info[] infos = getMidiDeviceInfo();
    for (int i = 0; i < infos.length; i++)
    {
      MidiDevice device = getMidiDevice(infos[i]);
      if (device instanceof Transmitter)
        return (Transmitter) device;
    }
    throw new MidiUnavailableException("No Transmitter device available");
  }

  /**
   * Get the default Synthesizer instance.  This just picks the first one
   * it finds for now.
   *
   * @return the default Synthesizer instance
   * @throws MidiUnavailableException if no Synthesizer is found
   */
  public static Synthesizer getSynthesizer() throws MidiUnavailableException
  {
    // TODO: The 1.5 spec has a fancy mechanism to specify the default
    // Synthesizer device.  For now, well just return the first one we find.
    MidiDevice.Info[] infos = getMidiDeviceInfo();
    for (int i = 0; i < infos.length; i++)
    {
      MidiDevice device = getMidiDevice(infos[i]);
      if (device instanceof Synthesizer)
        return (Synthesizer) device;
    }
    throw new MidiUnavailableException("No Synthesizer device available");
  }

  /**
   * Get the default Sequencer instance.  This just picks the first one
   * it finds for now.
   *
   * @return the default Sequencer instance
   * @throws MidiUnavailableException if no Sequencer is found
   */
  public static Sequencer getSequencer() throws MidiUnavailableException
  {
    // TODO: The 1.5 spec has a fancy mechanism to specify the default
    // Sequencer device.  For now, well just return the first one we find.
    MidiDevice.Info[] infos = getMidiDeviceInfo();
    for (int i = 0; i < infos.length; i++)
    {
      MidiDevice device = getMidiDevice(infos[i]);
      if (device instanceof Sequencer)
        return (Sequencer) device;
    }
    throw new MidiUnavailableException("No Sequencer device available");
  }

  /**
   * Read a Soundbank object from the given stream.
   *
   * @param stream the stream from which to read the Soundbank
   * @return the Soundbank object
   * @throws InvalidMidiDataException if we were unable to read the soundbank
   * @throws IOException if an I/O error happened while reading
   */
  public static Soundbank getSoundbank(InputStream stream)
    throws InvalidMidiDataException, IOException
  {
    Iterator readers = ServiceFactory.lookupProviders(SoundbankReader.class);
    while (readers.hasNext())
    {
      SoundbankReader sr = (SoundbankReader) readers.next();
      Soundbank sb = sr.getSoundbank(stream);
      if (sb != null)
        return sb;
    }
    throw new InvalidMidiDataException("Cannot read soundbank from stream");
  }

  /**
   * Read a Soundbank object from the given url.
   *
   * @param url the url from which to read the Soundbank
   * @return the Soundbank object
   * @throws InvalidMidiDataException if we were unable to read the soundbank
   * @throws IOException if an I/O error happened while reading
   */
  public static Soundbank getSoundbank(URL url)
    throws InvalidMidiDataException, IOException
  {
    Iterator readers = ServiceFactory.lookupProviders(SoundbankReader.class);
    while (readers.hasNext())
    {
      SoundbankReader sr = (SoundbankReader) readers.next();
      Soundbank sb = sr.getSoundbank(url);
      if (sb != null)
        return sb;
    }
    throw new InvalidMidiDataException("Cannot read from url " + url);
  }

  /**
   * Read a Soundbank object from the given file.
   *
   * @param file the file from which to read the Soundbank
   * @return the Soundbank object
   * @throws InvalidMidiDataException if we were unable to read the soundbank
   * @throws IOException if an I/O error happened while reading
   */
  public static Soundbank getSoundbank(File file)
    throws InvalidMidiDataException, IOException
  {
    Iterator readers = ServiceFactory.lookupProviders(SoundbankReader.class);
    while (readers.hasNext())
    {
      SoundbankReader sr = (SoundbankReader) readers.next();
      Soundbank sb = sr.getSoundbank(file);
      if (sb != null)
        return sb;
    }
    throw new InvalidMidiDataException("Cannot read soundbank from file "
                                       + file);
  }

  /**
   * Read a MidiFileFormat object from the given stream.
   *
   * @param stream the stream from which to read the MidiFileFormat
   * @return the MidiFileFormat object
   * @throws InvalidMidiDataException if we were unable to read the MidiFileFormat
   * @throws IOException if an I/O error happened while reading
   */
  public static MidiFileFormat getMidiFileFormat(InputStream stream)
    throws InvalidMidiDataException, IOException
  {
    Iterator readers = ServiceFactory.lookupProviders(MidiFileReader.class);
    while (readers.hasNext())
    {
      MidiFileReader sr = (MidiFileReader) readers.next();
      MidiFileFormat sb = sr.getMidiFileFormat(stream);
      if (sb != null)
        return sb;
    }
    throw new InvalidMidiDataException("Can't read MidiFileFormat from stream");
  }

  /**
   * Read a MidiFileFormat object from the given url.
   *
   * @param url the url from which to read the MidiFileFormat
   * @return the MidiFileFormat object
   * @throws InvalidMidiDataException if we were unable to read the MidiFileFormat
   * @throws IOException if an I/O error happened while reading
   */
  public static MidiFileFormat getMidiFileFormat(URL url)
    throws InvalidMidiDataException, IOException
  {
    Iterator readers = ServiceFactory.lookupProviders(MidiFileReader.class);
    while (readers.hasNext())
    {
      MidiFileReader sr = (MidiFileReader) readers.next();
      MidiFileFormat sb = sr.getMidiFileFormat(url);
      if (sb != null)
        return sb;
    }
    throw new InvalidMidiDataException("Cannot read from url " + url);
  }

  /**
   * Read a MidiFileFormat object from the given file.
   *
   * @param file the file from which to read the MidiFileFormat
   * @return the MidiFileFormat object
   * @throws InvalidMidiDataException if we were unable to read the MidiFileFormat
   * @throws IOException if an I/O error happened while reading
   */
  public static MidiFileFormat getMidiFileFormat(File file)
    throws InvalidMidiDataException, IOException
  {
    Iterator readers = ServiceFactory.lookupProviders(MidiFileReader.class);
    while (readers.hasNext())
    {
      MidiFileReader sr = (MidiFileReader) readers.next();
      MidiFileFormat sb = sr.getMidiFileFormat(file);
      if (sb != null)
        return sb;
    }
    throw new InvalidMidiDataException("Can't read MidiFileFormat from file "
                                       + file);
  }

  /**
   * Read a Sequence object from the given stream.
   *
   * @param stream the stream from which to read the Sequence
   * @return the Sequence object
   * @throws InvalidMidiDataException if we were unable to read the Sequence
   * @throws IOException if an I/O error happened while reading
   */
  public static Sequence getSequence(InputStream stream)
    throws InvalidMidiDataException, IOException
  {
    Iterator readers = ServiceFactory.lookupProviders(MidiFileReader.class);
    while (readers.hasNext())
    {
      MidiFileReader sr = (MidiFileReader) readers.next();
      Sequence sq = sr.getSequence(stream);
      if (sq != null)
        return sq;
    }
    throw new InvalidMidiDataException("Can't read Sequence from stream");
  }

  /**
   * Read a Sequence object from the given url.
   *
   * @param url the url from which to read the Sequence
   * @return the Sequence object
   * @throws InvalidMidiDataException if we were unable to read the Sequence
   * @throws IOException if an I/O error happened while reading
   */
  public static Sequence getSequence(URL url)
    throws InvalidMidiDataException, IOException
  {
    Iterator readers = ServiceFactory.lookupProviders(MidiFileReader.class);
    while (readers.hasNext())
    {
      MidiFileReader sr = (MidiFileReader) readers.next();
      Sequence sq = sr.getSequence(url);
      if (sq != null)
        return sq;
    }
    throw new InvalidMidiDataException("Cannot read from url " + url);
  }

  /**
   * Read a Sequence object from the given file.
   *
   * @param file the file from which to read the Sequence
   * @return the Sequence object
   * @throws InvalidMidiDataException if we were unable to read the Sequence
   * @throws IOException if an I/O error happened while reading
   */
  public static Sequence getSequence(File file)
    throws InvalidMidiDataException, IOException
  {
    Iterator readers = ServiceFactory.lookupProviders(MidiFileReader.class);
    while (readers.hasNext())
    {
      MidiFileReader sr = (MidiFileReader) readers.next();
      Sequence sq = sr.getSequence(file);
      if (sq != null)
        return sq;
    }
    throw new InvalidMidiDataException("Can't read Sequence from file "
                                       + file);
  }

  /**
   * Return an array of supported MIDI file types on this system.
   *
   * @return the array of supported MIDI file types
   */
  public static int[] getMidiFileTypes()
  {
    // We only support a max of 3 MIDI file types.
    boolean supported[] = new boolean[3];
    // The number of supported formats.
    int count = 0;
    Iterator writers = ServiceFactory.lookupProviders(MidiFileWriter.class);
    while (writers.hasNext())
    {
      MidiFileWriter fw = (MidiFileWriter) writers.next();
      int types[] = fw.getMidiFileTypes();
      for (int i = types.length; i > 0;)
      {
        int type = types[--i];
        if (supported[type] == false)
        {
          count++;
          supported[type] = true;
        }
      }
    }
    int result[] = new int[count];
    for (int i = supported.length; i > 0;)
    {
      if (supported[--i])
        result[--count] = i;
    }
    return result;
  }

  /**
   * Return true if the system supports writing files of type fileType.
   *
   * @param fileType the MIDI file type we want to write
   * @return true if we can write fileType files, false otherwise
   */
  public static boolean isFileTypeSupported(int fileType)
  {
    Iterator writers = ServiceFactory.lookupProviders(MidiFileWriter.class);
    while (writers.hasNext())
    {
      MidiFileWriter fw = (MidiFileWriter) writers.next();

      if (fw.isFileTypeSupported(fileType))
        return true;
    }
    return false;
  }

  /**
   * Return an array of supported MIDI file types on this system
   * for the given sequnce.
   *
   * @param sequence the sequnce to write
   * @return the array of supported MIDI file types
   */
  public static int[] getMidiFileTypes(Sequence sequence)
  {
    // We only support a max of 3 MIDI file types.
    boolean supported[] = new boolean[3];
    // The number of supported formats.
    int count = 0;
    Iterator writers = ServiceFactory.lookupProviders(MidiFileWriter.class);
    while (writers.hasNext())
    {
      MidiFileWriter fw = (MidiFileWriter) writers.next();
      int types[] = fw.getMidiFileTypes(sequence);
      for (int i = types.length; i > 0;)
      {
        int type = types[--i];
        if (supported[type] == false)
        {
          count++;
          supported[type] = true;
        }
      }
    }
    int result[] = new int[count];
    for (int i = supported.length; i > 0;)
    {
      if (supported[--i])
        result[--count] = i;
    }
    return result;
  }

  /**
   * Return true if the system supports writing files of type fileType
   * for the given sequence.
   *
   * @param fileType the MIDI file type we want to write
   * @param sequence the Sequence we want to write
   * @return true if we can write fileType files for sequence, false otherwise
   */
  public static boolean isFileTypeSupported(int fileType, Sequence sequence)
  {
    Iterator writers = ServiceFactory.lookupProviders(MidiFileWriter.class);
    while (writers.hasNext())
    {
      MidiFileWriter fw = (MidiFileWriter) writers.next();

      if (fw.isFileTypeSupported(fileType, sequence))
        return true;
    }
    return false;
  }

  /**
   * Write a sequence to an output stream using a specific MIDI file format.
   *
   * @param in the sequence to write
   * @param fileType the MIDI file format to use
   * @param out the output stream to write to
   * @return the number of bytes written
   * @throws IOException if an I/O exception happens
   * @throws IllegalArgumentException if fileType is not supported for in
   */
  public static int write(Sequence in, int fileType, OutputStream out)
    throws IOException
  {
    Iterator writers = ServiceFactory.lookupProviders(MidiFileWriter.class);
    while (writers.hasNext())
    {
      MidiFileWriter fw = (MidiFileWriter) writers.next();

      if (fw.isFileTypeSupported(fileType, in))
        return fw.write(in, fileType, out);
    }
    throw new IllegalArgumentException("File type "
                                       + fileType + " is not supported");
  }

  /**
   * Write a sequence to a file using a specific MIDI file format.
   *
   * @param in the sequence to write
   * @param fileType the MIDI file format to use
   * @param out the file to write to
   * @return the number of bytes written
   * @throws IOException if an I/O exception happens
   * @throws IllegalArgumentException if fileType is not supported for in
   */
  public static int write(Sequence in, int fileType, File out)
    throws IOException
  {
    Iterator writers = ServiceFactory.lookupProviders(MidiFileWriter.class);
    while (writers.hasNext())
    {
      MidiFileWriter fw = (MidiFileWriter) writers.next();

      if (fw.isFileTypeSupported(fileType, in))
        return fw.write(in, fileType, out);
    }
    throw new IllegalArgumentException("File type "
                                       + fileType + " is not supported");
  }
}
