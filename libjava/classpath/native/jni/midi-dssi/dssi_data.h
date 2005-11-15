/* dssi_data.h - DSSI data
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

#include <stdlib.h>
#include <dlfcn.h>
#include <sys/time.h>
#include <jni.h>
#include <dssi.h>
#include <jack/jack.h>
#include <alsa/asoundlib.h>
#include <alsa/seq.h>

#include <stdio.h>

#include "target_native.h"
#include "target_native_misc.h"
#include "../classpath/jcl.h"

/* Specify the size of the circular buffer.  It only needs to be big
   enough to hold the events that happen between jack callbacks (~
   1/40th of a second).  */
#define EVENT_BUFFER_SIZE 1024

/* Helper macros for going between pointers and jlongs.  */
#define JLONG_TO_PTR(T,P) ((T *)(long)P)
#define PTR_TO_JLONG(P) ((jlong)(long)P)


/* Every DSSI Synthesizer has one of these associated with it.  The
   Java class sees it as a "long" handle.  */

typedef struct
{
  /* This is a handle to the dlopen'ed .so file containing the DSSI
     synthesizer.  */
  void *dlhandle;

  /* The function to call to get the DSS_Descriptor.  */
  DSSI_Descriptor_Function fn;

  /* The descriptor for this synthesizer.  See the dssi.h system
     header.  */
  const DSSI_Descriptor *desc;

  /* We currently open a jack client connection for every
     synthesizer.  */
  jack_client_t *jack_client;

  /* We currently only handle stereo jack connections.  Output from
     mono synthesizers is sent to both left and right ports.  */
  jack_port_t *jack_left_output_port;
  jack_port_t *jack_right_output_port;

  /* We use a circular buffer to hold MIDI events before processing
     them in the jack audio processing callback function.  */
  snd_seq_event_t midiEventBuffer[EVENT_BUFFER_SIZE];
  int midiEventReadIndex; 
  int midiEventWriteIndex;

  /* This is a handle the synthesizers underlying LADSPA structure.
     See the ladspa.h system header for details.  */
  LADSPA_Handle plugin_handle;

  /* These are buffers we pass to the DSSI Synthesizer for
     filling.  */
  float *left_buffer;
  float *right_buffer;

  /* The number of input controls for this synth.  */
  unsigned control_count;
  
  /* An array of control values, control_count in length.  */
  LADSPA_Data *control_values;

  /* A mapping of MIDI controllers to control values.  There are a
     maximum of 128 MIDI controllers.  */
  unsigned control_value_map[128];

  /* A mapping of MIDI controllers to LADSPA ports.  There are a
     maximum of 128 MIDI controllers.  */
  unsigned control_port_map[128];

  /* The sample rate.  */
  jack_nframes_t sample_rate;

} dssi_data;

