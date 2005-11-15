/* gnu_javax_sound_midi_dssi_DSSISynthesizer.c - DSSI Synth
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

/* The original get_port_default() and set_control() routines were
 * copied from the DSSI source distribution and are covered by the
 * following copyright and license...
 *
 * Copyright 2004 Chris Cannam, Steve Harris and Sean Bolton.
 * 
 * Permission to use, copy, modify, distribute, and sell this software
 * for any purpose is hereby granted without fee, provided that the
 * above copyright notice and this permission notice are included in
 * all copies or substantial portions of the software.
 */

#include <config.h>
#include <gnu_javax_sound_midi_dssi_DSSISynthesizer.h> 
#include <math.h>

#include "dssi_data.h"

/* Define this for debug output.  */
#undef DEBUG_DSSI_PROVIDER

static void set_control (dssi_data *data, snd_seq_event_t *event);


/**
 * The jack callback routine.
 *
 * This function is called by the jack audio system in its own thread
 * whenever it needs new audio data.
 *
 */
static int
process (jack_nframes_t nframes, void *arg)
{    
  dssi_data *data = (dssi_data *) arg;
  int index;
  jack_default_audio_sample_t *buffer;

  /* Look through the event buffer to see if any control values
     need changing.  */
  for ( index = data->midiEventReadIndex; 
	index != data->midiEventWriteIndex;
	index = (index + 1) % EVENT_BUFFER_SIZE)
    {
      if (data->midiEventBuffer[index].type == SND_SEQ_EVENT_CONTROLLER)
	set_control (data, & data->midiEventBuffer[index]);
    }

  if (data->desc->run_synth)
    {
      /* Call the synth audio processing routine.  */
      data->desc->run_synth
	(data->plugin_handle,
	 nframes,
	 &data->midiEventBuffer[data->midiEventReadIndex],
	 data->midiEventWriteIndex - data->midiEventReadIndex);
    }
  else 
    if (data->desc->run_multiple_synths)
      {
	snd_seq_event_t *events = 
	  &data->midiEventBuffer[data->midiEventReadIndex];
	unsigned long event_count = 
	  data->midiEventWriteIndex - data->midiEventReadIndex;

	/* Call the synth audio processing routine.  */
	data->desc->run_multiple_synths
	  (1,
	   & (data->plugin_handle),
	   nframes,
	   &events,
	   &event_count);
      }

  /* Update the read index on our circular buffer.  */
  data->midiEventReadIndex = data->midiEventWriteIndex;

  /* Copy output from the synth to jack.  

     FIXME: This is hack that only gets one channel from the synth and
     send that to both jack ports (until we handle stero synths
     properly).

     FIXME: Can we avoid this copying?  */
  buffer = jack_port_get_buffer(data->jack_left_output_port, nframes);
  memcpy (buffer, data->left_buffer, nframes * sizeof(LADSPA_Data));
  buffer = jack_port_get_buffer(data->jack_right_output_port, nframes);
  memcpy (buffer, data->left_buffer, nframes * sizeof(LADSPA_Data));

  return 0;   
}


/**
 * Calculate a reasonable default value for a specific control port.
 * This is mostly copied from the DSSI example code.  Copyright info
 * is found at the top of this file.
 *
 */
static LADSPA_Data 
get_port_default (const LADSPA_Descriptor *plugin, 
		  int port, jack_nframes_t sample_rate)
{
  LADSPA_PortRangeHint hint = plugin->PortRangeHints[port];
  float lower = hint.LowerBound *
    (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor) ? sample_rate : 1.0f);
  float upper = hint.UpperBound *
    (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor) ? sample_rate : 1.0f);
  
  if (!LADSPA_IS_HINT_HAS_DEFAULT(hint.HintDescriptor)) 
    {
      if (!LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor) ||
	  !LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor)) 
	{
	  /* No hint, its not bounded, wild guess */
	  return 0.0f;
	}
    
      if (lower <= 0.0f && upper >= 0.0f) 
	{
	  /* It spans 0.0, 0.0 is often a good guess */
	  return 0.0f;
	}
    
      /* No clues, return minimum */
      return lower;
    }
  
  /* Try all the easy ones */
  
  if (LADSPA_IS_HINT_DEFAULT_0(hint.HintDescriptor))
    return 0.0f;
  else if (LADSPA_IS_HINT_DEFAULT_1(hint.HintDescriptor)) 
    return 1.0f;
  else if (LADSPA_IS_HINT_DEFAULT_100(hint.HintDescriptor)) 
    return 100.0f;
  else if (LADSPA_IS_HINT_DEFAULT_440(hint.HintDescriptor)) 
    return 440.0f;
   
  /* All the others require some bounds */
  
  if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor)
      && (LADSPA_IS_HINT_DEFAULT_MINIMUM(hint.HintDescriptor)))
    return lower;

  if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor))
    {
      if (LADSPA_IS_HINT_DEFAULT_MAXIMUM(hint.HintDescriptor))
	return upper;

      if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor)) 
	{
	  if (LADSPA_IS_HINT_DEFAULT_LOW(hint.HintDescriptor)) 
	    return lower * 0.75f + upper * 0.25f;
	  else if (LADSPA_IS_HINT_DEFAULT_MIDDLE(hint.HintDescriptor)) 
	    return lower * 0.5f + upper * 0.5f;
	  else if (LADSPA_IS_HINT_DEFAULT_HIGH(hint.HintDescriptor)) 
	    return lower * 0.25f + upper * 0.75f;
	}
    }
  
  /* fallback */
  return 0.0f;
}

/**
 * Set a control value by mapping the MIDI event to a suitable value
 * for this control.
 * This is mostly copied from the DSSI example code.  Copyright info
 * is found at the top of this file.
 *
 */
static void
set_control(dssi_data *data, snd_seq_event_t *event)
{
  unsigned control = event->data.control.param;
  unsigned port = data->control_port_map[control];

  const LADSPA_Descriptor *p = data->desc->LADSPA_Plugin;

  LADSPA_PortRangeHintDescriptor d = p->PortRangeHints[port].HintDescriptor;

  LADSPA_Data lb = p->PortRangeHints[port].LowerBound *
    (LADSPA_IS_HINT_SAMPLE_RATE(p->PortRangeHints[port].HintDescriptor) ?
     data->sample_rate : 1.0f);
  
  LADSPA_Data ub = p->PortRangeHints[port].UpperBound *
    (LADSPA_IS_HINT_SAMPLE_RATE(p->PortRangeHints[port].HintDescriptor) ?
     data->sample_rate : 1.0f);
  
  float value = (float)event->data.control.value;
  
  if (!LADSPA_IS_HINT_BOUNDED_BELOW(d)) 
    {
      if (!LADSPA_IS_HINT_BOUNDED_ABOVE(d)) 
	{
	  /* unbounded: might as well leave the value alone. */
	} 
      else 
	{
	  /* bounded above only. just shift the range. */
	  value = ub - 127.0f + value;
	}
    } 
  else 
    {
      if (!LADSPA_IS_HINT_BOUNDED_ABOVE(d)) 
	{
	  /* bounded below only. just shift the range. */
	  value = lb + value;
	} 
      else 
	{
	  /* bounded both ends.  more interesting. */
	  if (LADSPA_IS_HINT_LOGARITHMIC(d)) 
	    {
	      const float llb = logf(lb);
	      const float lub = logf(ub);
	      
	      value = expf(llb + ((lub - llb) * value / 127.0f));
	    } 
	  else 
	    {
	      value = lb + ((ub - lb) * value / 127.0f);
	    }
	}
    }
  
#ifdef DEBUG_DSSI_PROVIDER
  printf("MIDI controller %d=%d -> control in %u=%f\n", 
	 event->data.control.param,
	 event->data.control.value, 
	 data->control_value_map[control], value);
#endif
  
  data->control_values[data->control_value_map[control]] = value;
}

/**
 * Open a new synthesizer.  This currently involves instantiating a
 * new synth, creating a new jack client connection, and activating
 * both.
 *
 */
JNIEXPORT void JNICALL
Java_gnu_javax_sound_midi_dssi_DSSISynthesizer_open_1 
  (JNIEnv *env, jclass clazz __attribute__((unused)), jlong handle)
{
  unsigned int port_count, j, cindex;
  const char **ports;
  int controller = 0;
  dssi_data *data = (dssi_data *) (long) handle;
  if ((data->jack_client = jack_client_new (data->desc->LADSPA_Plugin->Label)) == 0)
    {
      /*	JCL_ThrowException (env, "javax/sound/midi/MidiUnavailableException",   */
      JCL_ThrowException (env, "java/io/IOException", 
			  "can't create jack client");
      return;
    } 

  /* Get the jack sample rate, which may be used in default control port
     value calculations.  */
  data->sample_rate = jack_get_sample_rate (data->jack_client);
  
  data->plugin_handle = 
    (data->desc->LADSPA_Plugin->instantiate)(data->desc->LADSPA_Plugin, 
					     data->sample_rate);
  
  if (jack_set_process_callback (data->jack_client, process, data) != 0)
    {
      JCL_ThrowException (env, "java/io/IOException", 
			  "can't set jack process callback");
      return;
    }
  
  data->jack_left_output_port =
    jack_port_register (data->jack_client, "output_left",
                        JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
  data->jack_right_output_port =
    jack_port_register (data->jack_client, "output_right",
                        JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);

  /* Count the number of controls and audio ouput ports.  */
  port_count = data->control_count = 0;
  for (j = 0; j < data->desc->LADSPA_Plugin->PortCount; j++) 
    {
      LADSPA_PortDescriptor pod =
    	data->desc->LADSPA_Plugin->PortDescriptors[j];
      
      if (LADSPA_IS_PORT_AUDIO(pod) && LADSPA_IS_PORT_OUTPUT(pod))
	port_count++;
      else if (LADSPA_IS_PORT_CONTROL(pod) && LADSPA_IS_PORT_INPUT(pod))
	data->control_count++;
    }

  /* Allocate the array of control values.  */
  data->control_values = 
    (LADSPA_Data *) JCL_malloc (env, 
				data->control_count * sizeof (LADSPA_Data));

  /* Initialize the MIDI control map.  */
  memset (data->control_value_map, 0, data->control_count * sizeof(unsigned));
  
  /* Create buffers for each port.  */
  for (cindex = 0, j = 0; j < data->desc->LADSPA_Plugin->PortCount; j++) 
    {  
      LADSPA_PortDescriptor pod =
	data->desc->LADSPA_Plugin->PortDescriptors[j];
      if (LADSPA_IS_PORT_AUDIO(pod) && LADSPA_IS_PORT_OUTPUT(pod))
  	{
	  data->left_buffer = 
	    (float *) calloc(jack_get_buffer_size(data->jack_client), 
			     sizeof(float));
	  (data->desc->LADSPA_Plugin->connect_port)(data->plugin_handle, j, 
						    data->left_buffer);
  	}
      else 
	if (LADSPA_IS_PORT_CONTROL(pod) && LADSPA_IS_PORT_INPUT(pod))
	  {
	    /* This is an input control port.  Connect it to a properly
	       initialized value in our controller value array.  */
	    (data->desc->LADSPA_Plugin->connect_port)
	      (data->plugin_handle, j, &(data->control_values[cindex]));
	    data->control_values[cindex] = 
	      get_port_default (data->desc->LADSPA_Plugin,
				j, data->sample_rate);

	    /* Set up the mapping between MIDI controllers and this
	       contoller value.  */
	    if (data->desc->get_midi_controller_for_port)
	      {
		controller = data->desc->
		  get_midi_controller_for_port(data->plugin_handle, j);

		if ((controller != DSSI_NONE) && DSSI_IS_CC(controller))
		  {
		    data->control_value_map[DSSI_CC_NUMBER(controller)] = cindex;
		    data->control_port_map[DSSI_CC_NUMBER(controller)] = j;

#ifdef DEBUG_DSSI_PROVIDER
		    printf ("MIDI Controller 0x%x [%s] = %g\n", 
			    DSSI_CC_NUMBER(controller),
			    data->desc->LADSPA_Plugin->PortNames[j],
			    data->control_values[cindex]);
#endif
		  }
	      }

	    cindex++;
	  }
    }
  
  (data->desc->LADSPA_Plugin->activate)(data->plugin_handle);

  if (jack_activate (data->jack_client))
    JCL_ThrowException (env, "java/io/IOException", 
			"can't activate jack client"); 

  /* Try to connect the synth output to hardware audio ports.  */
  ports = jack_get_ports (data->jack_client, NULL, NULL, 
			  JackPortIsPhysical | JackPortIsInput);
  if (ports)
    {
      if (ports[0] && ports[1]) 
	{
	  /* Don't bother checking return values.  Failing is OK.  */
	  jack_connect (data->jack_client, 
			jack_port_name (data->jack_left_output_port),
			ports[0]);
	  jack_connect (data->jack_client, 
			jack_port_name (data->jack_right_output_port),
			ports[1]);
	}
      free(ports);
    }
}

/**
 * This is called when we receive a new MIDI CONTROL CHANGE message.
 * Simply stick an appropriate event in the event buffer.  This will
 * get processed in the jack callback function.
 */
JNIEXPORT void JNICALL 
Java_gnu_javax_sound_midi_dssi_DSSISynthesizer_controlChange_1 
  (JNIEnv *env __attribute__((unused)), jclass clazz __attribute__((unused)), 
   jlong handle, jint channel, jint control, jint value)
{
  dssi_data *data = JLONG_TO_PTR(dssi_data,handle);

  /* Insert this event in the event buffer.  */
  snd_seq_event_t *ev = & data->midiEventBuffer[data->midiEventWriteIndex];

  /* Set the event value.  */
  snd_seq_ev_set_controller (ev, channel, control, value);

  data->midiEventWriteIndex = 
    (data->midiEventWriteIndex + 1) % EVENT_BUFFER_SIZE;
}

/**
 * This is called when we receive a new MIDI NOTE ON message.  Simply
 * stick an appropriate event in the event buffer.  This will get
 * processed in the jack callback function.
 */
JNIEXPORT void JNICALL 
Java_gnu_javax_sound_midi_dssi_DSSISynthesizer_noteOn_1 
  (JNIEnv *env __attribute__((unused)), jclass clazz __attribute__((unused)), 
   jlong handle, jint channel, jint note, jint velocity)
{
  dssi_data *data = JLONG_TO_PTR(dssi_data,handle);

  /* Insert this event in the event buffer.  */
  snd_seq_event_t *ev = & data->midiEventBuffer[data->midiEventWriteIndex];

  ev->type = SND_SEQ_EVENT_NOTEON;
  ev->data.control.channel = channel;
  ev->data.note.note = note;
  ev->data.note.velocity = velocity;

  data->midiEventWriteIndex = 
    (data->midiEventWriteIndex + 1) % EVENT_BUFFER_SIZE;
}

/**
 * This is called when we receive a new MIDI NOTE OFF message.  Simply
 * stick an appropriate event in the event buffer.  This will get
 * processed in the jack callback function.
 */
JNIEXPORT void JNICALL 
Java_gnu_javax_sound_midi_dssi_DSSISynthesizer_noteOff_1 
  (JNIEnv *env __attribute__((unused)), 
   jclass clazz __attribute__((unused)), 
   jlong handle, jint channel, jint note, jint velocity)
{
  dssi_data *data = JLONG_TO_PTR(dssi_data,handle);

  /* Insert this event in the event buffer.  */
  snd_seq_event_t *ev = & data->midiEventBuffer[data->midiEventWriteIndex];

  ev->type = SND_SEQ_EVENT_NOTEOFF;
  ev->data.control.channel = channel;
  ev->data.note.note = note;
  ev->data.note.velocity = velocity;

  data->midiEventWriteIndex = 
    (data->midiEventWriteIndex + 1) % EVENT_BUFFER_SIZE;
}

JNIEXPORT void JNICALL 
Java_gnu_javax_sound_midi_dssi_DSSISynthesizer_setPolyPressure_1 
  (JNIEnv *env __attribute__((unused)), jclass clazz __attribute__((unused)), 
   jlong handle __attribute__((unused)), jint channel __attribute__((unused)), 
   jint note __attribute__((unused)), jint velocity __attribute__((unused)))
{
}

JNIEXPORT jint JNICALL 
Java_gnu_javax_sound_midi_dssi_DSSISynthesizer_getPolyPressure_1 
  (JNIEnv *env __attribute__((unused)), jclass clazz __attribute__((unused)), 
   jlong handle __attribute__((unused)), jint channel __attribute__((unused)), 
   jint note __attribute__((unused)))
{
  return 0;
}

JNIEXPORT void JNICALL 
Java_gnu_javax_sound_midi_dssi_DSSISynthesizer_close_1 
  (JNIEnv *env __attribute__((unused)), jclass clazz __attribute__((unused)), 
   jlong handle __attribute__((unused)))
{
}

/* FIXME: These next three functions are really inefficient because
   we're instantiating and cleaning up plugin instances just to query
   values.  */

JNIEXPORT jstring JNICALL 
Java_gnu_javax_sound_midi_dssi_DSSISynthesizer_getProgramName_1 
  (JNIEnv *env, jclass clazz __attribute__((unused)), 
   jlong handle, jint index)
{
  LADSPA_Handle lhandle;
  jstring name = (jstring) NULL;
  dssi_data *data = JLONG_TO_PTR(dssi_data, handle);
  if (data->desc->get_program == NULL)
    return NULL;
  lhandle =
    (data->desc->LADSPA_Plugin->instantiate)(data->desc->LADSPA_Plugin, 
					     48000);
  const DSSI_Program_Descriptor *program = 
    (data->desc->get_program)(lhandle, (unsigned long) index);
  if (program)
    name = (*env)->NewStringUTF (env, program->Name);
  (data->desc->LADSPA_Plugin->cleanup)(lhandle);

  return name;
}

JNIEXPORT jint JNICALL
Java_gnu_javax_sound_midi_dssi_DSSISynthesizer_getProgramBank_1 
  (JNIEnv *env __attribute__((unused)), jclass clazz __attribute__((unused)),
   jlong handle, jint index)
{
  LADSPA_Handle lhandle;
  jint result = -1;
  dssi_data *data = JLONG_TO_PTR(dssi_data, handle);
  lhandle =
    (data->desc->LADSPA_Plugin->instantiate)(data->desc->LADSPA_Plugin, 
					     48000);
  const DSSI_Program_Descriptor *program = 
    (data->desc->get_program)(lhandle, (unsigned long) index);
  if (program)
    result = (jint) program->Bank;
  (data->desc->LADSPA_Plugin->cleanup)(lhandle);
  return result;
}

JNIEXPORT jint JNICALL 
Java_gnu_javax_sound_midi_dssi_DSSISynthesizer_getProgramProgram_1 
  (JNIEnv *env __attribute__((unused)), jclass clazz __attribute__((unused)), 
   jlong handle, jint index)
{
  LADSPA_Handle lhandle;
  jint result = -1;
  dssi_data *data = JLONG_TO_PTR(dssi_data, handle);
  lhandle =
    (data->desc->LADSPA_Plugin->instantiate)(data->desc->LADSPA_Plugin, 
					     48000);
  const DSSI_Program_Descriptor *program = 
    (data->desc->get_program)(lhandle, (unsigned long) index);
  if (program)
    result = (jint) program->Program;
  (data->desc->LADSPA_Plugin->cleanup)(lhandle);
  return result;
}


JNIEXPORT void JNICALL 
Java_gnu_javax_sound_midi_dssi_DSSISynthesizer_selectProgram_1 
  (JNIEnv *env __attribute__((unused)), jclass clazz __attribute__((unused)), 
   jlong handle, jint bank, jint program)
{
  dssi_data *data = JLONG_TO_PTR(dssi_data, handle);

  (data->desc->select_program)(data->plugin_handle, 
			       (unsigned) bank, (unsigned) program);
}
