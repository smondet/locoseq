/******************************************************************************/
/*      Copyright (c) 2007, Sebastien MONDET                                  */
/*                                                                            */
/*      Permission is hereby granted, free of charge, to any person           */
/*      obtaining a copy of this software and associated documentation        */
/*      files (the "Software"), to deal in the Software without               */
/*      restriction, including without limitation the rights to use,          */
/*      copy, modify, merge, publish, distribute, sublicense, and/or sell     */
/*      copies of the Software, and to permit persons to whom the             */
/*      Software is furnished to do so, subject to the following              */
/*      conditions:                                                           */
/*                                                                            */
/*      The above copyright notice and this permission notice shall be        */
/*      included in all copies or substantial portions of the Software.       */
/*                                                                            */
/*      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       */
/*      EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       */
/*      OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              */
/*      NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT           */
/*      HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,          */
/*      WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING          */
/*      FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR         */
/*      OTHER DEALINGS IN THE SOFTWARE.                                       */
/******************************************************************************/


/*
 * The 'C' Jack midi interface
 *
 * Author: S. MONDET (http://sebmdt.googlepages.com)
 *
 */

/* 
 * Classic includes:
 */
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <signal.h>

/*
 * Jack's includes:
 */
#include <jack/jack.h>
#include <jack/midiport.h>
#include <jack/ringbuffer.h>


/*
 * Caml wrapping includes:
 *
 */
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/custom.h>


typedef struct {
  jack_client_t *js_client;

  size_t js_oports_nb;
  jack_port_t **js_oports;
  jack_ringbuffer_t *js_o_rbuf;
  
  size_t js_iports_nb;
  jack_port_t **js_iports;
  jack_ringbuffer_t *js_i_rbuf;

} jack_seq_t;

typedef struct {
  int  me_port;
  char me_stat;
  char me_chan;
  char me_dat1;
  char me_dat2;
} jack_seq_midi_event_t;


/**** <REAL-TIME> ****/
void
rt_assert(int condition, char *msg) {
  if (condition == 0)  {
    fprintf(stderr, "[C][RT][ERR] Assert Failed: %s\n", msg) ;
    perror("[C][RT][ERR] Unix Error: ");
    /* TODO: find something TODO there !! */
  }
}

/*
 * The process schedulled by JACK
 * (real-time function: no mallocs & Co, see jack/jack.h)
 */
int
process_callback(jack_nframes_t nframes, void * context) {
  jack_seq_t *js;

  js = context;

  { /* Preprocess the output buffers: */
    size_t i;
    void *o_port_buf;
    for (i = 0; i < js->js_oports_nb; i++) {
#ifdef JACK_MIDI_NEEDS_NFRAMES      
      o_port_buf = jack_port_get_buffer(js->js_oports[i], nframes);
      jack_midi_clear_buffer(o_port_buf, nframes);
#else
      o_port_buf = jack_port_get_buffer(js->js_oports[i]);
      jack_midi_clear_buffer(o_port_buf);
#endif
    }
  }

  { /* Do the output */
    jack_seq_midi_event_t stack_event;
    jack_midi_data_t *midi_buf = NULL;
    void *o_port_buf;
    size_t to_read, i;
    int ret;

    to_read = jack_ringbuffer_read_space(js->js_o_rbuf);
    for (i = 0; i < to_read; i += sizeof(jack_seq_midi_event_t)) {
      ret = jack_ringbuffer_read(js->js_o_rbuf,
          (void*)&stack_event, sizeof(jack_seq_midi_event_t));
      rt_assert(ret == sizeof(jack_seq_midi_event_t),
          "Can't read in ring buffer !!");
      o_port_buf =
        jack_port_get_buffer(js->js_oports[stack_event.me_port], nframes);
#ifdef JACK_MIDI_NEEDS_NFRAMES      
      midi_buf = jack_midi_event_reserve(o_port_buf, 0, 3, nframes);
#else
      midi_buf = jack_midi_event_reserve(o_port_buf, 0, 3);
#endif
      midi_buf[0] = (stack_event.me_stat & 0xF0) + (stack_event.me_chan & 0x0F);
      midi_buf[1] = stack_event.me_dat1;
      midi_buf[2] = stack_event.me_dat2;
    }
  }

  { /* Get the input */
    jack_seq_midi_event_t stack_event;
    jack_nframes_t n_in;
    jack_midi_event_t event;
    void *i_port_buf;
    unsigned int i, j;
    size_t writable;
    int ret;

    for (i = 0; i < js->js_iports_nb; i++) {
      /* Get In Event count: */
      i_port_buf = jack_port_get_buffer(js->js_iports[i], nframes);
#ifdef JACK_MIDI_NEEDS_NFRAMES      
      n_in = jack_midi_get_event_count(i_port_buf, nframes);
#else
      n_in = jack_midi_get_event_count(i_port_buf);
#endif
      for (j = 0; j < n_in; j++) {
#ifdef JACK_MIDI_NEEDS_NFRAMES      
        jack_midi_event_get(&event, i_port_buf, j, nframes);
#else
        jack_midi_event_get(&event, i_port_buf, j);
#endif
        if (event.size >= 2) {
          stack_event.me_port = i;
          stack_event.me_stat = event.buffer[0] & 0xF0;
          stack_event.me_chan = event.buffer[0] & 0x0F;
          stack_event.me_dat1 = event.buffer[1];
          if (event.size == 2) {
            stack_event.me_dat2 = 0;
          } else {
            stack_event.me_dat2 = event.buffer[2];
          }
          writable = jack_ringbuffer_write_space(js->js_i_rbuf);
          if (writable >= sizeof(jack_seq_midi_event_t)) {
            ret = jack_ringbuffer_write(js->js_i_rbuf,
                (char *)&stack_event, sizeof(jack_seq_midi_event_t));
            rt_assert(ret == sizeof(jack_seq_midi_event_t),
                "Can't write in ring buffer !!");
          } /* else { higher level does not consume => we stop writing } */
        } else {
          char msg[34];
          sprintf(msg, "Got an event of size: %d", event.size);
          rt_assert(0, msg);
        }
      }
    }
  }

  return 0;
}

/**** </REAL-TIME> ****/

/*
 * An assert function that trows a Failure exception.
 *
 */
void
js_exn_assert(int condition, char *msg) {
  if (condition == 0)  {
    fprintf(stderr, "[C-jack][ERR] Assert Failed: %s\n", msg) ;
    perror("[C-jack][ERR] Unix Error: ");
    caml_failwith("GENERAL ASSERT FAILED in jackseq.c") ;
  }
}


/*
 *  Destroy the Sequencer (by ocaml's GC)
 */
void 
custom_ml_jackseq_destroy(value jseq){

  /* must not use CAMLparam1 in destructor */
  jack_seq_t *seq = NULL ;

  /* Get the sequencer: */
  seq = *((jack_seq_t **)Data_custom_val(jseq)) ;

  jack_ringbuffer_free(seq->js_o_rbuf);
  jack_ringbuffer_free(seq->js_i_rbuf);
  free(seq->js_oports);
  free(seq->js_iports);
  free(seq);
  /* printf("JACK SEQUENCER DETROYED\n"); */
}

static struct custom_operations sequencer_custom_ops = {
  "locoseq.jack_sequencer",
  /* custom_finalize_default,*/ custom_ml_jackseq_destroy, 
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};


/*
 * Constructor of the sequencer "object"
 *
 */
CAMLprim value
ml_jackseq_make(value app_name, value in_names_array, value ou_names_array) {

  CAMLparam3(app_name, in_names_array, ou_names_array) ;
  jack_seq_t *js = NULL ;
  unsigned int i;
  int ret;
  value the_sequencer;

  /* Allocating the structure */
  js = malloc (sizeof(jack_seq_t)) ;
  js_exn_assert(js != NULL, "jack_seq_t mallocation");
  memset(js , 0 , sizeof(jack_seq_t));

  /* try to become a client of the JACK server */
  js->js_client = jack_client_new ( String_val(app_name));
  js_exn_assert(js->js_client != NULL,
      "Couldn't create a client, is jackd running ?");

  /* Creating the input ports from string array in_names_array: */
  js->js_iports_nb = Wosize_val(in_names_array) ;
  js->js_iports = malloc(sizeof(jack_port_t *) * js->js_iports_nb) ;
  for ( i = 0 ; i < js->js_iports_nb ; i++ ) {
    js->js_iports[i] =  jack_port_register(js->js_client, 
        String_val(Field(in_names_array, i)),
        JACK_DEFAULT_MIDI_TYPE, JackPortIsInput, 0);
    js_exn_assert(js->js_iports != NULL, "Couldn't register input MIDI port");
  }

  /* Creating the input ports from string array ou_names_array: */
  js->js_oports_nb = Wosize_val(ou_names_array) ;
  js->js_oports = malloc(sizeof(jack_port_t *) * js->js_oports_nb) ;
  for ( i = 0 ; i < js->js_oports_nb ; i++ ) {
    js->js_oports[i] =  jack_port_register(js->js_client, 
        String_val(Field(ou_names_array, i)),
        JACK_DEFAULT_MIDI_TYPE, JackPortIsOutput, 0);
    js_exn_assert(js->js_iports != NULL, "Couldn't register output MIDI port");
  }

  /* The two ring buffers */
  js->js_i_rbuf = jack_ringbuffer_create (2048); /* */
  js_exn_assert(js->js_i_rbuf != NULL, "Error creating the ring buffer");
  memset(js->js_i_rbuf->buf, 0, js->js_i_rbuf->size);
  js->js_o_rbuf = jack_ringbuffer_create (2048); /* */
  js_exn_assert(js->js_o_rbuf != NULL, "Error creating the ring buffer");
  memset(js->js_o_rbuf->buf, 0, js->js_o_rbuf->size);

  ret = jack_set_process_callback(js->js_client, process_callback, js);
  js_exn_assert(ret == 0, "Couldn't set the callback procedure !");

  ret = jack_activate(js->js_client);
  js_exn_assert(ret == 0, "Couldn't activate the client !");
  

  /* Return an abstract value: */
  the_sequencer = alloc_custom(
      &sequencer_custom_ops, sizeof(jack_seq_t *), 0, 1);
  *((jack_seq_t **)Data_custom_val(the_sequencer)) = js;
  CAMLreturn (the_sequencer) ;
}


/* 
 * Close the sequencer
 */
CAMLprim value
ml_jackseq_close(value ml_seq){
  CAMLparam1 (ml_seq);
  jack_seq_t *js = NULL ;
  int ret;
  js = *((jack_seq_t **)Data_custom_val(ml_seq)) ;
  ret = jack_client_close(js->js_client);
  CAMLreturn(Val_int(0));
}

/*
 * Output event directly
 * (puts the event in jack_seq_t's output ringbuffer)
 */
CAMLprim value
ml_jackseq_output_event(value ml_seq, value ml_port, value ml_stat,
    value ml_chan, value ml_dat1, value ml_dat2) {
  CAMLparam5(ml_seq, ml_port, ml_stat, ml_chan, ml_dat1);
  CAMLxparam1(ml_dat2);

  jack_seq_t *js = NULL ;
  int ret;
  jack_seq_midi_event_t stack_event;
  
  stack_event.me_port = Int_val(ml_port);
  stack_event.me_stat = Int_val(ml_stat);
  stack_event.me_chan = Int_val(ml_chan);
  stack_event.me_dat1 = Int_val(ml_dat1);
  stack_event.me_dat2 = Int_val(ml_dat2);
  js = *((jack_seq_t **)Data_custom_val(ml_seq)) ;

  ret = jack_ringbuffer_write(js->js_o_rbuf,
      (char *)&stack_event, sizeof(jack_seq_midi_event_t));
  js_exn_assert(ret == sizeof(jack_seq_midi_event_t),
      "Couldn't write in the ring buffer");

  CAMLreturn(Val_int(0));
}
CAMLprim value
ml_jackseq_output_event_bytecode(value *argv, int argn)
{
  return ml_jackseq_output_event(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[argn - 1]);
}


/* 
 * Get the input midi events
 */
CAMLprim value
ml_jackseq_get_input(value ml_seq){
  CAMLparam1 (ml_seq);
  CAMLlocal2(return_array, one_ml_event);
  jack_seq_t *js = NULL ;
  jack_seq_midi_event_t stack_event;
  size_t i, to_read;
  int ret;
  js = *((jack_seq_t **)Data_custom_val(ml_seq)) ;

  to_read = jack_ringbuffer_read_space(js->js_i_rbuf)
    / sizeof(jack_seq_midi_event_t);

  return_array = caml_alloc(to_read , 0) ;

  for (i = 0; i < to_read; i++) {

    ret = jack_ringbuffer_read(js->js_i_rbuf,
        (void *)&stack_event, sizeof(jack_seq_midi_event_t));
    js_exn_assert(ret == sizeof(jack_seq_midi_event_t),
        "Couldn't read in the ring buffer");

    one_ml_event = caml_alloc(5, 0);
    Store_field(one_ml_event, 0, Val_int(stack_event.me_port));
    Store_field(one_ml_event, 1, Val_int(stack_event.me_stat & 0xFF));
    Store_field(one_ml_event, 2, Val_int(stack_event.me_chan & 0xFF));
    Store_field(one_ml_event, 3, Val_int(stack_event.me_dat1 & 0xFF));
    Store_field(one_ml_event, 4, Val_int(stack_event.me_dat2 & 0xFF));

    Store_field(return_array, i, one_ml_event);

  }

  CAMLreturn(return_array);
}








