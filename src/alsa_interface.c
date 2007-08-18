/**************************************************************************/
/*  Copyright (c) 2007, Sebastien MONDET                                  */
/*                                                                        */
/*  Permission is hereby granted, free of charge, to any person           */
/*  obtaining a copy of this software and associated documentation        */
/*  files (the "Software"), to deal in the Software without               */
/*  restriction, including without limitation the rights to use,          */
/*  copy, modify, merge, publish, distribute, sublicense, and/or sell     */
/*  copies of the Software, and to permit persons to whom the             */
/*  Software is furnished to do so, subject to the following              */
/*  conditions:                                                           */
/*                                                                        */
/*  The above copyright notice and this permission notice shall be        */
/*  included in all copies or substantial portions of the Software.       */
/*                                                                        */
/*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       */
/*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       */
/*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              */
/*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT           */
/*  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,          */
/*  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING          */
/*  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR         */
/*  OTHER DEALINGS IN THE SOFTWARE.                                       */
/**************************************************************************/

/*
 * C code used in AlsaSequencer module.
 *
 * Author: S. MONDET
 */

/* 
 * Classic includes:
 */
#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <alloca.h>


/*
 * ALSA's dot H.
 *
 * Note: it is the "not `gcc -ansi -pedantic` compliant" black sheep ;-)
 */
#include <alsa/asoundlib.h>


/*
 * Caml wrapping includes:
 *
 */
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/custom.h>


/******************************************************************************/

#define ALSASEQ_EVENT_STATUS_BIT        0x80
#define ALSASEQ_EVENT_NOTE_OFF          0x80
#define ALSASEQ_EVENT_NOTE_ON           0x90
#define ALSASEQ_EVENT_AFTERTOUCH        0xA0
#define ALSASEQ_EVENT_CONTROL_CHANGE    0xB0
#define ALSASEQ_EVENT_PROGRAM_CHANGE    0xC0
#define ALSASEQ_EVENT_CHANNEL_PRESSURE  0xD0
#define ALSASEQ_EVENT_PITCH_WHEEL       0xE0
#define ALSASEQ_EVENT_CLEAR_CHAN_MASK   0xF0
#define ALSASEQ_EVENT_MIDI_CLOCK        0xF8
#define ALSASEQ_EVENT_SYSEX             0xF0
#define ALSASEQ_EVENT_SYSEX_END         0xF7

#define ALSASEQ_POLL_TIMEOUT            100000

#define ALSASEQ_DEFAULT_BPM 120
#define ALSASEQ_DEFAULT_TPQ 128



typedef struct {
  snd_seq_t *seq_handle; /* "Pointer to alsa" */

  int *i_ports ;/*  input ports */
  int *o_ports ;/* output ports */

  int queue_id ;/* id for an alsa's event queue */

  int pfd_nb ;/* number of file descriptors */
  struct pollfd *pfd  ;/* poll of file descriptors */

  int bpm ; /* current tempo     */
  int tpq ; /* ticks per quarter */

} AlsaSequencer ;

/*
 *  DESTROY the SEQUENCER 
 *  
 */
void 
alsaseq_destroy(value alsa_seq){

  /* must not use CAMLparam1 in destructor */
  AlsaSequencer *seq = NULL ;

  /* Get the sequencer: */
  seq = *((AlsaSequencer **)Data_custom_val(alsa_seq)) ;

  snd_seq_stop_queue(seq->seq_handle, seq->queue_id, NULL);
  snd_seq_free_queue(seq->seq_handle, seq->queue_id);
  free(seq->o_ports);
  free(seq->i_ports);
  free(seq);
  /* printf("ALSA SEQUENCER DETROYED\n"); */
}

static struct custom_operations sequencer_custom_ops = {
  "locoseq.alsa_sequencer",
  /* custom_finalize_default,*/ alsaseq_destroy, 
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

/*
 * An assert function that trows a Failure exception.
 *
 */
void
exn_assert(int condition, char *msg) {
  if (condition == 0)  {
    fprintf(stderr, "[C][ERR] Assert Failed: %s\n", msg) ;
    perror("[C][ERR] Unix Error: ");
    caml_failwith("GENERAL ASSERT FAILED in alsa_interface.c") ;
  }
}

/*
 * Launch a "Failure" exception in case of negative return value from an ALSA
 * function.
 */
void
exn_alsa_assert(int err_code, char *msg) {
  if (err_code < 0)  {
    char exception[1024];
    /* (The msg can be on stack because caml_failwith will copy it) */
    fprintf(stderr, "[C][ERR] Assert Failed: %s\n", msg) ;
    fprintf(stderr, "[C][ERR] Alsa Error: %s\n", snd_strerror(err_code)) ;
    perror(         "[C][ERR] Unix Error: ");
    sprintf(exception, "[C] ALSA ASSERT FAILED: %255s (alsa_error: %255s)",
        msg, snd_strerror(err_code));
    caml_failwith(exception) ;
  }
}



/*
 * Constructor of the sequencer "object"
 *
 */
CAMLprim value
alsaseq_make( value app_name, value in_names_array, value ou_names_array) {

  CAMLparam3(app_name, in_names_array, ou_names_array) ;
  int ret = 0 , i, in_size, ou_size;
  AlsaSequencer *seq = NULL ;
  value the_sequencer;

  /* Allocating the structure */
  seq = malloc (sizeof(AlsaSequencer)) ;
  exn_assert(seq != NULL, "AlsaSequencer allocation");
  memset( seq , 0 , sizeof(AlsaSequencer) );


  /* Creating the client: */
  ret = snd_seq_open(&(seq->seq_handle), "default", SND_SEQ_OPEN_DUPLEX , 0) ;
  exn_alsa_assert(ret , "Alsa client creation") ;
  snd_seq_set_client_name(seq->seq_handle, String_val(app_name) );

  /* Creating the input ports from string array in_names_array: */
  in_size = Wosize_val(in_names_array) ;
  seq->i_ports = malloc(sizeof(int) * in_size) ;
  for ( i = 0 ; i < in_size ; i++ ) {
    seq->i_ports[i] = snd_seq_create_simple_port(seq->seq_handle,
        String_val(Field(in_names_array, i)),
        SND_SEQ_PORT_CAP_WRITE|SND_SEQ_PORT_CAP_SUBS_WRITE,
        SND_SEQ_PORT_TYPE_APPLICATION) ;
    exn_alsa_assert (seq->i_ports[i], "Input port creation" ) ;
  }

  /* Creating the output ports from string array ou_names_array: */
  ou_size = Wosize_val(ou_names_array) ;
  seq->o_ports = malloc(sizeof(int) * ou_size) ;
  for ( i = 0 ; i < ou_size ; i++ ) {
    seq->o_ports[i] = snd_seq_create_simple_port(seq->seq_handle,
        String_val(Field(ou_names_array, i)),
        SND_SEQ_PORT_CAP_READ|SND_SEQ_PORT_CAP_SUBS_READ,
        SND_SEQ_PORT_TYPE_APPLICATION) ;
    exn_alsa_assert (seq->o_ports[i], "Output port creation" ) ;
  }

  /* allocate the client's queue: */
  seq->queue_id = snd_seq_alloc_queue(seq->seq_handle);
  exn_alsa_assert (seq->queue_id, "Error in queue allocation" ) ;

  /* set the output pool size: */
  /* snd_seq_set_client_pool_output(seq->seq_handle, 1024 ); */
  /* XXX (here we don't know which size) */


  /* Get number of descriptors (generally 1): */
  seq->pfd_nb = snd_seq_poll_descriptors_count(seq->seq_handle, POLLIN);
  /* allocate space for them: */
  seq->pfd = malloc(seq->pfd_nb * sizeof(struct pollfd));
  /* get the descriptors */
  snd_seq_poll_descriptors(seq->seq_handle, seq->pfd, seq->pfd_nb, POLLIN);

  /* set default values for tempo: */
  seq->bpm = ALSASEQ_DEFAULT_BPM ;
  seq->tpq = ALSASEQ_DEFAULT_TPQ ;

  /* Return an abstract value: */
  the_sequencer = alloc_custom(
      &sequencer_custom_ops, sizeof(AlsaSequencer *), 0, 1);
  *((AlsaSequencer **)Data_custom_val(the_sequencer)) = seq ;
  CAMLreturn (the_sequencer) ;
}


/* 
 * Output directly a midi event on a given port:
 *  
 */
CAMLprim value
alsaseq_output_event_direct(value alsa_seq, value port_nb, value midi_event){

  CAMLparam3 (alsa_seq, port_nb, midi_event);
  AlsaSequencer *seq = NULL;
  
  int tick = 0;
  int port = 0;
  /* alsa midi parser */
  snd_midi_event_t *midi_ev;

  /* temp for midi data */
  unsigned char buffer[3];
	
  snd_seq_event_t ev;
  snd_seq_ev_clear(&ev);

  /* Get the sequencer: */
  seq = *((AlsaSequencer **)Data_custom_val(alsa_seq));

  port      = Int_val(port_nb);
  tick      = Int_val(Field(midi_event, 0));
  buffer[0] = Int_val(Field(midi_event, 1)) & 0xF0;
  buffer[0]+= Int_val(Field(midi_event, 2)) & 0x0F;
  buffer[1] = Int_val(Field(midi_event, 3));
  buffer[2] = Int_val(Field(midi_event, 4));

  snd_midi_event_new( 3, &midi_ev );
  snd_midi_event_encode( midi_ev, buffer, 3, &ev ); 
  snd_midi_event_free( midi_ev );

  /* outputting DIRECTLY the event: */
  snd_seq_ev_set_subs(&ev); /*macro: set broadcasting to subscribers  */
  snd_seq_ev_set_direct(&ev); /* set not-queue but direct */
  snd_seq_ev_set_source(&ev, seq->o_ports[port]); /*set the source port */
  /* output in_ev directly to the sequencer NOT through output buffer: */
  snd_seq_event_output_direct(seq->seq_handle, &ev);

  CAMLreturn (Val_int(0)) ;
}


/* 
 * get_next_input_event
 *
 */
CAMLprim value
alsaseq_get_next_input_event(value alsa_seq){

  CAMLparam1 (alsa_seq);
  AlsaSequencer *seq = NULL;

  CAMLlocal1(midi_event) ;
    
  int bytes = 0;
  snd_seq_event_t *ev = NULL;
  /* temp for midi data */
  unsigned char buffer[0x08];
  snd_midi_event_t *midi_ev;

  int ticks  = 0;
  int status = 0;
  int channel= 0;
  int data_1 = 0;
  int data_2 = 0;

  /* Get the sequencer: */
  seq = *((AlsaSequencer **)Data_custom_val(alsa_seq)) ;

  bytes = poll(seq->pfd, seq->pfd_nb, ALSASEQ_POLL_TIMEOUT ) ;
  /* (unsued var bytes) */

  snd_seq_event_input(seq->seq_handle, &ev);

  snd_midi_event_new( 0x08, &midi_ev );
    
  bytes = snd_midi_event_decode( midi_ev, buffer, 0x08, ev); 
  /* (unsued var bytes) */

  ticks  = ev->time.tick;
  status = buffer[0];
  channel= ev->data.note.channel;
  data_1 = buffer[1];
  data_2 = buffer[2] ;

  /* The ugly particular case of some synths: */
  /* ( NOTE_ON && Velocity == 0 ) => NOTE_OFF */
  if (status ==  ALSASEQ_EVENT_NOTE_ON && data_2 == 0x00 ){
    status = ALSASEQ_EVENT_NOTE_OFF;
  }

  snd_midi_event_free( midi_ev );

  snd_seq_free_event(ev);



  /* Allocate the ocaml struct and fill it: */
  midi_event = caml_alloc(5 , 0) ;

  Store_field(midi_event , 0 , Val_int(ticks  ) );
  Store_field(midi_event , 1 , Val_int(status ) );
  Store_field(midi_event , 2 , Val_int(channel) );
  Store_field(midi_event , 3 , Val_int(data_1 ) );
  Store_field(midi_event , 4 , Val_int(data_2 ) );

  CAMLreturn (midi_event) ;
}

/*
 * build an ocaml list of the current pending input events
 */
CAMLprim value
alsaseq_get_input_events_list(value alsa_seq){

  CAMLparam1 (alsa_seq) ;
  AlsaSequencer *seq = NULL ;

  CAMLlocal4(midi_event, midi_events, midi_events_tail, midi_new_item) ;

  snd_seq_event_t *ev = NULL;
  /* temp for midi data */
  unsigned char buffer[0x08];
  snd_midi_event_t *midi_ev;

  int ticks  = 0;
  int status = 0;
  int channel= 0;
  int data_1 = 0;
  int data_2 = 0;

  /* Start with empty list */ 
  midi_events = Val_int(0) ;
  midi_events_tail = Val_int(0) ;
  midi_new_item = Val_int(0) ;
  /* Get the sequencer: */
  seq = *((AlsaSequencer **)Data_custom_val(alsa_seq)) ;


  if (snd_seq_event_input_pending (seq->seq_handle,1)) {
    do {
      snd_seq_event_input(seq->seq_handle, &ev);
      snd_midi_event_new( 0x08, &midi_ev );
      snd_midi_event_decode( midi_ev, buffer, 0x08, ev); 

      ticks  = ev->time.tick;
      status = buffer[0];
      channel= ev->data.note.channel;
      data_1 = buffer[1];
      data_2 = buffer[2] ;

      snd_midi_event_free( midi_ev );
      snd_seq_free_event(ev);

      /* The ugly particular case of some synths: */
      /* ( NOTE_ON && Velocity == 0 ) => NOTE_OFF */
      if (status ==  ALSASEQ_EVENT_NOTE_ON && data_2 == 0x00 ){
        status = ALSASEQ_EVENT_NOTE_OFF;
      }
      /* list item: */
      midi_event = caml_alloc(5 , 0) ;
      Store_field(midi_event , 0 , Val_int(ticks  ) );
      Store_field(midi_event , 1 , Val_int(status ) );
      Store_field(midi_event , 2 , Val_int(channel) );
      Store_field(midi_event , 3 , Val_int(data_1 ) );
      Store_field(midi_event , 4 , Val_int(data_2 ) );

      /* Add the item to the output list: */
      if (midi_events == Val_int(0)){ /* list = [] */
        midi_events = caml_alloc(2,0) ;
        Store_field(midi_events , 0 , midi_event );
        Store_field(midi_events , 1 , Val_int(0) );
        midi_events_tail = midi_events ;
      } else {
        midi_new_item = caml_alloc(2,0) ;
        Store_field(midi_new_item , 0 , midi_event );
        Store_field(midi_new_item , 1 , Val_int(0) );

        caml_modify(&Field(midi_events_tail, 1), midi_new_item); 

        midi_events_tail = midi_new_item ;
      }

    } while (snd_seq_event_input_pending (seq->seq_handle,0));
  }

  CAMLreturn (midi_events) ;
}

/*
 * Internal "tool" function, takes the bpm and tpq from sequencer and updates 
 * the tempo of the queue.
 */
void 
alsaseq_update_tempo(AlsaSequencer *seq) {
  int tempo = 0 ;
  snd_seq_queue_tempo_t *queue_tempo;

  snd_seq_queue_tempo_malloc(&queue_tempo);
  exn_assert(queue_tempo != NULL,
      "Queue tempo allocation (alsaseq_update_tempo)");

  /* make tempo: (formula from miniArp.c by Matthias Nagorni) */
  tempo = (int)(6e7 / ((double)seq->bpm * (double)seq->tpq) * (double)seq->tpq);
  /* Set the tempo of a queue_status container */
  snd_seq_queue_tempo_set_tempo(queue_tempo, tempo);
  /* Set the ppq of a queue_status container: */
  snd_seq_queue_tempo_set_ppq(queue_tempo, seq->tpq );
  /* set the tempo of the queue: */
  snd_seq_set_queue_tempo(seq->seq_handle, seq->queue_id, queue_tempo);

  snd_seq_queue_tempo_free(queue_tempo);
}

/*
 * Set the current tempo
 */
CAMLprim value 
alsaseq_set_tempo(value alsa_seq, value bpm, value tpq){

  CAMLparam3 (alsa_seq, bpm, tpq) ;
  AlsaSequencer *seq = NULL ;
  /* Get the sequencer: */
  seq = *((AlsaSequencer **)Data_custom_val(alsa_seq)) ;

  seq->bpm = Int_val(bpm);
  seq->tpq = Int_val(tpq);

  alsaseq_update_tempo(seq) ;

  CAMLreturn (Val_int(0)) ;
}

/*
 * Get a tuple containing bpm and tpq values
 */
CAMLprim value
alsaseq_get_tempo(value alsa_seq){

  CAMLparam1 (alsa_seq) ;
  AlsaSequencer *seq = NULL ;
  CAMLlocal1(tempo) ;

  /* Get the sequencer: */
  seq = *((AlsaSequencer **)Data_custom_val(alsa_seq)) ;

  tempo = caml_alloc(2 , 0) ;
  Store_field(tempo , 0 , Val_int(seq->bpm) );
  Store_field(tempo , 1 , Val_int(seq->tpq) );

  CAMLreturn (tempo) ;
}


/*
 * Start the queue (ready to play)
 */
CAMLprim value
alsaseq_start_queue(value alsa_seq){

  CAMLparam1 (alsa_seq) ;
  AlsaSequencer *seq = NULL ;
  int ret = -1 ;
  /* Get the sequencer: */
  seq = *((AlsaSequencer **)Data_custom_val(alsa_seq)) ;

  alsaseq_update_tempo(seq) ;

  /* Start the queue: */
  ret = snd_seq_start_queue( seq->seq_handle, seq->queue_id, NULL);
  exn_alsa_assert(ret , "snd_seq_start_queue");
  /* drains all pending events on the output buffer: */
  ret = snd_seq_drain_output(seq->seq_handle);
  exn_alsa_assert(ret , "snd_seq_drain_output");

  CAMLreturn (Val_int(0)) ;
}

/* 
 * get current time in the queue 
 */
CAMLprim value
alsaseq_get_tick(value alsa_seq){

  CAMLparam1 (alsa_seq) ;
  AlsaSequencer *seq = NULL ;
  snd_seq_queue_status_t *status;
  snd_seq_tick_time_t current_tick;
  
  /* Get the sequencer: */
  seq = *((AlsaSequencer **)Data_custom_val(alsa_seq)) ;

  /* Get the tick: */
  snd_seq_queue_status_malloc(&status);
  exn_assert(status != NULL, "snd_seq_queue_status_malloc");
  snd_seq_get_queue_status(seq->seq_handle, seq->queue_id, status);
  current_tick = snd_seq_queue_status_get_tick_time(status);
  snd_seq_queue_status_free(status);

  CAMLreturn (Val_int(current_tick)) ;
}


/*
 * Schedule a given event on the queue
 */
CAMLprim value
alsaseq_put_event_in_queue(value alsa_seq, value port_nb, value midi_event){

  CAMLparam3 (alsa_seq, port_nb, midi_event) ;
  AlsaSequencer *seq = NULL ;
  
  int tick = 0;
  int port = 0;
  /* alsa midi parser */
  snd_midi_event_t *midi_ev;

  /* temp for midi data */
  unsigned char buffer[3];
	
  snd_seq_event_t ev;

  snd_seq_ev_clear(&ev);


  /* Get the sequencer: */
  seq = *((AlsaSequencer **)Data_custom_val(alsa_seq)) ;


  port      = Int_val(port_nb);
  tick      = Int_val(Field(midi_event, 0)) ;
  buffer[0] = Int_val(Field(midi_event, 1)) & 0xF0 ;
  buffer[0]+= Int_val(Field(midi_event, 2)) & 0x0F ;
  buffer[1] = Int_val(Field(midi_event, 3)) ;
  buffer[2] = Int_val(Field(midi_event, 4)) ;

  /* printf("[C:] buffer_0: %x\n", buffer[0] ); */
  snd_midi_event_new( 3, &midi_ev );

  snd_midi_event_encode( midi_ev, buffer, 3, &ev ); 
		
  snd_midi_event_free( midi_ev );

  /* set tick (i.e. sceduled time) value on the queue */
  snd_seq_ev_schedule_tick(&ev, seq->queue_id,  0, tick);
  /* trigger the event: */
  snd_seq_ev_set_source(&ev, seq->o_ports[port] );
  snd_seq_ev_set_subs(&ev);  
  snd_seq_event_output_direct(seq->seq_handle, &ev);

  CAMLreturn (Val_int(0)) ;
}



/* 
 * Clear the whole queue:
 */

CAMLprim value
alsaseq_clear_queue(value alsa_seq){

  CAMLparam1 (alsa_seq) ;
  AlsaSequencer *seq = NULL ;

  snd_seq_remove_events_t *remove_ev;
  
  /* Get the sequencer: */
  seq = *((AlsaSequencer **)Data_custom_val(alsa_seq)) ;

  snd_seq_remove_events_malloc(&remove_ev);
  snd_seq_remove_events_set_queue(remove_ev, seq->queue_id);
  snd_seq_remove_events_set_condition(remove_ev,
      SND_SEQ_REMOVE_OUTPUT | SND_SEQ_REMOVE_IGNORE_OFF);
  snd_seq_remove_events(seq->seq_handle, remove_ev);
  snd_seq_remove_events_free(remove_ev);

  CAMLreturn (Val_int(0)) ;
}


/*
 * Stop the queue.
 */
CAMLprim value
alsaseq_stop_queue(value alsa_seq){

  CAMLparam1 (alsa_seq) ;
  AlsaSequencer *seq = NULL ;
  /* Get the sequencer: */
  seq = *((AlsaSequencer **)Data_custom_val(alsa_seq)) ;

  alsaseq_update_tempo(seq) ;

  /* Start the queue: */
  snd_seq_stop_queue( seq->seq_handle, seq->queue_id, NULL);
  /* drains all pending events on the output buffer: */
  snd_seq_drain_output(seq->seq_handle);

  CAMLreturn (Val_int(0)) ;
}

/*
 * Get a timer_info structure describing the timer of the queue
 */
CAMLprim value
alsaseq_get_queue_timer(value alsa_seq){

  CAMLparam1 (alsa_seq) ;
  CAMLlocal1(timer_info);
  AlsaSequencer *seq = NULL ;

  snd_seq_queue_timer_t *queue_timer ;
  snd_timer_id_t * timer_id ;

  /* Get the sequencer: */
  seq = *((AlsaSequencer **)Data_custom_val(alsa_seq)) ;

  snd_seq_queue_timer_malloc (&queue_timer);
  exn_assert(queue_timer != NULL, "snd_seq_queue_timer_malloc");

  snd_seq_get_queue_timer (seq->seq_handle, seq->queue_id, queue_timer);

  /* We need the cast because the function returns a (const snd_timer_id_t*) */
  timer_id = (snd_timer_id_t *)snd_seq_queue_timer_get_id (queue_timer);

  /* Allocate the ocaml struct and fill it: */
  timer_info = caml_alloc(5 , 0) ;

  Store_field(timer_info , 0 , Val_int(snd_timer_id_get_class(    timer_id)) );
  Store_field(timer_info , 1 , Val_int(snd_timer_id_get_sclass(   timer_id)) );
  Store_field(timer_info , 2 , Val_int(snd_timer_id_get_card(     timer_id)) );
  Store_field(timer_info , 3 , Val_int(snd_timer_id_get_device(   timer_id)) );
  Store_field(timer_info , 4 , Val_int(snd_timer_id_get_subdevice(timer_id)) );

  snd_seq_queue_timer_free (queue_timer);
  CAMLreturn (timer_info) ;
}







/******************************************************************************
 *
 * ALSA TIMER INTERFACE
 *
 */

typedef struct {

  snd_timer_t * handle; /* "The" timer */
  struct pollfd * fds; /* the file descriptors to poll */
  int fd_count ;       /* their number */

} AlsaTimer ;


#define GET_TIMER(ml_val) (*((AlsaTimer **)Data_custom_val(ml_val))) 

void destroy_alsa_timer(value ml_timer){

  AlsaTimer * at = GET_TIMER(ml_timer) ;

  snd_timer_close(at->handle);
  free(at->fds);
  /* fprintf(stderr, "[C] AlsaTimer DESTROYED by Garbage Collector\n") ; */
}

static struct custom_operations alsa_timer_custom_ops = {
  "locoseq.alsa_timer",
  /* custom_finalize_default,  */
  destroy_alsa_timer, 
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};



/*
 * Get information about available timers
 */
CAMLprim value
alsatim_query_info(value unit) {

  CAMLparam1 (unit);
  CAMLlocal3(timer_info, ti_list, ti_item) ;

  snd_timer_query_t *qhandle = NULL;
  int err = 0 ;
  snd_timer_id_t *id = NULL;

  snd_timer_id_alloca(&id);
  exn_assert(id != NULL, "snd_timer_id_alloca");

  ti_list = Val_int(0) ; /* start with list:[] */

  err = snd_timer_query_open(&qhandle, "hw", 0) ;
  exn_alsa_assert(err, "snd_timer_query_open error");

  snd_timer_id_set_class(id, SND_TIMER_CLASS_NONE);
  while (1) {
    if ((err = snd_timer_query_next_device(qhandle, id)) < 0) {
      fprintf(stderr, "timer next device error: %s\n", snd_strerror(err));
      break;
    }
    if (snd_timer_id_get_class(id) < 0)
      break;


    /* Allocate the ocaml struct and fill it: */
    timer_info = caml_alloc(5 , 0) ;

    Store_field(timer_info , 0 , Val_int(snd_timer_id_get_class(    id)) );
    Store_field(timer_info , 1 , Val_int(snd_timer_id_get_sclass(   id)) );
    Store_field(timer_info , 2 , Val_int(snd_timer_id_get_card(     id)) );
    Store_field(timer_info , 3 , Val_int(snd_timer_id_get_device(   id)) );
    Store_field(timer_info , 4 , Val_int(snd_timer_id_get_subdevice(id)) );

    ti_item = caml_alloc_small(2, 0);     /* Allocate a cons cell */
    Field(ti_item, 0) = timer_info ;            /* car = the integer i2 */
    Field(ti_item, 1) = Val_int(0);             /* cdr = the empty list [] */

    if (ti_list == Val_int(0) ) {
      ti_list = ti_item ;
    } else {
      Field(ti_item, 1) = ti_list ; 
      ti_list = ti_item ;
    }

  }
  snd_timer_query_close(qhandle);

  CAMLreturn (ti_list) ;
}


/*
 * Build a timer from an "info" description
 */
CAMLprim value
alsatim_make_timer(value ml_info) {

  CAMLparam1 (ml_info);
  int count, err = 0;
  char timername[64];
  snd_timer_t *handle;
  /* snd_timer_id_t *id; */
  snd_timer_info_t *info = NULL;
  snd_timer_params_t *params = NULL;

  AlsaTimer *timer = NULL;
  value the_timer ;


  /* Allocating the structure */
  timer = malloc (sizeof(AlsaTimer)) ;
  exn_assert(timer != NULL, "AlsaTimer allocation");
  memset( timer , 0 , sizeof(AlsaTimer) );

  /* snd_timer_id_alloca(&id); */
  snd_timer_info_alloca(&info);
  snd_timer_params_alloca(&params);

  sprintf(timername , "hw:CLASS=%i,SCLASS=%i,CARD=%i,DEV=%i,SUBDEV=%i"
      , Int_val(Field(ml_info, 0))
      , Int_val(Field(ml_info, 1))
      , Int_val(Field(ml_info, 2))
      , Int_val(Field(ml_info, 3))
      , Int_val(Field(ml_info, 4))
      );

  /* printf("[C] Open timer: %s\n", timername); */
  err = snd_timer_open(&handle, timername, SND_TIMER_OPEN_NONBLOCK);
  exn_alsa_assert(err, "timer open error") ;

  err = snd_timer_info(handle, info);
  exn_alsa_assert(err, "timer info error") ;
/*
  printf("[C]Timer info:\n");
  printf(" |   slave = %s\n", snd_timer_info_is_slave(info) ? "yes" : "no");
  printf(" |   card = %i\n", snd_timer_info_get_card(info));
  printf(" |   id = '%s'\n", snd_timer_info_get_id(info));
  printf(" |   name = '%s'\n", snd_timer_info_get_name(info));
  printf(" |   average resolution = %li\n", snd_timer_info_get_resolution(info));
*/
  /* To have an auto-restart timer: */
  err = snd_timer_params_set_auto_start(params, 1);
  exn_alsa_assert(err, "timer snd_timer_params_set_auto_start error") ;

  
  /* Is void: */
  snd_timer_params_set_ticks(params, 1);

  err = snd_timer_params(handle, params);
  exn_alsa_assert(err, "timer params error");


  /* Filling the AlsaTimer structure: */
  timer->handle = handle ;

  /* Get the filedescriptors now to optimize real-time response: */
  count = snd_timer_poll_descriptors_count((snd_timer_t *)handle);
  exn_assert(count > 0, "Didn't get timer poll descriptors");
  timer->fds = calloc(count, sizeof(struct pollfd));
  exn_assert(timer->fds != NULL, "struct pollfd maloc error");

  err = snd_timer_poll_descriptors((snd_timer_t *)handle, timer->fds, count);
  exn_alsa_assert(err, "snd_timer_poll_descriptors error");

  timer->fd_count = count ;

  /* Return an abstract value to ocaml: */
  the_timer = alloc_custom(
      &alsa_timer_custom_ops, sizeof(AlsaTimer *), 0, 1);
  *((AlsaTimer **)Data_custom_val(the_timer)) = timer ;
  CAMLreturn (the_timer) ;

}

/*
 * Get the snd_timer_status of the timer
 */
CAMLprim value
alsatim_get_timer_status(value ml_timer) {

  CAMLparam1 (ml_timer);
  CAMLlocal1(ml_info) ;
  int err;
  snd_timer_status_t *status;
  
        
  snd_timer_status_alloca(&status);
  err = snd_timer_status(GET_TIMER(ml_timer)->handle, status);
  exn_alsa_assert(err, "timer status error");

  /* Allocate the ocaml struct and fill it: */
  ml_info = caml_alloc(4 , 0) ;

  Store_field(ml_info , 0 , Val_int( snd_timer_status_get_resolution(status)  ));
  Store_field(ml_info , 1 , Val_int( snd_timer_status_get_lost(status) ));
  Store_field(ml_info , 2 , Val_int( snd_timer_status_get_overrun(status) ));
  Store_field(ml_info , 3 , Val_int( snd_timer_status_get_queue(status) ));

  CAMLreturn (ml_info) ;
}

/*
 * Launch the timer
 */
CAMLprim value
alsatim_start_timer(value ml_timer) {
  CAMLparam1 (ml_timer);
  int err = 0;

  err = snd_timer_start(GET_TIMER(ml_timer)->handle) ;
  exn_alsa_assert(err, "timer start error");

  CAMLreturn (Val_int(0)) ; /* unit */
}

/*
 * Block until the next timer tick
 * and return the number of read ticks
 * if -1 -> poll has timed out
 * if 0  -> ???? (never observed)
 * if  >1  -> YOU ARE LATE ;-D
 *
 */
CAMLprim value
alsatim_wait_next(value ml_timer, value timeout) {

  CAMLparam2(ml_timer,timeout);
  CAMLlocal1(ret);
  int count, err;
  snd_timer_read_t tr;
  AlsaTimer *timer = GET_TIMER(ml_timer);

  err = poll(timer->fds, timer->fd_count, Int_val(timeout) );
  exn_assert(err >= 0, "poll timer error") ;
  if (err == 0) {
    fprintf(stderr, "timer time out!!\n");
    count = -1 ;
  } else {
    count = 0; 
    while (snd_timer_read(timer->handle, &tr, sizeof(tr)) == sizeof(tr)) {
      count++ ;
      /* printf("TIMER: resolution = %uns, ticks = %u\n", */
      /* tr.resolution, tr.ticks); */
      /* resol = tr.resolution ; */
      /* ticks = tr.ticks ; */
    }
  }
  /* printf("fds:%d, %d, %d\n",
     timer->fds[0].fd, timer->fds[0].events, timer->fds[0].revents  ); */

  CAMLreturn(Val_int(count));
}

/*
 * Stop the timer
 */ 
CAMLprim value
alsatim_stop_timer(value ml_timer) {
  CAMLparam1 (ml_timer);

  exn_alsa_assert( snd_timer_stop(GET_TIMER(ml_timer)->handle), "timer stop error");
  
  CAMLreturn (Val_int(0)) ;
}

/*
 * Set the number of real ticks (cf timer resolution) that we waant for 1 tick
 * of our sequencing timer
 */
CAMLprim value
alsatim_set_ticks(value ml_timer, value ticks) {

  CAMLparam2(ml_timer,ticks);

  int err = 0;

  snd_timer_params_t *params;

  snd_timer_params_alloca(&params);
  exn_assert(params != NULL, "snd_timer_params_alloca");

  /* To have an auto-restart timer: */
  err = snd_timer_params_set_auto_start(params, 1);
  exn_alsa_assert(err, "timer snd_timer_params_set_auto_start error") ;

  snd_timer_params_set_ticks(params, Int_val(ticks) );

  /* printf("get ticks: %d\n", snd_timer_params_get_ticks(params)); */
  err = snd_timer_params(GET_TIMER(ml_timer)->handle, params);
  exn_alsa_assert(err, "timer params error");

  CAMLreturn (Val_int(0)) ;
}
