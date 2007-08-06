(**************************************************************************)
(*  Copyright (c) 2007, Sebastien MONDET                                  *)
(*                                                                        *)
(*  Permission is hereby granted, free of charge, to any person           *)
(*  obtaining a copy of this software and associated documentation        *)
(*  files (the "Software"), to deal in the Software without               *)
(*  restriction, including without limitation the rights to use,          *)
(*  copy, modify, merge, publish, distribute, sublicense, and/or sell     *)
(*  copies of the Software, and to permit persons to whom the             *)
(*  Software is furnished to do so, subject to the following              *)
(*  conditions:                                                           *)
(*                                                                        *)
(*  The above copyright notice and this permission notice shall be        *)
(*  included in all copies or substantial portions of the Software.       *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT           *)
(*  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,          *)
(*  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING          *)
(*  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR         *)
(*  OTHER DEALINGS IN THE SOFTWARE.                                       *)
(**************************************************************************)


(** 
 
OCaml types and functions wrapping {b libasound}
and defined in alsa_interface.c

@author S. Mondet

 *)

(** {3 The sequencer object} *)

(** The sequencer object (abstract type) *)
type sequencer

(**
 The sequencer constructor
 should be called as  {[
 let my_seq = make_sequencer "alsa_client_name" 
   [| "input_port_A" ; "input_port_B" |]
   [| "out1" ; "out2" ; "outN" |] in
   ]}
 *)
external make_sequencer: string -> string array -> string array -> sequencer
= "alsaseq_make"


(******************************************************************************)
(** {3 INPUT interface} *)

(** Blocking wait for input *)
external wait_next_input_event: sequencer -> Midi.midi_event
= "alsaseq_get_next_input_event"

(** Non-Blocking way to get current events *)
external get_input_events: sequencer -> Midi.midi_event list
= "alsaseq_get_input_events_list"



(******************************************************************************)
(** {3 Direct OUTPUT interface} *)

(** Output an event on a given port without any queue *)
external output_event_direct: sequencer -> port:int -> Midi.midi_event -> unit
= "alsaseq_output_event_direct" ;;



(******************************************************************************)
(** {3 QUEUE interface} *)

(** Set BeatPerMinute and PerQuarterNote of the sequencer's queue *)
external set_queue_tempo: sequencer -> bpm:int -> ppqn:int -> unit
= "alsaseq_set_tempo"


(** Get BeatPerMinute and PerQuarterNote of the sequencer's queue *)
external get_queue_tempo: sequencer ->  int * int
= "alsaseq_get_tempo"


(** Start the queue ([ snd_seq_start_queue ]) *)
external start_queue: sequencer -> unit
= "alsaseq_start_queue"

(** Stop the queue *)
external stop_queue: sequencer -> unit
= "alsaseq_stop_queue"

(** Get current tick of the queue (with [ snd_seq_get_queue_status]) *)
external get_current_tick: sequencer -> int
= "alsaseq_get_tick"

(** Put an event in the queue, on a given output port *)
external put_event_in_queue: sequencer -> port:int -> Midi.midi_event -> unit
= "alsaseq_put_event_in_queue"

(** Remove all events in the queue *)
external clear_queue: sequencer -> unit
= "alsaseq_clear_queue"

(******************************************************************************)
(** {3 TIMER interface}  *)

(** Description  of alsa timer information *)
type timer_info = {
  mutable t_class     : int ; (** Class *)
  mutable t_sclass    : int ; (** Slave class *)
  mutable t_card      : int ; (** Card *)
  mutable t_device    : int ; (** Device *)
  mutable t_subdevice : int ; (** Subdevice *)   
}

(** Obtain the information of the timer of the sequencer's queue *)
external get_queue_timer_info: sequencer -> timer_info
= "alsaseq_get_queue_timer"


(** One default timer information
 (Class: global; SlaveClass: none; Card: system)
 *)
let default_timer_info = {
  t_class     =  1 ; (* SND_TIMER_CLASS_GLOBAL   *)
  t_sclass    = -1 ; (* SND_TIMER_CLASS_NONE     *)
  t_card      =  0 ; (* SND_TIMER_GLOBAL_SYSTEM  *)
  (* NOTE:seb: Those kind of anti-#define are not very convenient but... *)
  t_device    =  0 ; 
  t_subdevice =  0 ;    
}


external query_info : unit -> timer_info list =
  "alsatim_query_info" ;;

let timer_class_to_string t_class =
    match t_class with
    | -1 -> "SND_TIMER_CLASS_NONE"
    | 0  -> "SND_TIMER_CLASS_SLAVE"
    | 1  -> "SND_TIMER_CLASS_GLOBAL"
    | 2  -> "SND_TIMER_CLASS_CARD"
    | 3  -> "SND_TIMER_CLASS_PCM"
    | _  -> "UNKNOWN CLASS"
;;
 
let timer_slave_class_to_string t_sclass =
    match t_sclass with
    | 0 -> "SND_TIMER_SCLASS_NONE"
    | 1 -> "SND_TIMER_SCLASS_APPLICATION"
    | 2 -> "SND_TIMER_SCLASS_SEQUENCER"
    | 3 -> "SND_TIMER_SCLASS_OSS_SEQUENCER"
    | _ -> "UNKNOWN SLAVE CLASS"
;;

type timer 


external make_timer : timer_info -> timer = "alsatim_make_timer" ;;

type timer_status = {
  resolution : int ;
  lost : int ;
  overrun : int ;
  queue : int ;
};;

external get_status : timer -> timer_status = "alsatim_get_timer_status" ;;

external start_timer : timer -> unit = "alsatim_start_timer" ;;

external wait_next_tick : timer -> timeoutms:int -> int = "alsatim_wait_next" ;;

external stop_timer : timer -> unit = "alsatim_stop_timer" ;;

external set_ticks : timer -> ticks:int -> unit = "alsatim_set_ticks" ;;





