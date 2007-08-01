
(******************************************************************************)
(* Abstract Type:  sequencer *)
(* 
 *
 *
 *
 * *)
type sequencer

external make_sequencer : string -> string array -> string array -> sequencer =
  "alsaseq_make" ;;


(******************************************************************************)
(* New INPUT interface: *)

(* Blocking: *)
external wait_next_input_event : sequencer -> Midi.midi_event = 
  "alsaseq_get_next_input_event" ;;

(* Non-Blocking: *)
external get_input_events : sequencer -> Midi.midi_event list =
  "alsaseq_get_input_events_list" ;;



(******************************************************************************)
(* Direct OUTPUT interface: *)

external output_event_direct : sequencer -> port:int -> Midi.midi_event -> unit =
  "alsaseq_output_event_direct" ;;



(******************************************************************************)
(* QUEUE interface  *)

external set_queue_tempo : sequencer -> bpm:int -> ppqn:int -> unit =
  "alsaseq_set_tempo" ;;
external get_queue_tempo : sequencer ->  int * int =
  "alsaseq_get_tempo" ;;

external start_queue : sequencer -> unit =
  "alsaseq_start_queue" ;;
external stop_queue : sequencer -> unit =
  "alsaseq_stop_queue" ;;

external get_current_tick : sequencer -> int =
  "alsaseq_get_tick" ;;

external put_event_in_queue : sequencer -> port:int -> Midi.midi_event -> unit =
  "alsaseq_put_event_in_queue" ;;

external clear_queue : sequencer -> unit =
  "alsaseq_clear_queue" ;;

(******************************************************************************)
(* TIMER interface  *)

type timer_info = {
  mutable t_class     : int ;
  mutable t_sclass    : int ; 
  mutable t_card      : int ;
  mutable t_device    : int ; 
  mutable t_subdevice : int ;    
}

external get_queue_timer_info : sequencer -> timer_info = "alsaseq_get_queue_timer" ;;

let default_timer_info = {
  t_class     =  1 ; (* SND_TIMER_CLASS_GLOBAL   *)
  t_sclass    = -1 ; (* SND_TIMER_CLASS_NONE     *)
  t_card      =  0 ; (* SND_TIMER_GLOBAL_SYSTEM  *)
  (* NOTE:seb: Those kind of anti-#define are not very convenient but... *)
  t_device    =  0 ; 
  t_subdevice =  0 ;    
};;


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




