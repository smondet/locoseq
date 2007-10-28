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

module Seq = JackSequencer ;;


type arg_t =
  | ArgMidiStat
  | ArgMidiChan
  | ArgMidiNote
  | ArgMidiVelo
  | ArgInt of int

type action =
  | ActSetBPM of arg_t
  | ActIncrBPM of arg_t
  | ActDecrBPM of arg_t
  | ActToggleTrack of arg_t
  | ActTrackOn of arg_t
  | ActTrackOff of arg_t
  | ActScheduleToggleTrack of arg_t
  | ActScheduleTrackOn of arg_t
  | ActScheduleTrackOff of arg_t
  | ActPlay
  | ActStop
  | ActMuteAll


type midi_ev = {
  mutable status    : int ;
  mutable channel   : int ;
  mutable data_1    : int ;
  mutable data_2    : int ;
}


let unmatched_midi_val = -1

let empty_midi_ev () = {
  status  = unmatched_midi_val ;
  channel = unmatched_midi_val ;
  data_1  = unmatched_midi_val ;
  data_2  = unmatched_midi_val ;
}

let match_event (hev:midi_ev) (ev:Midi.midi_event) = (
  let no = unmatched_midi_val in 
  (hev.status  = no || hev.status  land 0xF0 = ev.Midi.status  land 0xF0) &&
  (hev.channel = no || hev.channel land 0xF0 = ev.Midi.channel land 0xF0) &&
  (hev.data_1  = no || hev.data_1  land 0xF0 = ev.Midi.data_1  land 0xF0) &&
  (hev.data_2  = no || hev.data_2  land 0xF0 = ev.Midi.data_2  land 0xF0) 
)


type manager = {
  mutable sequencer : Seq.sequencer ;

  mutable midi_handlers : (midi_ev * action) list ;
  mutable custom_handlers : (int * action) list ;

  mutable custom_events : int Queue.t ;

  mutable dump_midi_input : bool ;
}


let make_manager seq = {
  sequencer = seq ;
  midi_handlers = [] ;
  custom_handlers = [] ;
  custom_events = Queue.create () ;
  dump_midi_input = true ;
}


exception IncorrectSpecification of string

type input_specification = [
  | `midi_event of int * int * int * int
  | `custom of int
]

type act_spec_arg = [
  | `direct_int of int 
  | `midi_status 
  | `midi_channel
  | `midi_note
  | `midi_velocity
]
type act_spec_cmd = [
  | `set_BPM
  | `incr_BPM
  | `decr_BPM
  | `toggle_track
  | `track_on
  | `track_off
  | `schedule_toggle_track
  | `schedule_track_on
  | `schedule_track_off
  | `play
  | `stop
  | `mute_all
]
type action_specification = act_spec_cmd * act_spec_arg


let add_handler mgr (input_spec:input_specification)
(action_spec:action_specification) = (
  let must_be_midi = ref false in
  let action = 
    let act,arg = action_spec in
    let argument =
      match arg with
      | `direct_int v -> ArgInt v
      | `midi_status    -> (must_be_midi:=true) ; ArgMidiStat
      | `midi_channel   -> (must_be_midi:=true) ; ArgMidiChan
      | `midi_note      -> (must_be_midi:=true) ; ArgMidiNote
      | `midi_velocity  -> (must_be_midi:=true) ; ArgMidiVelo
    in
    match act with
    | `set_BPM                 ->  ActSetBPM   argument
    | `incr_BPM                ->  ActIncrBPM   argument
    | `decr_BPM                ->  ActDecrBPM   argument
    | `toggle_track            ->  ActToggleTrack   argument
    | `track_on                ->  ActTrackOn   argument
    | `track_off               ->  ActTrackOff   argument
    | `schedule_toggle_track   ->  ActScheduleToggleTrack   argument
    | `schedule_track_on       ->  ActScheduleTrackOn   argument
    | `schedule_track_off      ->  ActScheduleTrackOff   argument
    | `play                    ->  ActPlay                         
    | `stop                    ->  ActStop                         
    | `mute_all                ->  ActMuteAll                          
  in
  let _ =
    match input_spec with
    | `midi_event (stat,chan,note,velo) ->
        let ev = empty_midi_ev () in
        ev.status  <- stat ;
        ev.channel <- chan ;
        ev.data_1  <- note ;
        ev.data_2  <- velo ;
        mgr.midi_handlers <- (ev,action)::mgr.midi_handlers ;
    | `custom nb -> 
        if (!must_be_midi) then (
        raise (
          IncorrectSpecification
          "Can't take a midi argument from a custom event !!!"
        ) ;
        );
        mgr.custom_handlers <- (nb,action)::mgr.custom_handlers ;
  in ();
)

let remove_all_handlers mgr = (
  mgr.custom_handlers <- [] ;
  mgr.midi_handlers <- [] ;
)

let set_dump_midi_input mgr boolean = (
  mgr.dump_midi_input <- boolean ;
)

(******************************************************************************)
(* ==== REAL-TIME here: ==== *)

let clear_input mgr =
  let _ =  Seq.get_input mgr.sequencer in () ;
  Queue.clear  mgr.custom_events;
;;

let add_custom_event mgr id =
  Queue.add id mgr.custom_events ;;

let arg_to_int arg ev_opt = (
  match arg with
  | ArgInt i    -> i
  | _ -> (
    let mev = 
      match ev_opt with
      | Some e -> e
      | None -> raise (
        Failure "Custom event wth midi argument:\
        function add_handler shouldn't have allowed that happend !!"
      )
    in
    match arg with
    | ArgMidiStat -> mev.Midi.status
    | ArgMidiChan -> mev.Midi.channel
    | ArgMidiNote -> mev.Midi.data_1
    | ArgMidiVelo -> mev.Midi.data_2
    | _ -> failwith "Gni ??!!! Compiler Bug ???"
  )
)

let play_action action tracker (ev:Midi.midi_event option) =
  let module TRTC = Tracker.RTControl in
  match action with
  | ActSetBPM   argument                ->  
      TRTC.set_bpm tracker (arg_to_int argument ev) ;
  | ActIncrBPM   argument               -> 
      TRTC.add_to_bpm tracker (arg_to_int argument ev) ;
  | ActDecrBPM   argument               ->
      TRTC.add_to_bpm tracker ((-1) * (arg_to_int argument ev)) ;
  | ActToggleTrack   argument           -> 
      TRTC.toggle_playing_track tracker (arg_to_int argument ev) ;
  | ActTrackOn   argument               ->
      TRTC.set_playing_track tracker (arg_to_int argument ev) ;
  | ActTrackOff   argument            ->
      TRTC.set_stopping_track tracker (arg_to_int argument ev) ;
  | ActScheduleToggleTrack   argument   -> 
      TRTC.schedule_toggle tracker (arg_to_int argument ev)
  | ActScheduleTrackOn   argument       ->
      TRTC.schedule_play tracker (arg_to_int argument ev) ;
  | ActScheduleTrackOff   argument    ->
      TRTC.schedule_stop tracker (arg_to_int argument ev) ;
  | ActPlay -> Log.p "Not Implemented...\n" ;
  | ActStop -> TRTC.stop tracker ;
  | ActMuteAll -> TRTC.set_all_stopping tracker ;
;;


let manage_input mgr tracker =
  let ev_array = Seq.get_input mgr.sequencer in
  Array.iter (fun (port, stat, chan, dat1, dat2) ->
    (* fun ev -> *)
    if mgr.dump_midi_input then (
      Log.p "INPUT: [port:%d stat:%x chan:%d dat1:%d dat2:%d]\n"
      port stat chan dat1 dat2;
      (* ev.Midi.status ev.Midi.channel ev.Midi.data_1 ev.Midi.data_2 ; *)
    );
    let ev = {
      Midi.status = stat;
      ticks = 0; channel = chan; data_1 = dat1; data_2 = dat2;
    } in
    List.iter ( fun (hdl_ev,action) ->
      if (match_event hdl_ev ev) then (
        play_action action tracker (Some ev) ;
      );
    ) mgr.midi_handlers ;
  ) ev_array ;

  while not (Queue.is_empty mgr.custom_events) do
    let event = Queue.take mgr.custom_events in
    Log.p "custom event: %d\n" event ;
    List.iter (
      fun (nb,action) ->
        if event = nb then (
          play_action action tracker None ;
        );
    ) mgr.custom_handlers ;
    ();
  done;

  ();;



(*

let manage_input_old mgr tracker =

  let ev_list = Seq.get_input_events mgr.sequencer in () ;
  List.iter (
    fun ev ->
      let cmd = Midi.midi_cmd_of_event ev in
Log.p "MIDI INPUT: [%s][Ch:%d, T:%d]\n" (Midi.midi_cmd_to_string cmd)
ev.Midi.channel ev.Midi.ticks;
let _ =
  match cmd with
  | Midi.NoteON (119, velo) ->
      Tracker.set_bpm tracker (2 * velo) ;
      Log.p "set bpm to %d \n" (2 * velo) ;
  | Midi.NoteON (note, velo) -> (
    try 
      Tracker.toggle_playing_track tracker note ;
            with  exn -> Log.p "No track %d\n" note ;
)
  |  _ -> ()
in () ;
(* Printexc.print  Log.p "Event pending !\n"; *)
) ev_list ;
();;
*)
