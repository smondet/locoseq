(******************************************************************************)
(*      Copyright (c) 2007, Sebastien MONDET                                  *)
(*                                                                            *)
(*      Permission is hereby granted, free of charge, to any person           *)
(*      obtaining a copy of this software and associated documentation        *)
(*      files (the "Software"), to deal in the Software without               *)
(*      restriction, including without limitation the rights to use,          *)
(*      copy, modify, merge, publish, distribute, sublicense, and/or sell     *)
(*      copies of the Software, and to permit persons to whom the             *)
(*      Software is furnished to do so, subject to the following              *)
(*      conditions:                                                           *)
(*                                                                            *)
(*      The above copyright notice and this permission notice shall be        *)
(*      included in all copies or substantial portions of the Software.       *)
(*                                                                            *)
(*      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*      EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*      OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*      NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT           *)
(*      HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,          *)
(*      WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING          *)
(*      FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR         *)
(*      OTHER DEALINGS IN THE SOFTWARE.                                       *)
(******************************************************************************)

(** The {b Engine}
 @author S. Mondet
 *)

module HT = Hashtbl
module Seq = JackSequencer

(** Some utilities  *)
module Util = struct

  let remove_once l elt = (
    let already = ref false in
    List.filter (fun e ->
      if !already then
        true
      else if elt = e then
        (already := true; false)
      else 
        true
    ) l
  )

  let new_HT () = HT.create 50

end

(** The Automata of a track (midi or meta) *)
module Automata = struct

  type state = 
    | AS_On         (* *)
    | AS_Off        (* *)
    | AS_SchedOn    (* *)
    | AS_SchedOff   (* *)

  type action =
    | AA_TrackToggle   (* *)
    | AA_TrackOn       (* *)
    | AA_TrackOff      (* *)
    | AA_SchedOn       (* *)
    | AA_SchedOff      (* *)
    | AA_SchedToggle   (* *)
    | AA_TimeForSched  (* *)

  let process_TrackToggle  state = (
    match state with
    | AS_On         -> AS_Off
    | AS_Off        -> AS_On
    | AS_SchedOn    -> AS_On
    | AS_SchedOff   -> AS_Off
  )
  let process_TrackOn      state = (
    match state with
    | AS_On         -> AS_On
    | AS_Off        -> AS_On
    | AS_SchedOn    -> AS_On
    | AS_SchedOff   -> AS_On
  )
  let process_TrackOff     state = (
    match state with
    | AS_On         -> AS_Off
    | AS_Off        -> AS_Off
    | AS_SchedOn    -> AS_Off
    | AS_SchedOff   -> AS_Off
  )
  let process_SchedOn      state = (
    match state with
    | AS_On         -> AS_On
    | AS_Off        -> AS_SchedOn
    | AS_SchedOn    -> AS_SchedOn
    | AS_SchedOff   -> AS_On
  )
  let process_SchedOff     state = (
    match state with
    | AS_On         -> AS_SchedOff
    | AS_Off        -> AS_Off
    | AS_SchedOn    -> AS_Off
    | AS_SchedOff   -> AS_SchedOff
  )
  let process_SchedToggle  state = (
    match state with
    | AS_On         -> AS_SchedOff
    | AS_Off        -> AS_SchedOn
    | AS_SchedOn    -> AS_Off
    | AS_SchedOff   -> AS_On
  )
  let process_TimeForSched state = (
    match state with
    | AS_On         -> AS_On
    | AS_Off        -> AS_Off
    | AS_SchedOn    -> AS_On
    | AS_SchedOff   -> AS_Off
  )
  let process_action current_state action = (
    match action with
    | AA_TrackToggle   -> process_TrackToggle  current_state
    | AA_TrackOn       -> process_TrackOn      current_state
    | AA_TrackOff      -> process_TrackOff     current_state
    | AA_SchedOn       -> process_SchedOn      current_state
    | AA_SchedOff      -> process_SchedOff     current_state
    | AA_SchedToggle   -> process_SchedToggle  current_state
    | AA_TimeForSched  -> process_TimeForSched current_state
  )

end

module MidiEvent = struct

  type midi_event = {
    mutable e_tiks   : int ;
    mutable e_stat   : int ;
    mutable e_chan   : int ;
    mutable e_dat1   : int ;
    mutable e_dat2   : int ;
  }
  let of_midi mev = {
    e_tiks = mev.Midi.ticks  ;
    e_stat = mev.Midi.status ;
    e_chan = mev.Midi.channel;
    e_dat1 = mev.Midi.data_1 ;
    e_dat2 = mev.Midi.data_2 ;
  }
  let create tiks stat chan dat1 dat2 = {
    e_tiks = tiks;
    e_stat = stat;
    e_chan = chan;
    e_dat1 = dat1;
    e_dat2 = dat2;
  }
  let to_midi tracker_event = {
    Midi.ticks   = tracker_event.e_tiks;
    Midi.status  = tracker_event.e_stat;
    Midi.channel = tracker_event.e_chan;
    Midi.data_1  = tracker_event.e_dat1;
    Midi.data_2  = tracker_event.e_dat2;
  }
  let copy_midi_event e = {
    e_tiks = e.e_tiks;
    e_stat = e.e_stat;
    e_chan = e.e_chan;
    e_dat1 = e.e_dat1;
    e_dat2 = e.e_dat2;
  }
end

module MetaEvent = struct

  type meta_action = 
    | TrackOn of int
    | TrackOff of int
    | SetBPM of int
    | TrackKeepOn of int

  type meta_event = {
    mutable ev_ticks : int * int ;
    mutable ev_action : meta_action ;
  }

  type meta_action_spec = [
    | `track_set_on of int * int 
    | `track_set_off of int * int 
    | `set_bpm of int * int
    | `track_on of int * int * int 
  ]

  let meta_event_of_action_spec (spec:meta_action_spec) = 
    match spec with
    | `track_set_on  (i , tk) -> {ev_ticks = i,i; ev_action = TrackOn tk;}
    | `track_set_off (i , tk) -> {ev_ticks = i,i; ev_action = TrackOff tk;}
    | `set_bpm      (i , bpm) -> {ev_ticks = i,i; ev_action = SetBPM bpm;}
    | `track_on    (b, e, tk) -> {ev_ticks = b,e; ev_action = TrackKeepOn tk;}

  let meta_events_of_meta_actions action_list = (
    List.rev (List.rev_map meta_event_of_action_spec action_list)
  )
  let is_a_range_action = function
    | TrackKeepOn _ -> true
    | _ -> false

  (** Some functions for input specification manipulation *)
  module MetaSpecs = struct
    let spec_to_string spec = ( 
      let spr = Printf.sprintf in
      match spec with
      | `track_set_on  (_,id) -> spr "Track %d On" id
      | `track_set_off (_,id) -> spr "Track %d Off" id
      | `set_bpm       (_,b ) -> spr "Set BPM = %d" b
      | `track_on    (_,_,id) -> spr "Keep Track [%d] ON " id
    )
    let spec_to_tick = (function
      | `track_set_on  (t, _) | `track_set_off (t, _) | `set_bpm (t, _) -> t
      | `track_on       _     ->
          failwith ( "MetaEvent.MetaSpecs.spec_to_tick " ^
          "does not accept track_on meta-events!")
    )
    let spec_to_range = (function
      | `track_on       (b,e,_) -> (b,e)
      | spec -> failwith ( 
        "MetaUtil.spec_to_range does not accept" ^
        (spec_to_string spec) ^ " meta-event"
      )
    )
  end


end

module Tracks = struct
  type track = {

    mutable t_state: Automata.state;
    mutable t_prev_state: Automata.state;
    mutable t_start_tick: int;

    mutable t_track_length: int;
    mutable t_sched_length: int;

    mutable t_name: string;
  }
  let new_track name tk_length sched_length = {
    t_state = Automata.AS_Off;
    t_prev_state = Automata.AS_Off;
    t_start_tick = -1;
    t_track_length = tk_length;
    t_sched_length = sched_length;
    t_name = name;
  }

  type midi_track = {
    mutable mi_track: track;
    mutable mi_events: MidiEvent.midi_event list;
    mutable mi_outport: int;
  }
  let make_midi_track  name tk_length sched_length outport events = {
    mi_track = new_track name tk_length sched_length;
    mi_outport = outport;
    mi_events = events;
  }

  type meta_track = {
    mutable me_track: track;
    mutable me_events: MetaEvent.meta_event list;
  }
  let make_meta_track name tk_length sched_length action_list = {
    me_track = new_track name tk_length sched_length;
    me_events = MetaEvent.meta_events_of_meta_actions action_list;
  }


  let midi_name       track = track.mi_track.t_name
  let meta_name       track = track.me_track.t_name
  let midi_track_lgth track = track.mi_track.t_track_length
  let meta_track_lgth track = track.me_track.t_track_length
  let midi_sched_lgth track = track.mi_track.t_sched_length
  let meta_sched_lgth track = track.me_track.t_sched_length

  let set_midi_name       track v = track.mi_track.t_name         <- v
  let set_meta_name       track v = track.me_track.t_name         <- v
  let set_midi_track_lgth track v = track.mi_track.t_track_length <- v
  let set_meta_track_lgth track v = track.me_track.t_track_length <- v
  let set_midi_sched_lgth track v = track.mi_track.t_sched_length <- v
  let set_meta_sched_lgth track v = track.me_track.t_sched_length <- v

  (* The state: *)
  let midi_state      track = track.mi_track.t_state
  let meta_state      track = track.me_track.t_state
  let midi_prev_state track = track.mi_track.t_prev_state
  let meta_prev_state track = track.me_track.t_prev_state
  let midi_start_tick track = track.mi_track.t_start_tick
  let meta_start_tick track = track.me_track.t_start_tick
  let set_midi_state track v = (
    track.mi_track.t_prev_state <- track.mi_track.t_state;
    track.mi_track.t_state <- v;
  )
  let update_midi_previous track =
    track.mi_track.t_prev_state <- track.mi_track.t_state
  let set_meta_state track v = (
    track.me_track.t_prev_state <- track.me_track.t_state;
    track.me_track.t_state <- v;
  )
  let update_meta_previous track =
    track.me_track.t_prev_state <- track.me_track.t_state
  let set_midi_start_tick track v = track.mi_track.t_start_tick <- v
  let set_meta_start_tick track v = track.me_track.t_start_tick <- v
end

type tracker_engine = {
  mutable t_sequencer : Seq.sequencer ;
  mutable t_midi_tracks : (int,Tracks.midi_track) Hashtbl.t;
  mutable t_meta_tracks : (int,Tracks.meta_track) Hashtbl.t;
  mutable t_ppqn: int;
  mutable t_bpm: int;
  mutable t_queue_delay: int;
  mutable t_timer_ticks: int;
  mutable t_is_playing: bool;
  mutable t_do_before: tracker_engine -> unit;
  mutable t_do_after:  tracker_engine -> unit;
}

module ManageTracks = struct
  module HT = Hashtbl

  let midi_new_id trkr = (
    let id = ref 1 in
    try (
      while true do
        ignore(HT.find trkr.t_midi_tracks !id);
        incr id;
      done;
      !id
    ) with Not_found -> !id
  )
  let meta_new_id trkr = (
    let id = ref (-1) in
    try (
      while true do
        ignore(HT.find trkr.t_meta_tracks !id);
        decr id;
      done;
      !id
    ) with Not_found -> !id
  )
  let is_midi index = (index > 0)

  let midi_get_track trkr id = HT.find trkr.t_midi_tracks id
  let meta_get_track trkr id = HT.find trkr.t_meta_tracks id

  let midi_set_track trkr id mtk = HT.replace trkr.t_midi_tracks id mtk
  let meta_set_track trkr id mtk = HT.replace trkr.t_meta_tracks id mtk

  let midi_iteri app func = (HT.iter  func  app.t_midi_tracks)
  let meta_iteri app func = (HT.iter  func  app.t_meta_tracks)

  let get_midi_tracks_number tr = HT.length tr.t_midi_tracks
  let get_meta_tracks_number tr = HT.length tr.t_meta_tracks 

  let remove_midi_track tr id = HT.remove tr.t_midi_tracks id 
  let remove_meta_track tr id = HT.remove tr.t_meta_tracks id 

  let add_midi_track tr mtk = (
    let the_id = midi_new_id tr in HT.add tr.t_midi_tracks the_id mtk; the_id
  )

  let add_meta_track tr mtk = (
    let the_id = meta_new_id tr in HT.add tr.t_meta_tracks the_id mtk; the_id
  )

  let add_or_replace_midi_track tr id mtk = (
    HT.remove tr.t_midi_tracks id ; HT.add tr.t_midi_tracks id mtk;
  )
  let add_or_replace_meta_track tr id mtk = (
    HT.remove tr.t_meta_tracks id ; HT.add tr.t_meta_tracks id mtk;
  )

end

let make_engine ~pqn ~bpm ~sequencer ~before ~after = {
  t_sequencer    = sequencer   ;  
  t_midi_tracks  = Util.new_HT ();  
  t_meta_tracks  = Util.new_HT ();  
  t_ppqn         = pqn         ;  
  t_bpm          = bpm         ;  
  t_queue_delay  = 4           ;  
  t_timer_ticks  = 4           ;  
  t_is_playing   = false       ;
  t_do_before    = before      ;
  t_do_after     = after       ;
}

(** Do some controls and optimizations *)
let compile trkr = (
  let module T = Tracks in
  let module Mi = MidiEvent in
  ManageTracks.midi_iteri trkr (fun id track ->
    List.iter (fun midi_ev ->
      let tk_length = Tracks.midi_track_lgth track in
      if midi_ev.Mi.e_tiks >= tk_length then (
        Log.warn
        "Detected a midi event with ticks after end of track %d\n" id;
        let old = midi_ev.Mi.e_tiks in
        midi_ev.Mi.e_tiks <- midi_ev.Mi.e_tiks mod tk_length;
        Log.log "Corrected with modulo: %d -> %d\n" old midi_ev.Mi.e_tiks;
      );
    ) track.Tracks.mi_events;
  );
)


module Services = struct 

  let get_midi_tracks_infos tr = (
    let module T = Tracks in
    let res =
      Array.create (ManageTracks.get_midi_tracks_number tr) (0,"",0,0) in
    let index = ref 0 in
    ManageTracks.midi_iteri tr (
      fun i tk ->
        res.(!index)
        <- (i, T.midi_name tk, tk.T.mi_outport, T.midi_track_lgth tk);
        incr index;
    );
    res
  )
  let get_meta_tracks_infos tr = (
    let l = ManageTracks.get_meta_tracks_number tr in 
    let res = Array.create l (0,"",0,0) in
    let index = ref 0 in
    ManageTracks.meta_iteri tr (
      fun i tk ->
        res.(l - !index - 1)
        <- (i, Tracks.meta_name tk, -1 , Tracks.meta_track_lgth tk);
        incr index ;
    );
    res
  )

  let get_meta_track_actions trkr id =
    let module M = MetaEvent in
    List.rev (List.rev_map (fun ev ->
      match ev.M.ev_action with
      | M.TrackOn id -> `track_set_on ((fst ev.M.ev_ticks), id)
      | M.TrackOff id -> `track_set_off ((fst ev.M.ev_ticks), id)
      | M.SetBPM bpm -> `set_bpm  ((fst ev.M.ev_ticks), bpm)
      | M.TrackKeepOn id ->
          `track_on ((fst ev.M.ev_ticks), (snd ev.M.ev_ticks), id)
    ) (ManageTracks.meta_get_track trkr id).Tracks.me_events)

  let get_midi_track_events trkr id =
    (ManageTracks.midi_get_track trkr id).Tracks.mi_events 

  let remove_midi_event_from_track trkr id midi_ev = (
    let track = (ManageTracks.midi_get_track trkr id) in
    track.Tracks.mi_events <- Util.remove_once track.Tracks.mi_events midi_ev;
  )
  let remove_meta_event_from_track trkr id meta_ev_spec = (
    let track = (ManageTracks.meta_get_track trkr id) in
    let meta_ev = MetaEvent.meta_event_of_action_spec meta_ev_spec in
    track.Tracks.me_events  <- Util.remove_once track.Tracks.me_events meta_ev;
  )

  let add_midi_event_to_track trkr id midi_ev = (
    let track = (ManageTracks.midi_get_track trkr id) in
    track.Tracks.mi_events  <- midi_ev :: track.Tracks.mi_events;
  )
  let add_meta_event_to_track trkr id meta_ev_spec = (
    let track = (ManageTracks.meta_get_track trkr id) in
    let meta_ev = MetaEvent.meta_event_of_action_spec meta_ev_spec in
    track.Tracks.me_events  <- meta_ev :: track.Tracks.me_events;
  )
  let add_midi_tracks  trkr midi_tracks = (
    Array.iteri (fun index midi_track ->
      let name = ref "Untitled" in
      let length = ref 0 in
      let tk_cur = ref 0 in
      let tmp_list = ref [] in
      Array.iter (
        function 
          | Midi.MidiEvent mev ->
              let seq_ev = Midi.empty_midi_event () in
              tk_cur := !tk_cur + mev.Midi.ticks; 
              seq_ev.Midi.ticks   <-  !tk_cur;
              seq_ev.Midi.status  <- (mev.Midi.status land 0xF0);
              seq_ev.Midi.channel <- mev.Midi.channel;
              seq_ev.Midi.data_1  <- mev.Midi.data_1;
              seq_ev.Midi.data_2  <- mev.Midi.data_2;
              tmp_list := seq_ev::!tmp_list ;
          | Midi.MetaEvent me when me.Midi.service_id = 0x03 ->
              name := Midi.basic_data_to_string me.Midi.service_data;
              tk_cur := !tk_cur + me.Midi.meta_ticks;
          | Midi.MetaEvent me when me.Midi.service_id = 0x2f ->
              (* End Of Track ! *)
              tk_cur := !tk_cur + me.Midi.meta_ticks;
              length := !tk_cur;
          | Midi.MetaEvent me ->
              (* Debate: do we count ticks for unknown meta events ? *)
              tk_cur := !tk_cur + me.Midi.meta_ticks;
          | _ -> ()
      ) midi_track.Midi.events ;
      let the_new_track =
        Tracks.make_midi_track !name !length !length 0
        (List.rev_map (MidiEvent.of_midi) !tmp_list) in
      HT.add trkr.t_midi_tracks (ManageTracks.midi_new_id trkr) the_new_track;
    ) midi_tracks ;
    compile trkr;
  )

  let clear_tracker trkr = (
    trkr.t_midi_tracks <- Util.new_HT () ;
    trkr.t_meta_tracks <- Util.new_HT () ;
  )

  let set_ppqn tr ppqn = tr.t_ppqn <- ppqn
  let get_ppqn tr = tr.t_ppqn

  let get_midi_track_infos tr id = 
    let tk = ManageTracks.midi_get_track tr id in
    (Tracks.midi_name tk, tk.Tracks.mi_outport,
    Tracks.midi_track_lgth tk, Tracks.midi_sched_lgth tk)
    
  let set_midi_track_infos tr id name port lgth sched = (
    let tk = ManageTracks.midi_get_track tr id in
    Tracks.set_midi_name tk name;
    Tracks.set_midi_track_lgth tk lgth;
    Tracks.set_midi_sched_lgth tk sched;
    tk.Tracks.mi_outport <- port;
  )

  let get_meta_track_infos tr id = 
    let tk = ManageTracks.meta_get_track tr id in
    (Tracks.meta_name tk, Tracks.meta_track_lgth tk, Tracks.meta_sched_lgth tk)

  let set_meta_track_infos tr id name lgth sched = (
    let tk = ManageTracks.meta_get_track tr id in
    Tracks.set_meta_name tk name;
    Tracks.set_meta_track_lgth tk lgth;
    Tracks.set_meta_sched_lgth tk sched;
  )

  let replace_meta_track tr id mtk = ManageTracks.meta_set_track tr id mtk 

  let get_track_stat trkr id =
    if (ManageTracks.is_midi id) then (
      let tk = ManageTracks.midi_get_track trkr id in
      Tracks.midi_state tk
    ) else (
      let tk = ManageTracks.meta_get_track trkr id in
      Tracks.meta_state tk
    )

end

module XML_IO = struct
  let soi = string_of_int
  let ios = int_of_string 

  let xml_tracker = "tracker"
  let xml_meta_tracks = "meta_tracks"
  let xml_pqn = "pqn"
  let xml_bpm = "bpm"
  let xml_queue_delay = "queue_delay"
  let xml_timer_ticks = "timer_ticks"

  let xml_midi_event = "midi_event" 
  let xml_midi_track = "midi_track" 
  let xml_midi_tracks= "midi_tracks"
  let xml_meta_event = "meta_event" 

  let xml_id = "id"
  let xml_name = "name"
  let xml_track_length = "track_lgth"
  let xml_sched_length = "sched_lgth"
  let xml_outport = "outport"

  let xml_tcks = "tcks"
  let xml_stat = "stat"
  let xml_chan = "chan"
  let xml_dat1 = "dat1"
  let xml_dat2 = "dat2"

  let xml_begin          = "begin"
  let xml_end            = "end"
  let xml_action         = "action"
  let xml_arg1           = "arg1"
  let xml_track_set_on   = "track_set_on"
  let xml_track_set_off  = "track_set_off"
  let xml_set_bpm        = "set_bpm"
  let xml_track_keep_on  = "track_keep_on"

  let to_xml tr = (
    let module MT = ManageTracks in
    let module Mi = MidiEvent in
    let module Tk = Tracks in
    let module X = Xml in
    let l_xml_midi_tracks = ref [] in
    MT.midi_iteri tr (fun id tk ->
      let xml_events = List.rev_map (
        fun event ->
          X.Element (xml_midi_event , [
            (xml_tcks, soi event.Mi.e_tiks);
            (xml_stat, soi event.Mi.e_stat);
            (xml_chan, soi event.Mi.e_chan);
            (xml_dat1, soi event.Mi.e_dat1);
            (xml_dat2, soi event.Mi.e_dat2); 
          ] , [] )
      ) tk.Tk.mi_events in
      let xml_track = X.Element (xml_midi_track , [
        (xml_id, soi id);
        (xml_name , Tk.midi_name tk);
        (xml_track_length, soi (Tk.midi_track_lgth tk));
        (xml_sched_length, soi (Tk.midi_sched_lgth tk));
        (xml_outport, soi tk.Tk.mi_outport) ;
      ] , List.rev xml_events ) in
      l_xml_midi_tracks := xml_track::!l_xml_midi_tracks ;
    );
    let xml_all_midi_tracks =
      X.Element (xml_midi_tracks, [], List.rev !l_xml_midi_tracks) in
    let l_xml_meta_tracks = ref [] in
    MT.meta_iteri tr (fun id tk ->
      let xml_events = List.rev_map (fun event ->
        let b,e = event.MetaEvent.ev_ticks in
        let act_str,arg = 
          match event.MetaEvent.ev_action with
          | MetaEvent.TrackOn o ->  (xml_track_set_on  , o)
          | MetaEvent.TrackOff o -> (xml_track_set_off , o)
          | MetaEvent.SetBPM o   -> (xml_set_bpm   , o)
          | MetaEvent.TrackKeepOn o -> (xml_track_keep_on ,o)
          in
          X.Element (xml_meta_event , [
            (xml_begin, soi b); (xml_end, soi e) ;
            (xml_action , act_str) ; (xml_arg1 , soi arg)
          ] , [] )
      ) tk.Tk.me_events in
      let xml_track = X.Element ("meta_track" , [
        (xml_id, soi id);
        (xml_name , Tk.meta_name tk);
        (xml_track_length, soi (Tk.meta_track_lgth tk));
        (xml_sched_length, soi (Tk.meta_sched_lgth tk));
      ] , List.rev xml_events ) in
      l_xml_meta_tracks := xml_track::!l_xml_meta_tracks ;
    );
    let xml_all_meta_tracks =
      X.Element (xml_meta_tracks, [], !l_xml_meta_tracks) in
    X.Element (xml_tracker , [
      (xml_pqn, soi tr.t_ppqn); (xml_bpm, soi tr.t_bpm);
      (xml_queue_delay, soi tr.t_queue_delay);
      (xml_timer_ticks, soi tr.t_timer_ticks) 
    ] , [ xml_all_midi_tracks ; xml_all_meta_tracks ])
  )

  let load_xml trkr xml = (
    let module X = Xml in
    if (X.tag xml) <> xml_tracker then (
      failwith ("Expecting a tracker tag and got: " ^ (X.tag xml))
    );

    trkr.t_ppqn        <- ios (X.attrib xml xml_pqn );
    trkr.t_bpm         <- ios (X.attrib xml xml_bpm);
    trkr.t_queue_delay <- ios (X.attrib xml xml_queue_delay);
    trkr.t_timer_ticks <- ios (X.attrib xml xml_timer_ticks);

    trkr.t_midi_tracks <- Util.new_HT () ;
    trkr.t_meta_tracks <- Util.new_HT () ;

    X.iter (fun child ->
      match X.tag child with
      | s when s = xml_midi_tracks -> (
        X.iter (fun mtk ->
          let the_id = ios (X.attrib mtk xml_id) in
          let the_name    =     (X.attrib mtk xml_name) in
          let the_tick_nb = ios (X.attrib mtk xml_track_length) in
          let the_schd_tk = ios (X.attrib mtk xml_sched_length) in
          let the_outport = ios (X.attrib mtk xml_outport) in
          let the_rev_events =
            List.rev_map (fun xev ->
              MidiEvent.create (ios (X.attrib xev xml_tcks))
              (ios (X.attrib xev xml_stat)) (ios (X.attrib xev xml_chan))
              (ios (X.attrib xev xml_dat1)) (ios (X.attrib xev xml_dat2))
            ) (X.children mtk) in
          let the_track =
            Tracks.make_midi_track the_name the_tick_nb the_schd_tk
            the_outport (List.rev the_rev_events) in
          ManageTracks.add_or_replace_midi_track trkr the_id the_track;
        ) child ;
      )
      | s when s = xml_meta_tracks -> (
        X.iter (
          fun mtk ->
            let the_name = X.attrib mtk xml_name in
            let the_id = ios (X.attrib mtk xml_id) in
            let the_tick_nb = ios (X.attrib mtk xml_track_length) in
            let the_schd_tk = ios (X.attrib mtk xml_sched_length) in
            let the_rev_events =
              List.rev_map (fun xev ->
                let b = ios (X.attrib xev xml_begin) in
                let e = ios (X.attrib xev xml_end) in
                let action = X.attrib xev xml_action in
                let arg = ios (X.attrib xev xml_arg1) in
                match action with
                | s when s = xml_track_set_on  -> `track_set_on  (b, arg)
                | s when s = xml_track_set_off -> `track_set_off (b, arg)
                | s when s = xml_set_bpm       -> `set_bpm       (b, arg)
                | s when s = xml_track_keep_on -> `track_on (b,e, arg)
                | s -> failwith ("Unreocgnizable meta_event: "^s)
              ) (X.children mtk) in
            let the_track =
              Tracks.make_meta_track
              the_name the_tick_nb the_schd_tk (List.rev the_rev_events) in
            ManageTracks.add_or_replace_meta_track trkr the_id the_track;
        ) child ;
      )
      | s ->
          failwith ("Unknown tag while expecting (midi_ or meta_)tracks: " ^ s);
    ) xml ;
    compile trkr;
  )
end


(** Some {i printfs} for debug *)
module Debug = struct

  let print_info chan tr = (
    let pr = Printf.fprintf chan in
    let l = ManageTracks.get_midi_tracks_number tr in
    pr 
    "[Tracker: midi_tracks:%d, bpm:%d,\
    ppqn:%d, queue_delay:%d, timer_ticks:%d]\n"
    l tr.t_bpm tr.t_ppqn tr.t_queue_delay tr.t_timer_ticks;

    ManageTracks.midi_iteri tr (fun i tk ->
      Printf.fprintf chan
      "  [TK %3d] [midi:%2d] [%-.20s] [%5d events, %4d ticks, sched on %d]\n%!"
      i tk.Tracks.mi_outport (Tracks.midi_name tk)
      (List.length tk.Tracks.mi_events)
      (Tracks.midi_track_lgth tk) (Tracks.midi_sched_lgth tk);
    );
    ManageTracks.meta_iteri tr (fun i tk ->
      Printf.fprintf chan
      "  [TK %3d] [ meta   ] [%-.20s] [%5d events, %4d ticks, sched on %d]\n%!" 
      i (Tracks.meta_name tk) (List.length tk.Tracks.me_events)
      (Tracks.meta_track_lgth tk) (Tracks.meta_sched_lgth tk);
    );
  )

end

module RTControl = struct

  let _apply_action_to_track act tr tk = (
    if ManageTracks.is_midi tk then (
      let tk = ManageTracks.midi_get_track tr tk in
      let cur_state = Tracks.midi_state tk in
      Tracks.set_midi_state tk (Automata.process_action cur_state act)
    ) else (
      let tk = ManageTracks.meta_get_track tr tk in
      let cur_state = Tracks.meta_state tk in
      Tracks.set_meta_state tk (Automata.process_action cur_state act)
    );
  )

  let toggle_playing_track = _apply_action_to_track Automata.AA_TrackToggle
  let set_playing_track    = _apply_action_to_track Automata.AA_TrackOn
  let set_stopping_track   = _apply_action_to_track Automata.AA_TrackOff
  let schedule_play        = _apply_action_to_track Automata.AA_SchedOn
  let schedule_stop        = _apply_action_to_track Automata.AA_SchedOff
  let schedule_toggle      = _apply_action_to_track Automata.AA_SchedToggle

  let set_all_stopping tr = (
    (* TODO: can be optimized: there are two Hashtbl lookups per track *)
    ManageTracks.midi_iteri tr (fun i _ -> set_stopping_track tr i);
    ManageTracks.meta_iteri tr (fun i _ -> set_stopping_track tr i);
  )

  let is_tracker_playing tr = tr.t_is_playing

  let stop tr = (
    tr.t_is_playing <- false;
  )

  let set_bpm tr bpm = (
    tr.t_bpm <- bpm;
    (* if tr.t_is_playing then ( *)
      (* Seq.set_queue_tempo tr.t_sequencer tr.t_bpm tr.t_ppqn *)
    (* ); *)
  )
  let add_to_bpm tr value = (
    tr.t_bpm <- tr.t_bpm + value;
    (* if tr.t_is_playing then ( *)
      (* Seq.set_queue_tempo tr.t_sequencer tr.t_bpm tr.t_ppqn *)
    (* ); *)
  )

  module PlayUtil = struct

    let is_crossing_the_end  lgth (t_b,t_e) = (
      let t_bm = t_b mod lgth in
      let t_em = t_e mod lgth in
      (* XXX RFC: why the 2nd part: *)
      (t_em < t_bm) || (t_bm = 0)
    )

    module Times = struct
      let interval_intersection (m_b,m_e) (t_b,t_e) t_modulo = (
        let t_bm = t_b mod t_modulo in
        let t_em = t_e mod t_modulo in
        Log.warn "interval_intersection: %s\n"
        "If this is called, tracks scheduling is incoherent\n";
        if t_em < t_bm then (
          (* [t_b,t_e] intersects the loop_tick of the meta track so  *)
          (* [0 , t_em] and [ t_bm, t_modulo ] *)
          ( m_b <= t_em ) || ( t_bm <= m_e )
        ) else (
          (* [t_bm,t_em] is in the track interval *)
          ( t_bm <= m_b && m_b <= t_em ) || ( t_bm <= m_e && m_e <= t_em ) ||
          ( m_b <= t_bm && t_bm <= m_e ) || ( m_b <= t_em && t_em <= m_e ) 
        );
      )

      let tick_in_interval m_tik  (t_b,t_e) t_modulo = (
        let t_bm = t_b mod t_modulo in
        let t_em = t_e mod t_modulo in
        Log.warn "tick_in_interval: %s\n"
        "If this is called, tracks scheduling is incoherent\n";
        if t_em < t_bm then (
          (* [t_b,t_e] intersects the loop_tick of the meta track so  *)
          (* [0 , t_em] and [ t_bm, t_modulo ] *)
          ( m_tik <= t_em ) || ( t_bm <= m_tik )
        ) else (
          (* [t_bm,t_em] is in the track interval *)
          (t_bm <= m_tik && m_tik <= t_em)
        )
      )

      let int_just_over_division n d =
        (n / d) + (if n mod d = 0 then 0 else 1)

      let is_it_time_for_schedule ~current:(tik_b, tik_e) sched_length = (
        let nb_of_sched_intervals =
          int_just_over_division tik_b sched_length in
        let current_sched = nb_of_sched_intervals * sched_length in
        (tik_b <= current_sched) && (current_sched <= tik_e)
      )

      (** Returns a boolean: {i should the event be played}  *)
      let should_play_event ~current:(tik_b, tik_e)
      ~started_time ~loop_length ~event_tick = (
        (** 
        {[ tik_b <= started_time + event_tick + k * loop_length <= tik_e ]}
        *)
        let arbitrary_delta = 
          loop_length *
          (int_just_over_division (started_time + event_tick) loop_length)
        in
        let diff_b = tik_b - started_time - event_tick + arbitrary_delta in
        let diff_e = tik_e - started_time - event_tick + arbitrary_delta in
        (** now the problem: [ extisting k where diff_b <= k*loop <= diff_e ]
            ([arbitrary_delta] is a multiple of k used make all positive) *)
        if diff_b mod loop_length = 0
        then true (** [ diff_b / loop_length ] is a solution *)
        else (
          if diff_e mod loop_length = 0
          then true (** [diff_e / loop_length] is a solution *)
          else (
            (** Now
            [possibles = N <inter> \] (diff_b. /. loop.), (diff_e. /. loop.) \[]
                ... so let's compute [|possibles|] 
            *)
            (* if diff_b <= 0 || diff_e <= 0 then *)
            (* Log.p "card: %d cur:%d,%d l:%d\n" *)
            (* ((diff_e / loop_length) - (diff_b / loop_length)) *)
            (* diff_b diff_e loop_length; *)
            (diff_e / loop_length) - (diff_b / loop_length)  > 0
          )
        )
        (* in *)
      (* if diff_b <= 0 || diff_e <= 0 then *)
      (* Log.p "[%d,%d] loop:%d started_time:%d event:%d => %b\n" *)
      (* tik_b tik_e loop_length started_time event_tick should; *)
      )
        
      (* let is_it_time_for_loop ~current:(tik_b, tik_e) *)
      (* ~started_time ~loop_length = ( *)
        (* tik_b <= started_time + k * loop_length <= tik_e *)
        (* should_play_event ~current:(tik_b, tik_e) ~started_time *)
        (* ~loop_length ~event_tick:0 *)
      (* ) *)

      type set_started_method = SyncOnLength | SyncOnSched | NoSync

      (* For now a global variable: *)
      let current_method = ref SyncOnSched

      let get_start_tick ~play_tick ~sched_length =
        match !current_method with
        | SyncOnSched -> (play_tick / sched_length) * sched_length
        | NoSync -> play_tick
        | SyncOnLength -> 0

    end

    let on_meta_track_off tr track prev_tick cur_tick = (
      List.iter (fun ev ->
        match ev.MetaEvent.ev_action with
        | MetaEvent.TrackOff tk_id ->
            set_stopping_track tr tk_id;
        | MetaEvent.TrackKeepOn tk_id ->
            (* TODO That's FALSE !!! *)
            if (
              Times.interval_intersection ev.MetaEvent.ev_ticks
              (prev_tick + 1, cur_tick) (Tracks.meta_track_lgth track) 
            ) then (
              set_stopping_track tr tk_id;
            );
        | _ -> ()
      ) track.Tracks.me_events;
    )
    let when_meta_track_plays tr track prev_tick cur_tick = (
      let module M = MetaEvent in
      let loop_length = Tracks.meta_track_lgth track in
      let started_time = Tracks.meta_start_tick track in
      let current = (prev_tick + 1, cur_tick) in
      List.iter (fun ev ->
        (* if interval_intersection ev.M.ev_ticks interval track_length then ( *)
        if M.is_a_range_action ev.M.ev_action then (

          (* TODO *)
          (*
          let is_end_in =
            tick_in_interval (snd ev.M.ev_ticks) interval track_length in
          let is_beg_in =
            tick_in_interval (fst ev.M.ev_ticks) interval track_length in
          if not is_beg_in && is_end_in then (
            set_stopping_track tr trk;
          );
          if not is_end_in || (is_beg_in && is_end_in) then (
            set_playing_track tr trk;
          );
          *)
          (* TODO: see if both conditions are sufficient *)
        ) else (
          (* ONE TICK EVENTS: *)
          if (
            Times.should_play_event
            ~current ~loop_length ~started_time ~event_tick:(fst ev.M.ev_ticks)
          ) then (
            begin match ev.M.ev_action with
            | M.TrackOn trk -> set_playing_track tr trk;
            | M.TrackOff tk -> set_stopping_track tr tk;
            | M.SetBPM x -> set_bpm tr x;
            | _ -> failwith "when_meta_track_plays: don't that action !!"
            end;
          );
        );
      ) track.Tracks.me_events;

    )

    let play_meta_events tr prev_tick cur_tick = (
      let module T = Tracks in
      let module A = Automata in
      ManageTracks.meta_iteri tr (fun id tk ->

        let cur_state = T.meta_state tk in
        let prev_state = T.meta_prev_state tk in

        (* Do the schedules: *)
        if (
          Times.is_it_time_for_schedule
          ~current:(prev_tick + 1, cur_tick) (T.meta_sched_lgth tk)
        ) then (
          T.set_meta_state tk (A.process_TimeForSched cur_state);
        );

        (* Set the start time of the track if just played *)
        if cur_state = A.AS_On && prev_state != A.AS_On then (
          let start_time =
            Times.get_start_tick ~play_tick:cur_tick
            ~sched_length:(Tracks.meta_sched_lgth tk) in
          Tracks.set_meta_start_tick tk start_time;
          Tracks.update_meta_previous tk;
        );
        (* If track just comes off, we do some stuff -> parametrize ? *)
        if cur_state = A.AS_Off && prev_state != A.AS_Off then (
          on_meta_track_off tr tk prev_tick cur_tick;
          Tracks.update_meta_previous tk;
        );

        (* If he is playing, we process his events: *)
        if cur_state = A.AS_On || cur_state = A.AS_SchedOff then (
          when_meta_track_plays tr tk prev_tick cur_tick;
        );

      );
    )

    let fill_and_send_event tracker_event preallocated_midi seq port tick = (
      preallocated_midi.Midi.ticks   <- tick;
      preallocated_midi.Midi.status  <- tracker_event.MidiEvent.e_stat;
      preallocated_midi.Midi.channel <- tracker_event.MidiEvent.e_chan;
      preallocated_midi.Midi.data_1  <- tracker_event.MidiEvent.e_dat1;
      preallocated_midi.Midi.data_2  <- tracker_event.MidiEvent.e_dat2;
      (* Seq.output_event_direct seq port preallocated_midi; *)
      (* Seq.put_event_in_queue seq port preallocated_midi; *)
      Seq.output_event seq ~port 
      ~stat:tracker_event.MidiEvent.e_stat
      ~chan:tracker_event.MidiEvent.e_chan
      ~dat1:tracker_event.MidiEvent.e_dat1
      ~dat2:tracker_event.MidiEvent.e_dat2;
    )

    let on_midi_track_off tr track prev_tick cur_tick prealloc = (
      List.iter (fun ev ->
        (* Hack: we send all the NoteOFF we find... *)
        if ev.MidiEvent.e_stat = 0x80 then (
          fill_and_send_event ev prealloc tr.t_sequencer track.Tracks.mi_outport
          (cur_tick + tr.t_queue_delay - 1);
          (* TODO comment the above formula *)
        );
      ) track.Tracks.mi_events;
    )

    let when_midi_track_plays tr track prev_tick cur_tick prealloc = (
      let loop_length = (Tracks.midi_track_lgth track) in
      let started_time = Tracks.midi_start_tick track in
      let current = (prev_tick + 1, cur_tick) in
      List.iter (fun ev ->
        if 
          Times.should_play_event ~current ~started_time
          ~loop_length ~event_tick:ev.MidiEvent.e_tiks
        then (
          fill_and_send_event ev prealloc tr.t_sequencer track.Tracks.mi_outport
          ((prev_tick + 1) + tr.t_queue_delay - 1);
          (* TODO comment the above formula *)
        );
      ) track.Tracks.mi_events;
    )

    let play_midi_events preallocated_midi_event tr prev_tick cur_tick = (
      let module T = Tracks in
      let module A = Automata in
      ManageTracks.midi_iteri tr (fun id tk ->
        let cur_state = T.midi_state tk in
        let prev_state = T.midi_prev_state tk in

        (* Do the schedules: *)
        if (
          Times.is_it_time_for_schedule
          ~current:(prev_tick + 1, cur_tick) (T.midi_sched_lgth tk)
        ) then (
          T.set_midi_state tk (A.process_TimeForSched cur_state);
        );

        (* Set the start time of the track if just played *)
        if cur_state = A.AS_On && prev_state != A.AS_On then (
          let start_time =
            Times.get_start_tick ~play_tick:cur_tick
            ~sched_length:(Tracks.midi_sched_lgth tk) in
          Tracks.set_midi_start_tick tk start_time;
          Tracks.update_midi_previous tk;
          Log.p "Start time: %d(cur: %d)\n" start_time cur_tick;
        );

        (* If track just comes off, we do some stuff -> parametrize ? *)
        if cur_state = A.AS_Off && prev_state != A.AS_Off then (
          on_midi_track_off tr tk prev_tick cur_tick
          preallocated_midi_event;
          Tracks.update_midi_previous tk;
        );

        (* If he is playing, we process his events: *)
        if cur_state = A.AS_On || cur_state = A.AS_SchedOff then (
          when_midi_track_plays tr tk prev_tick cur_tick
          preallocated_midi_event;
        );

      );
    )
  end

  let play ~on_end tr = (
    let module Pl = PlayUtil in
    tr.t_is_playing <- true;

    let previous_tick = ref (-1) in

    (* Optimization: functions shall use the midi event so that they don't have
     * to allocate one *)
    let preallocated_midi_event = Midi.empty_midi_event () in

    let loop = ref tr.t_is_playing in
    let willstop = ref 0 in

    let begining =  (1000000. *. (Unix.gettimeofday ())) in
    let millisecs () =
      int_of_float (
        (1000000.  *. (Unix.gettimeofday ())) -. begining
      ) in
    let previous_time = ref (millisecs ()) in

    let max_ticks_to_manage = ref 0 in

    while !loop do
      (* Pl.wait_for_alarm_clock tr my_timer; *)
      Thread.delay (0.001 *. (float tr.t_timer_ticks));
      tr.t_do_before tr;

      (* let cur_tk = Seq.get_current_tick tr.t_sequencer in *)
      let delta = (millisecs ()) - !previous_time in
      previous_time := !previous_time + delta;
      (* let cur_tk = Seq.get_current_tick tr.sequencer in *)
      (* Log.p "delta: %d millis: %d\n" delta (millisecs ()); *)
      let tick_duration = (60 * 1000000) / tr.t_bpm / tr.t_ppqn in
      let current_tick = !previous_tick + (delta / tick_duration) in


      let ticks_to_manage = current_tick - !previous_tick in
      if (ticks_to_manage < 0) 
      then (
        Log.warn "Going to fail:  Cur Tk: %d   Prev Tk: %d\n"
        current_tick !previous_tick;
        failwith (
          "Ticks to manage: " ^ (string_of_int ticks_to_manage) ^ "< 0"
        );
      );

      if (ticks_to_manage > 0) then (
        max_ticks_to_manage := max !max_ticks_to_manage ticks_to_manage;
        Pl.play_meta_events tr !previous_tick current_tick;
        Pl.play_midi_events preallocated_midi_event
        tr !previous_tick current_tick;
      );

      tr.t_do_after tr;
      previous_tick := current_tick;
      (* cpt_ticks := !cpt_ticks + tr.timer_ticks; *)

      if !willstop = 3 then (
        (* TODO should test with = 2 or 1... *)
        loop := false;
      );
      if not tr.t_is_playing then (
        if !willstop = 0 then (
          set_all_stopping tr;
        );
        incr willstop;
      );

    done;

    Log.p "Max Ticks To manage at once: %d\n" !max_ticks_to_manage;

    on_end ();
  )

end


