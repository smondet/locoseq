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

module Seq = AlsaSequencer
module Tim = AlsaSequencer
module HT = Hashtbl

type meta_action = 
  | TrackOn of int
  | TrackOff of int
  | SetBPM of int
  | TrackKeepOn of int

type meta_event = {
  mutable m_ticks : int * int ;
  mutable action : meta_action ;
}


type meta_track = {
  mutable meta_events :  meta_event list ;

  mutable m_name : string ;
  mutable m_tick_nb : int ;
  mutable m_playing : bool ;
  mutable m_stopped : bool ;

  mutable m_stop_scheduled : bool ;
  mutable m_play_scheduled : bool ;
}

type meta_action_spec = [
  | `track_set_on of int * int 
  | `track_set_off of int * int 
  | `set_bpm of int * int
  | `track_on of int * int * int 
]

type midi_track = {
  mutable midi_events: Midi.midi_event list ;
  mutable i_name : string ;
  mutable i_tick_nb : int ;

  mutable i_outport : int ;

  mutable i_stopped : bool ;
  mutable i_playing : bool ;
  mutable i_set_play : bool ;

  mutable i_play_scheduled : bool ;
  mutable i_stop_scheduled : bool ;

}
let empty_track () = {
  midi_events = [] ;
  i_name = "" ;
  i_tick_nb = 0 ;
  i_outport = 0 ;
  i_stopped = false ;
  i_playing = false ;
  i_set_play = false ;
  i_play_scheduled = false ;
  i_stop_scheduled = false ;
}


type tracker = {
  mutable sequencer : Seq.sequencer ;

  mutable midi_tracks : (int,midi_track) HT.t;
  mutable meta_tracks : (int,meta_track) HT.t;

  mutable ppqn : int ;
  mutable bpm : int ;

  mutable queue_delay : int ;
  mutable timer_ticks : int ;

  mutable is_playing : bool ;

  mutable do_before : tracker -> unit ;
  mutable do_after  : tracker -> unit ;
}

let meta_events_of_meta_actions action_list = (
  List.rev (
    List.rev_map (
      function
        | `track_set_on (i , tk) ->
            { m_ticks = i,i ; action = TrackOn tk ; }
        | `track_set_off  (i , tk) ->
            { m_ticks = i,i ; action = TrackOff tk ; }
        | `set_bpm  (i , bpm) ->
            { m_ticks = i,i ; action = SetBPM bpm ; }
        | `track_on  (b, e, tk) ->
            { m_ticks = b,e ; action = TrackKeepOn tk ; }
          (* | _ -> failwith "MetaAction Not Implemented" *)
    ) action_list
  )
)
let make_meta_track name tick_nb (action_list:meta_action_spec list) =
  let actions = meta_events_of_meta_actions action_list in
  {
    meta_events = actions ;
    m_name = name ;
    m_tick_nb = tick_nb ;
    m_playing = false ;
    m_stopped = false ;
    m_stop_scheduled = false ;
    m_play_scheduled = false ;
  }

let new_HT () =  HT.create 50

let make_tracker
(* ppqn:int -> bpm:int -> Midi.midi_track array -> unit : *)
ppqn bpm  sequencer before after =
  {
    sequencer    = sequencer  ;  
    midi_tracks  = new_HT ()  ;  
    meta_tracks  = new_HT ()  ;  
    ppqn         = ppqn       ;  
    bpm          = bpm        ;  
    queue_delay  = 4          ;  
    timer_ticks  = 4          ;  
    is_playing   = false      ;
    do_before    = before     ;
    do_after     = after      ;
  }

(* Transformations:  *)
let is_midi index = (index > 0)
let midi_index_to_id i =   i   
let midi_id_to_index i =   i 
let meta_index_to_id i =   i  
let meta_id_to_index i =   i

let midi_new_id trkr = (
  let id = ref 1 in
  try (
    while true do
      ignore(HT.find trkr.midi_tracks !id);
      incr id;
    done;
    !id
  ) with Not_found -> !id
)
let meta_new_id trkr = (
  let id = ref (-1) in
  try (
    while true do
      ignore(HT.find trkr.meta_tracks !id);
      decr id;
    done;
    !id
  ) with Not_found -> !id
)


let midi_get_track trkr id = HT.find trkr.midi_tracks id
let meta_get_track trkr id = HT.find trkr.meta_tracks id

let meta_set_track trkr id mtk = HT.replace trkr.meta_tracks id mtk

let midi_iteri app func = ( HT.iter  func  app.midi_tracks )
let meta_iteri app func = ( HT.iter  func  app.meta_tracks )
(* let meta_iteri app func = Array.iteri func app.meta_tracks *)

let get_midi_tracks_number tr = HT.length tr.midi_tracks
let get_meta_tracks_number tr = HT.length tr.meta_tracks 

let remove_midi_track tr id = HT.remove tr.midi_tracks id 
let remove_meta_track tr id = HT.remove tr.meta_tracks id 

let add_midi_track tr mtk = HT.add tr.midi_tracks (midi_new_id tr) mtk 
let add_meta_track tr mtk = HT.add tr.meta_tracks (meta_new_id tr) mtk 

let add_or_replace_midi_track tr id mtk = (
  HT.remove tr.midi_tracks id ; HT.add tr.midi_tracks id mtk;
)
let add_or_replace_meta_track tr id mtk = (
  HT.remove tr.meta_tracks id ; HT.add tr.meta_tracks id mtk;
)


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
        failwith "MetaUtil.spec_to_tick does not accept track_on meta-events!"
  )
  let spec_to_range = (function
    | `track_on       (b,e,_) -> (b,e)
    | spec -> failwith ( 
      "MetaUtil.spec_to_range does not accept" ^
      (spec_to_string spec) ^ " meta-event"
    )
  )
end


(** Do some controls and optimizations *)
let compile trkr = (

  midi_iteri trkr (fun id track ->
    List.iter (fun midi_ev ->
      if midi_ev.Midi.ticks >= track.i_tick_nb then (
        Log.warn "Detected a midi event with ticks after end of track %d\n" id;
        let old = midi_ev.Midi.ticks in
        midi_ev.Midi.ticks <- midi_ev.Midi.ticks mod track.i_tick_nb;
        Log.log "Corrected with modulo: %d -> %d\n" old midi_ev.Midi.ticks;
      );
    ) track.midi_events;
  );

)

let get_midi_tracks_infos tr = (
  let res = Array.create (HT.length tr.midi_tracks) (0,"",0,0) in
  let index = ref 0 in
  midi_iteri tr (
    fun i tk ->
      res.(!index) <-
        (midi_index_to_id i,tk.i_name, tk.i_outport, tk.i_tick_nb) ;
      incr index ;
  );
  res
)
let get_meta_tracks_infos tr = (
  let l = HT.length tr.meta_tracks in 
  let res = Array.create l (0,"",0,0) in
  let index = ref 0 in
  meta_iteri tr (
    fun i tk ->
      res.(l - !index - 1) <-
        (meta_index_to_id i,tk.m_name, -1 , tk.m_tick_nb) ;
      incr index ;
  );
  res
)


let get_meta_track_actions trkr id =
  List.rev (
    List.rev_map (
      fun ev ->
        match ev.action with
        | TrackOn id -> `track_set_on ((fst ev.m_ticks), id)
        | TrackOff id -> `track_set_off ((fst ev.m_ticks), id)
        | SetBPM bpm -> `set_bpm  ((fst ev.m_ticks), bpm)
        | TrackKeepOn id -> `track_on ((fst ev.m_ticks), (snd ev.m_ticks), id)
    ) (meta_get_track trkr id).meta_events ;
  )

let get_midi_track_events trkr id =
  (midi_get_track trkr id).midi_events 

let remove_midi_event_from_track trkr id midi_ev = (
  let track = (midi_get_track trkr id) in
  track.midi_events <- List.filter (fun m -> m <> midi_ev) track.midi_events;
)

let add_midi_event_to_track trkr id midi_ev = (
  let track = (midi_get_track trkr id) in
  track.midi_events  <- midi_ev :: track.midi_events;
)


let add_midi_tracks  trkr midi_tracks = (
  Array.iteri (
    fun index midi_track ->
      let result = empty_track () in
      let tk_cur = ref 0 in
      let tmp_list = ref [] in
      Array.iter (
        function 
          | Midi.MidiEvent mev ->
              let seq_ev = Midi.empty_midi_event () in
              tk_cur := !tk_cur + mev.Midi.ticks  ;
              seq_ev.Midi.ticks   <-  !tk_cur ;
              seq_ev.Midi.status  <- (mev.Midi.status land 0xF0) ;
              seq_ev.Midi.channel <- mev.Midi.channel ;
              seq_ev.Midi.data_1  <- mev.Midi.data_1  ;
              seq_ev.Midi.data_2  <- mev.Midi.data_2  ;
              tmp_list := seq_ev::!tmp_list ;
          | Midi.MetaEvent me when me.Midi.service_id = 0x03 ->
              result.i_name <- Midi.basic_data_to_string me.Midi.service_data ;
                tk_cur := !tk_cur + me.Midi.meta_ticks  ;
          | Midi.MetaEvent me when me.Midi.service_id = 0x2f ->
              (* End Of Track ! *)
              tk_cur := !tk_cur + me.Midi.meta_ticks  ;
              result.i_tick_nb <- !tk_cur ;
          | Midi.MetaEvent me ->
              (* Debate: do we count ticks for unknown meta events ? *)
              tk_cur := !tk_cur + me.Midi.meta_ticks  ;
          | _ -> ()
      ) midi_track.Midi.events ;
      result.midi_events <- List.rev !tmp_list ;
      HT.add trkr.midi_tracks (midi_new_id trkr) result ;
  ) midi_tracks ;
  compile trkr;
)

let clear_tracker trkr = (
  trkr.midi_tracks <- new_HT () ;
  trkr.meta_tracks <- new_HT () ;
)
(******************************************************************************)
(* SERIALIZATION TO XML: *)
module XmlSerialization = struct
  module X = Xml 
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
  let xml_length = "length"
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
    let l_xml_midi_tracks = ref [] in
    midi_iteri tr (
      fun id tk ->
        let xml_events = List.rev_map (
          fun event ->
            X.Element ( xml_midi_event , [
              (xml_tcks,soi event.Midi.ticks  ) ;
              (xml_stat,soi event.Midi.status ) ;
              (xml_chan,soi event.Midi.channel) ;
              (xml_dat1,soi event.Midi.data_1 ) ;
              (xml_dat2,soi event.Midi.data_2 ) ; 
            ] , [] )
        ) tk.midi_events in
        let xml_track = X.Element ( xml_midi_track , [
          (xml_id,soi id); (xml_name , tk.i_name); (xml_length,soi tk.i_tick_nb);
          (xml_outport,soi tk.i_outport) ;
        ] , List.rev xml_events ) in
        l_xml_midi_tracks := xml_track::!l_xml_midi_tracks ;
    );
    let xml_all_midi_tracks =
      X.Element (xml_midi_tracks, [], List.rev !l_xml_midi_tracks) in
    let l_xml_meta_tracks = ref [] in
    meta_iteri tr ( fun id tk ->
      let xml_events = List.rev_map (
        fun event ->
          let b,e = event.m_ticks in
          let act_str,arg = 
            match event.action with
            | TrackOn o ->  (xml_track_set_on  , o)
            | TrackOff o -> (xml_track_set_off , o)
            | SetBPM o   -> (xml_set_bpm   , o)
            | TrackKeepOn o -> (xml_track_keep_on ,o)
          in
          X.Element ( xml_meta_event , [
            (xml_begin,soi b  ) ; (xml_end  ,soi e  ) ;
            (xml_action , act_str) ; (xml_arg1 , soi arg)
          ] , [] )
      ) tk.meta_events in
      let xml_track = X.Element ( "meta_track" , [
        (xml_id,soi id) ; (xml_name , tk.m_name) ; (xml_length,soi tk.m_tick_nb) ;
      ] , List.rev xml_events ) in
      l_xml_meta_tracks := xml_track::!l_xml_meta_tracks ;
  );
  let xml_all_meta_tracks =
    X.Element (xml_meta_tracks, [], !l_xml_meta_tracks) in
  X.Element ( xml_tracker , [
    (xml_pqn,soi tr.ppqn) ; (xml_bpm,soi tr.bpm) ;
    (xml_queue_delay, soi tr.queue_delay) ; (xml_timer_ticks,soi  tr.timer_ticks) 
  ] , [ xml_all_midi_tracks ; xml_all_meta_tracks ])
  )

  let load_xml trkr xml = (
    if (X.tag xml) <> xml_tracker then (
      failwith ("Expecting a tracker tag and got: " ^ (X.tag xml))
    );

    trkr.ppqn        <- ios (X.attrib xml xml_pqn );
    trkr.bpm         <- ios (X.attrib xml xml_bpm);
    trkr.queue_delay <- ios (X.attrib xml xml_queue_delay);
    trkr.timer_ticks <- ios (X.attrib xml xml_timer_ticks);

    trkr.midi_tracks <- new_HT () ;
    trkr.meta_tracks <- new_HT () ;
    X.iter ( fun child ->
      match X.tag child with
      | s when s = xml_midi_tracks -> (
        X.iter (
          fun mtk ->
            let the_track = empty_track () in
            let the_id = ios (X.attrib mtk xml_id) in
            the_track.i_name    <-     (X.attrib mtk xml_name);
            the_track.i_tick_nb <- ios (X.attrib mtk xml_length);
            the_track.i_outport <- ios (X.attrib mtk xml_outport);
            let the_rev_events =
              List.rev_map (
                fun xev ->
                  let seq_ev = Midi.empty_midi_event () in
                  seq_ev.Midi.ticks   <- ios (X.attrib xev xml_tcks) ;
                  seq_ev.Midi.status  <- ios (X.attrib xev xml_stat) ;
                  seq_ev.Midi.channel <- ios (X.attrib xev xml_chan) ;
                  seq_ev.Midi.data_1  <- ios (X.attrib xev xml_dat1) ;
                  seq_ev.Midi.data_2  <- ios (X.attrib xev xml_dat2) ;
                  seq_ev
              ) (X.children mtk) in
            the_track.midi_events <- List.rev the_rev_events ;
            add_or_replace_midi_track trkr the_id the_track ;
        ) child ;
      )
      | s when s = xml_meta_tracks -> (
        X.iter (
          fun mtk ->
            let the_name = X.attrib mtk xml_name in
            let the_id = ios (X.attrib mtk xml_id) in
            let the_tick_nb = ios (X.attrib mtk xml_length) in
            let the_rev_events =
              List.rev_map (
                fun xev ->
                  let b = ios (X.attrib xev xml_begin) in
                  let e = ios (X.attrib xev xml_end) in
                  let action = X.attrib xev xml_action in
                  let arg = ios (X.attrib xev xml_arg1) in
                  match action with
                  | s when s = xml_track_set_on  -> `track_set_on  (b, arg )
                  | s when s = xml_track_set_off -> `track_set_off (b, arg )
                  | s when s = xml_set_bpm       -> `set_bpm       (b, arg )
                  | s when s = xml_track_keep_on -> `track_on (b,e, arg )
                  | s -> failwith ("Unreocgnizable meta_event: "^s)
              ) (X.children mtk) in
            let the_track =
              make_meta_track the_name the_tick_nb (List.rev the_rev_events) in
            add_or_replace_meta_track trkr the_id the_track ;
        ) child ;
      )
      | s ->
          failwith ("Unknown tag while expecting (midi_ or meta_)tracks: "^s);
    ) xml ;
    compile trkr;
  )
end

(******************************************************************************)
(* PRINTF's: (debug) *)

let print_info chan tr = (
  let pr = Printf.fprintf chan in
  let l = get_midi_tracks_number tr in
  pr 
  "[Tracker: midi_tracks:%d, bpm:%d, ppqn:%d, queue_delay:%d, timer_ticks:%d]\n"
  l tr.bpm tr.ppqn tr.queue_delay tr.timer_ticks;

  midi_iteri tr (
    fun i tk ->
      Printf.fprintf chan
      "  [TK %3d] [midi:%2d] [%-.20s] [%5d events, %4d ticks]\n%!" 
      (midi_index_to_id i) tk.i_outport tk.i_name
      (List.length tk.midi_events) tk.i_tick_nb ;
  );
  meta_iteri tr (
    fun i tk ->
      Printf.fprintf chan
      "  [TK %3d] [ meta  ] [%-.20s] [%5d events, %4d ticks]\n%!" 
      (meta_index_to_id i) tk.m_name (List.length tk.meta_events) tk.m_tick_nb ;
  );
)

let util_track_status_to_string tr trk = (
  if is_midi trk then (
    let tk = (midi_get_track tr trk) in
    "[MIDI]{playing: " ^ (string_of_bool tk.i_playing) ^
    ", just_played: " ^ (string_of_bool tk.i_set_play) ^
    ", just_stopped: " ^ (string_of_bool tk.i_stopped) ^
    ", scheduled_play: " ^ (string_of_bool tk.i_play_scheduled) ^
    ", scheduled_stop: " ^ (string_of_bool tk.i_stop_scheduled) ^ "}"
  ) else (
    let tk = meta_get_track tr trk in
    "[META]{playing: " ^ (string_of_bool tk.m_playing) ^
    (* ", just_played: " ^ (string_of_bool tk.m_ust_played) ^ *)
    ", just_stopped: " ^ (string_of_bool tk.m_stopped) ^
    ", scheduled_play: " ^ (string_of_bool tk.m_stop_scheduled) ^
    ", scheduled_stop: " ^ (string_of_bool tk.m_play_scheduled) ^ "}"
  )
)


(******************************************************************************)
let set_ppqn tr ppqn = tr.ppqn <- ppqn
let get_ppqn tr = tr.ppqn

let get_midi_track_infos tr id = (
  let tk = midi_get_track tr id in
  tk.i_name , tk.i_outport , tk.i_tick_nb
)
let set_midi_track_infos tr id name port ticks = (
  let tk = midi_get_track tr id in
  tk.i_name <- name ;
  tk.i_outport <- port ;
  tk.i_tick_nb <- ticks ;
)

let get_meta_track_infos tr id = (
  let tk = meta_get_track tr id in tk.m_name , tk.m_tick_nb
)
let set_meta_track_infos tr id name ticks = (
  let tk = meta_get_track tr id in
  tk.m_name <- name;
  tk.m_tick_nb <- ticks;
)

let replace_meta_track tr id mtk = meta_set_track tr id mtk 

let get_track_stat trkr id =
  if (is_midi id) then (
    let tk = midi_get_track trkr id in
    (tk.i_tick_nb, tk.i_playing, tk.i_play_scheduled, tk.i_stop_scheduled)
  ) else (
    let tk = meta_get_track trkr id in
    (tk.m_tick_nb, tk.m_playing, tk.m_play_scheduled, tk.m_stop_scheduled)
  )



(* =====================
   NOTE: REAL-TIME here:

 *)



let toggle_playing_track tr tk = (
  if is_midi tk then (
    let tk = midi_get_track tr tk in
    if not tk.i_playing then (
      tk.i_playing <- true ;
      tk.i_play_scheduled <- false ;
      tk.i_stop_scheduled <- false ;
    ) else (
      tk.i_stopped <- true ;
    );
  ) else (
    let tk = meta_get_track tr tk in
    if not tk.m_playing then (
      tk.m_playing <- true ;
      tk.m_stop_scheduled <- false ;
      tk.m_play_scheduled <- false ;
    ) else (
      tk.m_stopped <- true ;
    );
  );
)

let set_playing_track tr tk = (
  if is_midi tk then (
    let tk = midi_get_track tr tk in
    if not tk.i_playing || tk.i_stopped
    then (
      tk.i_playing <- true ;
      tk.i_stop_scheduled <- false ;
      tk.i_stopped <- false ;
    );
    tk.i_set_play <- true ;
  ) else (
    let tk = meta_get_track tr tk in
    if not tk.m_playing then (
      tk.m_playing <- true ;
      tk.m_stop_scheduled <- false ;
      tk.m_play_scheduled <- false ;
    );
  );
)

let set_stopping_track tr tk = (
  if is_midi tk then (
    let tk = midi_get_track tr tk in
    if tk.i_playing && not tk.i_set_play
    then (
      tk.i_stopped <- true ;
      tk.i_play_scheduled <- false ;
      tk.i_stop_scheduled <- false ;
    );
  ) else (
    let tk = meta_get_track tr tk in
    if tk.m_playing then (
      tk.m_stopped <- true ;
      tk.m_stop_scheduled <- false ;
      tk.m_play_scheduled <- false ;
    );
  );
)


let schedule_play tr tk = (
  if is_midi tk then (
    let tk = midi_get_track tr tk in
    if not tk.i_playing then 
      tk.i_play_scheduled <- true ;
      tk.i_stop_scheduled <- false ;
  ) else (
    let tk = meta_get_track tr tk in
    if not tk.m_playing then (
      tk.m_stop_scheduled <- false ;
      tk.m_play_scheduled <- true ;
    );
  )
)


let schedule_stop tr tk = (
  if is_midi tk then (
    let tk = midi_get_track tr tk in
    if tk.i_playing then 
      tk.i_stop_scheduled <- true ;
      tk.i_play_scheduled <- false ;
  ) else (
    let tk = meta_get_track tr tk in
    if tk.m_playing then (
      tk.m_stop_scheduled <- true ;
      tk.m_play_scheduled <- false ;
    );
  );
)

let is_track_playing tr tk = (
  if is_midi tk then (
    let tk = midi_get_track tr tk in
    tk.i_playing 
  ) else (
    let tk = meta_get_track tr tk in
    tk.m_playing
  )
)


let schedule_toggle tr tk = (
  if (is_track_playing tr tk) then (
    schedule_stop tr tk ;
  ) else (
    schedule_play tr tk ;
  )
)

let set_all_stopping tr = (
  midi_iteri tr ( fun i _ -> set_stopping_track tr i) ;
  meta_iteri tr ( fun i _ -> set_stopping_track tr i) ;
)

let is_tracker_playing tr = tr.is_playing

let stop tr = (
  tr.is_playing <- false ;
)

let set_bpm tr bpm = (
  tr.bpm <- bpm ;
  if tr.is_playing then (
    Seq.set_queue_tempo tr.sequencer tr.bpm tr.ppqn
  );
)
let add_to_bpm tr value = (
  tr.bpm <- tr.bpm + value ;
  if tr.is_playing then (
    Seq.set_queue_tempo tr.sequencer tr.bpm tr.ppqn
  );
)

let util_meta_ev_intersects_tick_interval (m_b,m_e) lgth (t_b,t_e) = (
  let t_bm = t_b mod lgth in
  let t_em = t_e mod lgth in
  if t_em < t_bm then (
    (* [t_b,t_e] intersects the loop_tick of the meta track so  *)
    (* [0 , t_em] and [ t_bm, lgth ] *)
    (* ( t_em <= m_e )  || ( m_b <= t_bm )    *)
    ( m_b <= t_em ) || ( t_bm <= m_e )
  ) else (
    (* [t_bm,t_em] is in the meta track interval *)
    ( t_bm <= m_b && m_b <= t_em ) || ( t_bm <= m_e && m_e <= t_em ) ||
    ( m_b <= t_bm && t_bm <= m_e ) || ( m_b <= t_em && t_em <= m_e ) 
  );
)

let util_meta_tick_in_interval tick lgth (t_b,t_e) =
  util_meta_ev_intersects_tick_interval (tick,tick) lgth (t_b,t_e) 
  (* Could be optimized !! *)

let util_do_we_cross_the_end lgth (t_b,t_e) = (
  (* Log.p "util_do_we_cross_the_end %d (%d,%d)\n" lgth t_b t_e ; *)
  let t_bm = t_b mod lgth in
  let t_em = t_e mod lgth in
  (t_em < t_bm) || (t_bm = 0)
)


let play_meta_events send_ev tr previous_tick cur_tk = (
  (* Managing META tracks *)
  meta_iteri tr (
    fun i tk ->
      if tk.m_stop_scheduled &&
      (util_do_we_cross_the_end tk.m_tick_nb ((!previous_tick + 1),cur_tk))
      then (
        Log.p "In MetaSchedStop \n" ;
        tk.m_stopped <- true ;
        tk.m_stop_scheduled <- false ;
      );
      if tk.m_stopped then (
        tk.m_stopped <- false ;
        tk.m_playing <- false ;
        List.iter (
          fun ev ->
            match ev.action with
            | TrackOff tk -> set_stopping_track tr tk ;
            | TrackKeepOn trk ->
                if (util_meta_ev_intersects_tick_interval ev.m_ticks
                tk.m_tick_nb (!previous_tick + 1, cur_tk) ) then (
                  set_stopping_track tr trk ;
                );
            | _ -> ()
        ) tk.meta_events ;
      );
      if tk.m_play_scheduled &&
      (util_do_we_cross_the_end tk.m_tick_nb ((!previous_tick + 1),cur_tk))
      then (
        Log.p "In MetaSchedPlay \n" ;
        tk.m_playing <- true ;
        tk.m_play_scheduled <- false ;
      );
      if tk.m_playing then (
        List.iter begin
          fun ev ->
            if (util_meta_ev_intersects_tick_interval ev.m_ticks tk.m_tick_nb
            (!previous_tick + 1, cur_tk)) then (
              let _ = match ev.action with
              | TrackOn trk -> set_playing_track tr trk ;
              | TrackOff tk -> set_stopping_track tr tk ;
              | SetBPM x -> set_bpm tr x ;
              | TrackKeepOn trk ->
                  let is_end_in =
                    util_meta_tick_in_interval (snd ev.m_ticks) tk.m_tick_nb
                    (!previous_tick + 1, cur_tk) in
                  let is_beg_in =
                    util_meta_tick_in_interval (fst ev.m_ticks) tk.m_tick_nb
                    (!previous_tick + 1, cur_tk) in
                  if not is_beg_in && is_end_in 
                  then (
                    (* Log.p "Set stopping %d: m:[%d,%d] i:[%d,%d] i':[%d,%d]\n  stat:%s\n" *)
                    (* trk (fst ev.m_ticks) (snd ev.m_ticks) *)
                    (* (!previous_tick + 1) cur_tk *)
                    (* ((!previous_tick + 1) mod tk.m_tick_nb) *)
                    (* (cur_tk  mod tk.m_tick_nb) *)
                    (* (util_track_status_to_string tr trk); *)
                    set_stopping_track tr trk ;
                  );
                  if not is_end_in || (is_beg_in && is_end_in)
                  then (
                    (* Log.p "Set playing: m:[%d,%d] i:[%d,%d] i':[%d,%d]\n" *)
                    (* (fst ev.m_ticks) (snd ev.m_ticks) *)
                    (* (!previous_tick + 1) cur_tk *)
                    (* ((!previous_tick + 1) mod tk.m_tick_nb) *)
                    (* (cur_tk  mod tk.m_tick_nb) ; *)
                    set_playing_track tr trk ;
                  );
                  (* TODO: see if both conditions are sufficient *)

                in () ;
            );
        end tk.meta_events ;
      );
  );
)

let copy_and_queue_on_time ev send_ev seq port time = (
  send_ev.Midi.ticks   <- time ;
  send_ev.Midi.status  <- ev.Midi.status ;
  send_ev.Midi.channel <- ev.Midi.channel ;
  send_ev.Midi.data_1  <- ev.Midi.data_1  ;
  send_ev.Midi.data_2  <- ev.Midi.data_2  ;
  (* Seq.output_event_direct tr.sequencer 0 send_ev ; *)
  Seq.put_event_in_queue seq port send_ev ;
)

let play_midi_events send_ev tr previous_tick cur_tk = (
  (* Playing MIDI tracks: *)
  let should_send_noteoffs = ref true in
  midi_iteri tr (
    fun i tk ->
      if tk.i_stop_scheduled &&
      (util_do_we_cross_the_end tk.i_tick_nb ((!previous_tick + 1),cur_tk))
      then (
        tk.i_stopped <- true ;
        tk.i_stop_scheduled <- false ;
        should_send_noteoffs := false ;
      );
      if tk.i_stopped then (
        tk.i_stopped <- false ;
        tk.i_playing <- false ;
        if (!should_send_noteoffs) then (
          List.iter (
            fun ev ->
              (* Hack: we send all the NoteOFF we find... *)
              if ev.Midi.status = 0x80 then (
                copy_and_queue_on_time ev send_ev
                tr.sequencer tk.i_outport (cur_tk + tr.queue_delay - 1);
              );
          ) tk.midi_events ;
        ) else (
          should_send_noteoffs := true ;
        );
      );
      if tk.i_play_scheduled &&
      (util_do_we_cross_the_end tk.i_tick_nb ((!previous_tick + 1),cur_tk))
      then (
        tk.i_playing <- true ;
        tk.i_play_scheduled <- false ;
      );
      if tk.i_playing then (
        tk.i_set_play <- false ;
        List.iter (
          fun ev ->
            (* Get The Tick:  *)
            let to_trigger = ref false in
            let tick = ref (!previous_tick + 1) in
            while !tick <= cur_tk && !to_trigger = false do
              if ev.Midi.ticks = (!tick mod tk.i_tick_nb)
              then  (
                to_trigger := true ;
              ) else (
                incr tick ;
              ) ;
            done;

            if !to_trigger then (
              copy_and_queue_on_time ev send_ev
              tr.sequencer tk.i_outport (!tick + tr.queue_delay);
            );
        ) tk.midi_events ;
      );
  );
)




let play on_end tr = (
  tr.is_playing <- true ;

  Seq.set_queue_tempo tr.sequencer tr.bpm tr.ppqn ;
  Seq.start_queue tr.sequencer ;

  let info = Seq.get_queue_timer_info tr.sequencer in
  (* XXX Why need this hack ??? *)
  info.Tim.t_card <- 0 ;
  let my_timer = Tim.make_timer info in

  let cpt_ticks = ref 0 in
  let previous_tick = ref (-1) in

  let send_ev = Midi.empty_midi_event () in

  Tim.set_ticks my_timer tr.timer_ticks ;
  Tim.start_timer my_timer ;

  Log.p "Timer started !\n" ;

  let loop = ref tr.is_playing in
  let willstop = ref 0 in
  while !loop do
    let count = Tim.wait_next_tick my_timer 1000 in
    if count <> 1 then (
      if count = -1 then
        Log.warn "At loop %d, Timer has returned -1. TIME OUT ?\n" !cpt_ticks  
      else
        Log.warn "[%d] Read %d timer events\n"  !cpt_ticks count 
    );
    let cur_tk = Seq.get_current_tick tr.sequencer in

    let ticks_to_manage = cur_tk - !previous_tick in
    if (ticks_to_manage < 0) 
    then (
      Log.warn "Going to fail:  Cur Tk: %d   Prev Tk: %d\n" cur_tk !previous_tick ;
      failwith ( "Ticks to manage: " ^ (string_of_int ticks_to_manage) ^ "< 0");
    );

    tr.do_before tr ;

    if (ticks_to_manage > 0) then (

      play_meta_events send_ev tr previous_tick cur_tk ;
      play_midi_events send_ev tr previous_tick cur_tk ;

    (* ) else ( *)
        (* Gc.minor () ;*) (*  is it a good idea ? *)
    );

    tr.do_after tr ;

    previous_tick := cur_tk ;
    cpt_ticks := !cpt_ticks + tr.timer_ticks ;

    if !willstop = 3 then (
      loop := false ;
    );
    if not tr.is_playing then (
      set_all_stopping tr ;
      incr willstop ;
    );
    
  done;

  Tim.stop_timer my_timer ;
  Seq.clear_queue tr.sequencer ;
  Seq.stop_queue  tr.sequencer ;

  on_end () ;
)




