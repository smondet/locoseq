module IM = InputManager ;;

type seq_app = {
  mutable a_sequencer : AlsaSequencer.sequencer ;
  mutable a_tracker : Tracker.tracker ;
  mutable a_input_mgr : InputManager.manager ;

  mutable a_play_thread : Thread.t option ;

  mutable a_songname :string ;

  mutable a_filename : string ;
  mutable a_is_saved : bool ;

  mutable a_input_actions :
    ( IM.input_specification * IM.action_specification ) list ;
  mutable a_unsaved_actions : 
    ( IM.input_specification * IM.action_specification ) list ;
};;

let make_app  ?(visitor=fun () -> ()) () =
  let default_ppqn = 192 in

  let my_seq =
    AlsaSequencer.make_sequencer
    (StringServer.alsa_client_name ())
    StringServer.in_put_ports
    StringServer.out_put_ports ;
  in

  let the_input_mgr = InputManager.make_manager my_seq in

  let my_tracker = 
    Tracker.make_tracker default_ppqn  170 my_seq
    (InputManager.manage_input the_input_mgr)
    (fun t -> visitor () ; Thread.delay 0.001) in

  {
    a_sequencer = my_seq ;
    a_tracker = my_tracker ;
    a_input_mgr = the_input_mgr ;
    a_play_thread = None ;
    a_input_actions = [] ;
    a_unsaved_actions = [] ;
    a_songname = "" ;
    a_filename = "" ;
    a_is_saved = true ;(* Nothing has been done neither ! *)
  };
;;

(******************************************************************************)
(* APPLICATION FUNCTIONS: *)
(******************************************************************************)

(******************************************************************************)
(* SONG VALUES: *)
let clear_song app = (
  Tracker.clear_tracker app.a_tracker ;
  app.a_input_actions <- [] ;
  app.a_unsaved_actions <- [] ;
  app.a_songname <- "" ;
  app.a_filename <- "" ;
  app.a_is_saved <- true ;(* Nothing has been done neither ! *)
)
let get_bpm_ppqn app =
  (app.a_tracker.Tracker.bpm, app.a_tracker.Tracker.ppqn)

let set_bpm app bpm = (
  Tracker.set_bpm app.a_tracker bpm ;
  app.a_is_saved <- false ;
)
let set_ppqn app ppqn = Tracker.set_ppqn app.a_tracker ppqn

let get_sequencer_info app =
  (app.a_tracker.Tracker.queue_delay, app.a_tracker.Tracker.timer_ticks) ;;

let set_sequencer_info app (qd, tt) = 
  if (
    app.a_tracker.Tracker.queue_delay <> qd ||
    app.a_tracker.Tracker.timer_ticks <> tt ) then (
      app.a_tracker.Tracker.queue_delay <- qd ;
      app.a_tracker.Tracker.timer_ticks <- tt ;
      Log.p "Setting to false\n" ;
      app.a_is_saved <- false ;
    )

let get_song_name app = app.a_songname
let set_song_name app str = (
  app.a_is_saved <- false ;
  app.a_songname <- str ;
)

(******************************************************************************)
(* TRACK MANAGEMENT: *)
let add_midi_file app file =
  let midi_data =  MidiFile.parse_smf file in

  Tracker.set_ppqn app.a_tracker midi_data.Midi.per_quarter_note ;
  Tracker.add_midi_tracks app.a_tracker midi_data.Midi.tracks;
  
  Log.p "Number of tracks: %d\n" (Tracker.get_midi_tracks_number app.a_tracker);

  app.a_is_saved <- false ;
  ();;

let threaded_play on_end app =
  app.a_play_thread <- Some (Thread.create (
    fun () ->
      InputManager.clear_input app.a_input_mgr ;
      Tracker.play on_end app.a_tracker;
  ) ()) ;;

let threaded_stop app =
    match app.a_play_thread with
    | None -> Log.p "You want to stop a stopped tracker...\n" ;
    | Some t -> (
      Tracker.stop app.a_tracker ;
      Thread.join t ;
      app.a_play_thread <- None ;
      Log.p "Tracker Stopped\n" ;
    )
;;
let is_playing app = Tracker.is_tracker_playing app.a_tracker

let get_current_tick app = AlsaSequencer.get_current_tick app.a_sequencer

let get_midi_tracks_information app = (
  Tracker.get_midi_tracks_infos app.a_tracker
)
let get_midi_track_information app id = (
  Tracker.get_midi_track_infos app.a_tracker id
)
let set_midi_track_information app id name port ticks= (
  Tracker.set_midi_track_infos app.a_tracker id name port ticks;
  app.a_is_saved <- false ;
)
let get_meta_tracks_information app = (
  Tracker.get_meta_tracks_infos app.a_tracker
)
let get_meta_track_information app id = (
  Tracker.get_meta_track_infos app.a_tracker id
)
let get_track_stat app = Tracker.get_track_stat app.a_tracker

let add_meta_track app name size actions = (
  let mtk =
    Tracker.make_meta_track name size actions in
  Tracker.add_meta_track app.a_tracker mtk ;
  app.a_is_saved <- false ;
)
let replace_meta_track app id name size actions  = (
  let mtk = Tracker.make_meta_track name size actions in
  Tracker.replace_meta_track app.a_tracker id mtk ;
  app.a_is_saved <- false ;
)
let remove_midi_track app id = (
  Tracker.remove_midi_track app.a_tracker id ;
  app.a_is_saved <- false ;
)
let remove_meta_track app id = (
  Tracker.remove_meta_track app.a_tracker id ;
  app.a_is_saved <- false ;
)

let get_meta_track app id = Tracker.get_meta_track_actions app.a_tracker id 


(******************************************************************************)
(* INPUT MANAGEMENT: *)

let add_uniq_custom_unsaved_action app cust_id action = (
  app.a_unsaved_actions <-
    ((`custom cust_id), action)::
      List.filter (
        function (`custom idm,_) -> cust_id <> idm | (_,_) -> true
      ) app.a_unsaved_actions ;
)

let basic_add_handler app hdlr = (
  app.a_input_actions <- hdlr::app.a_input_actions ;
  app.a_is_saved <- false ;
)

let update_input_mgr app =
  IM.remove_all_handlers app.a_input_mgr ;
  List.iter (
    fun (inp,act) ->
      IM.add_handler app.a_input_mgr inp act ;
  ) app.a_input_actions ;
  List.iter (
    fun (inp,act) ->
      IM.add_handler app.a_input_mgr inp act ;
  ) app.a_unsaved_actions ;
;;

let remove_inpacts app index_list = (
  let i = ref 0 in
  Log.p "before: %d\n" (List.length app.a_input_actions) ;
  app.a_input_actions <- List.filter (
    fun _ -> if (
      List.mem !i  index_list ;
    ) then (
      Log.p "removed: %d\n" !i ;
      incr i ; false
    ) else ( 
      incr i ; true
    )
  ) app.a_input_actions ;
  Log.p "after: %d\n" (List.length app.a_input_actions) ;
  app.a_is_saved <- false ;
)

let replace_handler app index hdlr = (
  let i = ref 0 in
  app.a_input_actions <- List.map (
    fun e -> if (  !i = index ) then (
      incr i ; hdlr 
    ) else ( 
      incr i ; e
    )
  ) app.a_input_actions ;
  app.a_is_saved <- false ;
)


let custom_event app number =
  InputManager.add_custom_event app.a_input_mgr number ;;

let get_input_action_list app = app.a_input_actions

(******************************************************************************)
(* SAVE & OPEN *)

let get_filename app = app.a_filename
let is_saved app = app.a_is_saved

module X = Xml ;;
let xml_inputmgr = "input_manager"
let xml_handler = "handler"

let xml_input_match  =  "input_match"
let xml_action_type  =  "action_type"
let xml_action_arg   =  "action_arg"

let xml_set_BPM =             "set_BPM"              
let xml_incr_BPM=             "incr_BPM"             
let xml_decr_BPM=             "decr_BPM"             
let xml_toggle_track=         "toggle_track"         
let xml_track_on=             "track_on"             
let xml_track_off=            "track_off"            
let xml_schedule_toggle_track="schedule_toggle_track"
let xml_schedule_track_on=    "schedule_track_on"    
let xml_schedule_track_off=   "schedule_track_off"   
let xml_play=                 "play"                 
let xml_stop=                 "stop"                 
let xml_mute_all=             "mute_all"             

let save_to_file_xml app filename = (
  let xml_tracker = Tracker.to_xml app.a_tracker in
  let xml_inpact =
    X.Element ( xml_inputmgr , [] ,
    List.rev (List.rev_map (
      fun (i,(act,arg)) ->
        let inp = 
          match i with
          | `midi_event (stat,chan,note,velo) -> (
            Printf.sprintf "midi:%d:%d:%d:%d" stat chan note velo )
          | `custom nb -> ( Printf.sprintf "custom:%d"   nb)
        in
        let act_str =
          match act with
          | `set_BPM                 ->  xml_set_BPM              
          | `incr_BPM                ->  xml_incr_BPM             
          | `decr_BPM                ->  xml_decr_BPM             
          | `toggle_track            ->  xml_toggle_track         
          | `track_on                ->  xml_track_on             
          | `track_off               ->  xml_track_off            
          | `schedule_toggle_track   ->  xml_schedule_toggle_track
          | `schedule_track_on       ->  xml_schedule_track_on    
          | `schedule_track_off      ->  xml_schedule_track_off   
          | `play                    ->  xml_play                 
          | `stop                    ->  xml_stop                 
          | `mute_all                ->  xml_mute_all             
        in
        let arg_str = 
          match arg with
          | `direct_int v   -> ("direct_int:" ^ (string_of_int v))
          | `midi_status    -> "midi_status"  
          | `midi_channel   -> "midi_channel" 
          | `midi_note      -> "midi_note"    
          | `midi_velocity  -> "midi_velocity"
        in
        X.Element ( xml_handler , [
          (xml_input_match, inp) ; (xml_action_type, act_str) ;
          (xml_action_arg, arg_str) ; ] , [])
    ) app.a_input_actions) )
  in

  let xml_app = X.Element (
    "song", [
      ("version" , "0") ;
      ("name" , app.a_songname) ;
    ] , [ xml_tracker ; xml_inpact ] 
  ) in

  let f = open_out filename in
  Printf.fprintf f "<?xml version=\"1.0\"?>\n%s" (Xml.to_string_fmt xml_app) ;
  close_out f ;
  app.a_is_saved <- true ;
  app.a_filename <- filename ;
)

let load_of_file_xml app filename = (
  try (
    let f = open_in filename in
    let xml = Xml.parse_in f in

    if (X.tag xml) = "song" then (
      app.a_songname <- X.attrib xml "name" ;
      app.a_input_actions <- [] ;
      X.iter (
        fun child ->
          match X.tag child with
          | s when s = Tracker.xml_tracker ->
              Tracker.load_xml app.a_tracker child ;
          | s when s = xml_inputmgr -> (
            X.iter (
              fun hdlr ->
                assert ( (X.tag hdlr) = xml_handler );
                let inp =
                  let str = X.attrib hdlr xml_input_match in
                  let diff = String.sub str 0 4 in
                  match diff with
                  | s when s = "midi" -> (
                    Scanf.sscanf str "midi:%d:%d:%d:%d" (
                      fun a b c d ->
                        Log.p "read midi: %d %d %d %d \n" a b c d;
                        `midi_event (a,b,c,d)
                    )
                  )
                  | s when s = "cust" -> 
                      (Scanf.sscanf str "custom:%d" (fun i -> `custom i))
                  | s -> failwith ("Unknown handler input: " ^ str) ;
                in
                let arg = 
                  let str = X.attrib hdlr xml_action_arg in
                  let diff = String.sub str 0 4 in
                  match diff with
                  | s when s = "midi" -> (
                    match str with
                    | "midi_status"   -> `midi_status   
                    | "midi_channel"  -> `midi_channel  
                    | "midi_note"     -> `midi_note     
                    | "midi_velocity" -> `midi_velocity 
                    | _ -> failwith ("Unknown handler midi argument: " ^ str) ;
                  )
                  | s when s = "dire" -> 
                      Scanf.sscanf str "direct_int:%d" (fun i -> `direct_int i)
                  | _ -> failwith ("Unknown handler argument: " ^ str) ;
                in
                let act = 
                  match X.attrib hdlr xml_action_type with
                  | s when s = xml_set_BPM                ->  `set_BPM              
                  | s when s = xml_incr_BPM               ->  `incr_BPM             
                  | s when s = xml_decr_BPM               ->  `decr_BPM             
                  | s when s = xml_toggle_track           ->  `toggle_track         
                  | s when s = xml_track_on               ->  `track_on             
                  | s when s = xml_track_off              ->  `track_off            
                  | s when s = xml_schedule_toggle_track  ->  `schedule_toggle_track
                  | s when s = xml_schedule_track_on      ->  `schedule_track_on    
                  | s when s = xml_schedule_track_off     ->  `schedule_track_off   
                  | s when s = xml_play                   ->  `play                 
                  | s when s = xml_stop                   ->  `stop                 
                  | s when s = xml_mute_all               ->  `mute_all             
                  | s -> failwith ("Unknown handler attribute: " ^ s) ;
                in
                basic_add_handler app (inp , (act , arg)) ;
                () ;

            ) child ;
          )
          | s -> failwith ("Unknown tag: " ^ s) ;
      ) xml ;

    ) else (
      failwith "Malformed XML:not a song tag" ;
    );
    close_in f;
    app.a_is_saved <- true ;
    app.a_filename <- filename ;
  ) with exn -> (
    Log.p "Error: %s\n" (Printexc.to_string exn) ;
    raise exn ;
  )
)

let save_to_file = save_to_file_xml
let load_of_file = load_of_file_xml
