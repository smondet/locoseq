
module GenGui = GeneratedGui ;;
module App = SeqApp ;;
module S = StringServer ;;

let get_option opt =
  match opt with
  | None -> failwith "NoneOptionException"
  | Some x -> x
;;


(******************************************************************************)
(*  GLOBAL APPLICATION AND WINDOW:  *)

let global_app = ref None ;;
let global_app_window = ref None ;;
let get_app () = get_option (!global_app:App.seq_app option) ;;
let get_aw () = get_option (!global_app_window:GenGui.app_window option) ;;

(******************************************************************************)
(* INPUT MANAGEMENT: *)

let global_mouse_l_click_offset = 10000
let global_mouse_m_click_offset = 20000

(******************************************************************************)
(* GUI UTILITIES *)
let util_make_time_combo_box packing = (
  let cbt =
    GEdit.combo_box_text
    ~strings:[S.gui_ticks ();S.gui_quarters ();S.gui_44_bars ()]
    ~add_tearoffs:false ~packing:packing ()
  in
  (fst cbt)#set_active 0 ; cbt
)
let util_time_of_combo_box combo = (
  let _,ppqn = App.get_bpm_ppqn (get_app()) in
  let s = GEdit.text_combo_get_active combo in
  match s with
  | Some s when s = S.gui_ticks () -> 1 
  | Some s when s = S.gui_quarters () -> ppqn
  | Some s when s = S.gui_44_bars () -> 4 * ppqn
  | _ -> Log.p "Default is Ticks:\n" ; 1
)

let util_set_playing_state playing = (
  (get_aw ())#button_play#misc#set_sensitive (not playing) ;
  (get_aw ())#button_new#misc#set_sensitive (not playing) ;
  (get_aw ())#button_open#misc#set_sensitive (not playing) ;
  (get_aw ())#button_save#misc#set_sensitive (not playing) ;
  (get_aw ())#button_saveas#misc#set_sensitive (not playing) ;
  (get_aw ())#button_update_bpm#misc#set_sensitive (not playing) ;
  (get_aw ())#button_update_bpm#misc#set_sensitive (not playing) ;
  (get_aw ())#button_update_sngnam#misc#set_sensitive (not playing) ;
  (* (get_aw ())#spinbutton_qd#misc#set_sensitive (not playing) ; *)
  (* (get_aw ())#spinbutton_tt#misc#set_sensitive (not playing) ; *)
  (* (get_aw ())#spinbutton_bpm#misc#set_sensitive (not playing) ; *)
  (get_aw ())#button_add_midi#misc#set_sensitive (not playing) ;
  (get_aw ())#button_edit_midi#misc#set_sensitive (not playing) ;
  (get_aw ())#button_suppr_midi#misc#set_sensitive (not playing) ;
  (get_aw ())#button_add_meta#misc#set_sensitive (not playing) ;
  (get_aw ())#button_edit_meta#misc#set_sensitive (not playing) ;
  (get_aw ())#button_suppr_meta#misc#set_sensitive (not playing) ;
  (get_aw ())#button_add_iact#misc#set_sensitive (not playing) ;
  (get_aw ())#button_edit_iact#misc#set_sensitive (not playing) ;
  (get_aw ())#button_suppr_iact#misc#set_sensitive (not playing) ;

  (get_aw ())#button_stop#misc#set_sensitive (playing) ;
)

(******************************************************************************)
(* UPDATING INFO-LABELS *)

let aw_update_title () = (
  (get_aw ())#app_window#set_title (
    S.make_appwin_title (App.get_song_name (get_app ())));
    (get_aw ())#entry_sngnam#set_text (App.get_song_name (get_app ())) ;
)
let lb_aw_update_filename () = (
  let fnm =
    match App.get_filename (get_app ()) with
    | "" -> "<i>(there's no filename...)</i>" 
    | s -> "<tt>"^s^"</tt>"
  in (get_aw ())#label_filename#set_label fnm ;
)
let aw_append_msg (typ:[`LOG |`WARN |`ERR]) str = (
  let aw = get_aw () in
  let mkp = 
    match typ with
    | `LOG -> "INFO: " ^ str
    | `WARN -> "WARNING: " ^ str
    | `ERR -> "ERROR: " ^ str
  in
  aw#textview_msg#buffer#set_text (
    (aw#textview_msg#buffer#get_text ()) ^ mkp  ^ "\n"
  );
)
let sp_aw_update_bpm_ppqn_values () = (
  let b,p = App.get_bpm_ppqn (get_app()) in
  (get_aw ())#spinbutton_bpm#adjustment#set_value (float b) ;
  (get_aw ())#spinbutton_pqn#adjustment#set_value (float p) ;
)
let aw_update_qd_tt () = (
  let qd,tt = App.get_sequencer_info (get_app()) in
  (get_aw ())#spinbutton_qd#adjustment#set_value (float qd) ;
  (get_aw ())#spinbutton_tt#adjustment#set_value (float tt) ;
)

let update_during_play () = (
  ignore( GtkBase.Widget.queue_draw (get_aw ())#treeview_midi#as_widget );
  ignore( GtkBase.Widget.queue_draw (get_aw ())#treeview_meta#as_widget );
  sp_aw_update_bpm_ppqn_values () ;
)

(******************************************************************************)
(* TREE VIEWS: *)

type tv_aw_trackview_info = {
  tkv_nbrs_col : int GTree.column ;
  tkv_dscr_col : string GTree.column ;
  tkv_port_col :   int  GTree.column option ;
  tkv_size_col : string GTree.column ;

  tkv_model : GTree.list_store ;
}
let global_tv_aw_midiview_info = ref None
let global_tv_aw_metaview_info = ref None
let global_tv_aw_metaview_selction = ref 0
let global_tv_aw_midiview_selction = ref 0

let tv_aw_init_trackview (kind:[`MIDI|`META]) treeview = (
  let app = get_app () in
  (* TreeView management: *)
  let tv_cols = new GTree.column_list in
  let nbrs_col = tv_cols#add Gobject.Data.int in
  let dscr_col = tv_cols#add Gobject.Data.string in
  let port_col =
    if kind = `MIDI then Some (tv_cols#add Gobject.Data.int)
    else None in
  let size_col = tv_cols#add Gobject.Data.string in
  let tv_model = GTree.list_store tv_cols in
  let nbrs_renderer =  GTree.cell_renderer_text [
    `WEIGHT `BOLD ; `FOREGROUND "White"; `FOREGROUND_SET true;
    `CELL_BACKGROUND "Black"  ; `CELL_BACKGROUND_SET true ;
  ] in
  let dscr_renderer =  GTree.cell_renderer_text [
    `FAMILY "Monospace" ;
    `FOREGROUND "White"; `FOREGROUND_SET true;
    `CELL_BACKGROUND "black"  ; `CELL_BACKGROUND_SET true ;
  ] in
  let stat_renderer =  GTree.cell_renderer_progress [
    (* `FAMILY "Monospace" ; *)
  ] in
  let size_renderer =  GTree.cell_renderer_text [
    `FAMILY "Monospace" ;
    `FOREGROUND "White"; `FOREGROUND_SET true;
    `CELL_BACKGROUND "black"  ; `CELL_BACKGROUND_SET true ;
  ] in
  let nbrs_vcol =
    GTree.view_column ~title:"N°"
    ~renderer:(nbrs_renderer, ["text", nbrs_col]) () in
  nbrs_vcol#set_cell_data_func nbrs_renderer (
    fun (model:GTree.model) iter ->
      let nb = (model#get ~row:iter ~column:nbrs_col) in
      let color = 
        if (
          ( kind = `META && !global_tv_aw_metaview_selction = nb ) ||
          ( kind = `MIDI && !global_tv_aw_midiview_selction = nb ) 
        ) then (
          S.selected_color ) else (S.normal_color) in
      nbrs_renderer#set_properties [
        `FOREGROUND "#AAAAAA" ; `FOREGROUND_SET true;
        `CELL_BACKGROUND color  ; `CELL_BACKGROUND_SET true ;
      ];
  );
  let dscr_vcol =
    GTree.view_column ~title:"Track Description" 
    ~renderer:(dscr_renderer, ["text", dscr_col]) () in
  let size_vcol =
    GTree.view_column ~title:"Size" 
    ~renderer:(size_renderer, ["text", size_col]) () in
  let stat_vcol =
    GTree.view_column ~title:"Status" 
    ~renderer:(stat_renderer, ["text", nbrs_col]) () in
  stat_vcol#set_cell_data_func stat_renderer (
    fun (model:GTree.model) iter ->
      let nb = (model#get ~row:iter ~column:nbrs_col) in
      let (lgth, plays,schedplay,schedstop) =
        App.get_track_stat app nb in
      let status,color = S.status_color_of_bools (plays,schedplay,schedstop) in
      let tick = App.get_current_tick app in
      let percent =
        (100 * (tick mod lgth)) / lgth in

      stat_renderer#set_properties [
        `TEXT (Some status) ; `VALUE percent ;
        (* `FOREGROUND "#111111" ; `FOREGROUND_SET true; *)
        `CELL_BACKGROUND color  ; `CELL_BACKGROUND_SET true ;
      ];
  );
  (* nbrs_vcol#set_visible true ; *)

  ignore( treeview#append_column nbrs_vcol );
  ignore( treeview#append_column stat_vcol );
  ignore( treeview#append_column dscr_vcol );
  if (kind = `MIDI) then (
    let port_renderer =  GTree.cell_renderer_text [
      `FAMILY "Monospace" ;
      `FOREGROUND "White"; `FOREGROUND_SET true;
      `CELL_BACKGROUND "#222222"  ; `CELL_BACKGROUND_SET true ;
    ] in
    let port_vcol =
      GTree.view_column ~title:"Port" 
      ~renderer:(port_renderer, ["text", (get_option port_col)]) () in
    ignore( treeview#append_column port_vcol );
  );
  ignore( treeview#append_column size_vcol );

  treeview#selection#set_mode `NONE ;

  ignore (
    treeview#event#connect#button_release ~callback:(
      fun ev ->
        let x = int_of_float (GdkEvent.Button.x ev) in
        let y = int_of_float (GdkEvent.Button.y ev) in
        let tupl =  treeview#get_path_at_pos ~x ~y in
        if (tupl <> None) then (
          let (path, colu, i, j) = get_option tupl in
          let iter = tv_model#get_iter path in
          let id = tv_model#get ~row:iter ~column:nbrs_col in
          let _ =
            match GdkEvent.Button.button ev with
            | 1 -> App.custom_event app (global_mouse_l_click_offset + id) ;
            | 3 -> App.custom_event app (global_mouse_m_click_offset + id) ;
            | _ -> () ;
          in 
          let _ = match kind with
          | `MIDI -> global_tv_aw_midiview_selction := id
          | `META -> global_tv_aw_metaview_selction := id
          in
          GtkBase.Widget.queue_draw treeview#as_widget ;
          true
        ) else (
          false
        )
    );
  );

  GtkBase.Widget.queue_draw treeview#as_widget ;

  Some {
    tkv_nbrs_col = nbrs_col ;
    tkv_dscr_col = dscr_col ;
    tkv_port_col = port_col ;
    tkv_size_col = size_col ;
    tkv_model = tv_model;
  } 
)
let tv_aw_update_track_view (kind:[`MIDI|`META]) treeview tv_info  = (
  let m_info = 
    match kind with 
    | `MIDI -> App.get_midi_tracks_information (get_app ())
    | `META -> App.get_meta_tracks_information (get_app ())
  in
  let m_tknb = Array.length m_info in

  let nbrs_col = tv_info.tkv_nbrs_col  in
  let dscr_col = tv_info.tkv_dscr_col  in
  let size_col = tv_info.tkv_size_col  in
  let tv_model = tv_info.tkv_model     in

  (* optimization: we change the rows without GUI updating: *)
  treeview#set_model None;
  tv_model#clear () ;
  for i = 1 to m_tknb do
    let id,name,port,lgth = m_info.(i-1) in
    let iter = tv_model#append () in
    tv_model#set ~row:iter ~column:nbrs_col id ;
    let pqn = snd (App.get_bpm_ppqn (get_app ())) in
    let size_str = S.string_of_length lgth pqn in
    tv_model#set ~row:iter ~column:dscr_col name ;

    if kind = `MIDI then (
      tv_model#set ~row:iter ~column:(get_option tv_info.tkv_port_col) port ;
    );
    tv_model#set ~row:iter ~column:size_col (Printf.sprintf "%s" size_str) ;

    App.add_uniq_custom_unsaved_action (get_app ()) (global_mouse_l_click_offset + id)
    (`toggle_track, `direct_int id) ;
    App.add_uniq_custom_unsaved_action (get_app ()) (global_mouse_m_click_offset + id)
    (`schedule_toggle_track, `direct_int id) ;
  done;
  App.update_input_mgr (get_app ()) ;

  treeview#set_model (Some tv_model#coerce);
  ();
)

let tv_aw_update_midi_view () = (
  let tv_info = (get_option !global_tv_aw_midiview_info) in
  let g_mw = (get_aw ()) in
  tv_aw_update_track_view `MIDI g_mw#treeview_midi  tv_info ;
  global_tv_aw_midiview_selction :=  0 ;
)
let tv_aw_update_meta_view () = (
  let tv_info = (get_option !global_tv_aw_metaview_info) in
  let g_mw = (get_aw ()) in
  tv_aw_update_track_view `META g_mw#treeview_meta  tv_info ;
  global_tv_aw_metaview_selction :=  0 ;
)

type tv_aw_iactview_info = {
  imv_nbrs_col :   int  GTree.column ;
  imv_dscr_col : string GTree.column ;
  imv_model : GTree.list_store ;
}
let global_tv_aw_iactview_info = ref None

let tv_aw_init_iact_view () = (
  let treeview = (get_aw ())#treeview_iact in
  (* TreeView management: *)
  let tv_cols = new GTree.column_list in
  let nbrs_col = tv_cols#add Gobject.Data.int in
  let dscr_col = tv_cols#add Gobject.Data.string in
  let tv_model = GTree.list_store tv_cols in
  (* let nbrs_renderer =  GTree.cell_renderer_text [
    `WEIGHT `BOLD ; `FOREGROUND "White"; `FOREGROUND_SET true;
    `CELL_BACKGROUND "Black"  ; `CELL_BACKGROUND_SET true ;
  ] in *)
  let dscr_renderer =  GTree.cell_renderer_text [
    `FAMILY "Monospace" ;
    `FOREGROUND "White"; `FOREGROUND_SET true;
    `CELL_BACKGROUND "black"  ; `CELL_BACKGROUND_SET true ;
  ] in
  (* let nbrs_vcol = GTree.view_column ~title:"N°"
    ~renderer:(nbrs_renderer, ["text", nbrs_col]) () in *)
  let dscr_vcol =
    GTree.view_column ~title:"Track Description" 
    ~renderer:(dscr_renderer, ["text", dscr_col]) () in

  (* ignore( treeview#append_column nbrs_vcol ); *)
  ignore( treeview#append_column dscr_vcol );

  treeview#selection#set_mode `MULTIPLE ;

  GtkBase.Widget.queue_draw treeview#as_widget ;

  Some {
    imv_nbrs_col = nbrs_col ;
    imv_dscr_col = dscr_col ;
    imv_model = tv_model;
  } 
)
let tv_aw_update_iact_view () = (
  let m_info = App.get_input_action_list (get_app ())  in

  let nbrs_col = (get_option !global_tv_aw_iactview_info).imv_nbrs_col  in
  let dscr_col = (get_option !global_tv_aw_iactview_info).imv_dscr_col  in
  let tv_model = (get_option !global_tv_aw_iactview_info).imv_model     in
  let treeview = (get_aw ())#treeview_iact in
  treeview#set_model None;
  tv_model#clear () ;
  let count = ref 0 in
  List.iter (
    fun iact ->
      try (
        let desc = S.iact_to_string iact in
        (* This occurs only if the input is not mouse: (exception otherwise) *)
        let iter = tv_model#append () in
        tv_model#set ~row:iter ~column:nbrs_col !count ;
        incr count ;
        tv_model#set ~row:iter ~column:dscr_col desc ;
        (* Log.p "Model: %d: %s\n" !count desc ; *)
      ) with
      | Invalid_argument "index out of bounds" -> ()
      | exn -> (
        Log.p "Exception in tv_aw_update_iact_view: %s\n"
        (Printexc.to_string exn);()
      )
  ) m_info ;
  treeview#set_model (Some tv_model#coerce);
  GtkBase.Widget.queue_draw treeview#as_widget ;
)

(******************************************************************************)
(* BOTTONS' CALLBACKS: *)

let b_aw_add_midi    () = (
  let file_ok_sel fw () =
    App.add_midi_file (get_app ()) fw#filename;
    fw#destroy ();
    tv_aw_update_midi_view () ;
  in
  let filew =
    GWindow.file_selection ~title:"Select the midi-file" ~modal:true 
    ~select_multiple:false ~resizable:true
    ()
  in
  ignore(filew#ok_button#connect#clicked ~callback:(file_ok_sel filew));
  ignore(filew#cancel_button#connect#clicked ~callback:filew#destroy);

  filew#show ();
)
let b_aw_edit_midi   () = (
  let tk_id = !global_tv_aw_midiview_selction in

  if (tk_id = 0) then (
    aw_append_msg `ERR "You haven't selected any midi track..." ;
  ) else (

    let name,port,length =
      App.get_midi_track_information (get_app ()) tk_id in
    let ew = new GenGui.midi_window () in
    ignore(ew#midi_window#connect#destroy ~callback:ew#midi_window#destroy);
    ignore(ew#button_cancel#connect#clicked ~callback:ew#midi_window#destroy);

    ew#entry_name#set_text name ;

    let port_combo = 
      GEdit.combo_box_text
      ~strings:(Array.to_list S.out_put_ports)
      ~add_tearoffs:false
      (* ~active:1 *)
      (* ~allow_empty:false ~value_in_list:true *)
      ~packing:ew#hbox_port#add ()
    in
    (fst port_combo)#set_active port ;


    let mult_combo = util_make_time_combo_box ew#hbox_length#add in
    let _,p = App.get_bpm_ppqn (get_app()) in
    let lgth,unity = S.unitize_length length p in
    let cb,_ = mult_combo in
    cb#set_active unity ;
    ew#spinbutton_lgth#adjustment#set_value (float lgth) ;

    ignore(ew#button_ok#connect#clicked ~callback:(
      fun () ->
        App.set_midi_track_information (get_app ()) tk_id
        ew#entry_name#text (fst port_combo)#active (
          (int_of_float ew#spinbutton_lgth#value) *
          (util_time_of_combo_box mult_combo)
        );
        tv_aw_update_midi_view () ;
        ew#midi_window#destroy ();
    ));

    ew#midi_window#show () ;
  )
)
let b_aw_suppr_midi  () = (
  let tk_id = !global_tv_aw_midiview_selction in
  if (tk_id = 0) then (
    aw_append_msg `ERR "You haven't selected any midi track..." ;
  ) else (
    App.remove_midi_track (get_app ()) tk_id ;
    tv_aw_update_midi_view () ;
  );
)

let add_or_edit_meta (the_track: int option) = (
  let ew = new GenGui.meta_window () in
  ignore(ew#meta_window#connect#destroy ~callback:ew#meta_window#destroy);
  ignore(ew#button_cancel#connect#clicked ~callback:ew#meta_window#destroy);

  let meta_cols = new GTree.column_list in
  let events_col = meta_cols#add Gobject.Data.string in
  let index_col = meta_cols#add Gobject.Data.int in
  let meta_model = GTree.list_store meta_cols in
  let meta_renderer =  GTree.cell_renderer_text [
    `FOREGROUND "White"; `FAMILY "Monospace" ; `FOREGROUND_SET true;
    `CELL_BACKGROUND "Black"  ; `CELL_BACKGROUND_SET true ;
  ] in
  let events_vcol =
    GTree.view_column ~title:"Track Description" 
    ~renderer:(meta_renderer, ["text", events_col]) () in
  ignore( ew#treeview#append_column events_vcol );

  let real_events_input =
    match the_track with
    | None -> ref []
    | Some tk_id -> ref (App.get_meta_track (get_app()) tk_id)
  in

  let update_model  () =
    ew#treeview#set_model None;
    meta_model#clear () ;
    let index = ref 0 in
    List.iter (
      fun event ->
        let iter = meta_model#append () in
        meta_model#set ~row:iter ~column:events_col (
          S.string_of_meta_event event (snd (App.get_bpm_ppqn (get_app())))) ;
        meta_model#set ~row:iter ~column:index_col !index ;
        incr index ;
    ) !real_events_input ;
    ew#treeview#set_model (Some meta_model#coerce);
  in
  update_model  () ;


  let mult_combo = util_make_time_combo_box ew#hbox_name_lgth#add in

  let _ =
    match the_track with
    | None -> () | Some tk_id -> (
      let name,size = (App.get_meta_track_information (get_app ()) tk_id) in
      ew#entry_name#set_text name ;
      let _,p = App.get_bpm_ppqn (get_app()) in
      let lgth,unity = S.unitize_length size p in
      let cb,_ = mult_combo in
      cb#set_active unity ;
      ew#spinbutton_lgth#adjustment#set_value (float lgth) ;
    ) in

  ignore(ew#button_add#connect#clicked ~callback:(
    fun () ->
      let box = ew#hbox_edit in
      List.iter (fun w -> w#destroy ()) box#children ;
      let menu = GMenu.menu () in
      let menuitem_tkson =
        GMenu.menu_item ~label:"Set Track On" ~packing:menu#append () in
      ignore(menuitem_tkson#connect#activate ~callback:(
        fun () ->
          let _ =
            GMisc.label ~text:("Track ON:  ID: ") ~packing:(box#add) () in
          let track_adj =
            GData.adjustment ~value:0.0 ~lower:(-2000.0) ~upper:2100.0
            ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
          let _ =
            GEdit.spin_button ~adjustment:track_adj ~packing:(box#add) () in
          let _ =
            GMisc.label ~text:(" at ")  ~packing:(box#add) () in
          let begin_adj =
            GData.adjustment ~value:0.0 ~lower:0.0 ~upper:10000.0
            ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
          let _ =
            GEdit.spin_button ~adjustment:begin_adj ~packing:(box#add) () in
          let t_beg_mult = util_make_time_combo_box box#add in
          let button_ok = GButton.button ~label:"Add"  ~packing:(box#add) () in
          ignore(button_ok#connect#clicked ~callback:(
            fun () -> 
              let tk_id = int_of_float track_adj#value in
              let beg =
                (int_of_float begin_adj#value)
                * (util_time_of_combo_box t_beg_mult) in
              real_events_input := (`track_set_on (beg,tk_id))::!real_events_input ;
              List.iter (fun w -> w#destroy ()) box#children ;
              update_model ();
          ));
      ));
      let menuitem_tksoff=
        GMenu.menu_item ~label:"Set Track Off" ~packing:menu#append () in
      ignore(menuitem_tksoff#connect#activate ~callback:(
        fun () ->
          let _ =
            GMisc.label ~text:("Track OFF:  ID: ") ~packing:(box#add) () in
          let track_adj =
            GData.adjustment ~value:0.0 ~lower:(-2000.0) ~upper:2100.0
            ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
          let _ =
            GEdit.spin_button ~adjustment:track_adj ~packing:(box#add) () in
          let _ =
            GMisc.label ~text:(" at ")  ~packing:(box#add) () in
          let begin_adj =
            GData.adjustment ~value:0.0 ~lower:0.0 ~upper:10000.0
            ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
          let _ =
            GEdit.spin_button ~adjustment:begin_adj ~packing:(box#add) () in
          let t_beg_mult = util_make_time_combo_box box#add in
          let button_ok = GButton.button ~label:"Add"  ~packing:(box#add) () in
          ignore(button_ok#connect#clicked ~callback:(
            fun () -> 
              let tk_id = int_of_float track_adj#value in
              let beg =
                (int_of_float begin_adj#value)
                * (util_time_of_combo_box t_beg_mult) in
              real_events_input := (`track_set_off (beg,tk_id))::!real_events_input ;
              List.iter (fun w -> w#destroy ()) box#children ;
              update_model ();
          ));
      ));
      let menuitem_sbpm  =
        GMenu.menu_item ~label:"Set BPM" ~packing:menu#append () in
      ignore(menuitem_sbpm#connect#activate ~callback:(
        fun () ->
          let _ =
            GMisc.label ~text:("Set BPM: ") ~packing:(box#add) () in
          let track_adj =
            GData.adjustment ~value:0.0 ~lower:(0.0) ~upper:2100.0
            ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
          let _ =
            GEdit.spin_button ~adjustment:track_adj ~packing:(box#add) () in
          let _ =
            GMisc.label ~text:(" at ")  ~packing:(box#add) () in
          let begin_adj =
            GData.adjustment ~value:0.0 ~lower:0.0 ~upper:10000.0
            ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
          let _ =
            GEdit.spin_button ~adjustment:begin_adj ~packing:(box#add) () in
          let t_beg_mult = util_make_time_combo_box box#add in
          let button_ok = GButton.button ~label:"Add"  ~packing:(box#add) () in
          ignore(button_ok#connect#clicked ~callback:(
            fun () -> 
              let bpm = int_of_float track_adj#value in
              let beg =
                (int_of_float begin_adj#value)
                * (util_time_of_combo_box t_beg_mult) in
              real_events_input := (`set_bpm (beg,bpm))::!real_events_input ;
              List.iter (fun w -> w#destroy ()) box#children ;
              update_model ();
          ));
      ));
      let menuitem_tkon  =
        GMenu.menu_item ~label:"Keep Track On" ~packing:menu#append () in
      ignore(menuitem_tkon#connect#activate ~callback:(
        fun () ->
          let _ =
            GMisc.label ~text:("Keep Track ON:  ID: ") ~packing:(box#add) () in
          let track_adj =
            GData.adjustment ~value:0.0 ~lower:(-2000.0) ~upper:2100.0
            ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
          let _ =
            GEdit.spin_button ~adjustment:track_adj ~packing:(box#add) () in
          let _ =
            GMisc.label ~text:(" from ")  ~packing:(box#add) () in
          let begin_adj =
            GData.adjustment ~value:0.0 ~lower:0.0 ~upper:10000.0
            ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
          let _ =
            GEdit.spin_button ~adjustment:begin_adj ~packing:(box#add) () in
          let t_beg_mult = util_make_time_combo_box box#add in
          let _ =
            GMisc.label ~text:(" to ")  ~packing:(box#add) () in
          let end_adj =
            GData.adjustment ~value:0.0 ~lower:0.0 ~upper:10000.0
            ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
          let _ =
            GEdit.spin_button ~adjustment:end_adj ~packing:(box#add) () in
          let t_end_mult = util_make_time_combo_box box#add in
          let button_ok = GButton.button ~label:"Add"  ~packing:(box#add) () in
          ignore(button_ok#connect#clicked ~callback:(
            fun () -> 
              let tk_id = int_of_float track_adj#value in
              let beg =
                (int_of_float begin_adj#value)
                * (util_time_of_combo_box t_beg_mult) in
              let e_d =
                (int_of_float end_adj#value)
                * (util_time_of_combo_box t_end_mult)  in
              real_events_input := (`track_on (beg,e_d,tk_id))::!real_events_input ;

              List.iter (fun w -> w#destroy ()) box#children ;
              update_model ();
          ));
      ));
      menu#popup  ~button:1 ~time:0l ;
  ));
  ignore(ew#button_suppr#connect#clicked ~callback:(
    fun () ->
      List.iter (
        fun x ->
          let index =
            let iter = meta_model#get_iter x in
            meta_model#get ~row:iter ~column:index_col
          in
          let cur =  ref (-1) in
          real_events_input := List.filter (
            fun _ -> 
              incr cur ;
              !cur <> index
          ) !real_events_input ;
          update_model ();
      ) ew#treeview#selection#get_selected_rows ;
  ));

  ignore(ew#button_ok#connect#clicked ~callback:(
    fun () ->
      let what_to_call =
        match the_track with
        | None ->  App.add_meta_track (get_app ())
        | Some tk_id -> App.replace_meta_track (get_app ()) tk_id
      in
      what_to_call ew#entry_name#text (
        (int_of_float ew#spinbutton_lgth#value) *
        (util_time_of_combo_box mult_combo)
      ) !real_events_input ;
      tv_aw_update_meta_view () ;
      ew#meta_window#destroy ();
  ));

  ew#meta_window#show () ;
)
let b_aw_add_meta () = (
  add_or_edit_meta None ;
)
let b_aw_edit_meta   () = (
  if (!global_tv_aw_metaview_selction <> 0) then ( 
    add_or_edit_meta (Some !global_tv_aw_metaview_selction) ;
  ) else (
    aw_append_msg `ERR "You should select one track before..."
  );
)
let b_aw_suppr_meta  () = (
  let tk_id = !global_tv_aw_metaview_selction in
  if (tk_id = 0) then (
    aw_append_msg `ERR "You haven't selected any meta track..." ;
  ) else (
    App.remove_meta_track (get_app ()) tk_id ;
    tv_aw_update_meta_view () ;
  );
)

let  add_or_edit_iact ?to_edit () = (
  let iw = new GenGui.iact_window () in
  ignore(iw#iact_window#connect#destroy ~callback:iw#iact_window#destroy);
  ignore(iw#button_cancel#connect#clicked ~callback:iw#iact_window#destroy);
  let values_to_get = 
    match to_edit with 
    | None -> iw#label_msg#set_label S.create_new_handler ; false
    | Some (i,m) -> iw#label_msg#set_label (S.edit_handler m) ; true
  in
  let input_type_combo = 
    GEdit.combo_box_text ~strings:[ S.keyboard ; S.midi_evt ]
    ~packing:iw#hbox_input#add () in
  let arg_input_hbox = GPack.hbox ~packing:iw#hbox_input#add () in
  let the_input_box,_ = input_type_combo in
  let current_input = ref "" in
  let rest_of_input = [| -1 ; -1 ; -1 ; -1 ; -1 |] in
  let on_changed_callback ?(get_values=false) () =
      let choice = GEdit.text_combo_get_active input_type_combo in
      if (choice <> Some !current_input) then (
        List.iter (fun w -> w#destroy ()) arg_input_hbox#children ;
        match choice with
        | Some s when s = S.keyboard -> (
          let key_entry =
            GEdit.combo_box_text ~strings:(
              Array.to_list S.global_available_keys)
            ~packing:arg_input_hbox#add () in
          if get_values then (
            (fst key_entry)#set_active rest_of_input.(0) ;
          );
          let cbo,_ = key_entry in
          ignore(cbo#connect#changed ~callback:(
            fun () ->
              match GEdit.text_combo_get_active key_entry with
              | Some c -> (
                rest_of_input.(0) <- (S.key_to_int c);
              )
              | None -> () ;
          ));
          current_input := S.keyboard ;
        )
        | Some s when s = S.midi_evt -> (
          let stat_entry = 
            GEdit.combo_box_text ~strings:S.global_available_midi_events
            ~packing:arg_input_hbox#add () in
          let cbo,_ = stat_entry in
          ignore(cbo#connect#changed ~callback:( fun () ->
            match GEdit.text_combo_get_active stat_entry with
            | Some c -> rest_of_input.(1) <- S.midi_status_of_string c
            | None -> () ;
          ));
          let _ =  GMisc.label ~text:" chan: " ~packing:arg_input_hbox#add () in
          let chan_entry = 
            GEdit.combo_box_text ~strings:S.midi_channel_strings
            ~packing:arg_input_hbox#add () in
          let cbo,_ = chan_entry in
          ignore(cbo#connect#changed ~callback:( fun () ->
            rest_of_input.(2) <- 
              S.midi_channel_of_string (GEdit.text_combo_get_active chan_entry) ;
          ));
          let _ =  GMisc.label ~text:" note: " ~packing:arg_input_hbox#add () in
          let note_adj =
            GData.adjustment ~value:(-1.0) ~lower:(-1.0) ~upper:256.0
            ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
          let spin =
            GEdit.spin_button ~adjustment:note_adj ~packing:(arg_input_hbox#add) () in
          ignore (spin#connect#changed ~callback:( fun () ->
            rest_of_input.(3) <- int_of_float note_adj#value ;));
          let _ =  GMisc.label ~text:" velocity: " ~packing:arg_input_hbox#add () in
          let velo_adj =
            GData.adjustment ~value:(-1.0) ~lower:(-1.0) ~upper:256.0
            ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
          let spin =
            GEdit.spin_button ~adjustment:velo_adj ~packing:(arg_input_hbox#add) () in
          ignore (spin#connect#changed ~callback:( fun () ->
            rest_of_input.(4) <- int_of_float velo_adj#value ;));
          current_input := S.midi_evt ;
          if get_values then (
            (fst stat_entry)#set_active (S.int_of_midi_status rest_of_input.(1));
            (fst chan_entry)#set_active (rest_of_input.(2)+1) ;
            note_adj#set_value (float rest_of_input.(3)) ;
            velo_adj#set_value (float rest_of_input.(4)) ;
          );
        )
        | _ -> Log.p "Problem... in b_aw_add_iact\n" ;
      );
  in
  ignore(the_input_box#connect#changed 
  ~callback:(on_changed_callback ~get_values:values_to_get));

  let action_combo = 
    GEdit.combo_box_text ~strings:S.action_strings
    ~packing:iw#hbox_action#add () in
  let arg_combo = 
    GEdit.combo_box_text ~strings:S.argument_string_list
    ~packing:iw#hbox_action#add () in
  let act_adj =
    GData.adjustment ~value:(0.0) ~lower:(-256.0) ~upper:256.0
    ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
  let _ =
    GEdit.spin_button ~adjustment:act_adj ~packing:iw#hbox_action#add  () in

  if to_edit <> None then (
    let index = fst (get_option to_edit) in
    let inp,(act,arg) =
      List.nth (App.get_input_action_list (get_app ())) index in
    let _ =
      match inp with
      | `midi_event (a,b,c,d) ->
          (* current_input := S.midi_evt ; *)
          rest_of_input.(0)<- -1;
          rest_of_input.(1)<-a;
          rest_of_input.(2)<-b;
          rest_of_input.(3)<-c;
          rest_of_input.(4)<-d;
          (fst input_type_combo)#set_active 0 ;
          (fst input_type_combo)#set_active 1 ;
      | `custom a ->
          (* current_input := S.keyboard ; *)
          rest_of_input.(0)<-  a;
          rest_of_input.(1)<- -1;
          rest_of_input.(2)<- -1;
          rest_of_input.(3)<- -1;
          rest_of_input.(4)<- -1;
          (fst input_type_combo)#set_active 0 ;
    in
    let _ =
      match act with
      | `set_BPM               ->  (fst action_combo)#set_active 0 ;
      | `incr_BPM              ->  (fst action_combo)#set_active 1 ;
      | `decr_BPM              ->  (fst action_combo)#set_active 2 ;
      | `toggle_track          ->  (fst action_combo)#set_active 3 ;
      | `track_on              ->  (fst action_combo)#set_active 4 ;
      | `track_off             ->  (fst action_combo)#set_active 5 ;
      | `schedule_toggle_track ->  (fst action_combo)#set_active 6 ;
      | `schedule_track_on     ->  (fst action_combo)#set_active 7 ;
      | `schedule_track_off    ->  (fst action_combo)#set_active 8 ;
      | `play                  ->  (fst action_combo)#set_active 9 ;
      | `stop                  ->  (fst action_combo)#set_active 10 ;
      | `mute_all              ->  (fst action_combo)#set_active 11 ;
    in
    let _ =
      match arg with
      | `direct_int i  -> (
        (fst arg_combo)#set_active 0 ; act_adj#set_value (float i)
      )
      | `midi_status   -> (fst arg_combo)#set_active 1 ;
      | `midi_channel  -> (fst arg_combo)#set_active 2 ;
      | `midi_note     -> (fst arg_combo)#set_active 3 ;
      | `midi_velocity -> (fst arg_combo)#set_active 4 ;
    in

    ()
  );

  ignore(iw#button_ok#connect#clicked ~callback:( fun () ->
    try (
      let input = 
        match !current_input with
        | s when s = S.keyboard -> `custom rest_of_input.(0)
        | s when s = S.midi_evt ->
            `midi_event (rest_of_input.(1), rest_of_input.(2),
            rest_of_input.(3), rest_of_input.(4))
        | _ -> (
          iw#label_msg#set_text S.err_choose_input_type;
          raise (Not_found);
        )
      in 
      let action =
        match (GEdit.text_combo_get_active action_combo) with
        | Some s -> S.action_of_string s
        | None -> (
          iw#label_msg#set_text S.err_choose_action_type;
          raise (Not_found);
        )
      in
      let argument = 
        match (GEdit.text_combo_get_active arg_combo) with
        | Some s -> S.arg_spec_of_string s (int_of_float act_adj#value)
        | None -> (
          iw#label_msg#set_text S.err_choose_argument_type;
          raise Not_found;
        )
      in
      if (!current_input = S.keyboard) then (
        match argument with
        |`direct_int _ -> ()
        | _ -> (
          iw#label_msg#set_label S.err_choose_argument_type;
          raise Not_found;
        )
      );
      let _ = 
        match to_edit with 
        | None -> App.basic_add_handler (get_app ()) (input, (action,argument)) ;
        | Some (i,m) -> App.replace_handler (get_app ()) i (input, (action,argument)) ;
      in
      tv_aw_update_iact_view ();
      iw#iact_window#destroy () ;
    ) with Not_found -> () ;
  ));
  iw#iact_window#show () ;
)
let b_aw_add_iact  () = (
  add_or_edit_iact () ;
)
let b_aw_edit_iact   () = (
  let info = get_option !global_tv_aw_iactview_info in
  let iact_model = info.imv_model in
  let index_col = info.imv_nbrs_col in
  let str_col = info.imv_dscr_col in
  List.iter (
    fun x ->
      let iter = iact_model#get_iter x in
      let index = iact_model#get ~row:iter ~column:index_col in
      let str = iact_model#get ~row:iter ~column:str_col in
      add_or_edit_iact ~to_edit:(index,str) () ;
  ) (get_aw ())#treeview_iact#selection#get_selected_rows ;
  tv_aw_update_iact_view () ;
)
let b_aw_suppr_iact  () = (
  (* let ll = (get_aw ())#treeview_iact#selection#get_selected_rows in *)
  let info = get_option !global_tv_aw_iactview_info in
  let iact_model = info.imv_model in
  let index_col = info.imv_nbrs_col in
  let index_list = ref [] in
  List.iter (
    fun x ->
      let index =
        let iter = iact_model#get_iter x in
        iact_model#get ~row:iter ~column:index_col
      in
      index_list := index::!index_list ;
  ) (get_aw ())#treeview_iact#selection#get_selected_rows ;
  App.remove_inpacts (get_app()) !index_list ;
  tv_aw_update_iact_view () ;
)

(* Buttons for settings ant "file" actions: *)
let b_aw_saveas ?(and_then=fun () -> ()) () = (
  let file_ok_sel fw () =
    App.save_to_file (get_app ()) fw#filename;
    aw_update_title () ;
    fw#destroy ();
    lb_aw_update_filename () ;
    and_then () ;
  in
  let filew = GWindow.file_selection ~title:"Save As" ~modal:true () in
  ignore(filew#ok_button#connect#clicked ~callback:(file_ok_sel filew));
  ignore(filew#cancel_button#connect#clicked ~callback:filew#destroy);
  filew#show ();
)
let b_aw_save   ?(and_then=fun ()->())  () = (
  let fil = App.get_filename (get_app ()) in
  Log.p "Filename: \"%s\"\n" fil ;
  if (fil <> "") then (
    App.save_to_file (get_app ()) fil;
    and_then () ;
    lb_aw_update_filename () ;
  ) else (
    b_aw_saveas ~and_then () ;
  );
)

let util_make_achtung_notsaved_dialog ~msg ~and_then ~save ~dont ~cancel = (

  let dlg = new GenGui.dialog_notsaved () in
  dlg#dialog_notsaved#set_title S.notsaved_title ;
  dlg#button_ok#set_label save ;
  ignore(dlg#button_ok#connect#clicked ~callback:(
    fun () ->
      Log.p "but ok\n" ;
      dlg#dialog_notsaved#destroy () ;
      b_aw_save ~and_then () ;
  ));
  dlg#button_dsq#set_label dont ;
  ignore(dlg#button_dsq#connect#clicked ~callback:(
    fun () ->
      Log.p "bye...\n" ;
      dlg#dialog_notsaved#destroy () ;
      and_then () ;
  ));
  dlg#button_cancel#set_label cancel ;
  ignore(dlg#button_cancel#connect#clicked ~callback:(
    fun () ->
      dlg#dialog_notsaved#destroy () ;
  ));
  dlg#label_msg#set_text msg ;
  dlg#dialog_notsaved#show ();
)
let quit_app () = (

  Log.p "quit_app : %b\n" (App.is_saved (get_app ()));
  if not (App.is_saved (get_app ())) then (
    util_make_achtung_notsaved_dialog ~msg:S.save_before_quit_msg
    ~save:S.save_before_quit_save ~dont:S.save_before_quit_dont
    ~cancel:S.save_before_quit_cancel ~and_then:GMain.quit ;
  ) else (
    GMain.quit () ;
  );
)

let b_aw_quit   () = (
  quit_app () ;
)
let b_aw_new    () = (
  let do_the_new () =
    App.clear_song (get_app ()) ;
    tv_aw_update_midi_view ();
    tv_aw_update_meta_view ();
    tv_aw_update_iact_view ();
    sp_aw_update_bpm_ppqn_values ();
    aw_update_title () ;
    lb_aw_update_filename () ;
    aw_update_qd_tt () ;
  in
  if not (App.is_saved (get_app ())) then (
    util_make_achtung_notsaved_dialog ~msg:S.save_before_new_msg
    ~save:S.save_before_new_save ~dont:S.save_before_new_dont
    ~cancel:S.save_before_new_cancel ~and_then:do_the_new ;
  ) else (
    do_the_new ();
  );
)
let b_aw_open   () = (
  let do_the_open () = 
    let filew = GWindow.file_selection ~title:"Open File" ~modal:true () in
    ignore(filew#ok_button#connect#clicked ~callback:(
      fun () ->
        App.load_of_file (get_app ()) filew#filename;
        tv_aw_update_midi_view ();
        tv_aw_update_meta_view ();
        tv_aw_update_iact_view ();
        sp_aw_update_bpm_ppqn_values ();
        aw_update_title () ;
        lb_aw_update_filename () ;
        aw_update_qd_tt () ;
        filew#destroy ();
    ));
    ignore(filew#cancel_button#connect#clicked ~callback:filew#destroy);
    filew#show ();
  in
  if not (App.is_saved (get_app ())) then (
    util_make_achtung_notsaved_dialog ~msg:S.save_before_open_msg
    ~save:S.save_before_open_save ~dont:S.save_before_open_dont
    ~cancel:S.save_before_open_cancel ~and_then:do_the_open ;
  ) else (
    do_the_open ();
  );

)
let b_aw_help   () = (Log.p "b_aw_help not implemented\n" ; )
let b_aw_play   () = (
  App.set_sequencer_info (get_app ()) (
    int_of_float (get_aw ())#spinbutton_qd#value,
    int_of_float (get_aw ())#spinbutton_tt#value
  ) ;
  App.update_input_mgr (get_app());
  App.threaded_play (fun () ->
    update_during_play () ;
  ) (get_app ());
  util_set_playing_state true ;
  aw_append_msg `LOG "start playing"
)
let b_aw_stop   () = (
  App.threaded_stop (get_app ());
  util_set_playing_state false ;
)
let b_aw_update_bpm   () = (
  App.set_bpm (get_app()) 
  (int_of_float (get_aw())#spinbutton_bpm#adjustment#value);
)
let b_aw_update_pqn   () = (
  App.set_ppqn (get_app()) 
  (int_of_float (get_aw())#spinbutton_pqn#adjustment#value);

)
let b_aw_update_sngnam   () = (
  App.set_song_name (get_app ()) (get_aw ())#entry_sngnam#text ;
  aw_update_title () ;
)

(******************************************************************************)
(* ON KEYPRESS: *)
let k_aw_on_key_press x = (
  try (
    (* Log.p "Key %s -> %d Pressed !!\n" (GdkEvent.Key.string x)  *)
    (* (S.key_to_int (GdkEvent.Key.string x)) ; *)
    App.custom_event  (get_app()) (S.key_to_int (GdkEvent.Key.string x)) ;
    false
  ) with exn ->  false (* we are not the last ones to collect the keyboard. *)
)

(******************************************************************************)
let start () = (

  let count_calls = ref 0 in
  global_app := Some (
    App.make_app
    ~visitor:(
      fun () ->
        incr count_calls ;
        if (!count_calls mod 64) = 0 then (
          update_during_play () ;
        );
    ) ()
  ) ;
  let app = get_app () in
  let mw = new GenGui.app_window () in
  global_app_window := Some mw ;
  ignore(mw#app_window#event#connect#delete ~callback:(
    fun _ ->
      quit_app () ;
      true (* we prevent window from closing *)
  ));
  mw#label_title#set_label ("<b>" ^ S.app_name ^ " " ^ (S.version ()) ^ "</b>");
  ignore(mw#button_new   #connect#clicked ~callback:b_aw_new   );
  ignore(mw#button_open  #connect#clicked ~callback:b_aw_open  );
  ignore(mw#button_save  #connect#clicked ~callback:b_aw_save  );
  ignore(mw#button_saveas#connect#clicked ~callback:b_aw_saveas);
  ignore(mw#button_quit  #connect#clicked ~callback:b_aw_quit  );


  
  (* XXX how to use it ? *)
  (* mw#button_quit#misc#add_accelerator *)
      (* sgn:(Gtk.widget, unit -> unit) GtkSignal.t -> *)
      (* group:Gtk.accel_group -> *)
      (* ?modi:Gdk.Tags.modifier list -> *)
      (* ?flags:Gtk.Tags.accel_flag list -> *)
      (* Gdk.keysym -> unit *)

  ignore(mw#button_help  #connect#clicked ~callback:b_aw_help  );
  ignore(mw#button_play  #connect#clicked ~callback:b_aw_play  );
  ignore(mw#button_stop  #connect#clicked ~callback:b_aw_stop  );
  sp_aw_update_bpm_ppqn_values () ;
  ignore(mw#button_update_bpm  #connect#clicked ~callback:b_aw_update_bpm );
  ignore(mw#button_update_pqn  #connect#clicked ~callback:b_aw_update_pqn );


  mw#entry_sngnam#set_text (App.get_song_name app) ;
  aw_update_title ();
  ignore(mw#button_update_sngnam  #connect#clicked ~callback:b_aw_update_sngnam );

  aw_update_qd_tt ();

  lb_aw_update_filename ();

  global_tv_aw_midiview_info := tv_aw_init_trackview `MIDI mw#treeview_midi ;
  tv_aw_update_midi_view () ;
  ignore(mw#button_add_midi   #connect#clicked ~callback:b_aw_add_midi   );
  ignore(mw#button_edit_midi  #connect#clicked ~callback:b_aw_edit_midi  );
  ignore(mw#button_suppr_midi #connect#clicked ~callback:b_aw_suppr_midi );

  global_tv_aw_metaview_info := tv_aw_init_trackview `META mw#treeview_meta ;
  tv_aw_update_meta_view () ;
  ignore(mw#button_add_meta   #connect#clicked ~callback:b_aw_add_meta   );
  ignore(mw#button_edit_meta  #connect#clicked ~callback:b_aw_edit_meta  );
  ignore(mw#button_suppr_meta #connect#clicked ~callback:b_aw_suppr_meta );

  global_tv_aw_iactview_info := tv_aw_init_iact_view () ;
  tv_aw_update_iact_view () ;
  ignore(mw#button_add_iact   #connect#clicked ~callback:b_aw_add_iact   );
  ignore(mw#button_edit_iact  #connect#clicked ~callback:b_aw_edit_iact  );
  ignore(mw#button_suppr_iact #connect#clicked ~callback:b_aw_suppr_iact );

  ignore(mw#app_window#event#connect#key_press ~callback:k_aw_on_key_press);

  util_set_playing_state false ;
  aw_append_msg `LOG "GUI initialized." ;

  GtkThread.main () ; (* Needed for Thread usage !!! *)
)


