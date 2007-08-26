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


(**
 Generic track editor that should work for MIDI and META events.
 {b Work in progress}
 @author S. Mondet
 *)


module App = SeqApp
module S = StringServer

(** {3 Internal GUI utilities} *)

(** append a label to a box *)
let util_append_label text (box:GPack.box) = (
  let _ =
    GMisc.label ~text
    ~packing:(box#pack ~expand:false ~fill:false ~padding:0) () in
  ()
)

(** Append a vertical separator to a box *)
let util_append_vertsepar (box:GPack.box) = (
  let sep =
    GMisc.separator `VERTICAL
    ~packing:(box#pack ~expand:false ~fill:false ~padding:0) 
    ~show:true () in
  sep#misc#set_size_request ~width:30 ();
  ()
)

(** Append an horizontal separator to a box *)
let util_append_horzsepar (box:GPack.box) = (
  let sep =
    GMisc.separator `HORIZONTAL
    ~packing:(box#pack ~expand:false ~fill:false ~padding:0) ~show:true () in
  sep#misc#set_size_request ~width:10 ();
  ()
)
let util_append_button text (box:GPack.box) = (
  GButton.button ~label:text
  ~packing:(box#pack ~expand:false ~fill:false ~padding:0)  ~show:true ()
)
let util_append_toggle text (box:GPack.box) = (
  GButton.toggle_button ~label:text ~active:false ~relief:`NORMAL
  ~packing:(box#pack ~expand:false ~fill:false ~padding:0)  ~show:true ()
)

let util_append_hbox (box:GPack.box) = (
  GPack.hbox  ~homogeneous:false
  ~packing:(box#pack ~expand:false ~fill:false ~padding:0) ~show:true ()
)
let util_int_spin_button min max box = (
  let adj =
    GData.adjustment ~value:min ~lower:min ~upper:max
    ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
  GEdit.spin_button ~adjustment:adj ~packing:(box#add) ()
)

(******************************************************************************)
(** {3 More generic utilities} *)

let util_note_octave_of_event ev = (
  ev.Midi.data_1 mod 12 , (ev.Midi.data_1 / 12) + 1
)
let util_note_of_val_octave value octave = (octave - 1) * 12 + value

let util_note_to_string note = (
  let value,octave = util_note_octave_of_event note in
  let virtual_note = StringServer.note_names.(value) in
  let chan = note.Midi.channel in
  Printf.sprintf "NOTE: %s%d -> %d" virtual_note octave chan
)

let util_midi_event_to_string midi_ev = (
  let cmd = Midi.midi_cmd_of_event midi_ev in
  let chan = midi_ev.Midi.channel in
  Printf.sprintf "%s -> %d" (Midi.midi_cmd_to_string cmd) chan
)

(******************************************************************************)
(** {3 The model} *)

(** A {i minimalistic} variant to optimize matching *)
type track_type = MIDI_TRACK | META_TRACK

type editable_event =
  | EE_None
  | EE_Midi of Midi.midi_event
  | EE_MidiNote of (Midi.midi_event * Midi.midi_event) list
  | EE_MetaSpec of Tracker.meta_action_spec


(** An {i ugly} mapping structure that encloses both midi and meta tracks for
 the editor  *)
type track_values = {
  mutable tv_app: SeqApp.seq_app;
  mutable tv_tk_id: int;
  mutable tv_type : track_type;
  mutable tv_type_str: string;
  mutable tv_name: string;
  mutable tv_length_b: int;
  mutable tv_length_q: int;
  mutable tv_length_t: int;
  mutable tv_pqn : int ;
  mutable tv_port: int;
  mutable tv_edit_evts : editable_event array;
}

let util_make_midi_editables midi_events = (

  let module HT = Hashtbl in

  let note_ons  = HT.create 50 in
  let note_offs  = HT.create 50 in
  let note_on_keys = ref [] in
  let add_key note = 
    note_on_keys := note::(List.filter (fun x -> x <> note) !note_on_keys) in
    

  let other_list = ref [] in
  let notes_list = ref [] in

  List.iter (
    fun midi_ev ->
      match midi_ev.Midi.status with
      | rs when ((0x80<= rs) && (rs <= 0x8F)) ->
          HT.add note_offs midi_ev.Midi.data_1 midi_ev;
      | rs when ((0x90<= rs) && (rs <= 0x9F)) -> 
          HT.add note_ons midi_ev.Midi.data_1 midi_ev;
          add_key midi_ev.Midi.data_1;
      | _ -> other_list := (EE_Midi midi_ev)::!other_list;
  ) midi_events;

  List.iter (
    fun x -> 
      let for_this_note = ref [] in

      let ons  = ref (
        List.fast_sort (fun x y -> compare x.Midi.ticks y.Midi.ticks)
        (HT.find_all note_ons x)) in
      let ofs = ref (
        List.fast_sort (fun x y -> compare x.Midi.ticks y.Midi.ticks)
        (HT.find_all note_offs x)) in
      let ons_iter = ref (List.length !ons) in
      let ofs_iter = ref (List.length !ofs) in
      while !ons_iter > 0 && !ofs_iter > 0 do
        let tuple = List.hd !ons , List.hd !ofs in
        for_this_note := tuple::!for_this_note;
        ons := List.tl !ons;
        ofs := List.tl !ofs;
        decr ons_iter;
        decr ofs_iter;
      done;
      notes_list :=  (EE_MidiNote !for_this_note)::!notes_list;
      List.iter (
        fun x ->  other_list := (EE_Midi x)::!other_list;
      ) !ons;
      List.iter (
        fun x ->  other_list := (EE_Midi x)::!other_list;
      ) !ofs;

  ) !note_on_keys;

  (List.append (List.fast_sort (
    fun x y -> 
      let get_note = function | EE_MidiNote ((mev,_)::_) -> mev.Midi.data_1
      | _ -> failwith "not a note in note list" in
      - (compare (get_note x) (get_note y))
  ) !notes_list) (List.rev (!other_list)))
)

(** [track_values] constructor *)
let tv_make_track_values app tk_ref = (
  match tk_ref with
  | `MIDI tk -> 
      let name,port,length =
        App.get_midi_track_information app tk in
      let _,p = App.get_bpm_ppqn app in
      let b,q,t = S.unitize_length_tuple length p in
      let midi_events = App.get_midi_track app tk in
      let editables_list = util_make_midi_editables midi_events in
        (* List.rev (List.rev_map ( *)
        (* fun midi_ev -> EE_Midi midi_ev) midi_events) in *)
      let editables_array = Array.of_list editables_list in
      {
        tv_app = app;
        tv_tk_id = tk;
        tv_type = MIDI_TRACK;
        tv_type_str = "MIDI";
        tv_name = name; 
        tv_length_b = b; 
        tv_length_q = q; 
        tv_length_t = t; 
        tv_pqn = p ;
        tv_port = port;
        tv_edit_evts = editables_array;
      }
  | `META tk ->
      let name,length = App.get_meta_track_information app tk in
      let _,p = App.get_bpm_ppqn app in
      let b,q,t = S.unitize_length_tuple length p in
      let meta_events = App.get_meta_track app tk in
      let editables_list = List.rev (List.rev_map (
        fun meta_ev -> EE_MetaSpec meta_ev) meta_events) in
      let editables_array = Array.of_list editables_list in
      {
        tv_app = app;
        tv_tk_id = tk;
        tv_type = META_TRACK;
        tv_type_str = "META";
        tv_name = name; 
        tv_length_b = b; 
        tv_length_q = q; 
        tv_length_t = t; 
        tv_pqn = p ;
        tv_port = 0;
        tv_edit_evts = editables_array;
      }
)

let tv_rebuild_editables tv = (
  begin match tv.tv_type with
  | MIDI_TRACK ->
      let midi_events = App.get_midi_track tv.tv_app tv.tv_tk_id in
      let editables_list = util_make_midi_editables midi_events in
      let editables_array = Array.of_list editables_list in
      tv.tv_edit_evts <- editables_array;
  | META_TRACK ->
      let meta_events = App.get_meta_track tv.tv_app tv.tv_tk_id in
      let editables_list = List.rev (List.rev_map (
        fun meta_ev -> EE_MetaSpec meta_ev) meta_events) in
      let editables_array = Array.of_list editables_list in
      tv.tv_edit_evts <- editables_array;
  end;
)

let tv_update_track_info tv = (
  begin match tv.tv_type with
  | MIDI_TRACK ->
      App.set_midi_track_information tv.tv_app tv.tv_tk_id
      tv.tv_name tv.tv_port 
      ( (tv.tv_length_b * tv.tv_pqn * 4)
      + (tv.tv_length_q * tv.tv_pqn) + tv.tv_length_t) ;
  | META_TRACK ->
      Log.warn "tv_update_track_info (META) NOT IMPLEMENTED\n" ;
  end

)
(******************************************************************************)
(** {3 The event editor frame} *)

type edit_pointer_tool =
  | EPTool_None
  | EPTool_Write
  | EPTool_Erase
  | EPTool_Resize

type edit_pointer_status =
  | EPStatus_Idle

type edit_pointer = {
  mutable ep_tool: edit_pointer_tool;
  mutable ep_status: edit_pointer_status;

}


(** The "draw" object *)
type event_frame = {
  mutable ef_model: track_values;
  mutable ef_draw: GDraw.pixmap;
  mutable ef_imag: GMisc.image;

  mutable ef_pointer: edit_pointer; (** The pointer for edition *)

  mutable ef_grid_begin_x: int;
  mutable ef_h: int;
  mutable ef_w: int;
  mutable ef_zoom: int;

  mutable ef_current_selection: int;

  mutable ef_cut_quarters: int; (** The number of sub-quarter divisions *)

  ef_set_cursor: Gdk.Cursor.cursor_type -> unit;
  ef_on_selection: event_frame -> unit;
}


(** The editor font: *)
let global_main_font = ref "Monospace 6"


let make_color str = (`NAME str : GDraw.color)
let global_bg_color   = ref (make_color "#4A00DD")
let global_grid_color = ref (make_color "#E8B500")
let global_text_color = ref (make_color "#B9FFB1")
let global_selected_text_color = ref (make_color "#FF0C00")
let global_text_velocity_color = ref (make_color "#D40000")
let global_midi_event_tick_color = ref (make_color "#BB006A")
let global_midi_event_range_color = ref (make_color "#5AFE00")

let global_separ_width = ref 3
let global_horiz_lines_width = ref 2

let global_vert_cut_quarter_width = ref 1
let global_vert_quarter_width = ref 2

let ef_update_size ef = (
  (* we add some pixels to have an elegant drawing... *)
  (* ef.ef_area#set_size ~width:(ef.ef_w+2) ~height:(ef.ef_h+2); *)
  let pixmap = GDraw.pixmap ~width:(ef.ef_w+2) ~height:(ef.ef_h+2) () in
  ef.ef_draw <- pixmap;
)

let ef_make_layout ef str = (
  let lo = 
    Pango.Layout.create ef.ef_imag#misc#pango_context#as_context in
  Pango.Layout.set_text lo str;
  lo
)
let ef_set_font ef  str = (
  let font = Pango.Font.from_string str in
  let ctx = ef.ef_imag#misc#pango_context#as_context in
  Pango.Context.set_font_description ctx font;
)

let ef_event_number ef = ( Array.length ef.ef_model.tv_edit_evts ;)

let ef_ticks_to_pixels ef ticks = (ef.ef_zoom * ticks / 50 )

let ef_draw_background ef = (

  (* The Width *)
  let b,q,t =
    ef.ef_model.tv_length_b, ef.ef_model.tv_length_q, ef.ef_model.tv_length_t in
  let p = ef.ef_model.tv_pqn in
  let ticks_length = (b * 4 * p + q * p + t) in
  let pixel_length = ef_ticks_to_pixels ef ticks_length in
  ef.ef_w <- ef.ef_grid_begin_x + pixel_length;

  (* The Height *)
  let dummy_layout = ef_make_layout ef "Chouniard !!!" in
  let _, ly = Pango.Layout.get_pixel_size dummy_layout in
  let ev_nb = ef_event_number ef in
  ef.ef_h <- 1 + (ly * ev_nb);

  ef_update_size ef ;

  (* The background: *)
  ef.ef_draw#set_foreground !global_bg_color;
  ef.ef_draw#rectangle ~x:0 ~y:0
  ~width:ef.ef_w ~height:ef.ef_h ~filled:true ();
  ef.ef_draw#set_foreground !global_grid_color;
  ef.ef_draw#rectangle ~x:0 ~y:0
  ~width:ef.ef_w ~height:ef.ef_h ~filled:false ();

  (* The vertical separator: *)
  ef.ef_draw#rectangle ~x:(ef.ef_grid_begin_x - !global_separ_width) ~y:0
  ~width:!global_separ_width ~height:ef.ef_h ~filled:true ();

  (* Horizontal lines: *)
  for i = 1 to ev_nb do
    ef.ef_draw#rectangle ~x:0 ~y:( 1 + (i * ly) )
    ~width:ef.ef_w ~height:!global_horiz_lines_width ~filled:true ();
  done;


  (* Vertical lines: *)
  for i = 0 to ticks_length do
    if ( i mod p ) = 0 then (
      let x = ef_ticks_to_pixels ef i in
      ef.ef_draw#rectangle ~x:(ef.ef_grid_begin_x + x) ~y:0
      ~width:!global_vert_quarter_width
      ~height:ef.ef_h ~filled:true ();
    ) else (
      if ( i mod (p / ef.ef_cut_quarters) ) = 0 then (
        let x = ef_ticks_to_pixels ef i in
        ef.ef_draw#rectangle ~x:(ef.ef_grid_begin_x + x) ~y:0
        ~width:!global_vert_cut_quarter_width
        ~height:ef.ef_h ~filled:true ();
      )
    )
  done;
)

let ef_draw_event ef index event = (
  let choose_text_color () = 
    if (index = ef.ef_current_selection) then (
      ef.ef_draw#set_foreground !global_selected_text_color;
    ) else (
      ef.ef_draw#set_foreground !global_text_color;
    );
  in
  begin match event with
  | EE_Midi ev ->
      let str = util_midi_event_to_string ev in
      let lo = ef_make_layout ef str in
      let _, l_y = Pango.Layout.get_pixel_size lo in
      let cur_y = 2 + (index * l_y) in
      choose_text_color ();
      ef.ef_draw#put_layout ~x:10 ~y:cur_y lo ;
      
      let x_in_grid =
        ef.ef_grid_begin_x + 
        (ef_ticks_to_pixels ef ev.Midi.ticks) in
      ef.ef_draw#set_foreground !global_midi_event_tick_color;
      ef.ef_draw#rectangle ~x:x_in_grid ~y:(cur_y + 1)
      ~width:3 ~height:(l_y - 2) ~filled:true ();
  | EE_MidiNote [] -> Log.p "An empty midi note...\n" ;
  | EE_MidiNote ev_list ->
      let ev_on, _ = List.hd ev_list in
      let str = util_note_to_string ev_on in
      let lo = ef_make_layout ef str in
      let _, l_y = Pango.Layout.get_pixel_size lo in
      let cur_y = 2 + (index * l_y) in
      choose_text_color ();
      ef.ef_draw#put_layout ~x:10 ~y:cur_y lo ;
      
      List.iter (
        fun (ev_on , ev_off) ->
          let x_on_in_grid =
            ef.ef_grid_begin_x + (ef_ticks_to_pixels ef ev_on.Midi.ticks) in
          let x_off_in_grid =
            ef.ef_grid_begin_x + (ef_ticks_to_pixels ef ev_off.Midi.ticks) in
          ef.ef_draw#set_foreground !global_midi_event_range_color;
          ef.ef_draw#rectangle ~x:x_on_in_grid ~y:(cur_y + 2)
          ~width:(x_off_in_grid - x_on_in_grid) ~height:(l_y - 3)
          ~filled:true ();

          let str = Printf.sprintf "[%d]" ev_on.Midi.data_2 in
          ef.ef_draw#set_foreground !global_text_velocity_color;
          ef.ef_draw#put_layout ~x:(x_on_in_grid  + 3)
          ~y:(cur_y + 1) (ef_make_layout ef str);
     
      ) ev_list;
  | _ -> Log.warn "NOT IMPLEMENTED\n" ;
  end
)

let ef_make_draw ef = (
  ef_draw_background ef;
  Array.iteri (ef_draw_event ef) ef.ef_model.tv_edit_evts;
  ef.ef_imag#set_pixmap ef.ef_draw;
)

let ef_cmd_redraw ef = (
  (* GtkBase.Widget.queue_draw ef.ef_imag#as_widget ; *)
  ef_make_draw ef;
)

let ef_y_to_event ef y = (
  let ev_size = (ef.ef_h - 1) / (ef_event_number ef) in
  if (y / ev_size) < (ef_event_number ef) then
    y / ev_size
  else 
    -1
)

let ef_on_mouse_press ef x y = (
  Log.p "ef_on_mouse_press is called: (%d,%d) on event: %d !!\n"
  x y (ef_y_to_event ef y);
  if (ef.ef_pointer.ep_tool = EPTool_None && x < ef.ef_grid_begin_x) then (
    ef.ef_current_selection <- (ef_y_to_event ef y);
    ef_cmd_redraw ef;
    ef.ef_on_selection ef;
  );
)

let ef_on_mouse_release ef x y = (
  Log.p "ef_on_mouse_release is called: (%d,%d) on event: %d !!\n"
  x y (ef_y_to_event ef y);
)


let ef_set_tool ef tool_type = (
  let cursor_type =
    match tool_type with
    | EPTool_Resize -> `SB_H_DOUBLE_ARROW
    | EPTool_None   -> `TOP_LEFT_ARROW
    | EPTool_Write  -> `PENCIL
    | EPTool_Erase  -> `PIRATE
  in
  ef.ef_pointer.ep_tool <- tool_type;
  ef.ef_set_cursor cursor_type;
)

let ef_make (box:GPack.box) values ~on_selection = (

  let scrolled_window = GBin.scrolled_window ~border_width:10
  ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:box#add () in
  let event_box =
    GBin.event_box ~packing:scrolled_window#add_with_viewport () in

  let draw_area = GMisc.image ~packing:event_box#add () in 
  draw_area#set_xpad 0;
  draw_area#set_ypad 0;
  draw_area#set_xalign 0.;
  draw_area#set_yalign 0.;

  let pixmap = GDraw.pixmap ~width:1 ~height:1 () in

  let the_event_frame = {
    ef_model = values;
    (* ef_area = draw_area; *)
    ef_imag = draw_area;
    ef_draw = pixmap;
    ef_pointer = {
      ep_tool = EPTool_None;
      ep_status = EPStatus_Idle;
    };
    ef_grid_begin_x = 200;
    ef_zoom = 1;
    ef_h = 0;
    ef_w = 0;
    ef_cut_quarters = 8;

    ef_current_selection = -1;

    ef_set_cursor = (
      fun cur_typ ->
        let cursor = Gdk.Cursor.create cur_typ in
        Gdk.Window.set_cursor event_box#misc#window cursor;
    );
    ef_on_selection = on_selection;
  } in

  ef_set_font the_event_frame !global_main_font;

  ef_cmd_redraw the_event_frame;

  ignore(event_box#event#connect#button_press ~callback:(
    fun ev ->
      let x = int_of_float (GdkEvent.Button.x ev) in
    	let y = int_of_float (GdkEvent.Button.y ev) in
      ef_on_mouse_press the_event_frame x y ;
      true
  ));
  ignore(event_box#event#connect#button_release ~callback:(
    fun ev ->
      let x = int_of_float (GdkEvent.Button.x ev) in
    	let y = int_of_float (GdkEvent.Button.y ev) in
      ef_on_mouse_release the_event_frame x y ;
      true
  ));
    
  the_event_frame
)

(******************************************************************************)
(** {3 The event parameters' line} *)

(** Update the GUI line where we can edit the parameters 
(declared {i rec} because referenced in event callbacks)
 *)
let rec util_update_add_edit_line box ef = (

  let module S = StringServer in

  List.iter (fun x -> x#destroy ()) box#all_children;
  let add_button = util_append_button "Add event" box in
  ignore(add_button#connect#clicked ~callback:(
    fun () -> Log.p "Clicked !\n" ;
  ));
  if ef.ef_current_selection >= 0 then (
    let event = ef.ef_model.tv_edit_evts.(ef.ef_current_selection) in
    begin match event with
    | EE_MidiNote ((mev_b,mev_e)::t as ev_list) ->
        Log.p "editing a note\n" ;
        util_append_label "NOTE: " box;

        
        let note_value, octave = util_note_octave_of_event mev_b in
        (* The note: *)
        util_append_label "Value:" box;
        let note_entry = 
          GEdit.combo_box_text ~strings:(Array.to_list S.note_names)
          ~packing:box#add () in
        let cbo,_ = note_entry in
        cbo#set_active note_value;
        ignore(cbo#connect#changed ~callback:( fun () ->
          Log.p "The active is: %d\n" cbo#active;
          let _, octave = util_note_octave_of_event mev_b in
          let new_note = cbo#active in
          List.iter (
            fun (evb, eve) ->
              evb.Midi.data_1 <- util_note_of_val_octave new_note octave;
              eve.Midi.data_1 <- util_note_of_val_octave new_note octave;
          ) ev_list;
          tv_rebuild_editables ef.ef_model;
          ef.ef_current_selection <- -1;
          util_update_add_edit_line box ef;
          ef_cmd_redraw ef;
        ));

        (* The octave: *)
        util_append_label "Octave:" box;
        let octave_adj =
          (* TODO: choose a good upper !! *)
          GData.adjustment ~value:(float octave) ~lower:(1.0) ~upper:25.0
          ~step_incr:1.0 ~page_incr:5.0 ~page_size:0.0 () in
        let octave_spin =
          GEdit.spin_button ~adjustment:octave_adj ~packing:(box#add) () in
        ignore (octave_spin#connect#changed ~callback:(fun () ->
          let note, _ = util_note_octave_of_event mev_b in
          let new_octave = int_of_float octave_adj#value in
          List.iter (
            fun (evb, eve) ->
              evb.Midi.data_1 <- util_note_of_val_octave note new_octave;
              eve.Midi.data_1 <- util_note_of_val_octave note new_octave;
          ) ev_list;
          tv_rebuild_editables ef.ef_model;
          ef.ef_current_selection <- -1;
          util_update_add_edit_line box ef;
          ef_cmd_redraw ef;
        ));

        (* The channel: *)
        util_append_label "Channel:" box;
        let chan_entry = 
          GEdit.combo_box_text ~strings:S.midi_channel_strings
          ~packing:box#add () in
        let cbo,_ = chan_entry in
        cbo#set_active (mev_b.Midi.channel + 1);
        ignore(cbo#connect#changed ~callback:( fun () ->
          Log.p "The active is: %d\n" cbo#active;
          let new_chan = cbo#active - 1 in
          List.iter (
            fun (evb, eve) ->
              evb.Midi.channel <- new_chan;
              eve.Midi.channel <- new_chan;
          ) ev_list;
          ef_cmd_redraw ef;
        ));

    | _ ->
        Log.warn "In util_update_add_edit_line, far from being implemented\n";
    end;
  );

)


(******************************************************************************)
(** {3 The "public" constructor} *)

(** Launch the editor *)
let track_editor app (to_edit:[`MIDI of int|`META of int]) change_callback = (

  let tk_values = tv_make_track_values app to_edit in

  let te = GWindow.window ~title:(tk_values.tv_type_str ^ " Track Editor") () in

  ignore(te#connect#destroy ~callback:te#destroy);

  let main_vbox = GPack.vbox ~homogeneous:false
  (* ~spacing:int *) (* ~border_width:int -> *)
  (* ~width:int -> *) (* ~height:int -> *)
  ~packing:te#add  ~show:true () in

  let track_settings_hbox = util_append_hbox main_vbox in

  (* The name: *)
  util_append_label "Name: " track_settings_hbox ;
  let name_entry = GEdit.entry ~text:tk_values.tv_name ~max_length:256
  ~editable:true ~has_frame:true ~width_chars:16
  ~packing:track_settings_hbox#add ~show:true () in

  (* The length (bar,quarter,tick): *)
  util_append_vertsepar track_settings_hbox ;
  util_append_label "Length: " track_settings_hbox ;

  let length_b_spin = util_int_spin_button 0. 20000. track_settings_hbox in
  length_b_spin#adjustment#set_value (float tk_values.tv_length_b);

  util_append_label " 4/4 bars, " track_settings_hbox ;
  
  let length_q_spin = util_int_spin_button 0. 20000. track_settings_hbox in
  length_q_spin#adjustment#set_value (float tk_values.tv_length_q);

  util_append_label " quarters and " track_settings_hbox ;
  
  let length_t_spin = util_int_spin_button 0. 200. track_settings_hbox in
  length_t_spin#adjustment#set_value (float tk_values.tv_length_t);

  util_append_label " ticks" track_settings_hbox ;

  (* The only MIDI ouput port: *)
  let port_combo = match tk_values.tv_type with
  | MIDI_TRACK -> (
    util_append_vertsepar track_settings_hbox ;
    util_append_label " Port: " track_settings_hbox ;
    let port_combo =
      GEdit.combo_box_text
      ~strings:(Array.to_list S.out_put_ports)
      ~add_tearoffs:false
      (* ~active:1 *)
      (* ~allow_empty:false ~value_in_list:true *)
      ~packing:track_settings_hbox#add () in
    (fst port_combo)#set_active tk_values.tv_port ;
    Some port_combo
  ) 
  | _ -> None
  in
  
  (* next line: *)
  util_append_horzsepar main_vbox ;
  let tools_hbox = util_append_hbox main_vbox in

  (* "Tools" buttons: *)
  let write_toggle = util_append_toggle "Write" tools_hbox in
  let erase_toggle = util_append_toggle "Erase" tools_hbox in
  let resiz_toggle = util_append_toggle "Resize" tools_hbox in
  util_append_vertsepar tools_hbox ;

  (* Zoom: *)
  util_append_label "Zoom:" tools_hbox ;
  let zoom_adj = 
    GData.adjustment ~value:(1.0) ~lower:(1.0) ~upper:200.0
    ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
  let zoom_scale = 
    GRange.scale `HORIZONTAL ~adjustment:zoom_adj  
    ~draw_value:false ~update_policy:`CONTINUOUS
    ~packing:(tools_hbox#pack ~expand:true ~fill:true ~padding:0)
    ~show:true () in
  util_append_vertsepar tools_hbox ;

  (* Mouse behaviour: *)
  util_append_label "Snap:" tools_hbox ;
  let snap_combo = 
    GEdit.combo_box_text
    ~strings:["1"; "1/2"; "1/4"; "1/16"; "none"]
    ~add_tearoffs:false
    (* ~active:1 *)
    (* ~allow_empty:false ~value_in_list:true *)
    ~packing:(tools_hbox#pack ~expand:false ~fill:false ~padding:0) ()
  in
  (fst snap_combo)#set_active 0;

  util_append_horzsepar main_vbox;

  let add_edit_hbox = util_append_hbox main_vbox in
  (* The "edit" line: *)
  let add_button = util_append_button "Add" add_edit_hbox in

  (* Current edited event goes there: *)

  util_append_vertsepar tools_hbox;

  util_append_horzsepar main_vbox;

  let ev_frame =
    ef_make main_vbox tk_values
    ~on_selection:(util_update_add_edit_line add_edit_hbox)
  in


  (* Connections: *)
  ignore(zoom_scale#connect#value_changed ~callback:( fun () -> 
    ev_frame.ef_zoom <- int_of_float zoom_adj#value;
    ef_cmd_redraw ev_frame;
  ));

  (* Tool/Pointer Automata: *)
  ignore( write_toggle#connect#toggled ~callback:(fun () -> 
    if write_toggle#active then (
      ef_set_tool ev_frame EPTool_Write;
      erase_toggle#set_active false;
      resiz_toggle#set_active false;
      Log.p "now write !\n" ;
    ) else (
      if (ev_frame.ef_pointer.ep_tool = EPTool_Write) then (
        ef_set_tool ev_frame EPTool_None;
        Log.p "now none (from write)\n" ;
      );
    );
  ));
  ignore(erase_toggle#connect#toggled ~callback:(fun () -> 
    if erase_toggle#active then (
      ef_set_tool ev_frame EPTool_Erase;
      write_toggle#set_active false;
      resiz_toggle#set_active false;
      Log.p "now erase !\n" ;
    ) else (
      if (ev_frame.ef_pointer.ep_tool = EPTool_Erase) then (
        ef_set_tool ev_frame EPTool_None;
        Log.p "now none (from erase)\n" ;
      );
    );
  ));
  ignore(resiz_toggle#connect#toggled ~callback:(fun () -> 
    if resiz_toggle#active then (
      ef_set_tool ev_frame EPTool_Resize;
      erase_toggle#set_active false;
      write_toggle#set_active false;
      Log.p "now resize !\n" ;
    ) else (
      if (ev_frame.ef_pointer.ep_tool = EPTool_Resize) then (
        ef_set_tool ev_frame EPTool_None;
        Log.p "now none (from resize)\n" ;
      );
    );
  ));


  (* Set the name (directly): *)
  ignore(name_entry#connect#changed ~callback:( fun () ->
    Log.p "Name changed: %s\n" name_entry#text;
    tk_values.tv_name <- name_entry#text;
    tv_update_track_info tk_values; change_callback ();
  ));
  (* Set the length: *)
  ignore(length_b_spin#connect#changed ~callback:(fun () ->
    tk_values.tv_length_b <- int_of_float length_b_spin#adjustment#value;
    tv_update_track_info tk_values; change_callback ();
  ));
  ignore(length_q_spin#connect#changed ~callback:(fun () ->
    tk_values.tv_length_q <- int_of_float length_q_spin#adjustment#value;
    tv_update_track_info tk_values; change_callback ();
  ));
  ignore(length_t_spin#connect#changed ~callback:(fun () ->
    tk_values.tv_length_t <- int_of_float length_t_spin#adjustment#value;
    tv_update_track_info tk_values; change_callback ();
  ));
  (* Set the port (MIDI) *)
  begin match port_combo with
  | Some (pc, _) -> 
      ignore( pc#connect#changed ~callback:( fun () ->
        tk_values.tv_port <- pc#active;
          tv_update_track_info tk_values; change_callback ();
      ));
  | _ -> ()
  end;
    

  let _ = (* NOTE:
  Avoiding "not-used" warnings during development,
  allows to distinguish REAL warnings !!
  *)
    add_button in


  te#show ();
)


