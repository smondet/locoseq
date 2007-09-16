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

(** {3 Practical Renamings} *)

module App = SeqApp
module S = StringServer

(** {3 Internal modules} *)

(** GUI simple generic utilities *)
module GuiUtil = struct

  (** append a label to a box *)
  let append_label text (box:GPack.box) = ignore (
    GMisc.label ~text
    ~packing:(box#pack ~expand:false ~fill:false ~padding:0) ()
  )


  (** Append a vertical separator to a box *)
  let append_vertsepar (box:GPack.box) = (
    let sep =
      GMisc.separator `VERTICAL
      ~packing:(box#pack ~expand:false ~fill:false ~padding:0) 
      ~show:true () in
    sep#misc#set_size_request ~width:30 ();
  )

  (** Append an horizontal separator to a box *)
  let append_horzsepar (box:GPack.box) = (
    let sep =
      GMisc.separator `HORIZONTAL
      ~packing:(box#pack ~expand:false ~fill:false ~padding:0) ~show:true () in
    sep#misc#set_size_request ~width:10 ();
  )

  (** Make a text mono-stable button and append it to a box *)
  let append_button text (box:GPack.box) = (
    GButton.button ~label:text
    ~packing:(box#pack ~expand:false ~fill:false ~padding:0)  ~show:true ()
  )

  (** Make a text bi-stable button and append it to a box *)
  let append_toggle text (box:GPack.box) = (
    GButton.toggle_button ~label:text ~active:false ~relief:`NORMAL
    ~packing:(box#pack ~expand:false ~fill:false ~padding:0)  ~show:true ()
  )

  (** Make an horizontal box (1 row) *)
  let append_hbox (box:GPack.box) = (
    GPack.hbox  ~homogeneous:false
    ~packing:(box#pack ~expand:false ~fill:false ~padding:0) ~show:true ()
  )

  (** Make a [spin_button] for an integer value *)
  let int_spin_button min max box = (
    let adj =
      GData.adjustment ~value:min ~lower:min ~upper:max
      ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
    GEdit.spin_button ~adjustment:adj ~packing:(box#add) ()
  )
end


(******************************************************************************)
(** Midi related utilities *)
module MidiUtil = struct

  let note_octave_of_event ev = (
    ev.Midi.data_1 mod 12 , (ev.Midi.data_1 / 12) + 1
  )

  let note_of_val_and_oct value octave = (octave - 1) * 12 + value

  let note_to_string note = (
    let value,octave = note_octave_of_event note in
    let virtual_note = StringServer.note_names.(value) in
    let chan = note.Midi.channel in
    Printf.sprintf "NOTE: %s%d -> %d" virtual_note octave chan
  )

  let midi_event_to_string midi_ev = (
    let cmd = Midi.midi_cmd_of_event midi_ev in
    let chan = midi_ev.Midi.channel in
    Printf.sprintf "%s -> %d" (Midi.midi_cmd_to_string cmd) chan
  )
end

(******************************************************************************)
(** Meta events related utilities *)
module MetaUtil = struct

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

(** Editor global settings (colors, fonts...) *)
module Settings = struct

  (** The editor font: *)
  let global_main_font = ref "Monospace 6"

  let make_color str = (`NAME str : GDraw.color)
  let global_bg_color   = ref (make_color "#4A00DD")
  let global_grid_color = ref (make_color "#E8B500")
  let global_text_color = ref (make_color "#B9FFB1")
  let global_selected_text_color = ref (make_color "#FF0C00")
  let global_text_velocity_color = ref (make_color "#D40000")

  let event_tick_color = ref (make_color "#CB000A")
  let event_range_color = ref (make_color "#2DCB00")

  let global_separ_width = ref 3
  let global_horiz_lines_width = ref 2

  let global_vert_cut_quarter_width = ref 1
  let global_vert_quarter_width = ref 2

  let midi_note_minimum_tick_size = ref 15
end
open Settings

(******************************************************************************)
(** {3 The model} *)

(** A {i minimalistic} variant to optimize matching *)
type track_type = MIDI_TRACK | META_TRACK


(** Type of midi or meta events for the editor*)
type editable_event =
  | EE_None
  (** The [editable_event] is a kind of [option] *)
  | EE_Midi of Midi.midi_event
  (** Generic midi event *)
  | EE_MidiNote of (Midi.midi_event * Midi.midi_event) list
  (** One midi note, and its instances in the track (NoteOn, NoteOff) *)
  | EE_MetaSpecOneTick of Tracker.meta_action_spec
  (** Meta-event based on one-tick-date *)
  | EE_MetaSpecRange of Tracker.meta_action_spec
  (** Meta-event based on two ticks range *)


(** An {i ugly} mapping structure that encloses both midi and meta tracks for
 the editor  *)
type track_values = {
  mutable tv_app: SeqApp.seq_app; (** The {i application} handle *)
  mutable tv_tk_id: int; (** The track ID *)
  mutable tv_type : track_type; (** Midi or Meta *)
  mutable tv_name: string;
  mutable tv_length_b: int;
  mutable tv_length_q: int;
  mutable tv_length_t: int;
  mutable tv_pqn : int;
  mutable tv_port: int;
  mutable tv_edit_evts : editable_event array;
}

let util_make_midi_editables midi_events track_length = (

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
        let the_on,the_of = List.hd !ons , List.hd !ofs in
        if the_on.Midi.ticks < the_of.Midi.ticks
        then (
          (* "normal" case: *)
          for_this_note := (the_on, the_of) :: !for_this_note;
          ons := List.tl !ons; decr ons_iter;
          ofs := List.tl !ofs; decr ofs_iter;
        ) else (
          Log.p "Not off (%d) before note on (%d)\n"
          the_of.Midi.ticks the_on.Midi.ticks;
          other_list := (EE_Midi the_of)::!other_list;
          ofs := List.tl !ofs; decr ofs_iter;
        );
      done;
      if !for_this_note <> [] then (
        notes_list :=  (EE_MidiNote !for_this_note)::!notes_list;
      );
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

let util_make_meta_editables meta_events = (
  List.rev (List.rev_map (fun meta_ev ->
    match meta_ev with
    | `track_set_on  _ -> EE_MetaSpecOneTick meta_ev
    | `track_set_off _ -> EE_MetaSpecOneTick meta_ev
    | `set_bpm       _ -> EE_MetaSpecOneTick meta_ev
    | `track_on      _ -> EE_MetaSpecRange meta_ev
  ) meta_events)
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
      let editables_list = util_make_midi_editables midi_events length in
        (* List.rev (List.rev_map ( *)
        (* fun midi_ev -> EE_Midi midi_ev) midi_events) in *)
      let editables_array = Array.of_list editables_list in
      {
        tv_app = app;
        tv_tk_id = tk;
        tv_type = MIDI_TRACK;
        tv_name = name; 
        tv_length_b = b; 
        tv_length_q = q; 
        tv_length_t = t; 
        tv_pqn = p;
        tv_port = port;
        tv_edit_evts = editables_array;
      }
  | `META tk ->
      let name,length = App.get_meta_track_information app tk in
      let _,p = App.get_bpm_ppqn app in
      let b,q,t = S.unitize_length_tuple length p in
      let meta_events = App.get_meta_track app tk in
      let editables_list = util_make_meta_editables meta_events in
      let editables_array = Array.of_list editables_list in
      {
        tv_app = app;
        tv_tk_id = tk;
        tv_type = META_TRACK;
        tv_name = name; 
        tv_length_b = b; 
        tv_length_q = q; 
        tv_length_t = t; 
        tv_pqn = p;
        tv_port = 0;
        tv_edit_evts = editables_array;
      }
)

let tv_ticks_length tv = (
  let b,q,t =
    tv.tv_length_b, tv.tv_length_q, tv.tv_length_t in
  let p = tv.tv_pqn in
  b * 4 * p + q * p + t
)
let tv_rebuild_editables tv = (
  begin match tv.tv_type with
  | MIDI_TRACK ->
      let midi_events = App.get_midi_track tv.tv_app tv.tv_tk_id in
      let length = tv_ticks_length tv in
      let editables_list = util_make_midi_editables midi_events length in
      let editables_array = Array.of_list editables_list in
      tv.tv_edit_evts <- editables_array;
  | META_TRACK ->
      let meta_events = App.get_meta_track tv.tv_app tv.tv_tk_id in
      let editables_list = util_make_meta_editables meta_events in
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
      + (tv.tv_length_q * tv.tv_pqn) + tv.tv_length_t);
  | META_TRACK ->
      App.set_meta_track_information tv.tv_app tv.tv_tk_id tv.tv_name
      ( (tv.tv_length_b * tv.tv_pqn * 4)
      + (tv.tv_length_q * tv.tv_pqn) + tv.tv_length_t);
  end

)
(******************************************************************************)
(** {3 The event editor frame} *)

type edit_pointer_tool =
  | EPTool_None
  | EPTool_Write
  | EPTool_Erase
  | EPTool_Resize

(** The current status of the pointer *)
type edit_pointer_status =
  | EPStatus_Idle
  | EPStatus_DragStarted of int * int * int (** event_id, x, y *)
  (* going deprecated ? *)
  | EPStatus_XDrag of (int -> bool) * (int -> unit)
  (** dragging callback X -> continue?, mouse release callback: gets X *)

type edit_pointer = {
  mutable ep_tool: edit_pointer_tool;
  mutable ep_status: edit_pointer_status;

  mutable ep_precision: int;
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

let ef_event_number ef = ( Array.length ef.ef_model.tv_edit_evts)

let ef_ticks_to_pixels ef ticks = (ef.ef_zoom * ticks / 50 )
let ef_pixels_to_ticks ef pixels = (pixels * 50 / ef.ef_zoom)

let ef_draw_background ef = (

  (* The Width *)
  let ticks_length = tv_ticks_length ef.ef_model in
  let pixel_length = ef_ticks_to_pixels ef ticks_length in
  ef.ef_w <- ef.ef_grid_begin_x + pixel_length;

  (* The Height *)
  let dummy_layout = ef_make_layout ef "Chouniard !!!" in
  let _, ly = Pango.Layout.get_pixel_size dummy_layout in
  let ev_nb = ef_event_number ef in
  ef.ef_h <- 1 + (ly * ev_nb);

  ef_update_size ef;

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
    if ( i mod ef.ef_model.tv_pqn) = 0 then (
      let x = ef_ticks_to_pixels ef i in
      ef.ef_draw#rectangle ~x:(ef.ef_grid_begin_x + x) ~y:0
      ~width:!global_vert_quarter_width
      ~height:ef.ef_h ~filled:true ();
    ) else (
      if ( i mod (ef.ef_model.tv_pqn / ef.ef_cut_quarters) ) = 0 then (
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
  let make_event_label str = (
    let lo = ef_make_layout ef str in
    let _, l_y = Pango.Layout.get_pixel_size lo in
    let cur_y = 2 + (index * l_y) in
    choose_text_color ();
    ef.ef_draw#put_layout ~x:10 ~y:cur_y lo;
    (cur_y, l_y)
  ) in
  let draw_range_event ~start_t ~end_t ~current_y ~unit_y = 
    let x_on_in_grid = ef.ef_grid_begin_x + (ef_ticks_to_pixels ef start_t) in
    let x_off_in_grid = ef.ef_grid_begin_x + (ef_ticks_to_pixels ef end_t) in
    ef.ef_draw#set_foreground !Settings.event_range_color;
    ef.ef_draw#rectangle ~x:x_on_in_grid ~y:(current_y + 2)
    ~width:(x_off_in_grid - x_on_in_grid) ~height:(unit_y - 3)
    ~filled:true ();
  in
  let draw_tick_event ~t_value  ~current_y ~unit_y =
    let x_in_grid =
      ef.ef_grid_begin_x + 
      (ef_ticks_to_pixels ef t_value) in
    ef.ef_draw#set_foreground !Settings.event_tick_color;
    ef.ef_draw#rectangle ~x:x_in_grid ~y:(current_y + 1)
    ~width:3 ~height:(unit_y - 2) ~filled:true ();
  in
  begin match event with
  | EE_Midi ev ->
      let cur_y, unit_y = make_event_label (MidiUtil.midi_event_to_string ev) in
      draw_tick_event ~t_value:ev.Midi.ticks ~current_y:cur_y ~unit_y;

  | EE_MidiNote [] -> Log.p "An empty midi note...\n";
  | EE_MidiNote ev_list ->
      let ev, _ = List.hd ev_list in
      let cur_y, unit_y = make_event_label (MidiUtil.midi_event_to_string ev) in
      
      List.iter (
        fun (ev_on , ev_off) ->
          let start_t, end_t = ev_on.Midi.ticks, ev_off.Midi.ticks in
          draw_range_event ~start_t ~end_t ~current_y:cur_y ~unit_y;

          let str = Printf.sprintf "[%d]" ev_on.Midi.data_2 in
          let x_on_in_grid =
            ef.ef_grid_begin_x + (ef_ticks_to_pixels ef start_t) in
          ef.ef_draw#set_foreground !global_text_velocity_color;
          ef.ef_draw#put_layout ~x:(x_on_in_grid  + 3)
          ~y:(cur_y + 1) (ef_make_layout ef str);
     
      ) ev_list;
  | EE_MetaSpecOneTick spec -> 
      let cur_y, unit_y = make_event_label (MetaUtil.spec_to_string spec) in
      let t_value = (MetaUtil.spec_to_tick spec) in
      draw_tick_event ~t_value ~current_y:cur_y ~unit_y;
  | EE_MetaSpecRange spec -> 
      let cur_y, unit_y = make_event_label (MetaUtil.spec_to_string spec) in
      let start_t, end_t = MetaUtil.spec_to_range spec in
      draw_range_event ~start_t ~end_t ~current_y:cur_y ~unit_y;
  | _ -> Log.warn "NOT IMPLEMENTED\n";
  end
)

let ef_make_draw ef = (
  ef_draw_background ef;
  Array.iteri (ef_draw_event ef) ef.ef_model.tv_edit_evts;
  ef.ef_imag#set_pixmap ef.ef_draw;
)

let ef_cmd_redraw ef = (
  (* GtkBase.Widget.queue_draw ef.ef_imag#as_widget *)
  ef_make_draw ef;
)

let ef_y_to_event ef y = (
  let ev_size = (ef.ef_h - 1) / (ef_event_number ef) in
  let ev = y / ev_size in
  if (0 <= ev) && (ev < (ef_event_number ef)) then
    y / ev_size
  else 
    -1
)

let ef_pointer_touches_ticks ef x_point ticks = (
  let x_of_ticks = ef_ticks_to_pixels ef ticks in
  let ep = ef.ef_pointer in
  let x = x_point - ef.ef_grid_begin_x in
  Log.p "--- ticks:%d(%d) x:%d\n" ticks x_of_ticks x;
  (x - ep.ep_precision < x_of_ticks ) && (x_of_ticks < x + ep.ep_precision)
)
let ef_pointer_in_ticks_interval ef x_point (t_min, t_max) = (
  let x_min = ef_ticks_to_pixels ef t_min in
  let x_max = ef_ticks_to_pixels ef t_max in
  let x = x_point - ef.ef_grid_begin_x in
  (* Log.p "--- ticks:%d(%d) in [%d, %d] ?\n" x x_point x_min x_max; *)
  (x_min <= x ) && (x <= x_max)
)

let ef_set_editable_selected ef editable = (
  let module M = Midi in
  let idx = ref 0 in
  let ended = ref false in
  while !idx < (Array.length ef.ef_model.tv_edit_evts) && not !ended do
    if (
      match ef.ef_model.tv_edit_evts.(!idx) with
      | EE_Midi mev ->  editable = EE_Midi mev 
      | EE_MidiNote ((b,_) :: _) -> (
          match editable with
          | EE_MidiNote ((bb,_) :: _) when b.M.data_1 = bb.M.data_1 -> true
          | _ -> false
      )
      | event ->
          Log.p "Using default equality...\n";
          event = editable
    ) then (
      ended := true;
    ) else (
      incr idx;
    );
  done;
  ef.ef_current_selection <- if !ended then !idx else (-1);
)


let ef_on_mouse_press ef x y = (
  (* Log.p "ef_on_mouse_press is called: (%d,%d) on event: %d !!\n" *)
  (* x y (ef_y_to_event ef y); *)
  if (ef.ef_pointer.ep_tool = EPTool_None && x < ef.ef_grid_begin_x) then (
    ef.ef_current_selection <- (ef_y_to_event ef y);
    ef_cmd_redraw ef;
    ef.ef_on_selection ef;
  );

  (* Utilities for this automata: *)
  let module LocalUtil = struct

    let start_resize_midi_ticks ef midi_ev ~valid_interval = (
      let (x_min,x_max) = valid_interval in
      let on_drag x = (x_min <= x) && (x <= x_max) in
      let on_release x_release =
        let x_ticks = 
          ef_pixels_to_ticks ef (x_release - ef.ef_grid_begin_x) in
        midi_ev.Midi.ticks <- x_ticks;
        ef_cmd_redraw ef;
      in
      ef.ef_pointer.ep_status <- EPStatus_XDrag (on_drag, on_release);
    )
    let start_removing_midi_event ef midi_ev  = (
      let on_drag x = true in
      let on_release x =
        if (ef.ef_grid_begin_x <= x && x <= ef.ef_w) then (
          App.remove_midi_event_from_track
          ef.ef_model.tv_app ef.ef_model.tv_tk_id midi_ev;
          tv_rebuild_editables ef.ef_model;
          ef_cmd_redraw ef;
        );
      in
      ef.ef_pointer.ep_status <- EPStatus_XDrag (on_drag, on_release);
    )
    let start_removing_midi_note ef (ev_b, ev_e) = (
      let on_drag x =
        ef_pointer_in_ticks_interval ef x
        (ev_b.Midi.ticks,ev_e.Midi.ticks)
      in
      let on_release x =
        if (ef.ef_grid_begin_x <= x && x <= ef.ef_w) then (
          App.remove_midi_event_from_track
          ef.ef_model.tv_app ef.ef_model.tv_tk_id ev_b;
          App.remove_midi_event_from_track
          ef.ef_model.tv_app ef.ef_model.tv_tk_id ev_e;
          tv_rebuild_editables ef.ef_model;
          ef_cmd_redraw ef;
        );
      in
      ef.ef_pointer.ep_status <- EPStatus_XDrag (on_drag, on_release);
    )
    let start_adding_note_instance ef x_start ev_list = (
      let on_drag x = (ef.ef_grid_begin_x <= x && x <= ef.ef_w) in
      let on_release x_release =
        let x_start_ticks = 
          ef_pixels_to_ticks ef (x_start - ef.ef_grid_begin_x) in
        let x_release_ticks =
          ef_pixels_to_ticks ef (x_release - ef.ef_grid_begin_x) in
        if (
          (abs (x_start_ticks - x_release_ticks)) 
          >= !Settings.midi_note_minimum_tick_size
        ) then (
          let example_on, example_of = List.hd ev_list in
          let note_on, note_of =
            Midi.copy_midi_event example_on, Midi.copy_midi_event example_of
          in
          if (x_start_ticks < x_release_ticks) then (
            note_on.Midi.ticks <- x_start_ticks;
            note_of.Midi.ticks <- x_release_ticks;
          ) else (
            note_on.Midi.ticks <- x_release_ticks;
            note_of.Midi.ticks <- x_start_ticks;
          );
          App.add_midi_event_to_track 
          ef.ef_model.tv_app ef.ef_model.tv_tk_id note_on;
          App.add_midi_event_to_track
          ef.ef_model.tv_app ef.ef_model.tv_tk_id note_of;
          tv_rebuild_editables ef.ef_model;
          ef_cmd_redraw ef;
        ) else (
          Log.warn "Event too small !\n";
        )
      in
      ef.ef_pointer.ep_status <- EPStatus_XDrag (on_drag, on_release);
    )

    let pixelize ticks = ef.ef_grid_begin_x + (ef_ticks_to_pixels ef ticks)
    
    let rec iter_note_instances_for_resize = function
      | [] -> () 
      | (ev_b, ev_e) :: q ->
          if (ef_pointer_touches_ticks ef x ev_b.Midi.ticks) then (
            start_resize_midi_ticks ef ev_b
            ~valid_interval:(ef.ef_grid_begin_x, pixelize ev_e.Midi.ticks);
          ) else if (ef_pointer_touches_ticks ef x ev_e.Midi.ticks)
          then (
            start_resize_midi_ticks ef ev_e
            ~valid_interval:(pixelize ev_b.Midi.ticks, ef.ef_w);
          ) else (
            iter_note_instances_for_resize q;
          )

    let rec iter_note_instances_for_erase = function
      | [] -> ()
      | (ev_b, ev_e) :: q ->
          if (
            ef_pointer_in_ticks_interval ef x (ev_b.Midi.ticks,ev_e.Midi.ticks)
          ) then (
            start_removing_midi_note ef (ev_b,ev_e);
          ) else (
            iter_note_instances_for_erase q;
          )

  end
  in

  (* The "drag" automata: *)
  if (ef.ef_pointer.ep_tool <> EPTool_None && x > ef.ef_grid_begin_x) then (
    let event_id = (ef_y_to_event ef y) in
    if (event_id <> -1) then (
      begin match ef.ef_pointer.ep_tool with
      | EPTool_Resize ->
          let event = ef.ef_model.tv_edit_evts.(event_id) in
          begin match event with
          | EE_Midi mev ->
              if (ef_pointer_touches_ticks ef x mev.Midi.ticks) then (
                LocalUtil.start_resize_midi_ticks ef mev
                ~valid_interval:(ef.ef_grid_begin_x, ef.ef_w);
              );
          | EE_MidiNote ev_list ->
              LocalUtil.iter_note_instances_for_resize ev_list;
          | _ ->
              Log.warn "ef_on_mouse_press: NOT IMPLEMENTED\n";
              ef.ef_pointer.ep_status <- EPStatus_DragStarted (event_id, x, y);
          end;
      | EPTool_Erase ->
          let event = ef.ef_model.tv_edit_evts.(event_id) in
          begin match event with
          | EE_Midi mev ->
              if (ef_pointer_touches_ticks ef x mev.Midi.ticks) then (
                LocalUtil.start_removing_midi_event ef mev;
              );
          | EE_MidiNote  ev_list ->
              LocalUtil.iter_note_instances_for_erase ev_list;
          | _ ->
              Log.warn "ef_on_mouse_press: NOT IMPLEMENTED\n";
          end;
      | EPTool_Write ->
          let event = ef.ef_model.tv_edit_evts.(event_id) in
          begin match event with
          | EE_Midi mev ->
              Log.warn "ef_on_mouse_press: %s\n"
              "Write action on generic midi events is not allowed";
          | EE_MidiNote  ev_list ->
              LocalUtil.start_adding_note_instance ef x ev_list;
          | _ ->
              Log.warn "ef_on_mouse_press: NOT IMPLEMENTED\n";
          end;
      | _ -> Log.warn "ef_on_mouse_press: Tool not implemented\n";
      end;
    );
  );
)

let ef_on_mouse_drag ef x y = (
  begin match ef.ef_pointer.ep_status with
  | EPStatus_DragStarted (event, xb, yb) ->
      Log.p "Dragged, on event %d, from (%d,%d) currently (%d,%d)\n"
      event xb yb x y;
  | EPStatus_XDrag (on_drag, _) -> 
      if not (on_drag x) then (
        ef.ef_pointer.ep_status <- EPStatus_Idle;
      );
  | _ -> ();
  end;
)

let ef_on_mouse_release ef x y = (
  Log.p "ef_on_mouse_release is called: (%d,%d) on event: %d !!\n"
  x y (ef_y_to_event ef y);
  begin match ef.ef_pointer.ep_status with
  | EPStatus_DragStarted (event, xb, yb) ->
      Log.p "Dragged, on event %d, from (%d,%d) to (%d,%d)\n" event xb yb x y;
  | EPStatus_XDrag (_, on_release) -> on_release x;
  | _ -> ();
  end;
  ef.ef_pointer.ep_status <- EPStatus_Idle;
)

(** Given a [edit_pointer_tool], set [ef]'s pointer and update the
 graphical cursor *)
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

(** Build an editor {i draw} frame *)
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
    ef_imag = draw_area;
    ef_draw = pixmap;
    ef_pointer = {
      ep_tool = EPTool_None;
      ep_status = EPStatus_Idle;
      ep_precision = 5;
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
      ef_on_mouse_press the_event_frame x y;
      true
  ));
  ignore(event_box#event#connect#button_release ~callback:(
    fun ev ->
      let x = int_of_float (GdkEvent.Button.x ev) in
    	let y = int_of_float (GdkEvent.Button.y ev) in
      ef_on_mouse_release the_event_frame x y;
      true
  ));
  ignore(event_box#event#connect#motion_notify ~callback:(
    fun ev ->
      let x = int_of_float (GdkEvent.Motion.x ev) in
    	let y = int_of_float (GdkEvent.Motion.y ev) in
      ef_on_mouse_drag the_event_frame x y;
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
  let add_button = GuiUtil.append_button "Add event" box in
  ignore(add_button#connect#clicked ~callback:(fun () ->
    let menu = GMenu.menu () in
    begin match ef.ef_model.tv_type with
    | MIDI_TRACK -> 
        let menuitem_note =  
          GMenu.menu_item ~label:"Midi Note" ~packing:menu#append () in
        ignore(menuitem_note#connect#activate ~callback:(fun () ->
          (* add the event, rebuild,  set it to selected *)
          let the_event_on = Midi.empty_midi_event () in
          let the_event_of = Midi.empty_midi_event () in
          the_event_on.Midi.status <- 0x90;
          the_event_of.Midi.status <- 0x80;
          the_event_of.Midi.ticks <- ef.ef_model.tv_pqn;
          App.add_midi_event_to_track 
          ef.ef_model.tv_app ef.ef_model.tv_tk_id the_event_on;
          App.add_midi_event_to_track 
          ef.ef_model.tv_app ef.ef_model.tv_tk_id the_event_of;
          tv_rebuild_editables ef.ef_model;
          ef_set_editable_selected ef (
            EE_MidiNote [(the_event_on,the_event_of)] 
          );
          util_update_add_edit_line box ef;
          ef_cmd_redraw ef;
        ));
        let menuitem_midi =  
          GMenu.menu_item ~label:"Midi Generic Event" ~packing:menu#append () in
        ignore(menuitem_midi#connect#activate ~callback:(fun () ->
          let the_event = Midi.empty_midi_event () in
          App.add_midi_event_to_track 
          ef.ef_model.tv_app ef.ef_model.tv_tk_id the_event;
          tv_rebuild_editables ef.ef_model;
          ef_set_editable_selected ef (EE_Midi the_event);
          util_update_add_edit_line box ef;
          ef_cmd_redraw ef;
        ));
    | META_TRACK -> 
        Log.warn "Add meta event Not implemented\n";
    end;
    menu#popup  ~button:1 ~time:0l;
          

  ));
  if ef.ef_current_selection >= 0 then (
    let event = ef.ef_model.tv_edit_evts.(ef.ef_current_selection) in
    begin match event with
    | EE_MidiNote ((mev_b,mev_e)::t as ev_list) ->
        GuiUtil.append_label "NOTE: " box;

        let note_value, octave = MidiUtil.note_octave_of_event mev_b in
        (* The note: *)
        GuiUtil.append_label "Value:" box;
        let note_entry = 
          GEdit.combo_box_text ~strings:(Array.to_list S.note_names)
          ~packing:box#add () in
        let cbo,_ = note_entry in
        cbo#set_active note_value;
        ignore(cbo#connect#changed ~callback:( fun () ->
          Log.p "The active is: %d\n" cbo#active;
          let _, octave = MidiUtil.note_octave_of_event mev_b in
          let new_note = cbo#active in
          List.iter (
            fun (evb, eve) ->
              evb.Midi.data_1 <- MidiUtil.note_of_val_and_oct new_note octave;
              eve.Midi.data_1 <- MidiUtil.note_of_val_and_oct new_note octave;
          ) ev_list;
          tv_rebuild_editables ef.ef_model;
          ef_set_editable_selected ef (EE_MidiNote [(mev_b,mev_e)]);
          util_update_add_edit_line box ef;
          ef_cmd_redraw ef;
        ));

        (* The octave: *)
        GuiUtil.append_label "Octave:" box;
        let octave_adj =
          GData.adjustment ~value:(float octave) ~lower:(1.0) ~upper:22.0
          ~step_incr:1.0 ~page_incr:5.0 ~page_size:0.0 () in
        let octave_spin =
          GEdit.spin_button ~adjustment:octave_adj ~packing:(box#add) () in
        ignore (octave_spin#connect#changed ~callback:(fun () ->
          let note, old_octave = MidiUtil.note_octave_of_event mev_b in
          let new_octave = int_of_float octave_adj#value in
          (* TODO verify that data_1 < 255 *)
          if old_octave <> new_octave then (
            List.iter (
              fun (evb, eve) ->
                evb.Midi.data_1 <- MidiUtil.note_of_val_and_oct note new_octave;
                eve.Midi.data_1 <- MidiUtil.note_of_val_and_oct note new_octave;
            ) ev_list;
            tv_rebuild_editables ef.ef_model;
            ef_set_editable_selected ef (EE_MidiNote [(mev_b,mev_e)]);
            util_update_add_edit_line box ef;
            ef_cmd_redraw ef;
          );
        ));

        (* The channel: *)
        GuiUtil.append_label "Channel:" box;
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
    | EE_Midi ev ->
        GuiUtil.append_label "MIDI EVENT: " box;
        let stat_entry = 
          GEdit.combo_box_text ~strings:(List.tl S.global_available_midi_events)
          ~packing:box#add () in
        let cbo,_ = stat_entry in
        let active_index = ref (-1) in
        begin try
          let _ =
            List.find (fun (_,b) -> 
              incr active_index; ev.Midi.status land 0xF0 = b
            ) S.midi_status_string_value in
          cbo#set_active !active_index;
          with Not_found -> ()
        end;
        ignore(cbo#connect#changed ~callback:( fun () ->
          begin match GEdit.text_combo_get_active stat_entry with
          | Some c -> ev.Midi.status <- S.midi_status_of_string c
          | None -> ()
          end;
          util_update_add_edit_line box ef;
          ef_cmd_redraw ef;
        ));
        GuiUtil.append_label " Channel:" box;
        let chan_entry = 
          GEdit.combo_box_text ~strings:S.midi_channel_strings
            ~packing:box#add () in
        let cbo,_ = chan_entry in
        cbo#set_active (ev.Midi.channel + 1);
        ignore(cbo#connect#changed ~callback:( fun () ->
          ev.Midi.channel <-  cbo#active - 1;
          util_update_add_edit_line box ef;
          ef_cmd_redraw ef;
        ));
        GuiUtil.append_label " Data 1:" box;
        let note_adj =
          GData.adjustment ~value:(float ev.Midi.data_1) ~lower:(0.)
          ~upper:255.0 ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
        let spin =
          GEdit.spin_button ~adjustment:note_adj ~packing:(box#add) () in
        ignore (spin#connect#changed ~callback:( fun () ->
          if ev.Midi.data_1 <> (int_of_float note_adj#value) then (
            ev.Midi.data_1 <- int_of_float note_adj#value;
            util_update_add_edit_line box ef;
            ef_cmd_redraw ef;
          );
        ));
        (* for 2-arg midi events: *)
        let rs = ev.Midi.status land 0xF0 in
        if not ((0xC0 <= rs) && (rs <= 0xDF)) then (
          GuiUtil.append_label " Data 2:" box;
          let velo_adj =
            GData.adjustment ~value:(float ev.Midi.data_2) ~lower:(0.0)
            ~upper:255.0 ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
          let spin =
            GEdit.spin_button ~adjustment:velo_adj ~packing:(box#add) () in
          ignore (spin#connect#changed ~callback:( fun () ->
            if ev.Midi.data_2 <> (int_of_float velo_adj#value) then (
              ev.Midi.data_2 <- int_of_float velo_adj#value;
              util_update_add_edit_line box ef;
              ef_cmd_redraw ef;
            );
          ));
        );
    | _ ->
        Log.warn "In util_update_add_edit_line, far from being implemented\n";
    end;
  );

)


(******************************************************************************)
(** {3 The "public" constructor} *)

(** Build an editor instance *)
let track_editor app (to_edit:[`MIDI of int|`META of int]) change_callback = (

  let tk_values = tv_make_track_values app to_edit in

  let te = 
    GWindow.window ~title:(
      match to_edit with
      | `MIDI tk -> Printf.sprintf "Midi-Track Editor [%d]" tk
      | `META tk -> Printf.sprintf "Meta-Track Editor [%d]" tk
    ) () in

  ignore(te#connect#destroy ~callback:te#destroy);

  let main_vbox = GPack.vbox ~homogeneous:false
  ~packing:te#add  ~show:true () in

  let track_settings_hbox = GuiUtil.append_hbox main_vbox in

  (* The name: *)
  GuiUtil.append_label "Name: " track_settings_hbox;
  let name_entry = GEdit.entry ~text:tk_values.tv_name ~max_length:256
  ~editable:true ~has_frame:true ~width_chars:16
  ~packing:track_settings_hbox#add ~show:true () in

  (* The length (bar,quarter,tick): *)
  GuiUtil.append_vertsepar track_settings_hbox;
  GuiUtil.append_label "Length: " track_settings_hbox;

  let length_b_spin = GuiUtil.int_spin_button 0. 20000. track_settings_hbox in
  length_b_spin#adjustment#set_value (float tk_values.tv_length_b);

  GuiUtil.append_label " 4/4 bars, " track_settings_hbox;
  
  let length_q_spin = GuiUtil.int_spin_button 0. 20000. track_settings_hbox in
  length_q_spin#adjustment#set_value (float tk_values.tv_length_q);

  GuiUtil.append_label " quarters and " track_settings_hbox;
  
  let length_t_spin = GuiUtil.int_spin_button 0. 200. track_settings_hbox in
  length_t_spin#adjustment#set_value (float tk_values.tv_length_t);

  GuiUtil.append_label " ticks" track_settings_hbox;

  (* The only MIDI ouput port: *)
  let port_combo = match tk_values.tv_type with
  | MIDI_TRACK ->
      GuiUtil.append_vertsepar track_settings_hbox;
      GuiUtil.append_label " Port: " track_settings_hbox;
      let port_combo =
        GEdit.combo_box_text ~strings:(Array.to_list S.out_put_ports)
        ~add_tearoffs:false ~packing:track_settings_hbox#add () in
      (fst port_combo)#set_active tk_values.tv_port;
      Some port_combo
  | _ -> None
  in
  
  (* next line: *)
  GuiUtil.append_horzsepar main_vbox;
  let tools_hbox = GuiUtil.append_hbox main_vbox in

  (* "Tools" buttons: *)
  let write_toggle = GuiUtil.append_toggle "Write" tools_hbox in
  let erase_toggle = GuiUtil.append_toggle "Erase" tools_hbox in
  let resiz_toggle = GuiUtil.append_toggle "Resize" tools_hbox in
  GuiUtil.append_vertsepar tools_hbox;

  (* Zoom: *)
  GuiUtil.append_label "Zoom:" tools_hbox;
  let zoom_adj = 
    GData.adjustment ~value:(1.0) ~lower:(1.0) ~upper:200.0
    ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
  let zoom_scale = 
    GRange.scale `HORIZONTAL ~adjustment:zoom_adj  
    ~draw_value:false ~update_policy:`CONTINUOUS
    ~packing:(tools_hbox#pack ~expand:true ~fill:true ~padding:0)
    ~show:true () in
  GuiUtil.append_vertsepar tools_hbox;

  (* Mouse behaviour: *)
  GuiUtil.append_label "Snap:" tools_hbox;
  let snap_combo = 
    GEdit.combo_box_text
    ~strings:["1"; "1/2"; "1/4"; "1/16"; "none"]
    ~add_tearoffs:false (* ~active:1 *)
    ~packing:(tools_hbox#pack ~expand:false ~fill:false ~padding:0) ()
  in
  (fst snap_combo)#set_active 0;

  GuiUtil.append_horzsepar main_vbox;

  (* The "edit" line: *)
  let add_edit_hbox = GuiUtil.append_hbox main_vbox in

  GuiUtil.append_horzsepar main_vbox;

  let ev_frame =
    ef_make main_vbox tk_values
    ~on_selection:(util_update_add_edit_line add_edit_hbox)
  in

  util_update_add_edit_line add_edit_hbox ev_frame;

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
    ) else (
      if (ev_frame.ef_pointer.ep_tool = EPTool_Write) then (
        ef_set_tool ev_frame EPTool_None;
      );
    );
  ));
  ignore(erase_toggle#connect#toggled ~callback:(fun () -> 
    if erase_toggle#active then (
      ef_set_tool ev_frame EPTool_Erase;
      write_toggle#set_active false;
      resiz_toggle#set_active false;
    ) else (
      if (ev_frame.ef_pointer.ep_tool = EPTool_Erase) then (
        ef_set_tool ev_frame EPTool_None;
      );
    );
  ));
  ignore(resiz_toggle#connect#toggled ~callback:(fun () -> 
    if resiz_toggle#active then (
      ef_set_tool ev_frame EPTool_Resize;
      erase_toggle#set_active false;
      write_toggle#set_active false;
    ) else (
      if (ev_frame.ef_pointer.ep_tool = EPTool_Resize) then (
        ef_set_tool ev_frame EPTool_None;
      );
    );
  ));


  (* Set the name (directly): *)
  ignore(name_entry#connect#changed ~callback:( fun () ->
    Log.p "Name changed: %s\n" name_entry#text;
    tk_values.tv_name <- name_entry#text;
    tv_update_track_info tk_values; change_callback ();
    ef_cmd_redraw ev_frame;
  ));
  (* Set the length: *)
  ignore(length_b_spin#connect#changed ~callback:(fun () ->
    tk_values.tv_length_b <- int_of_float length_b_spin#adjustment#value;
    tv_update_track_info tk_values; change_callback ();
    ef_cmd_redraw ev_frame;
  ));
  ignore(length_q_spin#connect#changed ~callback:(fun () ->
    tk_values.tv_length_q <- int_of_float length_q_spin#adjustment#value;
    tv_update_track_info tk_values; change_callback ();
    ef_cmd_redraw ev_frame;
  ));
  ignore(length_t_spin#connect#changed ~callback:(fun () ->
    tk_values.tv_length_t <- int_of_float length_t_spin#adjustment#value;
    tv_update_track_info tk_values; change_callback ();
    ef_cmd_redraw ev_frame;
  ));
  (* Set the port (MIDI) *)
  begin match port_combo with
  | Some (pc, _) -> 
      ignore( pc#connect#changed ~callback:( fun () ->
        tk_values.tv_port <- pc#active;
        tv_update_track_info tk_values; change_callback ();
        ef_cmd_redraw ev_frame;
      ));
  | _ -> ()
  end;

  te#show ();
)


