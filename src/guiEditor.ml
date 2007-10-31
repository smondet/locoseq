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

(******************************************************************************)
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
  let int_spin_button min max (box:GPack.box) = (
    let adj =
      GData.adjustment ~value:min ~lower:min ~upper:max
      ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
    GEdit.spin_button ~adjustment:adj
    ~packing:(box#pack ~expand:false ~fill:false ~padding:0) ()
  )


  (** To debug keyboard shortcuts with printfs... *)
  let modifier_to_string = function
    | `BUTTON1  -> "BUTTON1 "
    | `BUTTON2  -> "BUTTON2 "
    | `BUTTON3  -> "BUTTON3 "
    | `BUTTON4  -> "BUTTON4 "
    | `BUTTON5  -> "BUTTON5 "
    | `CONTROL  -> "CONTROL "
    | `LOCK     -> "LOCK    "
    | `MOD1     -> "MOD1    "
    | `MOD2     -> "MOD2    "
    | `MOD3     -> "MOD3    "
    | `MOD4     -> "MOD4    "
    | `MOD5     -> "MOD5    "
    | `SHIFT    -> "SHIFT   "

end


(******************************************************************************)
(** Midi related utilities *)
module MidiUtil = struct


  let note_octave_of_event ev = (
    ev.Tracker.MidiEvent.e_dat1 mod 12 , (ev.Tracker.MidiEvent.e_dat1 / 12) + 1
  )

  let note_of_val_and_oct value octave = (octave - 1) * 12 + value

  let note_to_string note = (
    let value,octave = note_octave_of_event note in
    let virtual_note = StringServer.note_names.(value) in
    let chan = note.Tracker.MidiEvent.e_chan in
    Printf.sprintf "NOTE: %s%d -> %d" virtual_note octave chan
  )

  let midi_event_to_string midi_ev = (
    (* To optimize !! TODO *)
    let cmd = Midi.midi_cmd_of_event (Tracker.MidiEvent.to_midi midi_ev) in
    let chan = midi_ev.Tracker.MidiEvent.e_chan in
    Printf.sprintf "%s -> %d" (Midi.midi_cmd_to_string cmd) chan
  )
end

(******************************************************************************)
(** Meta events related utilities *)
module MetaUtil = struct
  type meta_type = SetBPM | SetTrOn | SetTrOff | KeepTrOn
  let type_and_val_to_str t v = (
    let spr = Printf.sprintf in
    match t with 
    | SetBPM   -> spr "Set BPM = %d" v
    | SetTrOn  -> spr "Track %d On" v
    | SetTrOff -> spr "Track %d Off" v
    | KeepTrOn -> spr "Keep Track [%d] ON " v
  )
  let type_tiks_val_to_spec t tik ?end_tik v = (
    match t with 
    | SetBPM   -> `set_bpm       (tik, v)
    | SetTrOn  -> `track_set_on  (tik, v)
    | SetTrOff -> `track_set_off (tik, v)
    | KeepTrOn -> 
        match end_tik with
        | Some e -> `track_on    (tik,e, v)
        | None -> failwith "type_tiks_val_to_spec: no end_tik provided !"
  )

  let type_string_list = [
    "Set BPM";
    "Set Track On";
    "Set Track Off";
    "Keep Track On";
  ]
  let string_to_type = function
    | "Set BPM"        -> SetBPM  
    | "Set Track On"   -> SetTrOn 
    | "Set Track Off"  -> SetTrOff
    | "Keep Track On"  -> KeepTrOn
    | _ -> failwith "MetaUtil.string_to_type: unknown string"

  let type_to_index = function
    | SetBPM   -> 0
    | SetTrOn  -> 1
    | SetTrOff -> 2
    | KeepTrOn -> 3
  let type_of_arg_string = function
    | SetBPM   -> "value"
    | SetTrOn  -> "track"
    | SetTrOff -> "track"
    | KeepTrOn -> "track"

  let new_virgin_spec = function
    | SetBPM   -> `set_bpm       (0, 0)
    | SetTrOn  -> `track_set_on  (0, 0)
    | SetTrOff -> `track_set_off (0, 0)
    | KeepTrOn -> `track_on (0, 100, 0)
end

(******************************************************************************)
(** Editor global settings (colors, fonts...) *)
module Settings = struct

  (** The editor font: *)
  let main_font = ref "Monospace 6"

  (** The colors used: *)
  module Colors = struct
    let make_color str = (`NAME str : GDraw.color)
    let background   = ref (make_color "#4A00DD")
    let grid = ref (make_color "#E8B500")
    let text = ref (make_color "#B9FFB1")
    let selected_text = ref (make_color "#FF0C00")
    let text_velocity = ref (make_color "#D40000")

    let event_tick = ref (make_color "#CB000A")
    let event_range = ref (make_color "#2DCB00")
  end

  let separator_width = ref 3
  let horiz_lines_width = ref 2

  let vert_cut_quarter_width = ref 1
  let vert_quarter_width = ref 2

  let midi_note_minimum_tick_size = ref 15
end

(** One more renaming *)
module Col = Settings.Colors

(******************************************************************************)
(** {3 The model} *)

(** A {i minimalistic} variant to optimize matching *)
type track_type = MIDI_TRACK | META_TRACK


(** Type of midi or meta events for the editor*)
type editable_event =
  | EE_None
  (** The [editable_event] is a kind of [option] *)
  | EE_Midi of Tracker.MidiEvent.midi_event
  (** Generic midi event *)
  | EE_MidiNote of (Tracker.MidiEvent.midi_event * Tracker.MidiEvent.midi_event) list
  (** One midi note, and its instances in the track (NoteOn, NoteOff) *)
  | EE_MetaSpecOneTick of MetaUtil.meta_type * int * int 
  (** Meta-event based on one-tick-date *)
  | EE_MetaSpecRange of MetaUtil.meta_type * int * int * int
  (** Meta-event based on two ticks range *)


(** An {i ugly} mapping structure that encloses both midi and meta tracks for
 the editor  *)
type track_values = {
  mutable tv_app: SeqApp.seq_app; (** The {i application} handle *)
  mutable tv_tk_id: int; (** The track ID *)
  mutable tv_type : track_type; (** Midi or Meta *)
  mutable tv_name: string;
  mutable tv_track_length: int;
  mutable tv_sched_length: int;
  mutable tv_pqn : int;
  mutable tv_port: int;
  mutable tv_edit_evts : editable_event array;
}

let util_make_midi_editables midi_events track_length = (

  let module HT = Hashtbl in
  let module Mi = Tracker.MidiEvent in

  let note_ons  = HT.create 50 in
  let note_offs  = HT.create 50 in
  let note_on_keys = ref [] in
  let add_key note = 
    note_on_keys := note::(List.filter (fun x -> x <> note) !note_on_keys) in
    

  let other_list = ref [] in
  let notes_list = ref [] in

  List.iter (
    fun midi_ev ->
      match midi_ev.Mi.e_stat with
      | rs when ((0x80<= rs) && (rs <= 0x8F)) ->
          HT.add note_offs midi_ev.Mi.e_dat1 midi_ev;
      | rs when ((0x90<= rs) && (rs <= 0x9F)) -> 
          HT.add note_ons midi_ev.Mi.e_dat1 midi_ev;
          add_key midi_ev.Mi.e_dat1;
      | _ -> other_list := (EE_Midi midi_ev)::!other_list;
  ) midi_events;

  List.iter (
    fun x -> 
      let for_this_note = ref [] in

      let ons  = ref (
        List.fast_sort (fun x y -> compare x.Mi.e_tiks y.Mi.e_tiks)
        (HT.find_all note_ons x)) in
      let ofs = ref (
        List.fast_sort (fun x y -> compare x.Mi.e_tiks y.Mi.e_tiks)
        (HT.find_all note_offs x)) in
      let ons_iter = ref (List.length !ons) in
      let ofs_iter = ref (List.length !ofs) in
      while !ons_iter > 0 && !ofs_iter > 0 do
        let the_on,the_of = List.hd !ons , List.hd !ofs in
        if the_on.Mi.e_tiks < the_of.Mi.e_tiks
        then (
          (* "normal" case: *)
          for_this_note := (the_on, the_of) :: !for_this_note;
          ons := List.tl !ons; decr ons_iter;
          ofs := List.tl !ofs; decr ofs_iter;
        ) else (
          Log.p "Not off (%d) before note on (%d)\n"
          the_of.Mi.e_tiks the_on.Mi.e_tiks;
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
      let get_note = function | EE_MidiNote ((mev,_)::_) -> mev.Mi.e_dat1
      | _ -> failwith "not a note in note list" in
      - (compare (get_note x) (get_note y))
  ) !notes_list) (List.rev (!other_list)))
)

let util_meta_editable_of_spec meta_ev = (
  match meta_ev with
  | `track_set_on  (tik,v) -> EE_MetaSpecOneTick (MetaUtil.SetTrOn, tik, v)
  | `track_set_off (tik,v) -> EE_MetaSpecOneTick (MetaUtil.SetTrOff, tik, v)
  | `set_bpm       (tik,v) -> EE_MetaSpecOneTick (MetaUtil.SetBPM, tik, v)
  | `track_on      (t_b,t_e,v) -> 
      EE_MetaSpecRange (MetaUtil.KeepTrOn, t_b, t_e, v)
)
let util_make_meta_editables meta_events = (
  List.rev (List.rev_map util_meta_editable_of_spec meta_events)
)

(** [track_values] constructor *)
let tv_make_track_values app tk_ref = (
  match tk_ref with
  | `MIDI tk -> 
      let name, port, track_length, sched_length =
        App.get_midi_track_information app tk in
      let _,p = App.get_bpm_ppqn app in
      let midi_events = App.get_midi_track app tk in
      let editables_list =
        util_make_midi_editables midi_events track_length in
      let editables_array = Array.of_list editables_list in
      {
        tv_app = app;
        tv_tk_id = tk;
        tv_type = MIDI_TRACK;
        tv_name = name; 
        tv_track_length = track_length;
        tv_sched_length = sched_length;
        tv_pqn = p;
        tv_port = port;
        tv_edit_evts = editables_array;
      }
  | `META tk ->
      let name, track_length, sched_length =
        App.get_meta_track_information app tk in
      let _,p = App.get_bpm_ppqn app in
      let meta_events = App.get_meta_track app tk in
      let editables_list = util_make_meta_editables meta_events in
      let editables_array = Array.of_list editables_list in
      {
        tv_app = app;
        tv_tk_id = tk;
        tv_type = META_TRACK;
        tv_name = name; 
        tv_track_length = track_length;
        tv_sched_length = sched_length;
        tv_pqn = p;
        tv_port = 0;
        tv_edit_evts = editables_array;
      }
)

let tv_ticks_length tv = (tv.tv_track_length)
  (* let b,q,t = *)
    (* tv.tv_length_b, tv.tv_length_q, tv.tv_length_t in *)
  (* let p = tv.tv_pqn in *)
  (* b * 4 * p + q * p + t *)
(* ) *)
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
      tv.tv_name tv.tv_port tv.tv_track_length tv.tv_sched_length;
  | META_TRACK ->
      App.set_meta_track_information tv.tv_app tv.tv_tk_id tv.tv_name 
      tv.tv_track_length tv.tv_sched_length;
  end
)

let tv_do_changes_for_meta_track tv = (

  let event_list = Array.to_list (
    Array.map (function
      | EE_MetaSpecOneTick (typ, tik, v) -> 
          MetaUtil.type_tiks_val_to_spec typ tik v
      | EE_MetaSpecRange (typ, t_b, t_e, v) -> 
          MetaUtil.type_tiks_val_to_spec typ t_b ~end_tik:t_e v
      | _ ->
          Log.warn "Not a meta event in tv_do_changes_for_meta_track\n";
          failwith "Not a meta event in tv_do_changes_for_meta_track";
    ) tv.tv_edit_evts
  ) in

  App.replace_meta_track tv.tv_app tv.tv_tk_id tv.tv_name
  (tv_ticks_length tv) event_list;

  (* is it really necessary ? -> keep it *)
  tv_rebuild_editables tv;
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
  ef.ef_draw#set_foreground !Col.background;
  ef.ef_draw#rectangle ~x:0 ~y:0
  ~width:ef.ef_w ~height:ef.ef_h ~filled:true ();
  ef.ef_draw#set_foreground !Col.grid;
  ef.ef_draw#rectangle ~x:0 ~y:0
  ~width:ef.ef_w ~height:ef.ef_h ~filled:false ();

  (* The vertical separator: *)
  ef.ef_draw#rectangle ~x:(ef.ef_grid_begin_x - !Settings.separator_width) ~y:0
  ~width:!Settings.separator_width ~height:ef.ef_h ~filled:true ();

  (* Horizontal lines: *)
  for i = 1 to ev_nb do
    ef.ef_draw#rectangle ~x:0 ~y:( 1 + (i * ly) )
    ~width:ef.ef_w ~height:!Settings.horiz_lines_width ~filled:true ();
  done;


  (* Vertical lines: *)
  for i = 0 to ticks_length do
    if ( i mod ef.ef_model.tv_pqn) = 0 then (
      let x = ef_ticks_to_pixels ef i in
      ef.ef_draw#rectangle ~x:(ef.ef_grid_begin_x + x) ~y:0
      ~width:!Settings.vert_quarter_width ~height:ef.ef_h ~filled:true ();
    ) else (
      if ( i mod (ef.ef_model.tv_pqn / ef.ef_cut_quarters) ) = 0 then (
        let x = ef_ticks_to_pixels ef i in
        ef.ef_draw#rectangle ~x:(ef.ef_grid_begin_x + x) ~y:0
        ~width:!Settings.vert_cut_quarter_width ~height:ef.ef_h ~filled:true ();
      )
    )
  done;
)

let ef_x_for_velocity_of_midi_note ef tik_start tik_end = (
  (* for now we ignore 'tik_end' *)
  ef.ef_grid_begin_x + (ef_ticks_to_pixels ef tik_start) + 5
)


let ef_draw_event ef index event = (
  let choose_text_color () = 
    if (index = ef.ef_current_selection) then (
      ef.ef_draw#set_foreground !Col.selected_text;
    ) else (
      ef.ef_draw#set_foreground !Col.text;
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
    ef.ef_draw#set_foreground !Col.event_range;
    ef.ef_draw#rectangle ~x:x_on_in_grid ~y:(current_y + 2)
    ~width:(x_off_in_grid - x_on_in_grid) ~height:(unit_y - 5)
    ~filled:true ();
  in
  let draw_tick_event ~t_value  ~current_y ~unit_y =
    let x_in_grid =
      ef.ef_grid_begin_x + 
      (ef_ticks_to_pixels ef t_value) in
    ef.ef_draw#set_foreground !Col.event_tick;
    ef.ef_draw#rectangle ~x:x_in_grid ~y:(current_y + 1)
    ~width:3 ~height:(unit_y - 2) ~filled:true ();
  in
  begin match event with
  | EE_Midi ev ->
      let cur_y, unit_y = make_event_label (MidiUtil.midi_event_to_string ev) in
      draw_tick_event ~t_value:ev.Tracker.MidiEvent.e_tiks
      ~current_y:cur_y ~unit_y;

  | EE_MidiNote [] -> Log.warn "An empty midi note ??? keep going on\n";
  | EE_MidiNote ev_list ->
      let ev, _ = List.hd ev_list in
      let cur_y, unit_y = make_event_label (MidiUtil.midi_event_to_string ev) in
      
      List.iter (
        fun (ev_on , ev_off) ->
          let start_t, end_t =
            ev_on.Tracker.MidiEvent.e_tiks, ev_off.Tracker.MidiEvent.e_tiks in
          draw_range_event ~start_t ~end_t ~current_y:cur_y ~unit_y;

          let str = Printf.sprintf "v%d" ev_on.Tracker.MidiEvent.e_dat2 in
          ef.ef_draw#set_foreground !Col.text_velocity;
          let x = ef_x_for_velocity_of_midi_note ef start_t end_t in
          ef.ef_draw#put_layout ~x ~y:(cur_y + 1) (ef_make_layout ef str);
     
      ) ev_list;
  | EE_MetaSpecOneTick (typ, tik, v) -> 
      let cur_y, unit_y =
        make_event_label (MetaUtil.type_and_val_to_str typ v) in
      draw_tick_event ~t_value:tik ~current_y:cur_y ~unit_y;
  | EE_MetaSpecRange (typ, start_t, end_t, v) -> 
      let cur_y, unit_y =
        make_event_label (MetaUtil.type_and_val_to_str typ v) in
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
  let module M = Tracker.MidiEvent in
  let idx = ref 0 in
  let ended = ref false in
  while !idx < (Array.length ef.ef_model.tv_edit_evts) && not !ended do
    if (
      match ef.ef_model.tv_edit_evts.(!idx) with
      | EE_Midi mev ->  editable = EE_Midi mev 
      | EE_MidiNote ((b,_) :: _) -> (
          match editable with
          | EE_MidiNote ((bb,_) :: _) when b.M.e_dat1 = bb.M.e_dat1 -> true
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
        let x_ticks = ef_pixels_to_ticks ef (x_release - ef.ef_grid_begin_x) in
        midi_ev.Tracker.MidiEvent.e_tiks <- x_ticks;
        ef_cmd_redraw ef;
      in
      ef.ef_pointer.ep_status <- EPStatus_XDrag (on_drag, on_release);
    )
    let start_resize_midi_velocity ef midi_ev x_start = (
      let (x_min,x_max) = 0, ef.ef_w + 100 in
      let on_drag x = (x_min <= x) && (x <= x_max) in
      let on_release x_release =
        let distance = (float (x_release - x_start)) in
        let new_velo =
          midi_ev.Tracker.MidiEvent.e_dat2 + (int_of_float (distance *. 0.4)) in
        midi_ev.Tracker.MidiEvent.e_dat2 <- new_velo;
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
          ef.ef_current_selection <- -1;
          ef.ef_on_selection ef;
          ef_cmd_redraw ef;
        );
      in
      ef.ef_pointer.ep_status <- EPStatus_XDrag (on_drag, on_release);
    )
    let start_removing_midi_note ef (ev_b, ev_e) = (
      let on_drag x =
        ef_pointer_in_ticks_interval ef x
        (ev_b.Tracker.MidiEvent.e_tiks,ev_e.Tracker.MidiEvent.e_tiks)
      in
      let on_release x =
        if (ef.ef_grid_begin_x <= x && x <= ef.ef_w) then (
          App.remove_midi_event_from_track
          ef.ef_model.tv_app ef.ef_model.tv_tk_id ev_b;
          App.remove_midi_event_from_track
          ef.ef_model.tv_app ef.ef_model.tv_tk_id ev_e;
          tv_rebuild_editables ef.ef_model;
          ef.ef_current_selection <- -1;
          ef.ef_on_selection ef;
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
          let module Mi = Tracker.MidiEvent in
          let note_on, note_of =
            Mi.copy_midi_event example_on, Mi.copy_midi_event example_of
          in
          if (x_start_ticks < x_release_ticks) then (
            note_on.Mi.e_tiks <- x_start_ticks;
            note_of.Mi.e_tiks <- x_release_ticks;
          ) else (
            note_on.Mi.e_tiks <- x_release_ticks;
            note_of.Mi.e_tiks <- x_start_ticks;
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
          let t_b,t_e =
            ev_b.Tracker.MidiEvent.e_tiks, ev_e.Tracker.MidiEvent.e_tiks in
          if (ef_pointer_touches_ticks ef x t_b) then (
            start_resize_midi_ticks ef ev_b
            ~valid_interval:(ef.ef_grid_begin_x, pixelize t_e);
          ) else if (ef_pointer_touches_ticks ef x t_e)
          then (
            start_resize_midi_ticks ef ev_e
            ~valid_interval:(pixelize t_b, ef.ef_w);
          ) else (
            let x_v = (ef_x_for_velocity_of_midi_note ef t_b t_e) + 3 in
            let preci = ef.ef_pointer.ep_precision in
            if (x - preci < x_v) && (x_v < x + preci) then (
              start_resize_midi_velocity ef ev_b x;
            ) else (
              iter_note_instances_for_resize q;
            )
          )

    let rec iter_note_instances_for_erase = function
      | [] -> ()
      | (ev_b, ev_e) :: q ->
          if (
            ef_pointer_in_ticks_interval ef x
            (ev_b.Tracker.MidiEvent.e_tiks,ev_e.Tracker.MidiEvent.e_tiks)
          ) then (
            start_removing_midi_note ef (ev_b,ev_e);
          ) else (
            iter_note_instances_for_erase q;
          )
    let start_removing_meta_event ef typ b_tik e_tik edit_val = (
      let on_drag x =
        if (typ = MetaUtil.KeepTrOn) then
          ef_pointer_in_ticks_interval ef x (b_tik, e_tik)
        else
          true
      in
      let on_release x =
        if (ef.ef_grid_begin_x <= x && x <= ef.ef_w) then (
          Log.p "Removing \n" ;
          App.remove_meta_event_from_track
          ef.ef_model.tv_app ef.ef_model.tv_tk_id
          (MetaUtil.type_tiks_val_to_spec typ b_tik ~end_tik:e_tik edit_val);
          tv_rebuild_editables ef.ef_model;
          ef.ef_current_selection <- -1;
          ef.ef_on_selection ef;
          ef_cmd_redraw ef;
        );
      in
      ef.ef_pointer.ep_status <- EPStatus_XDrag (on_drag, on_release);
    )
    let start_resize_meta_ticks ef typ tik value event_index  = (
      let (x_min,x_max) = ef.ef_grid_begin_x, ef.ef_w in
      let on_drag x = (x_min <= x) && (x <= x_max) in
      let on_release x_release =
        let x_ticks = 
          ef_pixels_to_ticks ef (x_release - ef.ef_grid_begin_x) in
        ef.ef_model.tv_edit_evts.(event_index) <- EE_MetaSpecOneTick (
          typ, x_ticks, value
        );
        tv_do_changes_for_meta_track ef.ef_model;
        tv_rebuild_editables ef.ef_model;
        ef.ef_on_selection ef;
        ef_cmd_redraw ef;
      in
      ef.ef_pointer.ep_status <- EPStatus_XDrag (on_drag, on_release);
    )
    type to_edit = Begining | TheEnd | Nothing
    let start_resize_meta_range ef typ b_tik e_tik v event_index = (
      let what_to_edit =
        if (ef_pointer_touches_ticks ef x b_tik) then
          Begining
        else 
          if (ef_pointer_touches_ticks ef x e_tik) then
            TheEnd
          else
            Nothing
      in
      if what_to_edit <> Nothing then (
        let (x_min,x_max) = ef.ef_grid_begin_x, ef.ef_w in
        let on_drag x = (x_min <= x) && (x <= x_max) in
        let on_release x_release =
          let x_ticks = 
            ef_pixels_to_ticks ef (x_release - ef.ef_grid_begin_x) in
          if what_to_edit = Begining then (
            ef.ef_model.tv_edit_evts.(event_index) <- EE_MetaSpecRange (
              typ, x_ticks, e_tik, v
            );
          ) else (
            ef.ef_model.tv_edit_evts.(event_index) <- EE_MetaSpecRange (
              typ, b_tik, x_ticks, v
            );
          );
          tv_do_changes_for_meta_track ef.ef_model;
          tv_rebuild_editables ef.ef_model;
          ef.ef_on_selection ef;
          ef_cmd_redraw ef;
        in
        ef.ef_pointer.ep_status <- EPStatus_XDrag (on_drag, on_release);
      );
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
              if (ef_pointer_touches_ticks ef x mev.Tracker.MidiEvent.e_tiks
              ) then (
                LocalUtil.start_resize_midi_ticks ef mev
                ~valid_interval:(ef.ef_grid_begin_x, ef.ef_w);
              );
          | EE_MetaSpecOneTick (typ, tik, edit_val) ->
              if (ef_pointer_touches_ticks ef x tik) then (
                LocalUtil.start_resize_meta_ticks ef typ tik edit_val event_id;
              );
          | EE_MetaSpecRange (typ, b_tik, e_tik, v) ->
              LocalUtil.start_resize_meta_range ef typ b_tik e_tik v event_id;
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
              if (ef_pointer_touches_ticks ef x mev.Tracker.MidiEvent.e_tiks
              ) then (
                LocalUtil.start_removing_midi_event ef mev;
              );
          | EE_MidiNote  ev_list ->
              LocalUtil.iter_note_instances_for_erase ev_list;
          | EE_MetaSpecOneTick (typ, tik, edit_val) ->
              if (ef_pointer_touches_ticks ef x tik) then (
                LocalUtil.start_removing_meta_event ef typ tik tik edit_val;
              );
              Log.p "EE_MetaSpecOneTick\n";
          | EE_MetaSpecRange (typ, tik, end_tik, edit_val) ->
              LocalUtil.start_removing_meta_event ef typ tik end_tik edit_val;
              Log.p "EE_MetaSpecRange\n";
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
let ef_make (box:GPack.box) values ~on_selection ~on_mouse_move = (

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
    ef_zoom = 50;
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

  ef_set_font the_event_frame !Settings.main_font;

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

  (* This is needed to get all motions: (GTK's bug or feature ?) *)
  event_box#event#add [`POINTER_MOTION; ];
  (* http://marc.info/?l=php-gtk-general&m=117030947412128&w=2 *)
  (* http://www.mail-archive.com/gtk-list@gnome.org/msg25046.html *)

  ignore(event_box#event#connect#motion_notify ~callback:(
    fun ev ->
      let x = int_of_float (GdkEvent.Motion.x ev) in
    	let y = int_of_float (GdkEvent.Motion.y ev) in
      ef_on_mouse_drag the_event_frame x y;
      on_mouse_move the_event_frame x y;
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
        let module Mi = Tracker.MidiEvent in
        let menuitem_note =  
          GMenu.menu_item ~label:"Midi Note" ~packing:menu#append () in
        ignore(menuitem_note#connect#activate ~callback:(fun () ->
          (* add the event, rebuild,  set it to selected *)
          let the_event_on = Mi.create 0 0x90 0 0 0 in
          let the_event_of = Mi.create ef.ef_model.tv_pqn 0x80 0 0 0 in
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
          let the_event = Mi.create 0 0 0 0 0  in
          App.add_midi_event_to_track 
          ef.ef_model.tv_app ef.ef_model.tv_tk_id the_event;
          tv_rebuild_editables ef.ef_model;
          ef_set_editable_selected ef (EE_Midi the_event);
          util_update_add_edit_line box ef;
          ef_cmd_redraw ef;
        ));
    | META_TRACK -> 
        List.iter (fun str ->
          let current_item = 
            GMenu.menu_item ~label:str ~packing:menu#append () in
          ignore(current_item#connect#activate ~callback:(fun () ->
            let typ = MetaUtil.string_to_type str in
            let spec = MetaUtil.new_virgin_spec typ in
            App.add_meta_event_to_track 
            ef.ef_model.tv_app ef.ef_model.tv_tk_id spec;
            tv_rebuild_editables ef.ef_model;
            ef_set_editable_selected ef (util_meta_editable_of_spec spec);
            util_update_add_edit_line box ef;
            ef_cmd_redraw ef;
          ));
        ) MetaUtil.type_string_list;
    end;
    menu#popup  ~button:1 ~time:0l;
          

  ));
  let make_bar_for_meta_event typ start_t end_t edit_val = 

    GuiUtil.append_label "META EVENT: " box;
    let stat_entry = 
      GEdit.combo_box_text
      ~strings:MetaUtil.type_string_list ~packing:box#add () in
    let cbo,_ = stat_entry in
    cbo#set_active (MetaUtil.type_to_index typ);
    ignore(cbo#connect#changed ~callback:( fun () ->
      begin match GEdit.text_combo_get_active stat_entry with
      | Some c -> 
          begin match MetaUtil.string_to_type c with
          | MetaUtil.KeepTrOn ->
              ef.ef_model.tv_edit_evts.(ef.ef_current_selection) <- (
                EE_MetaSpecRange (MetaUtil.KeepTrOn, start_t, end_t, edit_val)
              );
          | new_typ -> 
              ef.ef_model.tv_edit_evts.(ef.ef_current_selection) <- (
                EE_MetaSpecOneTick (new_typ, start_t, edit_val)
              );
          end;
      | None -> ()
      end;
      tv_do_changes_for_meta_track ef.ef_model;
      util_update_add_edit_line box ef;
      ef_cmd_redraw ef;
    ));
    GuiUtil.append_label (MetaUtil.type_of_arg_string typ) box;
    let lower,upper = 
      match typ with MetaUtil.SetBPM -> 0., 255. | _ -> (-20000., 20000.) in
    let adj =
      GData.adjustment ~value:(float edit_val) ~lower ~upper
      ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
    let spin =
      GEdit.spin_button ~adjustment:adj ~packing:(box#add) () in
    ignore (spin#connect#changed ~callback:( fun () ->
      let new_val = int_of_float adj#value in
      begin match typ with
      | MetaUtil.KeepTrOn ->
          ef.ef_model.tv_edit_evts.(ef.ef_current_selection) <- (
            EE_MetaSpecRange (MetaUtil.KeepTrOn, start_t, end_t, new_val)
          );
      | new_typ -> 
          ef.ef_model.tv_edit_evts.(ef.ef_current_selection) <- (
            EE_MetaSpecOneTick (new_typ, start_t, new_val)
          );
      end;
      tv_do_changes_for_meta_track ef.ef_model;
      util_update_add_edit_line box ef;
      ef_cmd_redraw ef;
    ));
  in
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
          let module Mi = Tracker.MidiEvent in
          List.iter (
            fun (evb, eve) ->
              evb.Mi.e_dat1 <- MidiUtil.note_of_val_and_oct new_note octave;
              eve.Mi.e_dat1 <- MidiUtil.note_of_val_and_oct new_note octave;
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
          let module Mi = Tracker.MidiEvent in
          if old_octave <> new_octave then (
            List.iter (
              fun (evb, eve) ->
                evb.Mi.e_dat1 <- MidiUtil.note_of_val_and_oct note new_octave;
                eve.Mi.e_dat1 <- MidiUtil.note_of_val_and_oct note new_octave;
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
        cbo#set_active (mev_b.Tracker.MidiEvent.e_chan + 1);
        ignore(cbo#connect#changed ~callback:( fun () ->
          Log.p "The active is: %d\n" cbo#active;
          let module Mi = Tracker.MidiEvent in
          let new_chan = cbo#active - 1 in
          List.iter (
            fun (evb, eve) ->
              evb.Mi.e_chan <- new_chan;
              eve.Mi.e_chan <- new_chan;
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
              incr active_index; ev.Tracker.MidiEvent.e_stat land 0xF0 = b
            ) S.midi_status_string_value in
          cbo#set_active !active_index;
          with Not_found -> ()
        end;
        ignore(cbo#connect#changed ~callback:( fun () ->
          begin match GEdit.text_combo_get_active stat_entry with
          | Some c -> 
              ev.Tracker.MidiEvent.e_stat <- S.midi_status_of_string c
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
        cbo#set_active (ev.Tracker.MidiEvent.e_chan + 1);
        ignore(cbo#connect#changed ~callback:( fun () ->
          ev.Tracker.MidiEvent.e_chan <-  cbo#active - 1;
          util_update_add_edit_line box ef;
          ef_cmd_redraw ef;
        ));
        GuiUtil.append_label " Data 1:" box;
        let note_adj =
          GData.adjustment
          ~value:(float ev.Tracker.MidiEvent.e_dat1) ~lower:(0.)
          ~upper:255.0 ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
        let spin =
          GEdit.spin_button ~adjustment:note_adj ~packing:(box#add) () in
        ignore (spin#connect#changed ~callback:( fun () ->
          if ev.Tracker.MidiEvent.e_dat1 <> (int_of_float note_adj#value) then (
            ev.Tracker.MidiEvent.e_dat1 <- int_of_float note_adj#value;
            util_update_add_edit_line box ef;
            ef_cmd_redraw ef;
          );
        ));
        (* for 2-arg midi events: *)
        let rs = ev.Tracker.MidiEvent.e_stat land 0xF0 in
        if not ((0xC0 <= rs) && (rs <= 0xDF)) then (
          GuiUtil.append_label " Data 2:" box;
          let velo_adj =
            GData.adjustment
            ~value:(float ev.Tracker.MidiEvent.e_dat2) ~lower:(0.0)
            ~upper:255.0 ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
          let spin =
            GEdit.spin_button ~adjustment:velo_adj ~packing:(box#add) () in
          ignore (spin#connect#changed ~callback:( fun () ->
            if ev.Tracker.MidiEvent.e_dat2 <> (int_of_float velo_adj#value
            ) then (
              ev.Tracker.MidiEvent.e_dat2 <- int_of_float velo_adj#value;
              util_update_add_edit_line box ef;
              ef_cmd_redraw ef;
            );
          ));
        );
    | EE_MetaSpecOneTick (typ, tik, edit_val) ->
        make_bar_for_meta_event typ tik (tik + ef.ef_model.tv_pqn) edit_val;
    | EE_MetaSpecRange (typ, tik, end_tik, edit_val) ->
        make_bar_for_meta_event typ tik end_tik edit_val;
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
    GWindow.window
    ~width:500 ~height:300 ~allow_shrink:true
    ~title:(
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

  (* The only MIDI ouput port: *)
  let port_combo = match tk_values.tv_type with
  | MIDI_TRACK ->
      GuiUtil.append_vertsepar track_settings_hbox;
      GuiUtil.append_label " Port: " track_settings_hbox;
      let port_combo =
        GEdit.combo_box_text ~strings:(Array.to_list S.App.out_put_ports)
        ~add_tearoffs:false ~packing:(
          track_settings_hbox#pack ~expand:false ~fill:false ~padding:0
        ) () in
      (fst port_combo)#set_active tk_values.tv_port;
      Some port_combo
  | _ -> None
  in

  (* The looping length (bar,quarter,tick): *)
  GuiUtil.append_horzsepar main_vbox;
  let track_length_hbox = GuiUtil.append_hbox main_vbox in
  ignore(GMisc.label ~text:"Track Length (loop): " ~justify:`RIGHT
  ~packing:(track_length_hbox#pack ~expand:true ~fill:false ~padding:0) ());

  let length_b, length_q, length_t =
    S.unitize_length_tuple tk_values.tv_track_length tk_values.tv_pqn in

  let length_b_spin = GuiUtil.int_spin_button 0. 20000. track_length_hbox in
  length_b_spin#adjustment#set_value (float length_b);

  GuiUtil.append_label " 4/4 bars, " track_length_hbox;
  
  let length_q_spin = GuiUtil.int_spin_button 0. 20000. track_length_hbox in
  length_q_spin#adjustment#set_value (float length_q);

  GuiUtil.append_label " quarters and " track_length_hbox;
  
  let length_t_spin = GuiUtil.int_spin_button 0. 200. track_length_hbox in
  length_t_spin#adjustment#set_value (float length_t);

  GuiUtil.append_label " ticks" track_length_hbox;

  (* The schedulable length (bar,quarter,tick): *)
  GuiUtil.append_horzsepar main_vbox;
  let sched_length_hbox = GuiUtil.append_hbox main_vbox in
  (* XXX justification does not seem to work: *)
  ignore(GMisc.label ~text:"Schedule Length (trigger): " ~justify:`RIGHT
  ~packing:(sched_length_hbox#pack ~expand:true ~fill:false ~padding:0) ());

  let sched_b, sched_q, sched_t =
    S.unitize_length_tuple tk_values.tv_sched_length tk_values.tv_pqn in

  let sched_b_spin = GuiUtil.int_spin_button 0. 20000. sched_length_hbox in
  sched_b_spin#adjustment#set_value (float sched_b);

  GuiUtil.append_label " 4/4 bars, " sched_length_hbox;
  
  let sched_q_spin = GuiUtil.int_spin_button 0. 20000. sched_length_hbox in
  sched_q_spin#adjustment#set_value (float sched_q);

  GuiUtil.append_label " quarters and " sched_length_hbox;
  
  let sched_t_spin = GuiUtil.int_spin_button 0. 200. sched_length_hbox in
  sched_t_spin#adjustment#set_value (float sched_t);

  GuiUtil.append_label " ticks" sched_length_hbox;

  
  (* Editing Tools Line: *)
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
    GData.adjustment ~value:(1.) ~lower:(1.0) ~upper:200.0
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

  let status_bar_label = GMisc.label ~text:"<status>" ~justify:`LEFT () in

  let ev_frame =
    ef_make main_vbox tk_values
    ~on_selection:(util_update_add_edit_line add_edit_hbox)
    ~on_mouse_move:(fun ef x y ->
      status_bar_label#set_label (
        Printf.sprintf "STATUS:   pointer:(%d, %d)  time:[%s]" x y
        (S.string_of_length (ef_pixels_to_ticks ef (x - ef.ef_grid_begin_x))
        ef.ef_model.tv_pqn)
      );
    )
  in
  main_vbox#pack ~expand:false ~fill:false ~padding:0 status_bar_label#coerce;


  util_update_add_edit_line add_edit_hbox ev_frame;

  (* Connections: *)
  zoom_adj#set_value (float ev_frame.ef_zoom);
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
  let update_length () =
    let b = int_of_float length_b_spin#adjustment#value in
    let q = int_of_float length_q_spin#adjustment#value in
    let t = int_of_float length_t_spin#adjustment#value in
    tk_values.tv_track_length <- S.units_to_length b q t tk_values.tv_pqn;
    tv_update_track_info tk_values; change_callback ();
    ef_cmd_redraw ev_frame;
  in
  ignore(length_b_spin#connect#changed ~callback:update_length);
  ignore(length_q_spin#connect#changed ~callback:update_length);
  ignore(length_t_spin#connect#changed ~callback:update_length);
  (* Set the length: *)
  let update_sched () =
    let b = int_of_float sched_b_spin#adjustment#value in
    let q = int_of_float sched_q_spin#adjustment#value in
    let t = int_of_float sched_t_spin#adjustment#value in
    tk_values.tv_sched_length <- S.units_to_length b q t tk_values.tv_pqn;
    tv_update_track_info tk_values; change_callback ();
    ef_cmd_redraw ev_frame;
  in
  ignore(sched_b_spin#connect#changed ~callback:update_sched);
  ignore(sched_q_spin#connect#changed ~callback:update_sched);
  ignore(sched_t_spin#connect#changed ~callback:update_sched);
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
  ignore(te#event#connect#key_press ~callback:(fun x ->
    let modifiers = GdkEvent.Key.state x in
    (* List.iter (fun x -> Log.p "mod: %s\n" (modifier_to_string x)) modifiers; *)
    if List.exists (fun x -> x = `CONTROL) modifiers then (
      let key = (GdkEvent.Key.keyval x) in
      begin match key with
      | 119 ->  (* 'w' *)
          ef_set_tool ev_frame EPTool_Write;
          write_toggle#set_active true;
          erase_toggle#set_active false;
          resiz_toggle#set_active false;
      | 101 -> (* 'e' *)
          ef_set_tool ev_frame EPTool_Erase;
          write_toggle#set_active false;
          erase_toggle#set_active true;
          resiz_toggle#set_active false;
      | 114 ->(* 'r' *)
          ef_set_tool ev_frame EPTool_Resize;
          write_toggle#set_active false;
          erase_toggle#set_active false;
          resiz_toggle#set_active true;
      | 97 | 113 -> (* 'a' or 'q' *)
          ef_set_tool ev_frame EPTool_None;
          write_toggle#set_active false;
          erase_toggle#set_active false;
          resiz_toggle#set_active false;
      | _ ->
          Log.p "Key: %d (0x%x)\n" key key;
          ()
      end;
      true
    ) else (
      false
    )
  ));

  te#show ();
)


