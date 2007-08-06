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
 Generic track editor that should work for MIDI and META events.
 {b Work in progress}
 @author S. Mondet
 *)


module App = SeqApp ;;
module S = StringServer ;;


(** append a label to a box *)
let util_append_label text box = (
  let _ =
    GMisc.label ~text ~packing:(box#add) () in
  ()
)
let util_append_vertsepar box = (
  let sep =
    GMisc.separator `VERTICAL ~packing:(box#add) ~show:true () in
  sep#misc#set_size_request ~width:30 ();
  ()
)
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

let util_append_hbox (box:GPack.box) = (
  GPack.hbox  ~homogeneous:false
  ~packing:(box#pack ~expand:false ~fill:false ~padding:0) ~show:true ()
)

(** Launch the editor *)
let track_editor
app (to_edit:[`MIDI of int|`META of int|`NEW_MIDI|`NEW_META]) = (
  let te = GWindow.window ~title:"Track Editor" () in

  ignore(te#connect#destroy ~callback:te#destroy);

  let main_vbox = GPack.vbox ~homogeneous:false
  (* ~spacing:int *) (* ~border_width:int -> *)
  (* ~width:int -> *) (* ~height:int -> *)
  ~packing:te#add  ~show:true () in

  let track_settings_hbox = util_append_hbox main_vbox in

  util_append_label "Name: " track_settings_hbox ;
  let name_entry = GEdit.entry ~text:"Untitled" ~max_length:128
  ~editable:true ~has_frame:true ~width_chars:16
  ~packing:track_settings_hbox#add ~show:true () in

  util_append_vertsepar track_settings_hbox ;
  util_append_label "Length: " track_settings_hbox ;

  let lgth_quarters_adj =
    GData.adjustment ~value:(0.0) ~lower:(0.0) ~upper:20000.0
    ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
  let lgth_quarters_spin =
    GEdit.spin_button ~adjustment:lgth_quarters_adj
    ~packing:(track_settings_hbox#add) () in

  util_append_label " quarters and " track_settings_hbox ;
  
  let lgth_ticks_adj =
    GData.adjustment ~value:(0.0) ~lower:(0.0) ~upper:200.0
    ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
  let lgth_ticks_spin =
    GEdit.spin_button ~adjustment:lgth_ticks_adj
    ~packing:(track_settings_hbox#add) () in

  util_append_label " ticks" track_settings_hbox ;

  let port_combo = match to_edit with
  | `NEW_MIDI | `MIDI _ -> (
    util_append_vertsepar track_settings_hbox ;
    util_append_label " Port: " track_settings_hbox ;
    let port_combo =
      GEdit.combo_box_text
      ~strings:(Array.to_list S.out_put_ports)
      ~add_tearoffs:false
      (* ~active:1 *)
      (* ~allow_empty:false ~value_in_list:true *)
      ~packing:track_settings_hbox#add () in
    (fst port_combo)#set_active 0 ;
    Some port_combo
  ) 
  | _ -> None
  in
  
  util_append_horzsepar main_vbox ;
  let tools_hbox = util_append_hbox main_vbox in

  let write_button = util_append_button "Write" tools_hbox in
  let erase_button = util_append_button "Erase" tools_hbox in
  let resiz_button = util_append_button "Resize" tools_hbox in
  util_append_vertsepar tools_hbox ;
  util_append_label "Zoom:" tools_hbox ;
  let zoom_adj = 
    GData.adjustment ~value:(1.0) ~lower:(1.0) ~upper:200.0
    ~step_incr:1.0 ~page_incr:10.0 ~page_size:0.0 () in
  let zoom_scale = 
    GRange.scale `HORIZONTAL ~adjustment:zoom_adj  
    ~draw_value:false ~update_policy:`CONTINUOUS
    ~packing:tools_hbox#add ~show:true () in
  util_append_vertsepar tools_hbox ;
  util_append_label "Snap:" tools_hbox ;
  let snap_combo = 
    GEdit.combo_box_text
    ~strings:["1"; "1/2"; "1/4"; "1/16"; "none"]
    ~add_tearoffs:false
    (* ~active:1 *)
    (* ~allow_empty:false ~value_in_list:true *)
    ~packing:tools_hbox#add ()
  in
  (fst snap_combo)#set_active 0 ;

  util_append_horzsepar main_vbox ;

  let add_edit_hbox = util_append_hbox main_vbox in
  let add_button = util_append_button "Add" add_edit_hbox in
  

  util_append_vertsepar tools_hbox ;

  util_append_horzsepar main_vbox ;

  let scrolled_window = GBin.scrolled_window ~border_width:10
  ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:main_vbox#add () in
  let event_box =
    GBin.event_box ~packing:scrolled_window#add_with_viewport () in
  let draw_area = GMisc.drawing_area
  (* ~width:600 ~height:480 *)
  ~packing:event_box#add ()
  in

  let w = draw_area#misc#realize (); draw_area#misc#window in
  let drawing = new GDraw.drawable w in 
  let static_thing = ref (165,165) in
  let redraw _ =
    drawing#polygon ~filled:true
    [ 10,100; 35,35; 100,10; 165,35; 190,100;
    !static_thing ; 100,190; 35,165; 10,100 ];
    false
  in
  ignore(draw_area#event#connect#expose ~callback:redraw);
  draw_area#set_size ~width:600 ~height:400 ;
  ignore(event_box#event#connect#button_press ~callback:(
    fun ev ->
      Log.p "evb is called !!\n" ;
      let x = int_of_float (GdkEvent.Button.x ev) in
    	let y = int_of_float (GdkEvent.Button.y ev) in
      static_thing := (x,y) ;      
      Log.p "evb is called (%d,%d) !!\n" x y ;
      GtkBase.Widget.queue_draw draw_area#as_widget ;
      true
  ));



  let _ = (* NOTE:
  Avoiding "not-used" warnings during development,
  allows to remark real warnings
  *)
    add_button,zoom_scale,resiz_button,erase_button,write_button,port_combo,
    lgth_ticks_spin,lgth_quarters_spin,name_entry
  in

  te#show ();
)


