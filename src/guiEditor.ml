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



let util_append_label text box = (
  let _ =
    GMisc.label ~text ~packing:(box#add) () in
  ()
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

  let track_settings_hbox = GPack.hbox  ~homogeneous:false
  ~packing:main_vbox#add ~show:true () in

  util_append_label "Name: " track_settings_hbox ;
  let name_entry = GEdit.entry ~text:"Untitled" ~max_length:128
  ~editable:true ~has_frame:true ~width_chars:16
  ~packing:track_settings_hbox#add ~show:true () in

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
    util_append_label " Port: " track_settings_hbox ;
    let port_combo =
      GEdit.combo_box_text
      ~strings:(Array.to_list S.out_put_ports)
      ~add_tearoffs:false
      (* ~active:1 *)
      (* ~allow_empty:false ~value_in_list:true *)
      ~packing:track_settings_hbox#add () in
    Some port_combo
  ) 
  | _ -> None
  in
  



  (* ignore (spin#connect#changed ~callback:( fun () -> *)
    (* rest_of_input.(3) <- int_of_float note_adj#value ;)); *)



  te#show ();
)


