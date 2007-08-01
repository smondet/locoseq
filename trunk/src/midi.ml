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


type midi_event = {
  mutable ticks     : int ;
  mutable status    : int ;
  mutable channel   : int ;
  mutable data_1    : int ;
  mutable data_2    : int ;
  (* mutable cmd     : midi_cmd ; *)
};;

type meta_event =  {
  mutable meta_ticks   : int ;
  mutable service_id   : int ;
  mutable service_data : int array ;
};;

(* Not (yet) supported: *)
type syst_event = unit ;;

type event =
  | EmptyEvent
  | MidiEvent of midi_event 
  | MetaEvent of meta_event
  | SysEvent  of syst_event
;;

type midi_track =
  {
    mutable track_id       : int32 ;
    mutable track_length   : int32 ;(* length in bytes *)
    mutable events         : event array ;
  };;
type midi_file = 
  {
    mutable file_id          : int32 ;
    mutable header_length    : int32 ;
    mutable midi_format      : int ; (* "short" values (16 bits) *)
    mutable track_number     : int ; (* "short" values (16 bits) *)
    mutable per_quarter_note : int ; (* "short" values (16 bits) *) 
    mutable tracks           : midi_track array ;
  } ;;

(******************************************************************************)
(* 
 * Some Help about meta-event services:
 *
 * *)
type known_meta_service_util = {
  s_id : int ;
  s_name : string ;
  s_data_to_str : (int array -> string) option ;
};;
let basic_data_to_string data =          (* int array -> string *)
  let lgth =   (Array.length data) in
  let str = String.create lgth in
  for i = 0 to lgth -1 do
    String.set str i (char_of_int  data.(i)) ;
  done ;
  str
;;
let known_meta_services =
  let d2s = Some basic_data_to_string in
  let short = Some (
    fun data ->
      if (Array.length data) = 2
      then 
        string_of_int ( data.(0) * 256 + data.(1) )
      else 
        "Err"
  ) in
  let no  = None in
  [
    {s_id=0x00; s_name="SequenceNumber         "; s_data_to_str = short ;};
    {s_id=0x01; s_name="TextEvent              "; s_data_to_str = d2s;};
    {s_id=0x02; s_name="CopyrightNotice        "; s_data_to_str = d2s;};
    {s_id=0x03; s_name="SequenceOrTrackName    "; s_data_to_str = d2s;};
    {s_id=0x04; s_name="InstrumentName         "; s_data_to_str = d2s;};
    {s_id=0x05; s_name="LyricText              "; s_data_to_str = d2s;};
    {s_id=0x06; s_name="MarkerText             "; s_data_to_str = d2s;};
    {s_id=0x07; s_name="CuePoint               "; s_data_to_str = no ;};
    {s_id=0x20; s_name="MIDIChannelPrefixAssign"; s_data_to_str = no ;};
    {s_id=0x2F; s_name="EndOfTrack             "; s_data_to_str = no ;};
    {s_id=0x51; s_name="TempoSetting           "; s_data_to_str = no ;};
    {s_id=0x54; s_name="SMPTEOffset            "; s_data_to_str = no ;};
    {s_id=0x58; s_name="TimeSignature          "; s_data_to_str = no ;};
    {s_id=0x59; s_name="KeySignature           "; s_data_to_str = no ;};
    {s_id=0x7F; s_name="SequencerSpecificEvent "; s_data_to_str = no ;};
    ]
;;

(******************************************************************************)
(*
 * "Zero" Constructors:
 * 
 *)
let empty_midi_data () = 
{
  file_id          = 0l ;
  header_length    = 0l ;
  midi_format      = 0 ;
  track_number     = 0 ;
  per_quarter_note = 0 ;
  tracks           = [| |] ;
  };;

let empty_midi_track () = {
  track_id         = 0l ;
  track_length     = 0l ;
  events           = [| |] ;
};;
let empty_event () =  EmptyEvent ;;

let make_tracks nb =
  Array.init nb (fun x -> empty_midi_track ()) 
;;
let make_events nb =
  Array.init nb (fun x -> empty_event ()) 
;;

let empty_midi_event () = {
  ticks   = 0 ;
  status  = 0 ;
  channel = 0 ;
  data_1  = 0 ;
  data_2  = 0 ;
};;

(******************************************************************************)
(* 
 * Higher-levelizing the midi events:
 *
 *
 * *)
type midi_cmd =
  | EmptyCmd
  | Unkown of midi_event
  | NoteOFF of ( int * int )  (* note nb , velocity *)
  | NoteON  of ( int * int )  (* note nb , velocity *)
  | AfterTouch of ( int * int )
  | ControlChange of (int * int )
  | ProgramChange of int
  | ChannelAfterTouch of int
  | PitchRange of (int * int)
;;



let make_2B_cmd run_stat note velo =
  let ret =
    match run_stat with
    | rs when ((0x80<= rs) && (rs <= 0x8F)) -> NoteOFF (note , velo) 
    | rs when ((0x90<= rs) && (rs <= 0x9F)) -> 
        if velo = 0 (* If velocity = 0 -> note off ! *)
        then NoteOFF (note , velo)
        else NoteON (note , velo) 
    | rs when ((0xA0 <= rs) && (rs <= 0xAF)) -> AfterTouch (note,velo) 
    | rs when ((0xB0 <= rs) && (rs <= 0xBF)) -> (
      (* if ( 0x0 < note && note < 0x7F) *)
      (* then *)
        ControlChange (note,velo)
        (* else  *)
          (* (failwith "Error: Bad ControlChanged event"); *)
          )
    | rs when ((0xE0 <= rs) && (rs <= 0xEF)) -> (PitchRange (note,velo))
    | rs -> (failwith "Error: unknown 2 bytes command!!")
  in
  ret
;;

let make_1B_cmd run_stat note  =
  match run_stat with
  | rs when ((0xC0 <= rs) && (rs <= 0xCF)) -> ( ProgramChange note) 
  | rs when ((0xD0 <= rs) && (rs <= 0xDF)) -> ( ChannelAfterTouch note) 
  | _ -> (
    failwith "Error: unknown 1 byte command !!" ;
  ) 
;;

let midi_cmd_of_event ev =
  match ev.status with
  (* It is a 2 bytes MIDI EVENT: *)
  | rs when (((0x80 <= rs) && (rs <= 0xBF)) 
  || ((0xE0 <= rs) && (rs <= 0xEF))) -> (
    make_2B_cmd rs ev.data_1 ev.data_2
  )
  (* It is a 1 byte MIDI EVENT: *)
  | rs when ((0xB0 <= rs) && (rs <= 0xCF)) -> (
    make_1B_cmd rs ev.data_1
   )
  | _ -> (
    Unkown ev
  ) 
;;



(******************************************************************************)
(* 
 * Printer for midi_data:
 *
 * *)
let midi_cmd_to_string cmd =
  match cmd with
  | EmptyCmd            -> "EmptyCmd"
  | NoteOFF (n,v)       -> Printf.sprintf "NoteOFF            %d %d" n v 
  | NoteON  (n,v)       -> Printf.sprintf "NoteON             %d %d" n v 
  | AfterTouch (n,v)    -> Printf.sprintf "AfterTouch         %x %x" n v 
  | ControlChange (n,v) -> Printf.sprintf "ControlChange      %x %x" n v 
  | ProgramChange n     -> Printf.sprintf "ProgramChange      %x   " n  
  | ChannelAfterTouch n -> Printf.sprintf "ChannelAfterTouch  %x   " n  
  | PitchRange (n,v)    -> Printf.sprintf "PitchRange         %x %x" n v 
  | Unkown ev -> Printf.sprintf "Unkown [stat:%d [%x %x]]" ev.status ev.data_1 ev.data_2
;;


let meta_service_to_string ms =
  try 
    let some_help = 
      List.find (fun h -> h.s_id = ms.service_id) known_meta_services in
    match some_help.s_data_to_str with
    | Some f -> 
        Printf.sprintf "0x%x:%s: %S"
        some_help.s_id some_help.s_name (f ms.service_data)
    | None -> 
        Printf.sprintf "0x%x:%s: len=%d"
        some_help.s_id some_help.s_name (Array.length ms.service_data)
  with
  Not_found -> (
    Printf.sprintf "UnknownMetaEvent: %x (len:%d)" 
    ms.service_id (Array.length ms.service_data) )
;;


let event_to_string ev =
  match ev with
  | EmptyEvent           -> "EmptyEvent" 
  | MidiEvent miev ->   
      Printf.sprintf "MidiEvent [T=%8d] [C=%8x] [%s]"
      miev.ticks miev.channel (midi_cmd_to_string (midi_cmd_of_event miev));
  | MetaEvent meta_event -> 
      Printf.sprintf "MetaEvent [T=%8d] [%s]"
      meta_event.meta_ticks (meta_service_to_string meta_event) ;
  | SysEvent  syst_event -> "SysEvent  " 
;;

let midi_to_string  data =
  let ret_str = Buffer.create 16 in
  let pr = Printf.sprintf in
  Buffer.add_string ret_str (
    pr "MIDI DATA: ID[%8lX] len[%ld] fmt[%d] num[%d] per_quarter_note[%d]\n"
    data.file_id
    data.header_length
    data.midi_format
    data.track_number
    data.per_quarter_note
  );
  for i = 0 to data.track_number - 1 do
    Buffer.add_string ret_str (
      pr "Track: [%8lX] len[%ld]\n"
      data.tracks.(i).track_id
      data.tracks.(i).track_length 
    );
    for j = 0 to (Array.length data.tracks.(i).events) -1 do
      Buffer.add_string ret_str (
        pr "  %s\n" (event_to_string data.tracks.(i).events.(j)) 
      );
    done
  done ;
  Buffer.contents ret_str ;
;;


