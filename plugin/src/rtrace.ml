(******************************************************************************
 *        
 * REWRITELIB                        
 *                                   
 * trace.ml                       
 *                               
 * This file contains hooks for tracing. 
 *                               
 * (C) 2017, Kenneth Roe      
 *  
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 * will be provided when the work is complete.
 *
 * For a commercial license, contact Roe Mobile Development, LLC at
 * info@roemobiledevelopment.com
 *  
 *****************************************************************************)

(* require "trace-s.sml" ;  *)
(* require "list.sml" ;  *)
(* require "basis.__text_io" ;  *)

(* open listimpl ; *)
(* open TextIO ; *)

 let f = ref (open_out "trace.out") ;;
 let _ = close_out (!f) ;;

 let trace_on = ref false ;;

 let is_trace_on _ = (!trace_on) ;;

 let toggle_trace () =
     (if !trace_on then
         (close_out (!f) ;
          trace_on := false)
     else
         (f := open_out "trace.out" ;
          trace_on := true) ; ());;

(*val blocks = ref ["rewriteRule","rewriteRuleDisc","Disc","match","rewrite","derive"] ;*)
let blocks = ref ["match";"rewriteRuleDisc";"rewriteRule";"kbrewrite";"rewrite";"derive";"kbrewrite"] ;;
(*val blocks = ref ["env"] ;*)

let setBlocks bl = ((blocks := bl) ; ()) ;;

let ind = ref 0 ;;

let indent () = (ind := (!ind)+2) ;;
let undent () = ((ind := (!ind)-2) ;
                  if !trace_on then
                      ((close_out (!f)) ;
                       (f := open_out_gen [Open_append] 0o666 "trace.out"))
                  else
                      ()) ;;

let rec indent_line n = match n with
   | 0 -> ()
   | n ->
     if n > 0 then
         (output_string (!f) " " ; flush (!f) ;
          indent_line (n-1))
     else () ;;

let trace x s =
     if !trace_on && List.mem x (!blocks) then
         (indent_line (!ind) ;
          (*print_string ((s ()) ^ "\n") ; flush stdout ;*)
          output_string (!f) ((s ()) ^ "\n") ; flush (!f)) else () ;;

let trace_list x s =
     if List.mem x (!blocks) && (!trace_on) then
         (List.map
             (fun (x) ->
                 (indent_line (!ind) ;
                  (*print_string (x ^ "\n") ; flush stdout ;*)
                  output_string (!f) (x ^ "\n"))) (s ()) ; ()
     )
     else () ;;



