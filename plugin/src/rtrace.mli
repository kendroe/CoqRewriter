(******************************************************************************
 *        
 * REWRITELIB                        
 *                                   
 * trace.mli
 *                               
 * This file contains the code for contextual rewriting. 
 *                               
 * (C) 2017, Kenneth Roe      
 *  
 * All rights reserved--This is an incomplete work.  An appropriate license
 * will be provided when the work is complete.
 *
 * For a commercial license, contact Roe Mobile Development, LLC at
 * info@roemobiledevelopment.com
 *  
 *****************************************************************************)
   
val indent: unit -> unit ;;
val undent: unit -> unit ;;
val setBlocks: (string list) -> unit ;;
val trace: string -> (unit -> string) -> unit ;;
val trace_list: string -> (unit -> string list) -> unit ;;
val toggle_trace: unit -> unit ;;
val is_trace_on: unit -> bool ;;




