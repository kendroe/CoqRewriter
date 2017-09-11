(******************************************************************************
 *                                   
 * REWRITELIB                        
 *                                   
 * getfile.mli                        
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
                                     
val getfile: string -> string list ;;
val getcfile: string -> string list ;;
val is_prefix: string -> string -> bool ;;
val after_space: string -> string ;;
val before_space: string -> string ;;
val after_equal: string -> string ;;
val before_equal: string -> string ;;
val after_comma: string -> string ;;
val before_comma: string -> string ;;
val decode_integer: string -> int ;;
val append_text: string list -> string ;;




