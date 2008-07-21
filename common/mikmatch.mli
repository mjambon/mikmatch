(** A small text-oriented library *)

(** The [Mikmatch] module provides a submodule named [Text]. 
A normal usage is to place [open Mikmatch] at the beginning of 
user code that uses it. 

This module is part of the runtime environment of Mikmatch 
(the library run_mikmatch_pcre.cma or equivalent).
*)

module Text :
sig
  (** This module provides some general functions which are especially 
    useful for manipulating text and text files.
  *)

  val iter_lines_of_channel : (string -> unit) -> in_channel -> unit
    (** [iter_lines_of_channel f ic] reads input channel [ic]
      and applies successively the given function [f] to 
      each line until the end of file is reached. *)

  val iter_lines_of_file : (string -> unit) -> string -> unit
    (** [iter_lines_of_file f file] reads file [file]
      and applies successively the given function [f] to 
      each line until the end of file is reached. *)

  val lines_of_channel : in_channel -> string list
    (** [lines_of_channel ic] returns the list of the lines that can be 
      read from input channel [ic]. *)

  val lines_of_file : string -> string list
    (** [lines_of_file file] returns the list of the lines that can be 
      read from file [file]. *)

  val channel_contents : in_channel -> string
    (** [channel_contents ic] returns the string containing the bytes
      that can be read from the given input channel [ic]. *)

  val file_contents : ?bin:bool -> string -> string
    (** [file_contents file] returns the string containing the bytes
      that can be read from the given file.
      Option [bin] specifies if [Pervasives.open_in_bin] should be
      used instead of [Pervasives.open_in] to open the file. Default is
      [false]. *)

  val save : string -> string -> unit
    (** [save file data] stores the string [data] in [file].
      If the file already exists, its contents is discarded silently. *)

  val save_lines : string -> string list -> unit
    (** [save_lines file l] saves the given list [l] of strings in [file]
      and adds a newline characters (['\n']) after each of them.
      If the file already exists, its contents is discarded silently. *)


  exception Skip
    (** This exception can be used to skip an element of a list being 
      processed with [rev_map], [map], [fold_left], and [fold_right]. *)

  val map : ('a -> 'b) -> 'a list -> 'b list
    (** Like [List.map] but it is guaranteed that 
      the elements of the input list are processed from left to right.
      Moreover the [Skip] exception can be used to skip an element
      of the list.
      This function runs in constant stack space. *)

  val rev_map : ('a -> 'b) -> 'a list -> 'b list
    (** Like [List.rev_map], but it is guaranteed that 
      the elements of the input list are processed from left to right.
      Moreover the [Skip] exception can be used to skip an element
      of the list.
      This function runs in constant stack space and is slightly faster
      then [map]. *)

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    (** Like [List.fold_left] 
      but the [Skip] exception can be used to skip an element
      of the list.
      This function runs in constant stack space. *)

  val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
    (** Like [List.fold_right] 
      but the [Skip] exception can be used to skip an element
      of the list.
      This function runs in constant stack space. *)

  val map_lines_of_channel : (string -> 'a) -> in_channel -> 'a list
    (** [map_lines_of_channel f ic] is equivalent to
      [map f (lines_of_channel ic)] but faster. *)

  val map_lines_of_file : (string -> 'a) -> string -> 'a list
    (** [map_lines_of_file f file] is equivalent to
      [map f (lines_of_file file)] but faster. *)

end


module Fixed :
sig
  (** This module provides some functions which are useful 
    for manipulating files with fields of fixed width.
  *)

  val chop_spaces : string -> string
    (** [chop_spaces s] returns a string where the leading and trailing
      spaces are removed. *)

  val int : string -> int
    (** [int s] reads an int from a string where leading and
      trailing spaces are allowed. 
      Equivalent to [Pervasives.int_of_string (chop_spaces s)]. *)

  val float : string -> float
    (** [float s] reads an float from a string where leading and
      trailing spaces are allowed.
      Equivalent to [Pervasives.float_of_string (chop_spaces s)]. *)
end

module Directory : 
sig 
  (** Basic operations on directories *)

  val list : ?absolute:bool -> ?path:bool -> string -> string list 
    (** [list dir] returns the alphabetically sorted list 
      of the names of the files contained in directory [dir]. 
      The special names that refer to the parent directory (e.g. [..])
      and the directory itself (e.g. [.]) are ignored.

      If the option [absolute] is set to [true], the result is a list
      of absolute file paths, i.e. that do not depend on the current directory
      which is associated to the process 
      (default is false; implies [path = true]).

      If the option [path] is set to [true], the result is a list of paths
      instead of just the file names
      (default is [false] except if [absolute] is explicitely set to [true]).

      Exception [Invalid_argument "Directory.list"] is raised 
      if there is an incompatibility between the options.
      Unspecified exceptions will be raised if the given directory does not 
      exist or is not readable.
    *)

  val is_dir : ?nofollow:bool -> string -> bool
  (** [is_dir dir] returns true if [dir] is a directory, false otherwise.
    The [nofollow] option is false by default, but if true, 
    a symbolic link will not be followed. In that case false is returned
    even if the link points to a valid directory. *)
end

module Glob :
sig
  (** A generic file path matching utility *)

  val scan : 
    ?absolute:bool ->
    ?path:bool ->
    ?root:string -> 
    ?nofollow:bool ->
    (string -> unit) -> (string -> bool) list -> unit
  (** [scan action path_filter] returns all the file paths having a name
    that matches [path_filter]. [path_filter] is a list of filters that
    test whether a directory name or a file name should be selected.

    The path search starts from the current directory by default, or 
    from the directory specified by the [root] option. The file names 
    are examined in an undefined order. When a file path matches,
    [action] is applied to the string representing the path.
    Options [absolute] and [path] have the same meaning and the same 
    default values as in {!Mikmatch.Directory.list}.

    [nofollow] can be used to prevent from considering symbolic links 
    as directories. It is false by default. 
    See also {!Mikmatch.Directory.is_dir}.
  *)

  val lscan :
    ?rev:bool ->
    ?absolute:bool ->
    ?path:bool ->
    ?root:string list ->
    ?nofollow:bool ->
    (string list -> unit) -> (string -> bool) list -> unit
  (** Same as {!Mikmatch.Glob.scan} but file paths are kept as a list
    of strings that form a valid path when concatenated using
    [Filename.concat]. Option [rev] can be set if the lists representing
    paths are in reversed order, i.e. the root comes last.

    In [lscan action path_filter], options [rev], [absolute], and [path]
    take their default values which are all false.
    In this situation, it is guaranteed that the paths that are passed
    to [action] have the same length as [path_filter].
  *)

  val list :
    ?absolute:bool ->
    ?path:bool ->
    ?root:string -> 
    ?nofollow:bool -> 
    ?sort:bool -> 
    (string -> bool) list -> string list
    (** [list path_filter] works like {!Mikmatch.Glob.scan} but returns a list
      of all file paths that match [path_filter].

    An example in Mikmatch syntax is [list [FILTER _* ".ml" eos]].
    It returns the list of ".ml" files in the current directory.
    It could have been written as 
    [list [ fun s -> Filename.check_suffix s ".ml"]] and is equivalent
    to [*.ml] in shell syntax.
 *)

  val llist :
    ?rev:bool ->
    ?absolute:bool ->
    ?path:bool ->
    ?root:string list ->
    ?nofollow:bool ->
    ?sort:bool -> (string -> bool) list -> string list list
    (** [llist path_filter] works like {!Mikmatch.Glob.lscan} 
      but returns a list
      of all file paths that match [path_filter]. *)
end
