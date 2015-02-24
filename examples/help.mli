val timeout : int option
val wait : unit -> unit Lwt.t
val clean : unit -> unit

val write_something: string -> 'a Lwt.t
