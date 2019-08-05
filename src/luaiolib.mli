type 'a t = In of in_channel | Out of out_channel
val out :
    ('a t, 'b, 'b) Luavalue.ep ->
    ('b -> string -> out_channel) ->
    (out_channel, 'b, 'b) Luavalue.ep
val in' :
    ('a t, 'b, 'b) Luavalue.ep ->
    ('b -> string -> in_channel) ->
    (in_channel, 'b, 'b) Luavalue.ep

module T : Lua.Lib.USERTYPE  with type 'a t = 'a t
module Make (TV : Lua.Lib.TYPEVIEW with type 'a t = 'a t)
    : Lua.Lib.USERCODE with type 'a userdata' = 'a TV.combined
