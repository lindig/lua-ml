module Make (TV : Lua.Lib.TYPEVIEW with type 'a t = 'a Luaiolib.t)
    : Lua.Lib.USERCODE with type 'a userdata' = 'a TV.combined
