print( _VERSION  )
function Z()
  local function __z_bar(self) print "z" end
  return { foo = __z_bar }
end
z1, z2 = Z(), Z()
print( z1.foo, z2.foo )

-- As of Lua 5.2, two indistinguishable functions constructed
-- at the same call site may compare equal. earlier versions
-- always constructed a new closure everytime a function
-- expression was encountered.

-- This assertion passes on Lua 5.1 and fails on Lua 5.2.
-- That is expected behavior.
assert( Z().foo ~= Z().foo )

-- more info here:
-- http://lua.2524044.n2.nabble.com/questions-about-closure-construction-tp7647369p7647378.html
