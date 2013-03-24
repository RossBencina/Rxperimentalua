-- basic test/demo of different ways to construct closures

r = { foo = function(self) end }
j = { foo = function(self) end }
   
function X()
  return { foo = function(self) end }
end

function Y()
  local f = function(self) end
  return { foo = f }
end

local f_ = function(self) end
function Z()
  return { foo = f_ }
end

  
print(r.foo)
print(j.foo)

print( X().foo )
print( X().foo )
print( X().foo )

print( Y().foo )
print( Y().foo )
print( Y().foo )

print( Z().foo )
print( Z().foo )
print( Z().foo )