
--[[
Are all of the cases below different?

Or to some achieve the same result? 

When does a function definition act?


If a function doesn't capture variables from the 
surrounding scope is it processed as-if it was declared 
once globally? (ie does ClassY() and ClassZ() below
cause exactly the same code to be generated, or does
the function get computed every time.
]]--

function X() 
  return { foo = function() return "x" end } 
end

function __y_bar() return "y" end
function Y() 
  return { foo = __y_bar } 
end

function Z()
  local function __z_bar() return "z" end
  return { foo = __z_bar } 
end

X():foo(); Y():foo(); Z():foo()



-- "Classes" with no state

------------------------------
-- function defined inline in every call to constructor
function ClassX()
  return { foo = function() return "x" end }
end
x = ClassX()
x:foo()
------------------------------
-- function defined globally
function _Y_foo() return "y" end
function ClassY()
  return { foo = _Y_foo }
end
y = ClassY()
y:foo()
------------------------------
-- function defined globally
function ClassZ()
  local function _Z_foo() return "z" end
  return { foo = _Z_foo }
end
z = ClassZ()
z:foo()
------------------------------

-- Classes with instance state:

------------------------------
-- state captured in closure
function ClassA( x )
  return { foo = function() return x + 1 end }
end
a = ClassA(1)
print( a:foo() )
------------------------------
-- state stored in object field, 
-- function defined inline in every call to constructor
function ClassB( x )
  return { x_ = x, 
    foo = function(self) return self.x_ + 1 end }
end
b = ClassB(1)
print( b:foo() )
------------------------------
-- state stored in object field, 
-- function defined globally
function _C_foo( self )
  return self.x_ + 1
end
function ClassC( x )
  return { x_ = x, foo = _C_foo }
end
c = ClassC(1)
print( c:foo() )
------------------------------
-- state stored in object field, 
-- function defined locally in constructor
function ClassD( x )
  local function _D_foo( self ) return self.x_ + 1 end
  return { x_ = x, foo = _D_foo }
end
d = ClassD(1)
print( d:foo() )
------------------------------
-- state stored in object field, 
-- function defined globally
_E_global = {
_E_foo = function( self )
  return self.x_ + 1
end
}
function ClassE( x )
  return { x_ = x, foo = _E_global._E_foo }
end
e = ClassE(1)
print( e:foo() )
------------------------------
-- state stored in object field, 
-- function defined in metatable
F = {
foo = function( self )
  return self.x_ + 1
end
}
F_mt = { __index = F }
function ClassF( x )
  return setmetatable( { x_ = x }, F_mt )
end
f = ClassF(1)
print( f:foo() )
------------------------------
-- state stored in object field, 
-- function defined globally
-- function stored in array portion
_G_global = {
_G_foo = function( self )
  return self.x_ + 1
end
}
function ClassG( x )
  return { _G_global._G_foo, x_ = x  }
end
g = ClassG(1)
print( g[1](g) )
------------------------------

function timeIterations( label, f, n )
  collectgarbage()
  --memstart=collectgarbage("stop")
  --memstart=collectgarbage("count")
  start = os.clock()
  for i=1, n do
    f()  
  end
  d = os.clock() - start
  --md = collectgarbage("count") - memstart
  --memstart=collectgarbage("restart")
  print( label, d )
end

iterations = 100000000

timeIterations( "b", function() b=ClassB(10) end, iterations )
timeIterations( "d", function() d=ClassD(10) end, iterations )

b=ClassB(10)
timeIterations( "b foo", function() b:foo() end, iterations )
d=ClassD(10)
timeIterations( "d foo", function() d:foo() end, iterations )
  
--[[
  
print "construction time"
timeIterations( "x", function() x=ClassX() end, iterations )
timeIterations( "y", function() y=ClassY() end, iterations )
timeIterations( "z", function() z=ClassZ() end, iterations )

timeIterations( "a", function() a=ClassA(10) end, iterations )
timeIterations( "b", function() b=ClassB(10) end, iterations )
timeIterations( "c", function() c=ClassC(10) end, iterations )
timeIterations( "d", function() d=ClassD(10) end, iterations )
timeIterations( "e", function() e=ClassE(10) end, iterations )
timeIterations( "f", function() f=ClassE(10) end, iterations )
timeIterations( "g", function() g=ClassG(10) end, iterations )
  
print "method invocation time"

x=ClassX()
timeIterations( "x foo", function() x:foo() end, iterations )

y=ClassY()
timeIterations( "y foo", function() y:foo() end, iterations )

z=ClassZ()
timeIterations( "z foo", function() z:foo() end, iterations )

a=ClassA(10)
timeIterations( "a foo", function() a:foo() end, iterations )

b=ClassB(10)
timeIterations( "b foo", function() b:foo() end, iterations )

c=ClassC(10)
timeIterations( "c foo", function() c:foo() end, iterations )

d=ClassD(10)
timeIterations( "d foo", function() d:foo() end, iterations )

e=ClassE(10)
timeIterations( "e foo", function() e:foo() end, iterations )
  
f=ClassF(10)
timeIterations( "f foo", function() f:foo() end, iterations )
  
g=ClassG(10)
timeIterations( "g [1](g)", function() g[1](g) end, iterations )
  
]]--