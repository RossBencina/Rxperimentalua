--------------------------
--[[
Test the performance difference between using a
"class" metatable, and copying function references
into each class. The latter has more memory overhead
assuming that the metatable can be shared between instances.
]]


-- A: direct copy methods into instance table
-- B: use a metatable
--[[
Lua 5.1.4

construction time
10000000        A1      10.649
10000000        B1      10.401
10000000        A3      13.563
10000000        B3      10.375
method invocation time
20000000        a1 foo  3.129
30000000        b1 foo  3.231
20000000        a3 foo  2.998
30000000        b3 foo  3.307

Lua 5.2.1

construction time
10000000        A1      8.969
10000000        B1      8.62
10000000        A3      11.729
10000000        B3      9.335
method invocation time
20000000        a1 foo  2.766
30000000        b1 foo  3.005
20000000        a3 foo  2.487
30000000        b3 foo  2.927
]]--

-- single method version
A1 = {
  foo = function( self )
    return self.x_ + 1
  end
}
function ClassA1( x )
  return { x_ = x, foo = A1.foo }
end
a1 = ClassA1(1)
print( a1:foo() )


B1 = {
  foo = function( self )
   return self.x_ + 1
  end
}
B1_mt = { __index = B1 }
function ClassB1( x )
  return setmetatable( { x_ = x }, B1_mt )
end
b1 = ClassB1(2)
print( b1:foo() )


A3 = {
  foo = function( self )
    return self.x_ + 1
  end,
  bar = function( self )
    return self.x_ + 2
  end,
  baz = function( self )
    return self.x_ + 3
  end
}
function ClassA3( x )
  return { x_ = x, foo = A3.foo, bar = A3.bar, baz = A3.baz }
end
a3 = ClassA3(1)
print( a3:foo() )
print( a3:bar() )
print( a3:baz() )

B3 = {
  foo = function( self )
    return self.x_ + 1
  end,
  bar = function( self )
    return self.x_ + 2
  end,
  baz = function( self )
    return self.x_ + 3
  end
}
B3_mt = { __index = B3 }
function ClassB3( x )
  return setmetatable( { x_ = x }, B3_mt )
end
b3 = ClassB3(2)
print( b3:foo() )
print( b3:bar() )
print( b3:baz() )


--------------------------

function timeIterations( label, f, n )
  collectgarbage()
  start = os.clock()
  s = 0
  for i=1, n do
    s = s + f()  
  end
  d = os.clock() - start
  print( s, label, d )
end

function timeIterationsAndCheckMemory( label, f, n )
  collectgarbage()
  memstart=collectgarbage("count")
  --collectgarbage("stop")
  start = os.clock()
  s = 0
  for i=1, n do
    s = s + f()
  end
  d = os.clock() - start
  md = collectgarbage("count") - memstart
  --collectgarbage("restart")
  print( s, label, d, md )
end


f = function(profile, iterations)
  print "construction time"
  profile( "A1", function() a=ClassA1(1); return 1 end, iterations )
  profile( "B1", function() b=ClassB1(2); return 1 end, iterations )
    
  profile( "A3", function() a=ClassA3(1); return 1 end, iterations )
  profile( "B3", function() b=ClassB3(2); return 1 end, iterations )
    
  print "method invocation time"

  a1=ClassA1(1)
  profile( "a1 foo", function() return a1:foo() end, iterations )

  b1=ClassB1(2)
  profile( "b1 foo", function() return b1:foo() end, iterations )
    
  a3=ClassA3(1)
  profile( "a3 foo", function() return a3:foo() end, iterations )

  b3=ClassB3(2)
  profile( "b3 foo", function() return b3:foo() end, iterations )
end
n = 100000
f( timeIterations, n )
f( timeIterationsAndCheckMemory, n )