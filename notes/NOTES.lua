--[[
  principle: avoid introducing differences between .NET Rx and this implementation
  
  plan: initially, just implement operations on IObservable/IObserver. Don't attempt IEnumerable bridging
  
  assumption: assume all event streams are asynchronous/non-blocking (ie combinators like Amb don't need to spawn threads to work correctly)
]]--

--[[
Related and interesting projects:

"""raix (Reactive And Interactive eXtensions) is a functional, composable, API for AS3 that simplifies working with data, regardless of whether its interactive (arrays) or reactive (events).""
https://github.com/richardszalay/raix/wiki
https://github.com/richardszalay/raix/wiki/Reactive-Operators
]]--


 --[[
In the most general sense, the combinator functions have the following form:

IO<T> f( IO<T> )

function f( observable, params )
  local O = implements_IObservable()
  function O:init( source, params ) self.source_, self.params_ = source, params end
  function O:subscribe( observer ) 

  return o;
end

see experiments/implementation_strategies_exploration_1.lua


http://stackoverflow.com/questions/1596158/good-introduction-to-the-net-reactive-framework

Differences Between Versions of Rx
http://msdn.microsoft.com/en-us/library/hh242987(v=vs.103).aspx
]]--

