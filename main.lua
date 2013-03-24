------------------------------------------------------------------------------
-- Rxperimentalua Lua Rx experiments
-- Copyright (c) 2013 Ross Bencina <rossb@audiomulch.com>
-- Licensed under the Apache License, Version 2.0
-- http://www.apache.org/licenses/LICENSE-2.0
------------------------------------------------------------------------------
--
-- A little toy experiment in implementing the basic Rx (reactive extensions) mechanisms in Lua
-- Missing a vast number of combinators.
-- Based on the explanation in these two videos by Erik Meijer and Wes Dyer:
-- http://channel9.msdn.com/Shows/Going+Deep/E2E-Erik-Meijer-and-Wes-Dyer-Reactive-Framework-Rx-Under-the-Hood-1-of-2
-- http://channel9.msdn.com/Shows/Going+Deep/E2E-Erik-Meijer-and-Wes-Dyer-Reactive-Framework-Rx-Under-the-Hood-2-of-2
--
-- And the excellent descriptions here: http://www.introtorx.com
-- 
-- Please see rx_protocol_explanation.lua for an explanation of the ideas
-- and protocols and for more reading and video links.
------------------------------------------------------------------------------
ml = require 'ml'.import() -- https://github.com/stevedonovan/Microlight
------------------------------------------------------------------------------
function nonOperativeDocumentation()
  -- This section is non-operative code for documentation purposes only
  -- everything is redefined below.
  -- the Rx core interfaces, only the interfaces, to make things clear

  -- IDisposable Interface http://msdn.microsoft.com/en-us/library/system.idisposable.aspx
  -- In this Lua implementation, Disposables can be any table with an :dispose() method
  -- There is no requirement to derive from a base class.
  local IDisposable = {}
  -- detach a previous subscription. only the first call should have an effect
  function IDisposable:dispose()
  end

  -- IObservable<T> Interface http://msdn.microsoft.com/en-us/library/dd990377(v=vs.103).aspx
  -- Derived classes must implement :subscribe( observer )
  -- Defines combinators that have a self/this argument.
  -- In this Lua implementation, all Observables must derive from 
  -- IObservable to get the combinator extension methods in the base class.
  -- construct a subclass of IObservable using subclass_IObservable()
  local function subclass_IObservable() end
  local obs = subclass_IObservable()

  local IObservable = {}
  -- attach observer to event stream
  function IObservable:subscribe( observer ) -- implement this in subclasses
    return IDisposable() -- disposing disposable must unsubscribe observers
  end

  -- Observable class http://msdn.microsoft.com/en-us/library/system.reactive.linq.observable(v=vs.103).aspx
  -- Defines combinators that don't have a self/this argument.
  local Observable = {}

  -- IObserver<T> Interface http://msdn.microsoft.com/en-us/library/dd783449.aspx
  -- In this Lua implementation, Observers must implement these three methods. 
  -- There is no requirement to derive from a base class although the implementation
  -- might do.
  local IObserver = {}
  function IObserver:onNext(value)
  end
  function IObserver:onError(e)
    -- once onError is called that's the end of the event stream
    -- terminal observers should detach after recieving onError()
  end
  function IObserver:onCompleted()
    -- once onError is called that's the end of the event stream
    -- terminal observers should detach after recieving onCompleted()
  end
end

------------------------------------------------------------------------------
--
-- IObservable
--
-- Implementors of IObservable should construct concrete classes using
-- implements_IObservable(). You must implement :subscribe(o) and you can
-- implement :_init(...)
-- 
-- It is not possible to create instances of IObservable but it contains
-- a bunch of member method combinators that are inherited by subclasses.
-- You can add methods to IObservable at any time and subclasses will
-- have access to them.

IObservable = {}

function IObservable:subscribe( observer ) -- implement this in subclasses
    error( "some IObservable subclass didn't implement subscribe()" )
end

-- implements_IObservable() constructs subclasses of IObservable 
-- based on http://lua-users.org/wiki/InheritanceTutorial and ml.class()
-- Can't use ml.class() because we want to be able to extend base after
-- defining a derived class.
function implements_IObservable()
  local new_subclass = {}
  local subclass_mt = { __index = new_subclass }
  setmetatable( new_subclass, { 
      __index = IObservable,
      __call = function( subclass, ... )
        local obj = setmetatable( {}, subclass_mt )
        if rawget(subclass,'_init') then
          subclass._init(obj,...) -- call our constructor
        end
        return obj
      end      
      } )
  return new_subclass
end

function test_implements_IObservable()
  TestObservable = implements_IObservable() -- construct a new type
  function TestObservable:_init( x ) self.x_ = x end -- define a constructor
  function TestObservable:subscribe(o) assert( o == 15 ) end -- override a base method
  x = TestObservable( 1234 )
  assert( x.x_ == 1234 ) -- test that our constructor was called
  x:subscribe( 15 ) -- test that our override gets called
  
  -- test that we can define a method in IObservable and it is available in derived class
  function IObservable:foo() assert(true) end
  x:foo()
  IObservable.foo = nil
  --x:foo()
end
--test_implements_IObservable()

------------------------------------------------------------------------------

_NoOpDisposable = {
  dispose = function( self ) end
}

------------------------------------------------------------------------------

-- an identity observer does nothing. it passes all events through
-- useful when you only need to overload some of the methods

_IdentityObserver = ml.class()
function _IdentityObserver:_init( sinkObserver )
  self.sinkObserver_ = sinkObserver
end
function _IdentityObserver:onNext( value ) 
  self.sinkObserver_:onNext( value )
end
function _IdentityObserver:onError( e )
  self.sinkObserver_:onError(e)
end
function _IdentityObserver:onCompleted()
  self.sinkObserver_:onCompleted()
end

------------------------------------------------------------------------------

Observable = {}

------------------------------------------------------------------------------
-- Simple factory methods: return_, empty, never, throw, create

function Observable.return_( value )
  return Observable.createWithSubscribeMethod_( 
      function( self, observer ) 
        observer:onNext(value)
        observer:onCompleted()
        return _NoOpDisposable
      end );
end

function Observable.empty()
  return Observable.createWithSubscribeMethod_( 
      function( self, observer ) 
        observer:onCompleted()
        return _NoOpDisposable
      end );
end

function Observable.never()
  return Observable.createWithSubscribeMethod_( 
      function( self, observer )
        return _NoOpDisposable
      end );
end

function Observable.throw( e )
  return Observable.createWithSubscribeMethod_( 
      function( self, observer ) 
        observer:onError(e)
        return _NoOpDisposable
      end );
end

function Observable.create( subscribeFn )
  return _CreateObservable( subscribeFn )
end

_CreateObservable = implements_IObservable()
function _CreateObservable:_init( subscribeFn )
  self.subscribeFn_ = subscribeFn
end
function _CreateObservable:subscribe( observer )
  return self.subscribeFn( observer );
end

-- createWithSubscribeMethod_ subscribeMethod takes a self parameter and an observer
-- avoids additional indirection of create()

function Observable.createWithSubscribeMethod_( subscribeMethod )
  return _CreateObservableWithSubscribeMethod( subscribeMethod )
end

_CreateObservableWithSubscribeMethod = implements_IObservable()
function _CreateObservableWithSubscribeMethod:_init( subscribeMethod )
  self.subscribe = subscribeMethod
end

------------------------------------------------------------------------------
-- Generate and range
-- TODO generate needs work

function Observable.generate( initialState, conditionFn, iterateFn, resultSelectorFn )
  return _GenerateObservable( initialState, conditionFn, iterateFn, resultSelectorFn )
end

_GenerateObservable = implements_IObservable()
function _GenerateObservable:_init( initialState, conditionFn, iterateFn, resultSelectorFn )
  self.state_ = initialState
  self.conditionFn_ = conditionFn
  self.iterateFn_ = iterateFn
  self.resultSelectorFn_ = resultSelectorFn
  self.observers_ = ml.Array();
end
function _GenerateObservable:subscribe( observer )
  -- FIXME TODO: i'm not exactly sure but I assume that if observers are added and removed during
  -- notifications we should just invoke them next time around, not on the current iteration
  
  while self.conditionFn_( self.state_ ) do
    
    -- notify secondary observers here, accumulate new subscriptions
    
    observer:onNext( self.resultSelectorFn_( self.state_ ) )
    self.state_ = self.iterateFn_( self.state_ )  
  end
  
  observer:onCompleted()  
  return _NoOpDisposable
end

function Observable.range( start, count )
  return Observable.generate( 0,
      function( value ) return value < count end,
      function( value ) return value + 1 end,
      function( value ) return start + value end )  
end

------------------------------------------------------------------------------
-- IObservable combinators (factory methods on IObservable)
-- these are declared first because of the way ml.class implements inheritance 
-- it "bakes in" the methods in subclasses when they're declared.
-- that's probably not what we want
-- ideally we want a base class implementation that can do "extension methods": methods
-- added to the base class after the derived classes have been constructed

-- select( f ) -- apply function f to each value

function IObservable:select( f ) -- return an IObservable that applies the function f to each element
  local f = ml.function_arg(f)
  return _OnNextLambdaObserverFactory( self,
      function( sink, value ) sink:onNext(f(value)) end
    );
end

-- where( predicate ) -- filter values based on predicate

function IObservable:where( predicate )
  local p = ml.function_arg(predicate)
  return _OnNextLambdaObserverFactory( self,
      function( sink, value ) if p(value) then sink:onNext(value) end end
    );
    
  --[[
  -- alternate forms:
  
  -- pass a constructor to _ParameterlessGenericObserverFactory,
  -- binding the lambda in to _OnNextLambdaObserver on the way in
  return _ParameterlessGenericObserverFactory( self, bind2(_OnNextLambdaObserver, 
      function( sink, value ) if p(value) then sink:onNext(value) end end ) )
    
  -- as previous example but using XXX, which isn't needed because there are
  -- no additional ctor parameters
  return _GenericObserverFactory( self, bind2(_OnNextLambdaObserver, 
      function( sink, value ) if p(value) then sink:onNext(value) end end ) )
    
  -- use a separately defined observer object (below) with the predicate stored
  -- as an instance variable of the observer. Use _GenericObserverFactory to 
  -- store the predicate in the factory and pass it to the observer ctor
  -- this is the most straight forward OO way of doing things but results in a lot of boiler plate
  return _GenericObserverFactory( self, _WhereObserver, p )
  
  -- use a separately defined observable constructor
  return _WhereObservable( self, p )
  ]]--
end


------------------------------------------------------------------------------

-- IO<T> Flatten<T>( IO<IO<T>> ) // ("join")
-- listens to all incoming observables, subscribes to each one, and outputs all incoming values
function IObservable:flatten()
  return _FlattenObservable( self )
end

-- IO<U> SelectMany<T,U>( IO<T>, Func<T,IO<U>> ) // ("bind")
function IObservable:selectMany( f )
  return self:select( f ):flatten()
end

-- attempt to implement flatten based on
-- http://channel9.msdn.com/Shows/Going+Deep/E2E-Erik-Meijer-and-Wes-Dyer-Reactive-Framework-Rx-Under-the-Hood-2-of-2
-- provides an illustration that Rx is formally underpecified in their docs (I'm sure they know what they're doing though :)

_FlattenSubstreamObserver = ml.class()
function _FlattenSubstreamObserver:_init( parent, sinkObserver )
  self.sinkObserver_ = sinkObserver
  self.parent_ = parent;
end
function _FlattenSubstreamObserver:onNext( value )
  -- pass values to sink
  self.sinkObserver_:onNext( value )
end
function _FlattenSubstreamObserver:onError( e )
  -- dispose all parent's substreams, pass the error to sink
  -- we do this by delegating to parent's onError method:
  self.parent_:onError( e )
end
function _FlattenSubstreamObserver:onCompleted()
  -- when a substream completes, we detach and get garbage collected
  -- we want other substreams to keep delivering events to the sink
  -- so we don't send onCompleted() to the sink unless we're the last
  -- substream and the parent's source has completed
  if self.attachment_ then self.attachment_:dispose() end
  self.parent_.childStreams_:remove(self.parent_.childStreams_:indexof(self))
  self.hasCompleted_ = true
  
  -- if we are the last subtream and the parent has completed
  -- notify the sink that we've complete
  if #self.parent_.childStreams_ == 0 and self.parent_.hasCompleted_ then
    self.sinkObserver_:onCompleted()
  end
end

-------

_FlattenObserver = ml.class() -- observers IObservable<IObservable<T>>
function _FlattenObserver:_init( sinkObserver )
  self.sinkObserver_ = sinkObserver
  self.hasCompleted_ = false
  self.childStreams_ = ml.Array()
end
function _FlattenObserver:onNext( value )
  -- value is an IObservable<T>. let's call them substreams
  
  -- route all values emitted by value to sink observer
  substreamObserver = _FlattenSubstreamObserver(self, self.sinkObserver_)
  self.childStreams_:append(substreamObserver)
  local a = value:subscribe( substreamObserver )
  -- test for case where observer completes synchronously
  if substreamObserver.hasCompleted_ then
    a:dispose()
  else
    substreamObserver.attachment_  = a
  end
end
function _FlattenObserver:onError( e )
  self.hasCompleted_ = true
  
  -- detach all child streams and emit the error
  
  for i = 1,#self.childStreams_ do
    self.childStreams_[i].attachment_:dispose()
  end
  self.childStreams_ = nil
  self.sinkObserver_:onError( e )
end
function _FlattenObserver:onCompleted()
  -- when the source stream completes, there will be no more new substreams to process
  -- but the existing substreams should continue to deliver to client if they exist
  self.hasCompleted_ = true
  if #self.childStreams_ == 0 then
    self.sinkObserver_:onCompleted()
  end
end

_FlattenObservable = implements_IObservable()
function _FlattenObservable:_init( source )
  self.source_ = source
end
function _FlattenObservable:subscribe( observer )
  return self.source_:subscribe( _FlattenObserver( observer ) )
end

------------------------------------------------------------------------------
-- generic tools for constructing observers and observables

--[[
-- _GenericStatelessObserverFactory is an Observable
-- Many observables act as stateless factories for a particular observer type.
-- this class can create such observables. 
-- It stores the observer's constructor function
-- and the parameters that will be passed to the constructor

_GenericObserverFactory = implements_IObservable()
function _GenericObserverFactory:_init( sourceObservable, constructObserverFn, ... ) -- ... are the observer ctor function parameters
  self.sourceObservable = sourceObservable
  self.constructObserverFn = constructObserverFn
  self.constructObserverParams = {...}
end
function _GenericObserverFactory:subscribe( observer )
  return self.sourceObservable:subscribe( self.constructObserverFn( observer, unpack(self.constructObserverParams) ) )
end

-- when the observer has no constructor parameters (aside from its source)
-- you can use this one. Useful for use with _OnNextLambdaObserver which
-- bakes parameters into lambdas instead of storing them in the class table

_ParameterlessGenericObserverFactory = implements_IObservable()
function _ParameterlessGenericObserverFactory:_init( sourceObservable, constructObserverFn )
  self.sourceObservable = sourceObservable
  self.constructObserverFn = constructObserverFn
end
function _ParameterlessGenericObserverFactory:subscribe( observer )
  return self.sourceObservable:subscribe( self.constructObserverFn( observer ) )
end
]]

------------------------------------------------------------------------------

_OnNextLambdaObserverFactory = implements_IObservable()
function _OnNextLambdaObserverFactory:_init( sourceObservable, onNextFn )
  self.sourceObservable = sourceObservable
  self.onNextFn = onNextFn
end
function _OnNextLambdaObserverFactory:subscribe( observer )
  return self.sourceObservable:subscribe( _OnNextLambdaObserver( observer, self.onNextFn ) )
end

-- use _OnNextLambdaObserver to construct observers by passing a lambda for onNext

_OnNextLambdaObserver = ml.class(_IdentityObserver)
function _OnNextLambdaObserver:_init( sinkObserver, onNextFn ) -- onNextFn takes 2 args: a sink oberver and the value
  self:super( sinkObserver ) -- inits self.sinkObserver_
  self.onNextFn_ = onNextFn
end
function _OnNextLambdaObserver:onNext( value ) 
  self.onNextFn_( self.sinkObserver_, value )
end

------------------------------------------------------------------------------
-- IObservable combinators (implementations)
-- the IObservable constructor methods are above

--[[
-- class-based combinators not currently used. use lambdas above instead

-- select( f ) -- apply function f to each value

_SelectObserver = ml.class(_IdentityObserver)
function _SelectObserver:_init( sinkObserver, f )
  self:super( sinkObserver ) -- inits self.sinkObserver_
  self.f_ = f
end
function _SelectObserver:onNext( value ) 
  self.sinkObserver_:onNext( self.f_( value ) )
end

-- where( predicate ) -- only pass the values where predicate returns true

_WhereObserver = ml.class(_IdentityObserver)
function _WhereObserver:_init( sinkObserver, predicate )
  self:super( sinkObserver ) -- inits self.sinkObserver_
  self.predicate_ = predicate
end
function _WhereObserver:onNext( value ) 
  if self.predicate_(value) then
    self.sinkObserver_:onNext( value )
  end
end
]]--


--[[
-- for reference, this is what an observer and observable written without the genertic mechanisms look like:
_WhereObserver = ml.class(IdentityObserver)
function _WhereObserver:_init( sinkObserver, predicate )
  self:super( sinkObserver ) -- inits self.sinkObserver_
  self.predicate_ = predicate    
end
function _WhereObserver:onNext( value )
  if self.predicate_(value) then
    self.sinkObserver_:onNext( value )
  end
end
function _WhereObserver:onError( e )
  self.sinkObserver_:onError(e)
end
function _WhereObserver:onCompleted()
  self.sinkObserver_:onCompleted()
end

_WhereObservable = implements_IObservable()
function _WhereObservable:_init( source, predicate )
  self.source_ = source
  self.predicate_ = predicate
end
function _WhereObservable:subscribe( observer )
  return self.source_:subscribe( _WhereObserver( observer, self.predicate_ ) )
end
]]--

------------------------------------------------------------------------------

-- dump( prefix ) -- debugging, prints events prefixed by prefix

function IObservable:dump( prefix )
    return self:subscribe( DumpObserver(prefix) )
end

-- DumpObserver observer that prints the events
DumpObserver = ml.class()
function DumpObserver:_init( prefix )
  self.prefix_ = prefix
end
function DumpObserver:onNext(value)
  print( self.prefix_, "-->", value )
end
function DumpObserver:onError(e)
  print( self.prefix_, "failed-->", e )
end
function DumpObserver:onCompleted()
  print( self.prefix_, "completed." )
end


------------------------------------------------------------------------------
-- internal utilities

-- FIXME TODO i suspect that this implementation doesn't support 
-- attaching/detaching observers during notification (?)

_RemoveTableItemDisposable = ml.class()
function _RemoveTableItemDisposable:_init( table, item )
  self.table_ = table
  self.item_ = item
end
function _RemoveTableItemDisposable:dispose()
  self.table_:remove(self.table_:indexof(self.item_))
end

_ObserverCollection = ml.class()
function _ObserverCollection:_init()
  self.observers_ = ml.Array()
end

function _ObserverCollection:attach( observer ) -- returns an IDisposable that removes observer
  self.observers_:append(observer)
  return _RemoveTableItemDisposable(self.observers_, observer)
end

function _ObserverCollection:notifyOnNext( value )
  for i = 1,#self.observers_ do self.observers_[i]:onNext(value) end
end

function _ObserverCollection:notifyOnCompletedAndDropObservers( value )
  local oldObservers = self.observers_ -- collected at end of function
  self.observers_ = ml.Array() -- for new observers added during onCompleted, or later subscriptions
  for i = 1,#oldObservers do oldObservers[i]:onCompleted() end
end

function _ObserverCollection:notifyOnErrorAndDropObservers( e )
  local oldObservers = self.observers_ -- collected at end of function
  self.observers_ = ml.Array() -- for new observers added during onCompleted, or later subscriptions
  for i = 1,#oldObservers do oldObservers[i]:onError(e) end
end

------------------------------------------------------------------------------
-- a simple observable that outputs each item in an array when emitArrayItems() is called
-- this is a bit like ISubject but I've implemented it just for testing. i don't fully
-- understand ISubject yet

MyEmittingObservable = implements_IObservable(); -- inheriting from IObservable is desirable because that gives us all the combinators

function MyEmittingObservable:_init()
  self.observers_ = _ObserverCollection()
end

function MyEmittingObservable:subscribe( observer ) -- IObservable:subscribe(observer)
  return self.observers_:attach(observer);
end

function MyEmittingObservable:emitValue( v )
  self.observers_:notifyOnNext( v )
end

function MyEmittingObservable:emitArrayItems( t )
  for i = 1,#t do
    self.observers_:notifyOnNext( t[i] )
  end
end

function MyEmittingObservable:complete()
    self.observers_:notifyOnCompletedAndDropObservers()
end
  
function MyEmittingObservable:error( e )
  for i = 1,#t do
    self.observers_:notifyOnErrorAndDropObservers( t[i] )
  end
end

------------------------------------------------------------------------------

function test()

-- test MyArrayEmittingObservable

o1 = MyEmittingObservable()

-- multiple observers printing the same values

d1 = o1:subscribe( DumpObserver('a1') )
d2 = o1:dump( 'a2' )

--d2:dispose() -- FIXME doesn't work
o1:emitArrayItems( {1,2,3} )
o1:complete()

-- multiple observers printing separate values
-- demonstrates select (applying a function to each value) 
-- demonstrates where (filtering values)

o2 = MyEmittingObservable()
o2:dump('o2')

--o2a = o2:select( function( x ) return x + 1 end )
o2a = o2:select( 'X+4' ) -- example of using microlight string lambdas
o2a:dump('o2a select+4')
--
--o2a:where( function(x) return x % 3 == 0 end ):dump('o2 where mod3')
o2a:where( 'X%3==0' )
    :dump('o2a where mod3')
-- 
o2:emitArrayItems( {1,2,3,4,5,6} )
o2:complete()


Observable.range(1,7):dump("o3")

Observable.range(2,6)
  :select('X+3')
  :where('X%2==0')
  :dump('!')
  
Observable.return_(7):dump('7')
Observable.empty():dump('empty')
Observable.never():dump('never') -- shouldn't output anything
Observable.throw("eek"):dump('throw')

Observable.return_(3)
  :selectMany( function(i) return Observable.range(1,i) end )
  :dump("selectMany")

Observable.range(1,3)
  :selectMany( function(i) return Observable.range(1,i) end )
  :dump("selectMany")
  
end
test()

------------------------------------------------------------------------------
-- little experiments while developing

--[[
-- remove() doesn't error for non-existent elements.
x = ml.Array()
x:append(10)
x:remove(8)
print(x)
]]--


--[[
-- packing varargs into a table, used to make a generic IObservable ctor

function foo( ... ) -- pack all args into a table
    return {...}
end

function bar( a, b, c )
    print( "a:", a, "b:", b, "c:", c )
end

t = foo( 1, 2, 3 )
bar( unpack(t) )

t = foo( 1, 2 )
bar( 8, unpack(t) )
]]

--[[
-- checking that count is valid after removing an item in the middle of the array
x = ml.Array()

x:append(1)
x:append(2)
x:append(4)
x:remove(2)
print(x, #x)
]]
