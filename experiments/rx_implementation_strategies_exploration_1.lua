ml = require 'ml'.import() -- https://github.com/stevedonovan/Microlight

--[[

class "prototype" declaration {
  onNext = function
  onError = function
  onCompleted = function
}
 
constructInstance = function( sink, x ) {
    return { 
        sink_ = sink, x_ = x, 
        onNext=proto.onNext,
        onError=proto.onBreak,
        onCompleted.proto.onCompleted }
}

idea: use tail calls/return to avoid overhead of chaining

idea: switch onNext,onCompleted,onError between different implementations to implement state transitions (instead of setting flags)

idea: instead of sinkObserver 

query: not sure how to handle unexpected value, completed or error events after completed or error. probably we should install special handlers that error under these circumstances.

]]--

-- there are three main elements in an observables construction chain:
-- 1. a source observable
-- 2. zero or more operator observables
-- 3. one or more terminator observer 
-- - multiple terminators can be attached at different points in the chain
-- - similarly, operators can be forked off at any point in the chain
-- 
-- A source observable maintains a list of observers (attached using subscribe)
-- and posts events to each of them.
--
-- An operator constructs an operatable observable that wraps/decorates a source observable's
-- subscribe methodthat inserts an operator observer into the chain
--
-- We can further specialise and talk about "pipe operators" that take one input and output one output (ie they don't split, join or combine)
--
-- A terminator observer is a function that subscribes an observer to the end of
-- the chain.
--
-- in general:
-- IObservable sourceCtor()
-- IObservable operatorCtor( sourceObservable, ...params... )
-- void terminatorSubscriber( sourceObservable, ...params... )
--
-- there are some utility subscribers, but often people just call source:subscribe( nextFn, completedFn, breakFn )

-- the meta pattern for observables/operators is as follows
-- (with no magic, no too much syntactic sugar everything spelt out)

-- IObservable constructor (aka "Operator")
-- IObservable C( IObservable observable [, params] )
function C( sourceObservable, ... )
  
  -- 1. define concrete observer
  -- there is an specific concrete observer class definition associated with each combinator
  local ConcreteObserverClass = class();
  -- class has three methods: onNext(x), onError(e), onCompleted
  -- in general these may either sit in a class metatable,
  -- or in the object instance. note that if they're in the 
  -- object instance they don't need to be in the class
  -- Common operations for the methods
  -- are to do nothing, or to pass the result to self.sink_ (another observer)
  ConcreteObserverClass.onNext = function(self, x) end  
  ConcreteObserverClass.onError = function(self, e) end
  ConcreteObserverClass.onCompleted = function(self) end
  ConcreteObserverClass._init = function ( self, param1, param2, paramN ) 
      self.state_  = 0 -- each concrete observer may 
  end -- constructor (should be 0 or more parameters)

  
  -- 2. define concrete observable
  -- there is a specific concrete observable class definition associated with each combinator
  -- the concrete observable inherits a bunch of methods from IObservable
  -- but it only needs to implement one: subscribe
  -- we may want to call this raw_subscribe since the usual subscribe has a bunch of helper overloads
  local ConcreteObservableClass = inherits_IObservable()
  -- the concrete observable may store parameters that are used to construct the observer
  ConcreteObservableClass.observerParams_ = ...
  ConcreteObservableClass.sourceObservable_ = sourceObservable
  -- the alternative here is when there is no source observable: ie when implementing a source
  -- in that case the subscribe method will add sinkObserver to a list of observers
  -- rather than constructing a "wrapper observer"
  ConcreteObservableClass._subscribe = function(self, sinkObserver )
    -- 4. construct concrete observer
    return self.sourceObservable_.subscribe( ConcreteObserverClass( sinkObserver, unpack(self.observerParams_) ) )
  end  
  
  -- 3. construct concrete ovservable
  -- return an instance of the concrete observable
  return ConcreteObservableClass()
end


-- alternate parameterisations

-- there are two logical extremes of parameterisation:
--  1. observer and observable classes are implemented in an 
--  orthodox object oriented way. class objects are separately
--  defined at the outset.
--  2. observer and observable classes are implemented using 
--  maximal metaprogramming. An operator is implemented using an
--  operator constructor.
--
-- We exhibit each style below.

------------------------------------------------------------------------


-- background 

Disposable = {}

Disposable.empty = { dispose = function() end }
  
Disposable.create = function(f)
    return {
      f_ = f,
      dispose = function(self) 
        self.f_()
        self.dispose = Disposable.empty.dispose -- ensure idempotence
      end 
    }
end


Observable = {} -- just a namespace table
IObservable = ml.class()

implements_IObservable = function() return ml.class(IObservable) end -- stub. actually this needs to arrange for inheritance differently

-- implements_IObservable()
-- returns a new class object (table) that can have member methods added to it
-- it defines a call metamethod to support construction of new instances
-- construction calls the _init method if it is present

--[[
missing symbol lookup works as follows:

  look up in referenced instance x
  
  look up in x.metatable.__index
  look up in x.metatable.__index.metatable.index
  etc
]]

-- construct an IObservable class object
implements_IObservable = function()
  -- class object (and hence instances) inherit Observable extension methods from Observable
  local classObject_mt = { __index = Observable } -- note that we can't use Observable for this because we need the call method below
  local classObject = setmetatable({},classObject_mt)
  
  classObject._subscribe = function() print "you forgot to implement _subscribe" end
  
  -- instances inherit _init() and subscribe() from class object
  local objectInstance_mt = {} 
  objectInstance_mt.__index = classObject 
  
  classObject_mt.__call = function(classObject,...)
      local obj = setmetatable({},objectInstance_mt) -- captures objectInstance_mt
      if rawget(classObject,'_init') then
        classObject._init(obj,...) -- call our constructor
      end
      return obj
  end
  return classObject
end

------------------------------------------------------------------------

-- the zero operation
-- do nothing with the events
NullObserver = ml.class()
function NullObserver._init( self ) end
function NullObserver.onNext( self, x ) return end
function NullObserver.onCompleted( self ) return end
function NullObserver.onError( self, e ) end

-- util functions
-- enforce the Observer grammar

function complete( self )
  self.onNext = NullObserver.onNext
  self.onError = NullObserver.onError
  self.onCompleted = NullObserver.onCompleted
  return self.sinkObserver_:onCompleted()
end

function completeWithValue( self, value )
  self.onNext = NullObserver.onNext
  self.onError = NullObserver.onError
  self.onCompleted = NullObserver.onCompleted
  self.sinkObserver_:onNext( value )
  return self.sinkObserver_:onCompleted()
end

function terminateWithError( self, e )
  self.onNext = NullObserver.onNext
  self.onError = NullObserver.onError
  self.onCompleted = NullObserver.onCompleted
  return self.sinkObserver_:onError(e)
end

-- the unit operation
-- pass all events to sink observer
PassThroughObserver = ml.class()
function PassThroughObserver._init( self, sinkObserver ) self.sinkObserver_ = sinkObserver end
function PassThroughObserver.onNext( self, x ) return self.sinkObserver_:onNext( x ) end
function PassThroughObserver.onCompleted( self ) return complete( self ) end
function PassThroughObserver.onError( self, e ) return terminateWithError( self, e ) end



------------------------------------------------------------------------
-- absolutely vanilla obvious implementation of Observer.where(sourceObservable, predicate)

-- 1. define concrete observer
WhereObserver = ml.class(PassThroughOperatorObserver)
WhereObserver._init = function ( self, sinkObserver, predicate )
  self:super( sinkObserver )
  self.predicate_ = predicate 
end
WhereObserver.onNext = function( self, x ) 
  return self.sinkObserver_:onNext( x ) 
end

-- 2. define concrete observable
WhereObservable = implements_IObservable()
WhereObservable._init = function( self, sourceObservable, predicate )
  self.sourceObservable_ = sourceObservable_
  self.predicate_ = ml.function_arg(predicate)
end
WhereObservable._subscribe = function( self, observer )
    -- 4. construct concrete observer
    return self.sourceObservable_:subscribe( WhereObserver( observer, self.predicate_ ) )
end

-- (i)
Observable.where = function( sourceObservable, predicate )
  -- 3. construct concrete observable
  return WhereObservable( sourceObservable, predicate )
end
IObservable.where = Observable.where -- operator functions can be used free standing or reflexively (chaining), in which case self <=> sourceObservable

------------------------------------------------------------------------
-- parametric construction of everything using ... for parameters
-- pros: construction of observer is clear
-- cons: signature of operator function is unclear

-- all operator sink observer functions have signature 

-- t optionally contains: 
-- _init(self, sinkObserver, ...)
--

make_pipe_operator = function( t )
  -- 1. define concrete observer
  local ConcreteObserverClass = ml.class(PassThroughObserver)
  -- TODO FIXME: use t as the basis for our class table 
  -- instead of constructing another one here
  -- TODO FIXME: also if we used our own mechanism here we could
  -- avoid all of the subclasses having to explicitly receive sinkObserver_
  ConcreteObserverClass._init = t._init or PassThroughObserver._init
  ConcreteObserverClass.onNext = t.onNext or PassThroughObserver.onNext
  ConcreteObserverClass.onCompleted = t.onCompleted or PassThroughObserver.onCompleted
  ConcreteObserverClass.onError = t.onError or PassThroughObserver.onError
  
  -- 2. define concrete observable
  local ConcreteObservableClass = implements_IObservable()
  ConcreteObservableClass._init = function( self, sourceObservable, ... )
    self.sourceObservable_ = sourceObservable
    self.params_ = arg
  end
  ConcreteObservableClass._subscribe = function( self, observer )
    -- 4. construct concrete observer
    return self.sourceObservable_:subscribe( ConcreteObserverClass( observer, unpack(self.params_) ) )
  end
  
  -- return a function that constructs new observables
  -- this is equivalent to (i) above
  return function(...)
    -- 3. construct concrete observable
    return ConcreteObservableClass(...) 
  end
end

make_simple_generator_operator_given_subscribe_method = function( subscribe )
  -- 1. generators don't construct an observer class
  -- (nothing)
  
  -- 2. define concrete observable
  local ConcreteObservableClass = implements_IObservable()
  ConcreteObservableClass._init = function( self, ... )
    self.params_ = arg
  end
  ConcreteObservableClass._subscribe = subscribe
  
  -- return a function that constructs new observables
  -- this is equivalent to (i) above
  return function(...)
    -- 3. construct concrete observable
    return ConcreteObservableClass(...) 
  end
end

------------------------------------------------------------------------------
-- in order to support the various overloads of subscrbe() we 
-- use _subscribe() as the prototype method implemented in each
-- concrete observable.
-- h/t RxJS

AnonymousObserver = ml.class(PassThroughObserver)
AnonymousObserver._init = function(self, onNext, onError, onCompleted)
  self.onNext = function(self, x) return onNext(x) end or function(x) end
  self.onError = function(self, e) return onError(x) end or function(e) print(e); assert(false) end
  self.onCompleted = function(self) onCompleted() end or noop
end

Observable.subscribe = function( self, observableOrOnNext, onError, onCompleted )
  if type(observableOrOnNext) == "table" then
    return self:_subscribe( observableOrOnNext )
  else
    return self:_subscribe( AnonymousObserver( observableOrOnNext, onError, onCompleted ) )
  end    
end

------------------------------------------------------------------------------
-- Simple factory methods: return_, empty, never, throw, create

-- return(value)
Observable.return_ = make_simple_generator_operator_given_subscribe_method( 
  function( self, observer )
    local value = unpack(self.params_)
    observer:onNext(value)
    observer:onCompleted()
    return Disposable.empty
  end
)

-- empty()
Observable.empty = make_simple_generator_operator_given_subscribe_method( 
  function( self, observer )
    observer:onCompleted()
    return Disposable.empty
  end
)

-- never()
Observable.never = make_simple_generator_operator_given_subscribe_method( 
  function( self, observer )
    return Disposable.empty
  end
)

-- throw(e)
Observable.throw = make_simple_generator_operator_given_subscribe_method( 
  function( self, observer )
    local e = unpack(self.params_)
    observer:onError(e)
    return Disposable.empty
  end
)

-- create( subscribe )
-- subscribe is a subscribe function taking an observer as argument
-- subscribe may return a disposable, or a function to be called on disposal
Observable.create = make_simple_generator_operator_given_subscribe_method( 
  function( self, observer )
    local subscribe = unpack(self.params_)
    local subscription = subscribe( observer )
    if type(subscription) == "function" then subscription = Disposable.create(subscription) end
    return subscription
  end
)

------------------------------------------------------------------------

-- generate( initialState, condition, iterate, resultSelector )
Observable.generate = make_simple_generator_operator_given_subscribe_method( 
  function( self, observer )
    local initialState, condition, iterate, resultSelector = unpack(self.params_)
    condition = ml.function_arg(condition)   
    iterate = ml.function_arg(iterate)
    resultSelector = ml.function_arg(resultSelector)
    local state = initialState
    while condition( state ) do
      -- FIXME TODO notify secondary observers here, accumulate new subscriptions
      --- maybe do a test to see how secondary observers are handled in C# Rx (ie a subscription attached while the primary generate is running
      observer:onNext( resultSelector( state ) )
      state = iterate( state )  
    end  
    observer:onCompleted()  
    return Disposable.empty
  end
)

Observable.range = function( start, count )
  return Observable.generate( 0,
      function( value ) return value < count end,
      function( value ) return value + 1 end,
      function( value ) return start + value end )  
end

------------------------------------------------------------------------

-- See Observable Methods
-- http://msdn.microsoft.com/en-us/library/hh212048(v=vs.103).aspx
------------------------------------------------------------------------
-- Linq operators
------------------------------------------------------------------------
-- defaultIfEmpty( sourceObservable, defaultValue ) 
--
-- Returns the elements of the specified sequence or the type parameter's 
-- default value in a singleton sequence if the sequence is empty.
--
Observable.defaultIfEmpty = make_pipe_operator{
  _init = function( self, sinkObserver, defaultValue )
    self:super( sinkObserver )
    self.defaultValue_ = defaultValue
  end,
  onNext = function( self, x )
    -- once we have received a value we don't need to do anything
    self.onNext = PassThroughObserver.onNext
    self.onCompleted = PassThroughObserver.onCompleted
    return self.sinkObserver_:onNext( x )
  end,
  onCompleted = function( self )
    completeWithValue( self, self.defaultValue_ )
  end
}
------------------------------------------------------------------------
-- elementAt( sourceObservable, index ) 
--
-- Returns the element at a specified index in a sequence.
--
Observable.elementAt = make_pipe_operator{
  _init = function( self, sinkObserver, index )
    assert( index >= 0 )
    self:super( sinkObserver )
    self.index_ = index
    self.i_ = 0
  end,
  onNext = function( self, x )
    if self.i_ == self.index_ then
      return completeWithValue( self, x )
    else
      self.i_ = self.i_ + 1
    end
  end,
  onCompleted = function( self )
    if self.i_ < self.index_ then
      return terminateWithError(self, "index out of range")
    end
  end
}
------------------------------------------------------------------------
-- elementAtOrDefault( sourceObservable, index, defaultValue ) 
--
-- Returns the element at a specified index in a sequence or a default 
-- value if the index is out of range.
--
-- RX_API_DIVERGENCE: note that the LINQ version default-constructs the 
-- default value from its type. Lua has no such thing. If you omit 
-- default you'll get an onNext( nil ) event as the default.
--
Observable.elementAtOrDefault = make_pipe_operator{
  _init = function( self, sinkObserver, index, defaultValue )
    assert( index >= 0 )
    self:super( sinkObserver )
    self.index_ = index
    self.defaultValue_ = defaultValue
    self.i_ = 0
  end,
  onNext = function( self, x )
    if self.i_ == self.index_ then
      return completeWithValue( self, x )
    else
      self.i_ = self.i_ + 1
    end
  end,
  onCompleted = function( self )
    if self.i_ < self.index_ then
      return completeWithValue( self, self.defaultValue_ )
    end
  end
}
------------------------------------------------------------------------
-- contains( sourceObservable, value, comparator )
--
-- Determines whether a sequence contains a specified element.
--
Observable.contains = make_pipe_operator{
  _init = function( self, sinkObserver, value, comparator )
    self:super( sinkObserver )
    self.value_ = value
    if comparator == nil then
      self.comparator_ = function( a, b ) return a == b end
    else
      self.comparator_ = ml.function_arg(comparator)
    end
  end,
  onNext = function( self, x )
    if self.comparator_(x, self.value_) then
      return completeWithValue( self, true )
    end
  end,
  onCompleted = function( self )
    return completeWithValue( self, false )
  end
}
------------------------------------------------------------------------
-- take( sourceObservable, count )
--
-- Returns a specified number of contiguous elements from the start of a sequence.
--
Observable.take = make_pipe_operator{
  _init = function( self, sinkObserver, count )
    assert( count > 0 ) -- FIXME BUG we don't support take(0) at present
    self:super( sinkObserver )
    self.remaining_ = count
  end,
  onNext = function( self, x )
    self.sinkObserver_:onNext( x )
    self.remaining_ = self.remaining_ - 1
    if self.remaining_ == 0 then
      return complete( self )
    end
  end
}
------------------------------------------------------------------------
-- takeWhile( predicate )
-- takeWhile_withIndex( predicate)
--
-- Returns values from an observable sequence as long as a specified 
-- condition is true, and then skips the remaining values.
-- 
Observable.takeWhile = make_pipe_operator{
  _init = function( self, sinkObserver, predicate )
    self:super( sinkObserver )
    self.predicate_ = ml.function_arg(predicate)
  end,
  onNext = function( self, x )
    if self.predicate_(x) then
      return self.sinkObserver_:onNext( x )
    else
      return complete( self )
    end
  end
}

-- variant that also provides the predicate with the index of the item
Observable.takeWhile_withIndex = make_pipe_operator{
  _init = function( self, sinkObserver, predicate )
    self:super( sinkObserver )
    self.predicate_ = ml.function_arg(predicate)
    self.i_ = 0
  end,
  onNext = function( self, x )
    if self.predicate_(x, self.i_ ) then
      self.i_ = self.i_ + 1
      return self.sinkObserver_:onNext( x )
    else
      return complete( self )
    end
  end
}

------------------------------------------------------------------------
-- takeUntil
-- 
-- Returns the values from the source observable sequence until the 
-- other observable sequence produces a value.
-- 
Observable.takeUntil = make_pipe_operator{
  _init = function( self, sinkObserver, otherObservable )
    self:super( sinkObserver )
  end,
  TODO = nil
  -- ************************************************************************************
}
------------------------------------------------------------------------
-- takeLast(count)
-- 
-- Returns a specified number of contiguous elements from the end of 
-- an observable sequence.
-- 

function incrementTableIndexModN( i, N ) -- table indices are [1, N] (not [0, N) as in C)
  if i == N then
    return 1
  else
    return i + 1
  end
end

Observable.takeLast = make_pipe_operator{
  _init = function( self, sinkObserver, count )
    self:super( sinkObserver )
    self.count_ = count
    self.i_ = 1 -- next write position
    self.circularBuffer_ = {}
  end,
  onNext = function( self, x )
    self.circularBuffer_[self.i_] = x
    self.i_ = incrementTableIndexModN( self.i_, self.count_ )
  end,
  onCompleted = function( self )
    local j = self.i_ -- i_ is also the first read position
    if j > #self.circularBuffer_ then
      j = 1 -- unless the buffer isn't full
    end
    
    for k=1, min(self.count_, #self.circularBuffer_) do
      self.sinkObserver_:onNext( self.circularBuffer_[j] )
      j = incrementTableIndexModN( j, self.count_ )
    end
    self.circularBuffer_ = nil -- drop the buffer asap
    
    return complete( self )
  end
}
------------------------------------------------------------------------
-- skip(count)
-- 
-- Bypasses a specified number of values in an observable sequence 
-- and then returns the remaining values.
-- 
Observable.skip = make_pipe_operator{
  _init = function( self, sinkObserver, count )
    self:super( sinkObserver )
    self.count_ = count
    self.i_ = 0
  end,
  onNext = function( self, x )
    self.i_ = self.i_ + 1
    if self.i_ > self.count_ then
      self.onNext = PassThroughObserver.onNext -- flip to passthrough
      return self.sinkObserver_:onNext( x )
    end
  end
}
------------------------------------------------------------------------
-- skipWhile(predicate)
-- skipWhile_withIndex(predicate)
-- 
-- Bypasses values in an observable sequence as long as a specified 
-- condition is true and then returns the remaining values.
-- 
Observable.skipWhile = make_pipe_operator{
  _init = function( self, sinkObserver, predicate )
    self:super( sinkObserver )
    self.predicate_ = ml.function_arg(predicate)
  end,
  onNext = function( self, x )
    if self.predicate_(x) == false then
      self.onNext = PassThroughObserver.onNext -- flip to passthrough
      return self.sinkObserver_:onNext( x )
    end
  end
}

Observable.skipWhile_withIndex = make_pipe_operator{
  _init = function( self, sinkObserver, predicate )
    self:super( sinkObserver )
    self.predicate_ = ml.function_arg(predicate)
    self.i_ = 0
  end,
  onNext = function( self, x )
    local i = self.i_
    self.i_ = self.i_ + 1
    if self.predicate_(x,i) then
      self.onNext = PassThroughObserver.onNext -- flip to passthrough
      return self.sinkObserver_:onNext( x )
    end
  end
}
------------------------------------------------------------------------
-- skipUntil
-- 
-- Returns the values from the source observable sequence only 
-- after the other observable sequence produces a value.
-- 
Observable.skipUntil = make_pipe_operator{
  _init = function( self, sinkObserver, otherObservable )
    self:super( sinkObserver )
  end,
  TODO = nil
  -- ************************************************************************************
}
------------------------------------------------------------------------
-- skipLast(count)
-- 
-- Bypasses a specified number of elements at the end of an observable sequence.
-- 
Observable.skipLast = make_pipe_operator{
  _init = function( self, sinkObserver, count )
    self:super( sinkObserver )
    self.count_ = count
    self.i_ = 1 -- next read/write position
    self.circularBuffer_ = {}
  end,
  onNext = function( self, x )
    if self.i_ <= #self.circularBuffer_ then
      self.sinkObserver_:onNext( self.circularBuffer_[self.i_] )
    end
    self.circularBuffer_[self.i_] = x
    self.i_ = incrementTableIndexModN( self.i_, self.count_ )
  end,
  onCompleted = function( self )
    self.circularBuffer_ = nil -- drop the buffer asap
    return complete( self )
  end
}
------------------------------------------------------------------------
-- where( sourceObservable, predicate )
-- where_withIndex( sourceObservable, predicate )
-- 
-- Filters the elements of an observable sequence.
-- There is an overload where the predicate also gets the index
Observable.where = make_pipe_operator{
  _init = function( self, sinkObserver, predicate )
    self:super( sinkObserver )
    self.predicate_ = ml.function_arg(predicate)
  end,
  onNext = function( self, x )
    if self.predicate_(x) then
      return self.sinkObserver_:onNext( x )
    end
  end,
}

Observable.where_withIndex = make_pipe_operator{
  _init = function( self, sinkObserver, predicate )
    self:super( sinkObserver )
    self.predicate_ = ml.function_arg(predicate)
    self.i_ = 0
  end,
  onNext = function( self, x )
    local i = self.i_
    self.i_= self.i_ + 1
    if self.predicate_(x,i) then
      return self.sinkObserver_:onNext( x )
    end
  end,
}
------------------------------------------------------------------------
-- select( sourceObservable, f )
-- select_withIndex( sourceObservable, f ) 
-- 
-- Projects each element of an observable sequence into a new form 
-- with the specified source and selector.
-- There is a overload where the selector also gets the index
Observable.select = make_pipe_operator{
  _init = function( self, sinkObserver, f )
    self:super( sinkObserver )
    self.f_ = ml.function_arg(f)
  end,
  onNext = function( self, x )
    return self.sinkObserver_:onNext( self.f_(x) )
  end,
}

Observable.select_withIndex = make_pipe_operator{
  _init = function( self, sinkObserver, f )
    self:super( sinkObserver )
    self.f_ = ml.function_arg(f)
    self.i_ = 0
  end,
  onNext = function( self, x )
    local i = self.i_
    self.i_= self.i_ + 1
    return self.sinkObserver_:onNext( self.f_(x,i) )
  end,
}
------------------------------------------------------------------------
-- forEach
-- 
-- Invokes an action for each element in the observable sequence, 
-- and blocks until the sequence is terminated.
-- 
-- FIXME this isn't blocking
Observable.forEach = make_pipe_operator{
  _init = function( self, sinkObserver, f )
    self:super( sinkObserver )
    self.f_ = ml.function_arg(f)
  end,
  onNext = function( self, x )
    self.f_(x)
    return self.sinkObserver_:onNext( x )
  end,
}
------------------------------------------------------------------------
-- aggregate( accumulator )
-- aggregate( seed, accumulator )
-- 
-- Applies an accumulator function over an observable sequence.
-- Seed is zero if not supplied.
--

Observable.aggregate_withSeed = make_pipe_operator{
  _init = function( self, sinkObserver, seed, accumulator )
    self:super( sinkObserver )
    self.s_ = seed
    self.accumulator_ =  ml.function_arg(accumulator)
  end,
  onNext = function( self, x )
    self.s_ = self.accumulator_( self.s_, x )
  end,
  onCompleted = function( self )
    return completeWithValue( self, self.s_ )
  end
}

-- aggregate( sourceObservable, accumulator )
-- aggregate( sourceObservable, seed, accumulator )
Observable.aggregate = function( sourceObservable, a, b )
  if b == nil then
    return Observable.aggregate_withSeed(0,a) -- default seed to 0
  else
    return Observable.aggregate_withSeed(a,b)
  end
end

------------------------------------------------------------------------
-- count()
-- 
-- Returns an int that represents the total number of elements in 
-- an observable sequence.
--
Observable.count = make_pipe_operator{
  _init = function( self, sinkObserver )
    self:super( sinkObserver )
    self.i_ = 0
  end,
  onNext = function( self, x )
    self.i_ = self.i_ + 1
  end,
  onCompleted = function( self )    
    return completeWithValue( self, self.i_ )
  end
}
------------------------------------------------------------------------
-- sum()
-- 
-- Computes the sum of a sequence.
-- 
-- Accumulate the sum of all values. Emit the sum when the sequence
-- completes. An empty sequence has a sum of zero.
Observable.sum = make_pipe_operator{
  _init = function( self, sinkObserver )
    self:super( sinkObserver )
    self.s_ = 0
  end,
  onNext = function( self, x )
    self.s_ = self.s_ + x
  end,
  onCompleted = function( self )
    return completeWithValue( self, self.s_ )
  end
}
------------------------------------------------------------------------
-- max()
-- max(comparator)
-- 
-- Returns the maximum value.
-- 

-- uses the first value as the first value to accumulate
-- maybe aggregate is not the best name for this 
Observable.fold_requireOneValue = make_pipe_operator{
  _init = function( self, sinkObserver, accumulator )
    self:super( sinkObserver )
    self.accumulatedValue_ = nil
    self.accumulator_ = ml.function_arg(accumulator)
  end,
  onNext = function( self, x )
    -- first value
    self.accumulatedValue_ = x
    self.onNext = function( self, x )
      self.accumulatedValue_ = self.accumulator_( self.accumulatedValue_, x )
    end
    self.onCompleted = function( self, x )
      return completeWithValue( self, self.accumulatedValue_ )
    end
  end,
  onCompleted = function( self )
    return terminateWithError(self, "no values received")
  end
}

-- comparator function is optional
Observable.max = function( sourceObservable, comparator )
  if comparator == nil then
    return Observable.fold_requireOneValue(function(acc, x) if x > acc then return x else return acc end end)
  else
    return Observable.fold_requireOneValue(comparator)
  end
end

------------------------------------------------------------------------
-- maxBy
-- 
-- Returns the maximum value.
-- 
Observable.maxBy = nil -- NOT IMPLEMENTED use max
------------------------------------------------------------------------
-- min()
-- min(comparator)
-- 
-- Returns the minimum value.
--
-- Accumulates the minimum value then emits it when the 
-- sequence completes. Emits error if the sequence is empty.
--

-- comparator function is optional
Observable.min = function( sourceObservable, comparator )
  if comparator == nil then
    return Observable.fold_requireOneValue(function(acc, x) if x < acc then return x else return acc end end)
  else
    return Observable.fold_requireOneValue(comparator)
  end
end
------------------------------------------------------------------------
-- minBy
-- 
-- Returns the elements in an observable sequence with the minimum key value.
-- 
Observable.minBy = nil -- NOT IMPLEMENTED use min
------------------------------------------------------------------------
-- average()
-- 
-- Computes the average of an observable sequence.
--
-- Accumulates each value then emits the result divided by count when the 
-- sequence completes. Emits error if the sequence is empty.
--

Observable.average = make_pipe_operator{
  _init = function( self, sinkObserver )
    self:super( sinkObserver )
    self.accumulatedValue_ = nil
    self.i_ = 0
  end,
  onNext = function( self, x )
    -- first value
    self.accumulatedValue_ = x
    self.i_ = 1
    self.onNext = function( self, x )
      self.accumulatedValue_ = self.accumulatedValue_ + x
      self.i_ = self.i_ + 1
    end
    self.onCompleted = function( self, x )
      return completeWithValue( self, self.accumulatedValue_/self.i_ )
    end
  end,
  onCompleted = function( self )
    return terminateWithError(self, "no values received")
  end
}

------------------------------------------------------------------------
-- all(predicate)
-- 
-- Determines whether all elements of an observable sequence satisfies a condition.
-- 
Observable.all = make_pipe_operator{
  _init = function( self, sinkObserver, predicate )
    self:super( sinkObserver )
    self.predicate_ = ml.function_arg(predicate)
    self.s_ = true
  end,
  onNext = function( self, x )
    if self.s_ then
       self.s_ = self.predicate_(x)
       if not self.s_ then
         return completeWithValue( self, false )
       end
    end
  end,
  onCompleted = function( self )
    if self.s_ then
      return completeWithValue( self, true )
    end
  end
}
------------------------------------------------------------------------
-- toList()
-- 
-- Creates a list from an observable sequence.
-- 
Observable.toList = make_pipe_operator{
  _init = function( self, sinkObserver )
    self:super( sinkObserver )
    self.s_ = {}
  end,
  onNext = function( self, x )
    self.s_[#self.s_ + 1] = x
  end,
  onCompleted = function( self )
    return completeWithValue( self, self.s_ )
  end
}
------------------------------------------------------------------------
-- selectMany
-- 
-- Projects each element of an observable sequence to an observable 
-- sequence and flattens the resulting observable sequences into one
-- observable sequence
-- 
Observable.selectMany = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end,
  TODO = nil
  -- ************************************************************************************
}
------------------------------------------------------------------------
-- groupBy
-- 
-- Groups the elements of an observable sequence.
-- 
Observable.groupBy = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end,
  TODO = nil
  -- ************************************************************************************
}
------------------------------------------------------------------------
-- join
-- 
-- Correlates the elements of two sequences based on overlapping durations.
-- 
Observable.join = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end,
  TODO = nil
  -- ************************************************************************************
}
------------------------------------------------------------------------
-- groupJoin
-- 
-- Correlates the elements of two sequences based on overlapping 
-- durations, and groups the results.
-- 
Observable.groupJoin = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end,
  TODO = nil
  -- ************************************************************************************
}
------------------------------------------------------------------------
-- sequenceEqual
-- 
-- Determines whether two sequences are equal.
-- 
Observable.sequenceEqual = make_pipe_operator{
  _init = function( self, sinkObserver, secondObservable )
    self:super( sinkObserver )
  end,
  TODO = nil
  -- ************************************************************************************
}
------------------------------------------------------------------------
--[[

A number of the linq operators are some variant on foldr (fold right).

given a binary function f(), an initial value i and an accumulator a_ we have:

_init:
	a = i
onNext(x):
	a = f(a,x)
onCompleted:
	sink.onNext(a)
	sink.onCompleted()
	
These include:
	toList, sum, count, aggregate, 
	
	
	
	
A variant uses the first element (in a non-empty stream) and errors if the sequence is empty.

_init:
	isFirst = true
onNext(x):
	if isFirst:
		isFirst = false
		a = x
	else
		a = f(a,x)
onCompleted:
	if isFirst:
		sing.onError()
	else:
		sink.onNext(a)
		sink.onCompleted()
		
These include:
	average, min, max, minBy, maxBy
	
	
	
	
The following seem to have some commonalities, more analysis is needed:

take, takeWhile, takeUntil, takeLast

skip, skipWhile, skipUntil, skipLast


]]--

------------------------------------------------------------------------
-- Combinators
------------------------------------------------------------------------
-- merge
--

--[[
Merge<TSource>(IEnumerable<IObservable<TSource>>)	Merges an enumerable sequence of observable sequences into a single observable sequence.

  	Merge<TSource>(IObservable<IObservable<TSource>>)	Merges an observable sequence of observable sequences into an observable sequence.
    
  	Merge<TSource>(IObservable<TSource>[])	Merges all the observable sequences into a single observable sequence.
    
  	Merge<TSource>(IEnumerable<IObservable<TSource>>, Int32)	Merges an enumerable sequence of observable sequences into an observable sequence, limiting the number of concurrent subscriptions to inner sequences.
    
  	Merge<TSource>(IEnumerable<IObservable<TSource>>, IScheduler)	Merges an enumerable sequence of observable sequences into a single observable sequence.
    
  	Merge<TSource>(IObservable<IObservable<TSource>>, Int32)	Merges an enumerable sequence of observable sequences into an observable sequence, limiting the number of concurrent subscriptions to inner sequences.
    
  	Merge<TSource>(IObservable<TSource>, IObservable<TSource>)	Merges an observable sequence of observable sequences into an observable sequence.
    
  	Merge<TSource>(IScheduler, IObservable<TSource>[])	Merges all the observable sequences into a single observable sequence.
    
  	Merge<TSource>(IEnumerable<IObservable<TSource>>, Int32, IScheduler)	Merges an enumerable sequence of observable sequences into an observable sequence, limiting the number of concurrent subscriptions to inner sequences.
    
  	Merge<TSource>(IObservable<TSource>, IObservable<TSource>, IScheduler)	Merges two observable sequences into a single observable sequence.
]]--

--
--
TODO = nil
------------------------------------------------------------------------
-- zip
--[[
  Zip<TFirst, TSecond, TResult>(IObservable<TFirst>, IEnumerable<TSecond>, Func<TFirst, TSecond, TResult>)	Merges an observable sequence and an enumerable sequence into one observable sequence by using the selector function.

  Zip<TFirst, TSecond, TResult>(IObservable<TFirst>, IObservable<TSecond>, Func<TFirst, TSecond, TResult>)	Merges two observable sequences into one observable sequence by combining their elements in a pairwise fashion.
]]--
--
--
TODO = nil
------------------------------------------------------------------------
-- combineLatest
--[[
CombineLatest<TFirst, TSecond, TResult>	Merges two observable sequences into one observable sequence by using the selector function whenever one of the observable sequences produces an element.]]

--
--
TODO = nil
------------------------------------------------------------------------
-- amb
--
--[[
  Amb<TSource>(IEnumerable<IObservable<TSource>>)	Propagates the observable sequence that reacts first with a specified sources.
  
  Amb<TSource>(IObservable<TSource>[])	Propagates the observable sequence that reacts first with a specified sources.
  
  Amb<TSource>(IObservable<TSource>, IObservable<TSource>)	Propagates the observable sequence that reacts first with the specified first and second sequence.
]]--
--
TODO = nil
------------------------------------------------------------------------
-- forkJoin
--
-- "Wes Dyer: Looks like I missed a note. Use CombineLatest or Zip instead of ForkJoin."
-- http://social.msdn.microsoft.com/Forums/en-US/rx/thread/527002a3-18af-4eda-8e35-760ca0006b98
-- http://stackoverflow.com/questions/6551638/rx-forkjoin-missing

------------------------------------------------------------------------
-- concat
--
--[[
  Concat<TSource>(IEnumerable<IObservable<TSource>>)	Concatenates an enumerable sequence of observable sequences.

  Concat<TSource>(IObservable<IObservable<TSource>>)	Concatenates an observable sequence of observable sequences.
  
  Concat<TSource>(IObservable<TSource>[])	Concatenates all the observable sequences.
  
  Concat<TSource>(IObservable<TSource>, IObservable<TSource>)	Concatenates two observable sequences.
=]]
--
TODO = nil
------------------------------------------------------------------------
-- when
--
--[[
  When<TResult>(IEnumerable<Plan<TResult>>)	Joins together the results from several patterns.
  
  When<TResult>(Plan<TResult>[])	Joins together the results from several patterns.
  
  http://msdn.microsoft.com/en-us/library/system.reactive.joins(v=vs.103).aspx
]]
--
TODO = nil
------------------------------------------------------------------------
--
------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------
-- catch
--[[
Catch<TSource>(IEnumerable<IObservable<TSource>>)	Continues an observable sequence that is terminated by an exception with the next observable sequence.
  	Catch<TSource>(IObservable<TSource>[])	Continues an observable sequence that is terminated by an exception with the next observable sequence.
  	Catch<TSource>(IObservable<TSource>, IObservable<TSource>)	Continues an observable sequence that is terminated by an exception with the next observable sequence.
  	Catch<TSource, TException>(IObservable<TSource>, Func<TException, IObservable<TSource>>)	Continues an observable sequence that is terminated by an exception of the specified type with the observable sequence produced by the handler.]]
--
--
TODO = nil
------------------------------------------------------------------------
-- onErrorResumeNext
--[[
	OnErrorResumeNext<TSource>(IEnumerable<IObservable<TSource>>)	Continues an observable sequence that is terminated normally or by an exception with the next observable sequence.
  	OnErrorResumeNext<TSource>(IObservable<TSource>[])	Continues an observable sequence that is terminated normally or by an exception with the next observable sequence.
  	OnErrorResumeNext<TSource>(IObservable<TSource>, IObservable<TSource>)	Continues an observable sequence that is terminated normally or by an exception with the next observable sequence.
    ]]
--
--
TODO = nil
------------------------------------------------------------------------
-- retry
--
--[[
Retry<TSource>(IObservable<TSource>)	Repeats the source observable sequence until it successfully terminates.
  	Retry<TSource>(IObservable<TSource>, Int32)	Repeats the source observable sequence until it successfully terminates.
    ]]
--
TODO = nil
------------------------------------------------------------------------
--
------------------------------------------------------------------------
-- Rx
------------------------------------------------------------------------
-- sample_withTime
--
-- Samples the observable sequence at each interval with the specified
-- source, interval and scheduler.
-- 
--[[
Sample<TSource>(IObservable<TSource>, TimeSpan)	Samples the observable sequence at each interval.
  	Sample<TSource>(IObservable<TSource>, TimeSpan, IScheduler)
    ]]--
--
TODO = nil
------------------------------------------------------------------------
-- sample_withSampler
--
-- Samples the observable sequence at sampling ticks with the specified 
-- source and sampler.
-- 
--[[
Sample<TSource, TSample>(IObservable<TSource>, IObservable<TSample>)	
]]--
--
TODO = nil
------------------------------------------------------------------------
-- buffer_withCount
--
--
--[[
Buffer<TSource>(IObservable<TSource>, Int32)	Indicates each element of an observable sequence into consecutive non-overlapping buffers which are produced based on element count information.
Buffer<TSource>(IObservable<TSource>, Int32, Int32)	Indicates each element of an observable sequence into zero or more buffers which are produced based on element count information.
]]--
--
TODO = nil
------------------------------------------------------------------------
-- buffer_withTime
--
 --[[
 Buffer<TSource>(IObservable<TSource>, TimeSpan)	Indicates each element of an observable sequence into consecutive non-overlapping buffers which are produced based on timing information.
 Buffer<TSource>(IObservable<TSource>, TimeSpan, IScheduler)	Indicates each element of an observable sequence into consecutive non-overlapping buffers which are produced based on timing information.
 Buffer<TSource>(IObservable<TSource>, TimeSpan, TimeSpan)	Indicates each element of an observable sequence into zero or more buffers which are produced based on timing information.
 Buffer<TSource>(IObservable<TSource>, TimeSpan, TimeSpan, IScheduler)	Indicates each element of an observable sequence into zero or more buffers which are produced based on timing information.
 ]]--
--
--
TODO = nil
------------------------------------------------------------------------
-- buffer_withTimeOrCount
--
--
  --[[
  Buffer<TSource>(IObservable<TSource>, TimeSpan, Int32)	Indicates each element of an observable sequence into a buffer that’s sent out when either it’s full or a given amount of time has elapsed.
    Buffer<TSource>(IObservable<TSource>, TimeSpan, Int32, IScheduler)	Indicates each element of an observable sequence into a buffer that’s sent out when either it’s full or a given amount of time has elapsed.
 ]]--
--
TODO = nil
------------------------------------------------------------------------
-- buffer_???
--[[
  	Buffer<TSource, TBufferClosing>(IObservable<TSource>, Func<IObservable<TBufferClosing>>)	Indicates each element of an observable sequence into consecutive non-overlapping buffers.
  	Buffer<TSource, TBufferOpening, TBufferClosing>(IObservable<TSource>, IObservable<TBufferOpening>, Func<TBufferOpening, IObservable<TBufferClosing>>)	Indicates each element of a queryable observable sequence into consecutive non-overlapping buffers.
    ]]--
------------------------------------------------------------------------
-- window_withClosingSelector
--
--
--[[
	Window<TSource, TWindowClosing>(IObservable<TSource>, Func<IObservable<TWindowClosing>>)	Projects each element of an observable sequence into consecutive non-overlapping windows.
  	Window<TSource>(IObservable<TSource>, Int32)	Projects each element of an observable sequence into consecutive non-overlapping windows which are produced based on element count information.
  	Window<TSource>(IObservable<TSource>, TimeSpan)	Projects each element of an observable sequence into consecutive non-overlapping windows which are produced based on timing information.
  	Window<TSource>(IObservable<TSource>, Int32, Int32)	Projects each element of an observable sequence into zero or more windows which are produced based on element count information.
  	Window<TSource, TWindowOpening, TWindowClosing>(IObservable<TSource>, IObservable<TWindowOpening>, Func<TWindowOpening, IObservable<TWindowClosing>>)	Projects each element of an observable sequence into zero or more windows.
  	Window<TSource>(IObservable<TSource>, TimeSpan, Int32)	Projects each element of an observable sequence into a window that is completed when either it’s full or a given amount of time has elapsed.
  	Window<TSource>(IObservable<TSource>, TimeSpan, IScheduler)	Projects each element of an observable sequence into consecutive non-overlapping windows which are produced based on timing information.
  	Window<TSource>(IObservable<TSource>, TimeSpan, TimeSpan)	Projects each element of an observable sequence into zero or more windows which are produced based on timing information.
  	Window<TSource>(IObservable<TSource>, TimeSpan, Int32, IScheduler)	Projects each element of an observable sequence into a window that is completed when either it’s full or a given amount of time has elapsed.
  	Window<TSource>(IObservable<TSource>, TimeSpan, TimeSpan, IScheduler)	Projects each element of an observable sequence into zero or more windows which are produced based on timing information.

]]--

--[[
]]--
--[[
]]--


--
TODO = nil
------------------------------------------------------------------------
-- window_withCount
--
--
--
TODO = nil
------------------------------------------------------------------------
-- window_withTime
--
--
--
TODO = nil
------------------------------------------------------------------------
-- window_withTimeOrCount
--
--
--
TODO = nil
------------------------------------------------------------------------
-- delay
--
--[[
Delay<TSource>(IObservable<TSource>, DateTimeOffset)	Indicates the observable sequence by due time with the specified source and dueTime.
  	Delay<TSource>(IObservable<TSource>, TimeSpan)	Indicates the observable sequence by due time with the specified source and dueTime.
  	Delay<TSource>(IObservable<TSource>, DateTimeOffset, IScheduler)	Indicates the observable sequence by due time with the specified source, dueTime and scheduler.
  	Delay<TSource>(IObservable<TSource>, TimeSpan, IScheduler)	Indicates the observable sequence by due time with the specified source, dueTime and scheduler.
    ]]
--
--
TODO = nil
------------------------------------------------------------------------
-- throttle
--
--
--[[
	Throttle<TSource>(IObservable<TSource>, TimeSpan)	Ignores the values from an observable sequence which are followed by another value before due time with the specified source and dueTime.
  Throttle<TSource>(IObservable<TSource>, TimeSpan, IScheduler)	Ignores the values from an observable sequence which are followed by another value before due time with the specified source, dueTime and scheduler.
  ]]
--
TODO = nil
------------------------------------------------------------------------
-- distinct
--
--
--[[
	Distinct<TSource>(IObservable<TSource>)	Returns an observable sequence that contains only distinct elements with a specified source.
  Distinct<TSource>(IObservable<TSource>, IEqualityComparer<TSource>)	Returns an observable sequence that contains only distinct elements according to the comparer.
  Distinct<TSource, TKey>(IObservable<TSource>, Func<TSource, TKey>)	Returns an observable sequence that contains only distinct elements according to the keySelector.
  Distinct<TSource, TKey>(IObservable<TSource>, Func<TSource, TKey>, IEqualityComparer<TKey>)	Returns an observable sequence that contains only distinct elements according to the keySelector.
  ]]

--
TODO = nil
------------------------------------------------------------------------
-- distinctUntilChanged
--
--
--[[
	DistinctUntilChanged<TSource>(IObservable<TSource>)	Returns an observable sequence that contains only distinct contiguous elements with a specified source.
  DistinctUntilChanged<TSource>(IObservable<TSource>, IEqualityComparer<TSource>)	Returns an observable sequence that contains only distinct contiguous elements according to the comparer.
  DistinctUntilChanged<TSource, TKey>(IObservable<TSource>, Func<TSource, TKey>)	Returns an observable sequence that contains only distinct contiguous elements according to the keySelector.
  DistinctUntilChanged<TSource, TKey>(IObservable<TSource>, Func<TSource, TKey>, IEqualityComparer<TKey>)	Returns an observable sequence that contains only distinct contiguous elements according to the keySelector and the comparer.
  ]]
--
TODO = nil
------------------------------------------------------------------------
-- scan
--
--
--[[
  Scan<TSource>(IObservable<TSource>, Func<TSource, TSource, TSource>)	Applies an accumulator function over an observable sequence and returns each intermediate result with the specified source and accumulator.
  Scan<TSource, TAccumulate>(IObservable<TSource>, TAccumulate, Func<TAccumulate, TSource, TAccumulate>)	Applies an accumulator function over an observable sequence and returns each intermediate result with the specified source, seed and accumulator.
   ]]
--
TODO = nil
------------------------------------------------------------------------
-- switch
--
-- Switch<TSource>( IObservable<IObservable<TSource>> sources )
-- 
-- Transforms an observable sequence of observable sequences into an 
-- observable sequence producing values only from the most recent 
-- observable sequence.
--
TODO = nil
------------------------------------------------------------------------
-- timer
--
--[[
  Timer(DateTimeOffset)	Returns an observable sequence that produces a value at due time.
  Timer(TimeSpan)	Returns an observable sequence that produces a value after the due time has elapsed.
  Timer(DateTimeOffset, IScheduler)	Returns an observable sequence that produces a value at due time.
  Timer(DateTimeOffset, TimeSpan)	Returns an observable sequence that produces a value at due time and then after each period.
  Timer(TimeSpan, IScheduler)	Returns an observable sequence that produces a value after the due time has elapsed.
  Timer(TimeSpan, TimeSpan)	Returns an observable sequence that produces a value after due time has elapsed and then after each period.
  Timer(DateTimeOffset, TimeSpan, IScheduler)	Returns an observable sequence that produces a value at due time and then after each period.
  Timer(TimeSpan, TimeSpan, IScheduler)	Returns an observable sequence that produces a value after due time has elapsed and then each period.
  ]]
--
--
TODO = nil
------------------------------------------------------------------------
-- timeout
--
--
--[[
  Timeout<TSource>(IObservable<TSource>, DateTimeOffset)	Returns either the observable sequence or a TimeoutException if dueTime elapses.
 	Timeout<TSource>(IObservable<TSource>, TimeSpan)	Returns either the observable sequence or an TimeoutException if dueTime elapses.
  Timeout<TSource>(IObservable<TSource>, DateTimeOffset, IObservable<TSource>)	Returns either the observable sequence or an TimeoutException if dueTime elapses.
  Timeout<TSource>(IObservable<TSource>, DateTimeOffset, IScheduler)	Returns either the observable sequence or an TimeoutException if dueTime elapses.
  Timeout<TSource>(IObservable<TSource>, TimeSpan, IObservable<TSource>)	Returns the source observable sequence or the other observable sequence if dueTime elapses.
  Timeout<TSource>(IObservable<TSource>, TimeSpan, IScheduler)	Returns either the observable sequence or an TimeoutException if dueTime elapses.
  Timeout<TSource>(IObservable<TSource>, DateTimeOffset, IObservable<TSource>, IScheduler)	Returns the source observable sequence or the other observable sequence if dueTime elapses.
  Timeout<TSource>(IObservable<TSource>, TimeSpan, IObservable<TSource>, IScheduler)	Returns the source observable sequence or the other observable sequence if dueTime elapses.
  ]]
--
TODO = nil
------------------------------------------------------------------------
-- interval
--
--
--[[
	Interval(TimeSpan)	Returns an observable sequence that produces a value after each period.
  Interval(TimeSpan, IScheduler)	Returns an observable sequence that produces a value after each period.
]]
--
TODO = nil
------------------------------------------------------------------------
-- let (experimental?)
--
-- Not sure whether let still exists http://stackoverflow.com/questions/7190750/rx-let-function
-- There's a nice example of using let in the comments here: http://social.msdn.microsoft.com/Forums/en-US/rx/thread/0e742e29-d360-4663-b454-694dbb77d0b7/
--
TODO = nil
------------------------------------------------------------------------
-- publish
--
--
--[[
	Publish<TSource>(IObservable<TSource>)	Returns a connectable observable sequence that shares a single subscription to the underlying sequence.
	Publish<TSource>(IObservable<TSource>, TSource)	Returns a connectable observable sequence that shares a single subscription to the underlying sequence and starts with initialValue.
	Publish<TSource, TResult>(IObservable<TSource>, Func<IObservable<TSource>, IObservable<TResult>>)	Returns an observable sequence that is the result of invoking the selector on a connectable observable sequence that shares a single subscription to the underlying sequence.
	Publish<TSource, TResult>(IObservable<TSource>, Func<IObservable<TSource>, IObservable<TResult>>, TSource)	Returns a connectable observable sequence that shares a single subscription to the underlying sequence and starts with initialValue.
	PublishLast<TSource>(IObservable<TSource>)	Returns a connectable observable sequence that shares a single subscription to the underlying sequence containing only the last notification.
	PublishLast<TSource, TResult>(IObservable<TSource>, Func<IObservable<TSource>, IObservable<TResult>>)	Returns an observable sequence that is the result of invoking the selector on a connectable observable sequence that shares a single subscription to the underlying sequence containing only the last notification.
  ]]--
--
TODO = nil
------------------------------------------------------------------------
-- replay
--
--
--[[
Replay<TSource>(IObservable<TSource>)	Returns a connectable observable sequence that shares a single subscription to the underlying sequence replaying all notifications.
	Replay<TSource>(IObservable<TSource>, Int32)	Returns a connectable observable sequence that shares a single subscription to the underlying sequence replaying bufferSize notifications.
	Replay<TSource>(IObservable<TSource>, IScheduler)	Returns a connectable observable sequence that shares a single subscription to the underlying sequence replaying all notifications.
	Replay<TSource>(IObservable<TSource>, TimeSpan)	Returns a connectable observable sequence that shares a single subscription to the underlying sequence replaying all notifications within window.
	Replay<TSource>(IObservable<TSource>, Int32, IScheduler)	Returns a connectable observable sequence that shares a single subscription to the underlying sequence replaying bufferSize notifications.
	Replay<TSource>(IObservable<TSource>, Int32, TimeSpan)	Returns a connectable observable sequence that shares a single subscription to the underlying sequence replaying bufferSize notifications within window.
	Replay<TSource>(IObservable<TSource>, TimeSpan, IScheduler)	Returns a connectable observable sequence that shares a single subscription to the underlying sequence replaying all notifications within window.
	Replay<TSource>(IObservable<TSource>, Int32, TimeSpan, IScheduler)	Returns a connectable observable sequence that shares a single subscription to the underlying sequence replaying bufferSize notifications within window.
	Replay<TSource, TResult>(IObservable<TSource>, Func<IObservable<TSource>, IObservable<TResult>>)	Returns an observable sequence that is the result of invoking the selector on a connectable observable sequence that shares a single subscription to the underlying sequence and starts with initial value.
	Replay<TSource, TResult>(IObservable<TSource>, Func<IObservable<TSource>, IObservable<TResult>>, Int32)	Returns an observable sequence that is the result of invoking the selector on a connectable observable sequence that shares a single subscription to the underlying sequence replaying bufferSize notifications.
	Replay<TSource, TResult>(IObservable<TSource>, Func<IObservable<TSource>, IObservable<TResult>>, IScheduler)	Returns an observable sequence that is the result of invoking the selector on a connectable observable sequence that shares a single subscription to the underlying sequence replaying all notifications.
	Replay<TSource, TResult>(IObservable<TSource>, Func<IObservable<TSource>, IObservable<TResult>>, TimeSpan)	Returns an observable sequence that is the result of invoking the selector on a connectable observable sequence that shares a single subscription to the underlying sequence replaying all notifications within window.
	Replay<TSource, TResult>(IObservable<TSource>, Func<IObservable<TSource>, IObservable<TResult>>, Int32, IScheduler)	Returns an observable sequence that is the result of invoking the selector on a connectable observable sequence that shares a single subscription to the underlying sequence replaying bufferSize notifications.
	Replay<TSource, TResult>(IObservable<TSource>, Func<IObservable<TSource>, IObservable<TResult>>, Int32, TimeSpan)	Returns an observable sequence that is the result of invoking the selector on a connectable observable sequence that shares a single subscription to the underlying sequence replaying bufferSize notifications within window.
	Replay<TSource, TResult>(IObservable<TSource>, Func<IObservable<TSource>, IObservable<TResult>>, TimeSpan, IScheduler)	Returns an observable sequence that is the result of invoking the selector on a connectable observable sequence that shares a single subscription to the underlying sequence replaying all notifications within window.
	Replay<TSource, TResult>(IObservable<TSource>, Func<IObservable<TSource>, IObservable<TResult>>, Int32, TimeSpan, IScheduler)	Returns an observable sequence that is the result of invoking the selector on a connectable observable sequence that shares a single subscription to the underlying sequence replaying bufferSize notifications within window.
 ]]--
--
TODO = nil
------------------------------------------------------------------------
-- refCount
--
--
--[[

RefCount<TSource>	Returns an observable sequence that stays connected to the source as long as there is at least one subscription to the observable sequence.

]]--
--
TODO = nil
------------------------------------------------------------------------
-- expand (experimental?)
--
--
TODO = nil
------------------------------------------------------------------------
-- ignoreElements()
--
-- Ignores all values in an observable sequence leaving only the termination messages.
--
Observable.ignoreElements = make_pipe_operator{
  _init = function( self, sinkObserver )
    self:super( sinkObserver )
  end,
  onNext = NullObserver.onNext,
  onCompleted = PassThroughObserver.onCompleted,
  onError = PassThroughObserver.onError
}
------------------------------------------------------------------------
-- Statements
------------------------------------------------------------------------
-- while
--
-- As of April 2011 use TakeWhile()
-- http://stackoverflow.com/questions/9598140/why-are-methods-observable-while-and-observable-prune-in-the-microsoft-phone-rea
--
TODO = nil
------------------------------------------------------------------------
-- doWhile
--
-- (experimental?)
--
TODO = nil
------------------------------------------------------------------------
-- if
--
-- (experimental?)
--
TODO = nil
------------------------------------------------------------------------
-- case
--
-- (experimental?)
--
TODO = nil
------------------------------------------------------------------------
-- for
--
-- (experimental?)
--
TODO = nil
------------------------------------------------------------------------
--
-- Which Rx operators can be implemented as pipes, which can't, and why.
--[[

sample_withSampler 
captures events from one stream and replays the latest when an event is received on a second stream. 
therefore calling subscribe actually subscribes to two sources (two observers: sourceObserver and 
samplerObserver) these two observers access one sink.

buffer_withTime, buffer_withTimeOrCount
buffers events and emits them when timer expires. therefore the observer also subscribes to timer 
events as well as observations. Actually this can probably still be implemented as a pipe

refCount
depends on returning a different attachment/disposable than the source one. therefore it can't be implemented only in the Observer


]]
------------------------------------------------------------------------

-- dump( prefix )
-- print events

-- DumpObserver observer that prints the events
DumpObserver = ml.class()
function DumpObserver:_init( prefix )
  self.prefix_ = prefix or ""
  self.completed_ = false
end
function DumpObserver:onNext(value)
  print( self.prefix_ .. " --> " .. tostring(value) )
end
function DumpObserver:onError(e)
  print( self.prefix_ .. " failed--> " .. tostring(e) )
  if self.attachment_ ~= nil then self.attachment_:dispose() end
  self.completed_ = true
end
function DumpObserver:onCompleted()
  print( self.prefix_ .. " completed." )
  if self.attachment_ ~= nil then self.attachment_:dispose() end
  self.completed_ = true
end

function Observable:dump( prefix )
  observer = DumpObserver(prefix)
  observer.attachment_ = self:subscribe( observer )
  if observer.completed_ then
    if observer.attachment_ ~= nil then observer.attachment_:dispose() end
  end
end

------------------------------------------------------------------------

Observable.return_(42):dump()

s=Observable.range(2,17)
  :select( function(x) return x + 3 end )
  :where( "X%3==0" )
s:dump()
  
Observable.range(1,5):select("X+1"):dump()

  
Observable.return_("Value"):dump()
Observable.empty():dump("empty")
Observable.never():dump("never")
Observable.throw("expected failure"):dump("throw")

Observable.create( function(observer)
    observer:onNext("a")
    observer:onNext("b")
    observer:onCompleted()
    return Disposable.create( function() print "Observer has unsubscribed" end ) -- return a disposable
    end
  ):dump()
  
Observable.create( function(observer)
      observer:onNext("a")
      observer:onNext("b")
      observer:onCompleted()
      return function() print "Observer has unsubscribed" end -- or return a function
    end
  ):dump()

-- Disposables are idempotent
d = Disposable.create( function() print "being disposed" end )
print("calling dispose...")
d:dispose()
print("calling dispose again...")
d:dispose()

Observable.range(1,10):take(3):dump()

