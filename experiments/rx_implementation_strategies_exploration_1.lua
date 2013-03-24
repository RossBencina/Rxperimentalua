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

idea: coul as
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
  ConcreteObservableClass.subscribe = function(self, sinkObserver )
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
Observable = {} -- just a namespace table
IObservable = ml.class()

implements_IObservable = function() return ml.class(IObservable) -- stub. actually this needs to arrange for inheritance differently

-- pass all events to sink observer
PassThroughOperatorObserver = ml.class()
function PassThroughOperatorObserver._init( self, sinkObserver ) self.sinkObserver_ = sinkObserver end
function PassThroughOperatorObserver.onNext( self, x ) return elf.sinkObserver_:onNext( x ) end
function PassThroughOperatorObserver.onCompleted( self ) return elf.sinkObserver_:onCompleted() end
function PassThroughOperatorObserver.onError( self, e ) return elf.sinkObserver_:onError( e ) end

-- do nothing with the events
NullTerminatorObserver = ml.class()
function PassThroughOperatorObserver._init( self ) end
function PassThroughOperatorObserver.onNext( self, x ) return end
function PassThroughOperatorObserver.onCompleted( self ) return end
function PassThroughOperatorObserver.onError( self, e ) end


------------------------------------------------------------------------
-- absolutely vanilla obvious implementation

-- 1. define concrete observer
WhereObserver = ml.class(PassThroughOperatorObserver)
WhereObserver._init = function ( self, sinkObserver, predicate )
  self:super( sinkObserver )
  self.predicate_ = predicate    
end
WhereObserver.onNext = function( self, x ) 
  return elf.sinkObserver_:onNext( x ) 
end

-- 2. define concrete observable
WhereObservable = implements_IObservable()
WhereObservable._init = function( self, sourceObservable, predicate )
  self.sourceObservable_ = sourceObservable_
  self.predicate_ = predicate
end
WhereObservable.subscribe = function( self, observer )
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
  ConcreteObserverClass = ml.class(PassThroughOperatorObserver)
  -- TODO FIXME: use t as the basis for our class table 
  -- instead of constructing another one here
  ConcreteObserverClass._init = t._init or PassThroughOperatorObserver._init
  ConcreteObserverClass.onNext = t.onNext or PassThroughOperatorObserver.onNext
  ConcreteObserverClass.onCompleted = t.onCompleted or PassThroughOperatorObserver.onCompleted
  ConcreteObserverClass.onError = t.onError or PassThroughOperatorObserver.onError
  
  -- 2. define concrete observable
  local ConcreteObservableClass = implements_IObservable()
  ConcreteObservableClass._init = function( self, sourceObservable, ... )
    self.sourceObservable_ = sourceObservable
    self.params_ = ...
  end
  ConcreteObservableClass.subscribe = function( self, observer )
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
    self.isEmpty_ = true
  end,
  onNext = function( self, x )
    self.isEmpty_ = false
    return self.sinkObserver_:onNext( x )
  end,
  onCompleted = function( self )
    if self.isEmpty_ then self.sinkObserver_:onNext( self.defaultValue_ ) end
    return self.sinkObserver_:onCompleted()
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
      self.sinkObserver_:onNext( x )
      return self.sinkObserver_:onCompleted()
    else
      self.i_ = self.i_ + 1
      return
    end
  end,
  onCompleted = function( self )
    if self.i_ < self.index_ then -- index out of range
      return self.sinkObserver_:onCompleted()
    else
      assert( false ) -- shouldn't get onCompleted if we've already emitted on completed
      return
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
      self.sinkObserver_:onNext( x )
      return self.sinkObserver_:onCompleted()
    else
      self.i_ = self.i_ + 1
      return
    end
  end,
  onCompleted = function( self )
    if self.i_ < self.index_ then -- index out of range
      self.sinkObserver_:onNext( self.defaultValue_ )
      return self.sinkObserver_:onCompleted()
    else
      assert( false ) -- shouldn't get onCompleted if we've already emitted on completed
      return
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
    self.comparator_ = comparator or function( a, b ) return a == b end
  end,
  onNext = function( self, x )
    if self.comparator_(x, self.value_) then
      self.sinkObserver_:onNext( true )
      return self.sinkObserver_:onCompleted()
    else
      return
  end,
  onCompleted = function( self )
    self.sinkObserver_:onNext( false )
    return self.sinkObserver_:onCompleted()
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
    self.count_ = count
    self.i_ = 0
  end,
  onNext = function( self, x )
    self.sinkObserver_:onNext( x )
    self.i_ = self.i_ + 1
    if self.i_ == self.count_ then
      return self.sinkObserver_:onCompleted()
    else
      return
    end
  end
}
------------------------------------------------------------------------
-- takeWhile
-- 
-- Returns values from an observable sequence as long as a specified 
-- condition is true, and then skips the remaining values.
-- 
Observable.takeWhile = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end
  FIXME TODO ************************************************************************************
}

------------------------------------------------------------------------
-- takeUntil
-- 
-- Returns the values from the source observable sequence until the 
-- other observable sequence produces a value.
-- 
Observable.takeWhile = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end
  FIXME TODO ************************************************************************************
}
------------------------------------------------------------------------
-- takeLast
-- 
-- Returns a specified number of contiguous elements from the end of 
-- an observable sequence.
-- 
Observable.takeWhile = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end
  FIXME TODO ************************************************************************************
}
------------------------------------------------------------------------
-- skip
-- 
-- Bypasses a specified number of values in an observable sequence 
-- and then returns the remaining values.
-- 
Observable.takeWhile = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end
  FIXME TODO ************************************************************************************
}
------------------------------------------------------------------------
-- skipWhile
-- 
-- Bypasses values in an observable sequence as long as a specified 
-- condition is true and then returns the remaining values.
-- 
Observable.takeWhile = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end
  FIXME TODO ************************************************************************************
}
------------------------------------------------------------------------
-- skipUntil
-- 
-- Returns the values from the source observable sequence only 
-- after the other observable sequence produces a value.
-- 
Observable.takeWhile = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end
  FIXME TODO ************************************************************************************
}
------------------------------------------------------------------------
-- skipLast
-- 
-- Bypasses a specified number of elements at the end of an observable sequence.
-- 
Observable.takeWhile = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end
  FIXME TODO ************************************************************************************
}
------------------------------------------------------------------------
-- where( sourceObservable, predicate )
-- 
-- Filters the elements of an observable sequence.
-- FIXME TODO: there is a second overload where the predicate also gets the index
Observable.where = make_pipe_operator{
  _init = function( self, sinkObserver, predicate )
    self:super( sinkObserver )
    self.predicate_ = predicate
  end,
  onNext = function( self, x )
    if self.predicate_(x) then
      return self.sinkObserver_:onNext( x )
    else
      return
  end,
}
------------------------------------------------------------------------
-- select( sourceObservable, f ) 
-- 
-- Projects each element of an observable sequence into a new form 
-- with the specified source and selector.
-- FIXME TODO: there is a second overload where the predicate also gets the index
Observable.select = make_pipe_operator{
  _init = function( self, sinkObserver, f )
    self:super( sinkObserver )
    self.f_ = f
  end,
  onNext = function( self, x )
    return self.sinkObserver_:onNext( self.f_(x) )
  end,
}
------------------------------------------------------------------------
-- selectMany
-- 
-- Projects each element of an observable sequence to an observable 
-- sequence and flattens the resulting observable sequences into one
-- observable sequence
-- 
Observable.takeWhile = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end
  FIXME TODO ************************************************************************************
}
------------------------------------------------------------------------
-- aggregate
-- 
-- Applies an accumulator function over an observable sequence.
-- FIXME TODO: thiere is also a with-seed variant
Observable.takeWhile = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end
  FIXME TODO ************************************************************************************
}
------------------------------------------------------------------------
-- count
-- 
-- Returns an int that represents the total number of elements in 
-- an observable sequence.
Observable.takeWhile = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end
  FIXME TODO ************************************************************************************
}
------------------------------------------------------------------------
-- sum
-- 
-- Computes the sum of a sequence.
-- 
Observable.takeWhile = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end
  FIXME TODO ************************************************************************************
}
------------------------------------------------------------------------
-- max
-- 
-- Returns the maximum value.
-- 
Observable.max = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end
  -- FIXME TODO SEE MIN
}
------------------------------------------------------------------------
-- maxBy
-- 
-- Returns the maximum value.
-- 
Observable.maxBy = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end
  -- FIXME TODO SEE MIN
}
------------------------------------------------------------------------
-- min
-- 
-- Returns the minimum value.
-- there is also a version that takes a comparator
Observable.min = make_pipe_operator{
  _init = function( self, sinkObserver, comparator )
    self:super( sinkObserver )
    self.minValue_ = nil
    if comparator ~= nil then
      self.onNext = function( self, x )
        if self.minValue_ == nil then
          self.minValue_ = x
        elseif x < self.minValue_ then
          self.minValue_ = x
        end
      end
    else
      self.comparator_ = comparator
      self.onNext = function( self, x )
        if self.minValue_ == nil then
          self.minValue_ = x
        elseif self.comparator_( x, self.minValue_ ) then
          self.minValue_ = x
        end
      end
    end
  end,
  onCompleted = function( self )
    if self.minValue_ == nil then
      return self.sinkObserver_:onError("no values received")
    else
      return self.sinkObserver_:onCompleted()
    end,
  end
}
------------------------------------------------------------------------
-- minBy
-- 
-- Returns the elements in an observable sequence with the minimum key value.
-- 
Observable.minBy = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end
  -- FIXME TODO SEE MIN
}
------------------------------------------------------------------------
-- average
-- 
-- Computes the average of an observable sequence.
-- 
Observable.takeWhile = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end
  FIXME TODO ************************************************************************************
}
------------------------------------------------------------------------
-- all
-- 
-- Determines whether all elements of an observable sequence satisfies a condition.
-- 
Observable.takeWhile = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end
  FIXME TODO ************************************************************************************
}
------------------------------------------------------------------------
-- groupBy
-- 
-- Groups the elements of an observable sequence.
-- 
Observable.takeWhile = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end
  FIXME TODO ************************************************************************************
}
------------------------------------------------------------------------
-- join
-- 
-- Correlates the elements of two sequences based on overlapping durations.
-- 
Observable.takeWhile = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end
  FIXME TODO ************************************************************************************
}
------------------------------------------------------------------------
-- groupJoin
-- 
-- Correlates the elements of two sequences based on overlapping 
-- durations, and groups the results.
-- 
Observable.takeWhile = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end
  FIXME TODO ************************************************************************************
}
------------------------------------------------------------------------
-- toList
-- 
-- Creates a list from an observable sequence.
-- 
Observable.takeWhile = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end
  FIXME TODO ************************************************************************************
}
------------------------------------------------------------------------
-- sequenceEqual
-- 
-- Determines whether two sequences are equal.
-- 
Observable.takeWhile = make_pipe_operator{
  _init = function( self, sinkObserver, PARAMS_GO_HERE )
    self:super( sinkObserver )
  end
  FIXME TODO ************************************************************************************
}
------------------------------------------------------------------------

