------------------------------------------------------------------------------
-- Rxperimentalua Lua Rx experiments
-- Copyright (c) 2013 Ross Bencina <rossb@audiomulch.com>
-- Licensed under the Apache License, Version 2.0
-- http://www.apache.org/licenses/LICENSE-2.0
------------------------------------------------------------------------------

--[[
This is an attempt at a breif but complete explanation of the 
IObservable protocol as it applies to Rx.

There is a reading/watching list at the end of the file.

 NOTE: THE CODE IN THIS FILE DOES NOT DO ANYTHING
 it is a stub implementation.
 it just demonstrates function signatures and usage.

IObservable is a particular factoring of the Observer design pattern.
It is slightly different from the GoF factoring. The difference 
makes it easier to compose (combine) observers.

Rx deals principally with streams of values, often referred to as event
streams. Events can be of any type. Observers are subscribed to
event streams. Observers are notified of events via their
observer:onNext( value ) method (called notify() in the GoF pattern).

IObservable represents the subscription interface for an event stream.
You attach an observer to a stream by calling observable:subscribe(observer).
Subscribe() returns an object that represents the attachment. You 
detach an observer from an event stream by calling :dispose() on the 
attachment.

The participants in a basic observation scenario are an *observer* 
(implements IObserver), an *observable* (implements IObservable) and 
an object representing the a subscription (implements IDisposable).

I've expressed what these types look like in the stub lua implementation
below.
]]--

-- IDisposable is an abstract type for objects that can be disposed.
-- In Lua, any table with a dispose() method conforms to IDisposable.
-- Rx uses IDisposables to represent specific subscriptions of an
-- observer to an observable. In that case calling dispose() detaches 
-- the subscription. 
IDisposable = {
  -- dispose() disposes the thing represented by the IDisposable
  -- dispose() only acts the first time it is called. Subsequent calls do nothing.
  dispose = function(self) end,
}


-- IObservable is an abstract type that provides an interface for
-- subscribing and unsubscribing to an event stream.
-- Concrete IObservable classes reimplement the subscribe() method.
IObservable = {
  -- subscribe() attaches observer to IObservable's event stream,
  -- This is analogous to calling subject.attach(observer) in the GoF pattern.
  -- subscribe() returns an IDisposable that represents the attachment. 
  -- Calling dispose() on the IDisposable disolves the attachment, 
  -- that is analogous to calling detach(observer) in the GoF pattern.
  subscribe = function(self, observer) return IDisposable end
      
  -- (When implementing the IObservable interface, subscribe() is all
  -- you need to worry about.
  -- However, additional methods (combinators etc) are defined on
  -- IObservable to support function chaining. These are not strictly 
  -- part of the  IObservable interface. In C# they're implemented 
  -- as extension methods. They're presented further below.)
}


-- IObserver is an abstract type representing a subscriber to a value stream.
-- The environment feeds values into the observer by calling onNext(value)
-- If an error is encountered, onError(e) is invoked.
-- When the value stream completes, onCompleted() is invoked.
-- Once onCompleted() or onError() have been invoked the observer will not
-- be called again.
IObserver = {
   onNext = function( value ) end,
   onError = function( e ) end,
   onCompleted = function( e ) end
}


-- usage:

-- given an object s that implements IObservable
s = IObservable

-- and given an object o that implements IObserver
o = IObserver

-- we can attach observer o to observable s, 
-- which returns a disposable attachment that we've named a
a = s.subscribe( o )

-- we can detach the observer by disposing a
a:dispose()


-- End of Part 1 :)
---------------------------------------------------------------------------

--[[
The other major idea of Rx has nothing to do with the GoF 
observer design pattern.
It's the idea that you can construct observables that wrap 
other observables to create transformed or merged event streams. 
This reads a bit like a having a computed database view (for example).

Remember that observables are just a subscription interface, 
so what actually happens when you subscribe to a transformed 
stream is that a chain of observables representing the transformation
create a chain of delegate observers that perform the transform 
on the events as they travel between the original event source 
and the final observer.
]]--

-- For example consider an Observer class that receives a value
-- in its onNext(value) method, transforms the value, and then
-- calls onNext(value) to pass the value on to another observer.
-- The code below implements onNext() to apply function f to each
-- value then calls onNext on sinkObserver:
 
SelectObserver = {
  sinkObserver = IObserver, -- next observer attached in a transform chain
  f = function( v ) return v + 1 end, -- f transforms values
  onNext = function( self, value ) 
      self.sinkObserver:onNext( self.f(value) )
  end,
}

--[[
You can imagine a chain of these intermediate observers that
transform values in various ways. Each observer receives
notifications via onNext, onError and onCompleted. It may
pass values on to any of these methods in downstream 
observers. It may filter, store, generate or duplicate events.
It may receive and combine events from multiple observers.

A cool thing about Rx is how easy it is to construct these
transformation chains.

Typically you don't construct intermediate Observers
explicitly. Instead you use special methods to construct and 
compose Observables. The observables are then responsible 
for constructing intermediate Observers. In this way 
Observables act as constructors or factories for intermediate
observers.

The power of Rx is that it defines a whole language of these
"combinator" observables and observers.

The transforming combinators are exposed as extension methods of 
IObservable. The source observables are static methods of
Observable. Actually they're all defined as static methods
of Observable, but you usually want to transform one observable
into another, so you usuall call methods that transform 
an existing observable instance. There are a few examples 
below but see the introtorx.com web-book or an Rx user tutorial 
for more info.
]]--

-- In addition to the IObservable:subscribe(observer) method,
-- all IObservable instances define access to "extension methods".
-- The extension methods provide the "composable observables" 
-- (aka combinator) behavior of Rx. 
IObservable = {
  subscribe = function(self, observer) return IDisposable end,
  
  -- IObservable instances inherit a bunch of "combinator" methods  
  -- which construct new observables that base their event stream
  -- on the observable that they are called on. The ability to 
  -- compose (chain)  combinators is the source of Rx's power and 
  -- expressivity.
  -- a few examples:
  
  -- select() apply f to each value, return the result of f for each value
  select = function(self, f) return IObservable end,
    
  -- where() a stream including only the values where predicate(v) returns true
  where = function(self, predicate) return IObservable end,
    
  -- do() apply f to each value, return the value
  do_ = function( self, f) return IObservable end, 
  
  -- etc...
  
  -- IObservable instances also inherit methods that
  -- construct or attach observers
  
  -- dump() attaches an observer that prints 
  dump = function(self, label) return IDisposable end, 
}

s = IObservable
o = IObserver

-- the following code composes a chain of observables 
-- and then subscribes o to the final one:

a = s:select( function(v) return v + 4 end )
  :where( function(v) return x%3 == 0 end ) 
  :subscribe(o)
  
-- with intermediate named variables we can write:
  
-- construct an observable taking input from s, 
-- and adding 4 to each value
s1 = s:select( function(v) return v + 4 end )

-- construct an observable taking input from s1, 
-- only emitting values that are multiples of 3
s2 = s1:where( function(v) return x%3 == 0 end )
  
-- subscribe to the resulting composite observable
a = s2:subscribe(o)

a:dispose()


-- Observable is a concrete class/table that contains constructors
-- for useful observables
Observable = {
  -- return() returns an IObservable for a stream containing 1 value
  return_ = function( value ) return IObservable end,
  -- empty() zero length stream
  empty = function() return IObservable end,
  -- never() infinite stream that never returns anything
  never = function() return IObservable end,
  -- throw() throws an error
  throw = function( e ) return IObservable end,
  -- create() an observable that uses subscribeFunc for performing subscriptions
  create = function( subscribeFunc ) return IObservable end,
}

-- e.g.

a = Observable.return_(10)
  :select( function(v) return v + 1 end)
  :dump() -- prints 11




--[[
References
==========

C9 Videos
---------

I found the following 4 videos very helpful in explaining the concepts. 
I recommend watching the first two listed first.

http://channel9.msdn.com/Shows/Going+Deep/E2E-Erik-Meijer-and-Wes-Dyer-Reactive-Framework-Rx-Under-the-Hood-1-of-2
http://channel9.msdn.com/Shows/Going+Deep/E2E-Erik-Meijer-and-Wes-Dyer-Reactive-Framework-Rx-Under-the-Hood-2-of-2
  Decoder ring for the above videos vs. current Rx nomenclature:
    Attach => Subscribe
    Yeild => OnNext
    Break or OnDone => OnCompleted
    Throw => OnError

http://channel9.msdn.com/shows/Going+Deep/Kim-Hamilton-and-Wes-Dyer-Inside-NET-Rx-and-IObservableIObserver-in-the-BCL-VS-2010/
http://channel9.msdn.com/shows/Going+Deep/Expert-to-Expert-Brian-Beckman-and-Erik-Meijer-Inside-the-NET-Reactive-Framework-Rx/


introtorx.com book / website
----------------------------

The website is the whole book. There's also a Kindle version.
This is a very good source of introduction and information about 
Rx if you don't want to watch the C9 videos. It has a tutorial format.
http://www.introtorx.com/Content/v1.0.10621.0/02_KeyTypes.html#KeyTypes


MSDN
----

Rx learning centre at MSDN: http://msdn.microsoft.com/en-us/data/gg577609.aspx

Rx documentation http://msdn.microsoft.com/en-us/library/hh242985(v=vs.103).aspx

All of the Rx interfaces are documented on MSDN although it's not super-easy to navigate

Observable Class (including all extension methods)
http://msdn.microsoft.com/en-us/library/system.reactive.linq.observable(v=vs.103).aspx

IObservable<T> Interface 
http://msdn.microsoft.com/library/dd990377.aspx  

IObserver<T> Interface
http://msdn.microsoft.com/en-us/library/dd783449.aspx

IDisposable Interface
http://msdn.microsoft.com/en-us/library/system.idisposable.aspx

Further Reading
---------------

http://weblogs.asp.net/podwysocki/archive/2009/10/14/introducing-the-reactive-framework-part-i.aspx
http://netmatze.wordpress.com/2012/05/13/reimplementing-reactive-extensions-rx-part-1-introduction/
]]--
