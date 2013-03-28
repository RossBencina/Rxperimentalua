
Base1 = {}
Base1.f1 = function() print "f1" end
Base1.__index = Base1


-- this means that if we use Base as a metatable it will be used for lookups
Base1_mt = {}
Base1_mt.__index = Base1


Base2 = {}
Base2.f2 = function() print "f2" end
Base2.__index = Base2

--setmetatable(Base2,Base1_mt)
setmetatable(Base2,Base1)

Base2_mt = {}
Base2_mt.__index = Base2


b = {}
--setmetatable(b,Base2_mt)
setmetatable(b,Base2)

b.f1()
b.f2()