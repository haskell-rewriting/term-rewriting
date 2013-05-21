-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer

-- Here be dragons. (What are signatures exactly?)

-- fix some signature type. but which?

type Signature f = Map f Int -- or  Set f?
data TRS f v = TRS { rules :: [Rule f v], sig :: Signature f }

restrict :: Signature f -> [Rules f v] -> [Rules f v]
restrict = undefined

-- or define some extensible signature mechanism (overengineering alert)

class HasIsFunction sig f where
    isFunction :: sig -> f -> Bool

class HasIsFunction sig f => HasArity sig f where
    arity :: sig -> f -> Int

restrict :: HasIsFunction sig f => sig -> [Rules f v] -> [Rules f v]
restrict = undefined

-- or use type families

type family Signature f

-- ...and now? type classes?

-- or ...
