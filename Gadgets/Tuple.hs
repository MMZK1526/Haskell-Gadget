module Gadgets.Tuple where

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

snd4 :: (a, b, c, d) -> b
snd4 (_, b, _, _) = b

trd4 :: (a, b, c, d) -> c
trd4 (_, _, c, d) = c

frh4 :: (a, b, c, d) -> d
frh4 (_, _, _, d) = d

fst5 :: (a, b, c, d, e) -> a
fst5 (a, _, _, _, _) = a

snd5 :: (a, b, c, d, e) -> b
snd5 (_, b, _, _, _) = b

trd5 :: (a, b, c, d, e) -> c
trd5 (_, _, c, _, _) = c

frh5 :: (a, b, c, d, e) -> d
frh5 (_, _, _, d, _) = d

fif5 :: (a, b, c, d, e) -> e
fif5 (_, _, _, _, e) = e

fst6 :: (a, b, c, d, e, f) -> a
fst6 (a, _, _, _, _, _) = a

snd6 :: (a, b, c, d, e, f) -> b
snd6 (_, b, _, _, _, _) = b

trd6 :: (a, b, c, d, e, f) -> c
trd6 (_, _, c, _, _, _) = c

frh6 :: (a, b, c, d, e, f) -> d
frh6 (_, _, _, d, _, _) = d

fif6 :: (a, b, c, d, e, f) -> e
fif6 (_, _, _, _, e, _) = e

six6 :: (a, b, c, d, e, f) -> f
six6 (_, _, _, _, _, f) = f

fst7 :: (a, b, c, d, e, f, g) -> a
fst7 (a, _, _, _, _, _, _) = a

snd7 :: (a, b, c, d, e, f, g) -> b
snd7 (_, b, _, _, _, _, _) = b

trd7 :: (a, b, c, d, e, f, g) -> c
trd7 (_, _, c, _, _, _, _) = c

frh7 :: (a, b, c, d, e, f, g) -> d
frh7 (_, _, _, d, _, _, _) = d

fif7 :: (a, b, c, d, e, f, g) -> e
fif7 (_, _, _, _, e, _, _) = e

six7 :: (a, b, c, d, e, f, g) -> f
six7 (_, _, _, _, _, f, _) = f

svn7 :: (a, b, c, d, e, f, g) -> g
svn7 (_, _, _, _, _, _, g) = g

fst8 :: (a, b, c, d, e, f, g, h) -> a
fst8 (a, _, _, _, _, _, _, _) = a

snd8 :: (a, b, c, d, e, f, g, h) -> b
snd8 (_, b, _, _, _, _, _, _) = b

trd8 :: (a, b, c, d, e, f, g, h) -> c
trd8 (_, _, c, _, _, _, _, _) = c

frh8 :: (a, b, c, d, e, f, g, h) -> d
frh8 (_, _, _, d, _, _, _, _) = d

fif8 :: (a, b, c, d, e, f, g, h) -> e
fif8 (_, _, _, _, e, _, _, _) = e

six8 :: (a, b, c, d, e, f, g, h) -> f
six8 (_, _, _, _, _, f, _, _) = f

svn8 :: (a, b, c, d, e, f, g, h) -> g
svn8 (_, _, _, _, _, _, g, _) = g

eit8 :: (a, b, c, d, e, f, g, h) -> h
eit8 (_, _, _, _, _, _, _, h) = h

fst9 :: (a, b, c, d, e, f, g, h, i) -> a
fst9 (a, _, _, _, _, _, _, _, _) = a

snd9 :: (a, b, c, d, e, f, g, h, i) -> b
snd9 (_, b, _, _, _, _, _, _, _) = b

trd9 :: (a, b, c, d, e, f, g, h, i) -> c
trd9 (_, _, c, _, _, _, _, _, _) = c

frh9 :: (a, b, c, d, e, f, g, h, i) -> d
frh9 (_, _, _, d, _, _, _, _, _) = d

fif9 :: (a, b, c, d, e, f, g, h, i) -> e
fif9 (_, _, _, _, e, _, _, _, _) = e

six9 :: (a, b, c, d, e, f, g, h, i) -> f
six9 (_, _, _, _, _, f, _, _, _) = f

svn9 :: (a, b, c, d, e, f, g, h, i) -> g
svn9 (_, _, _, _, _, _, g, _, _) = g

eit9 :: (a, b, c, d, e, f, g, h, i) -> h
eit9 (_, _, _, _, _, _, _, h, _) = h

nin9 :: (a, b, c, d, e, f, g, h, i) -> i
nin9 (_, _, _, _, _, _, _, _, i) = i

fst10 :: (a, b, c, d, e, f, g, h, i, j) -> a
fst10 (a, _, _, _, _, _, _, _, _, _) = a

snd10 :: (a, b, c, d, e, f, g, h, i, j) -> b
snd10 (_, b, _, _, _, _, _, _, _, _) = b

trd10 :: (a, b, c, d, e, f, g, h, i, j) -> c
trd10 (_, _, c, _, _, _, _, _, _, _) = c

frh10 :: (a, b, c, d, e, f, g, h, i, j) -> d
frh10 (_, _, _, d, _, _, _, _, _, _) = d

fif10 :: (a, b, c, d, e, f, g, h, i, j) -> e
fif10 (_, _, _, _, e, _, _, _, _, _) = e

six10 :: (a, b, c, d, e, f, g, h, i, j) -> f
six10 (_, _, _, _, _, f, _, _, _, _) = f

svn10 :: (a, b, c, d, e, f, g, h, i, j) -> g
svn10 (_, _, _, _, _, _, g, _, _, _) = g

eit10 :: (a, b, c, d, e, f, g, h, i, j) -> h
eit10 (_, _, _, _, _, _, _, h, _, _) = h

nin10 :: (a, b, c, d, e, f, g, h, i, j) -> i
nin10 (_, _, _, _, _, _, _, _, i, _) = i

ten10 :: (a, b, c, d, e, f, g, h, i, j) -> j
ten10 (_, _, _, _, _, _, _, _, _, j) = j

fst11 :: (a, b, c, d, e, f, g, h, i, j, k) -> a
fst11 (a, _, _, _, _, _, _, _, _, _, _) = a

snd11 :: (a, b, c, d, e, f, g, h, i, j, k) -> b
snd11 (_, b, _, _, _, _, _, _, _, _, _) = b

trd11 :: (a, b, c, d, e, f, g, h, i, j, k) -> c
trd11 (_, _, c, _, _, _, _, _, _, _, _) = c

frh11 :: (a, b, c, d, e, f, g, h, i, j, k) -> d
frh11 (_, _, _, d, _, _, _, _, _, _, _) = d

fif11 :: (a, b, c, d, e, f, g, h, i, j, k) -> e
fif11 (_, _, _, _, e, _, _, _, _, _, _) = e

six11 :: (a, b, c, d, e, f, g, h, i, j, k) -> f
six11 (_, _, _, _, _, f, _, _, _, _, _) = f

svn11 :: (a, b, c, d, e, f, g, h, i, j, k) -> g
svn11 (_, _, _, _, _, _, g, _, _, _, _) = g

eit11 :: (a, b, c, d, e, f, g, h, i, j, k) -> h
eit11 (_, _, _, _, _, _, _, h, _, _, _) = h

nin11 :: (a, b, c, d, e, f, g, h, i, j, k) -> i
nin11 (_, _, _, _, _, _, _, _, i, _, _) = i

ten11 :: (a, b, c, d, e, f, g, h, i, j, k) -> j
ten11 (_, _, _, _, _, _, _, _, _, j, _) = j

elv11 :: (a, b, c, d, e, f, g, h, i, j, k) -> k
elv11 (_, _, _, _, _, _, _, _, _, _, k) = k

fst12 :: (a, b, c, d, e, f, g, h, i, j, k, l) -> a
fst12 (a, _, _, _, _, _, _, _, _, _, _, _) = a

snd12 :: (a, b, c, d, e, f, g, h, i, j, k, l) -> b
snd12 (_, b, _, _, _, _, _, _, _, _, _, _) = b

trd12 :: (a, b, c, d, e, f, g, h, i, j, k, l) -> c
trd12 (_, _, c, _, _, _, _, _, _, _, _, _) = c

frh12 :: (a, b, c, d, e, f, g, h, i, j, k, l) -> d
frh12 (_, _, _, d, _, _, _, _, _, _, _, _) = d

fif12 :: (a, b, c, d, e, f, g, h, i, j, k, l) -> e
fif12 (_, _, _, _, e, _, _, _, _, _, _, _) = e

six12 :: (a, b, c, d, e, f, g, h, i, j, k, l) -> f
six12 (_, _, _, _, _, f, _, _, _, _, _, _) = f

svn12 :: (a, b, c, d, e, f, g, h, i, j, k, l) -> g
svn12 (_, _, _, _, _, _, g, _, _, _, _, _) = g

eit12 :: (a, b, c, d, e, f, g, h, i, j, k, l) -> h
eit12 (_, _, _, _, _, _, _, h, _, _, _, _) = h

nin12 :: (a, b, c, d, e, f, g, h, i, j, k, l) -> i
nin12 (_, _, _, _, _, _, _, _, i, _, _, _) = i

ten12 :: (a, b, c, d, e, f, g, h, i, j, k, l) -> j
ten12 (_, _, _, _, _, _, _, _, _, j, _, _) = j

elv12 :: (a, b, c, d, e, f, g, h, i, j, k, l) -> k
elv12 (_, _, _, _, _, _, _, _, _, _, k, _) = k

tlv12 :: (a, b, c, d, e, f, g, h, i, j, k, l) -> l
tlv12 (_, _, _, _, _, _, _, _, _, _, _, l) = l

fst13 :: (a, b, c, d, e, f, g, h, i, j, k, l, m) -> a
fst13 (a, _, _, _, _, _, _, _, _, _, _, _, _) = a

snd13 :: (a, b, c, d, e, f, g, h, i, j, k, l, m) -> b
snd13 (_, b, _, _, _, _, _, _, _, _, _, _, _) = b

trd13 :: (a, b, c, d, e, f, g, h, i, j, k, l, m) -> c
trd13 (_, _, c, _, _, _, _, _, _, _, _, _, _) = c

frh13 :: (a, b, c, d, e, f, g, h, i, j, k, l, m) -> d
frh13 (_, _, _, d, _, _, _, _, _, _, _, _, _) = d

fif13 :: (a, b, c, d, e, f, g, h, i, j, k, l, m) -> e
fif13 (_, _, _, _, e, _, _, _, _, _, _, _, _) = e

six13 :: (a, b, c, d, e, f, g, h, i, j, k, l, m) -> f
six13 (_, _, _, _, _, f, _, _, _, _, _, _, _) = f

svn13 :: (a, b, c, d, e, f, g, h, i, j, k, l, m) -> g
svn13 (_, _, _, _, _, _, g, _, _, _, _, _, _) = g

eit13 :: (a, b, c, d, e, f, g, h, i, j, k, l, m) -> h
eit13 (_, _, _, _, _, _, _, h, _, _, _, _, _) = h

nin13 :: (a, b, c, d, e, f, g, h, i, j, k, l, m) -> i
nin13 (_, _, _, _, _, _, _, _, i, _, _, _, _) = i

ten13 :: (a, b, c, d, e, f, g, h, i, j, k, l, m) -> j
ten13 (_, _, _, _, _, _, _, _, _, j, _, _, _) = j

elv13 :: (a, b, c, d, e, f, g, h, i, j, k, l, m) -> k
elv13 (_, _, _, _, _, _, _, _, _, _, k, _, _) = k

tlv13 :: (a, b, c, d, e, f, g, h, i, j, k, l, m) -> l
tlv13 (_, _, _, _, _, _, _, _, _, _, _, l, _) = l

tht13 :: (a, b, c, d, e, f, g, h, i, j, k, l, m) -> m
tht13 (_, _, _, _, _, _, _, _, _, _, _, _, m) = m
