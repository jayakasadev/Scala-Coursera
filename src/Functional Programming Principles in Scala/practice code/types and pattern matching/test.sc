val list = List('a', 'b', 'b', 'c', 'a', 'd', 'e', 'b', 'b', 'a', 'f', 'e', 'g', 'o')
list map(x => list.count(_ == x))
list map(x => (x, list.count( _ == x)))