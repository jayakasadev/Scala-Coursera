val chars = List('a', 'b', 'b', 'c', 'a', 'd', 'e', 'b', 'b', 'a', 'f', 'e', 'g', 'o')
chars groupBy(identity) mapValues(list => list.size) toList