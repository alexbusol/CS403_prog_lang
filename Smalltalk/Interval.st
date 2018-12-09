Object subclass: #Interval.

Interval extend [
        | low high |
        getLow [^low]
        getHigh [^high]
        left: val right: val2 [low := val. high := val2]
        length [^high - low]

        disjoint: int2 [^(low > int2 getHigh) or: [high < int2 getLow]]
        intersect: int2 [^(low <= int2 getHigh) or: [high >= int2 getLow]]
]

X := Interval new left: 0 right: 4.
Y := Interval new left: 2 right: 7.

X length.
Y length.
X disjoint: Y.
X intersect: Y.
