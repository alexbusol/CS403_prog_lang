to: limit do: block [
        | count |
        count := self.
        [count <= limit] whileTrue: [
                block value: count. count := count + 1]
]

timesRepeat: block [
        | count |
        count := 1.
        [count <= self] whileTrue: [
                block value: count. count := count + 1]
]

collect: block [
        | col |
        col := self class new.
        self do: [:x | col add: (block value: x)].
        ^c
]

reject: block [
        |col|
        col := self class new.
        self do: [:x | (block value: x) ifFalse: [col add: x]].
        ^col
]

select: block [
        | res |
        res := self class new.
        self do: [:x | (block value: x) ifTrue: [res add: x]].
        ^res
]

fun [
^(self select: [:x | (x\\2==1) & (x>0)]) inject: 0 into: [:y :z | y + (z*z*z*z)]
]

totalsize [
        |n|
        n := 0
        self do: [:x | (x isKindOf: Collection) ifTrue: [n := n + x totalsize] ifFalse: [n := n+1]].
        n^
]

flatten [
        |c|
        c := OrderedCollection new.
        self do: [:x | (x isKindOfCollection) ifTrue: [c addAll: x flatten] ifFalse: [c add: x]].
        ^c
]
