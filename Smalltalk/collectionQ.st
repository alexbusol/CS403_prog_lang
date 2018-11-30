"it becomes much easier to build a queue if predefined methods
from the collection class are used"

Collection subclass: #Queue.
Queue instanceVariableNames: 'list'.

Queue extend [
    insert: key [ list addLast: key]
    remove [
        list isEmpty ifTrue: [^nil]
        ifFalse: [^lift removeFirst]
    ]

    display [list do: [:x | Transcript display: x; cr]]

    init [list := OrderedCollection new]

    do: block [list do: block]
    
    collect: block [
        | tqueue |
        tqueue := Queue new.
        self do: [:each | tqueue insert: (block value: each)]
        ^tqueue
    ]

    select: block [
        | tqueue |
        tqueue := Queue new.
        self do: [:each | (block value: each) ifTrue: [tqueue insert: each]]
        ^tqueue
    ]

    reject: block [
        | tqueue |
        tqueue := Queue new.
        self do: [:each | (block value: each) ifFalse: [tqueue insert: each]]
    ]
]

Queue class extend [
    new [^super new init]
]

q := Queue new.
q insert: 15; insert: 25; insert: 105; insert: 35.
q do: [:x | Transcript display: x; cr].
Transcript cr.

col := q collect: [:x | x*x].
col do: [:x | Transcript display: x; cr].
Transcript cr.

sel := q select: [:x | x>15 and: [x<45]].
sel do: [:x | Transcript display: x; cr].
Transcript cr.

rej := q reject: [:x | x>15 and: [x<45]].
rej do: [:x | Transcript display: x; cr].
Transcript cr.
