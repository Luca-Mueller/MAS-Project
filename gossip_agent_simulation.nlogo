directed-link-breed [initiated-conversations initiated-conversation]

turtles-own [
  interactions
  secrets
  in-conv?
  conv-timer
  group
  group-leader
  conv-history
  tot-conv-hist
  ind
  token?
]

globals [
  numb_conv
]

;; setup the environment and the agents
to setup
  clear-all
  reset-ticks

  ;; setup the turtles
  create-turtles number-of-agents [
    setxy random-xcor random-ycor
    set shape "person"
    set color white
    set interactions (list who)
    set secrets (list who)
    set in-conv? false
    set conv-timer 0
    set conv-history -1
    set tot-conv-hist -1
    set ind 0
    set token? true
  ]

  ask patches [
    set pcolor grey - 2
  ]
  ask turtles [
   create-conv
  ]
end

;; starts the simulation and stops it when all the agents are experts (meaning that all the turtles know all the secrets)
to go
  if (count turtles with [ length secrets = count turtles ]) = count turtles [
    distinguish-expert
    create-links
    stop
  ]
  ask turtles [
    tick-turtle
  ]
  tick
end

;;change the color of the turtles that become experts
to distinguish-expert
    ask turtles [
     if length secrets = number-of-agents [
        set color green
      ]
    ]
end

;; create the links between the turtles that exchanged secrets
to create-links
    if number-of-agents < 17 [
      layout-circle sort turtles number-of-agents
    ]
    ask turtles [
      show conv-history
      export-output "Conv-history.txt"

      foreach tot-conv-hist [
        if ind mod 2 != 0 [
          ask turtle item ind tot-conv-hist[
            create-initiated-conversation-to turtle item (ind + 1) tot-conv-hist
          ]
        ]
        set ind ind + 1
      ]
    ]
end

;; lets agents move and create conversation
to tick-turtle
  ifelse in-conv? = true [
    set conv-timer (conv-timer - 1)
    if conv-timer <= 0 [
      set in-conv? false
      set shape "person"
      set color white
    ]
  ]
  [
    move
    create-conv
  ]
end

;;any, token and spider protocols
to any-token-spider-strategy
  let ag-1 self
    let candidates turtles with [(in-conv? = false)] in-radius view-distance
    let target one-of candidates
    ifelse target != NOBODY [
      face target
    ]
    [
      lt random 90
      rt random 90
    ]
    fd 1
end

;; learn new secrets protocol
to lns-strategy
  let ag-1 self
    let candidates turtles with [in-conv? = false] in-radius view-distance
    set candidates candidates with [ new-secret ag-1 ]
    let target one-of candidates with-min [ distance ag-1 ]
    ifelse target != NOBODY [
      face target
    ]
    [
      lt random 90
      rt random 90
    ]
    fd 1
end

;; call-once protocol
to call-once-strategy
  let ag-1 self
    let candidates turtles with [(in-conv? = false) and (not member? who interactions)] in-radius view-distance
    let target one-of candidates with-min [ distance ag-1 ]
    ifelse target != NOBODY [
      face target
    ]
    [
      lt random 90
      rt random 90
    ]
    fd 1
end

;; learn new secrets in combination with call once protocol
to lns-co-strategy
      let ag-1 self
    let candidates turtles with [(in-conv? = false) and (not member? who interactions)] in-radius view-distance
    set candidates candidates with [ new-secret ag-1 ]
    let target one-of candidates with-min [ distance ag-1 ]
    ifelse target != NOBODY [
      face target
    ]
    [
      lt random 90
      rt random 90
    ]
    fd 1
end

;; Let agent turn and move based on the selected strategy
to move
  if agent-strategy = "Any" or agent-strategy = "Token" or agent-strategy = "Spider" [
    any-token-spider-strategy
  ]

  if agent-strategy = "Learn New Secrets" [
    lns-strategy
  ]

  if agent-strategy = "Call once" [
    call-once-strategy
  ]

  if agent-strategy = "LNS+CO" [
    lns-co-strategy
  ]
end

;; true if calling agent (ag-2) has a secret that is new to ag-1
to-report new-secret [ ag-1 ]
  let ag-2 self
  let ag-1-secrets -1
  ask ag-1 [ set ag-1-secrets secrets ]
  foreach secrets [s -> if (not member? s ag-1-secrets) [ report true ] ]
  report false
end

;; creates the conv betweeb agents
to create-conv
  if in-conv? = false [
    find-partner
    if form-group = true [
      let id who
      let ag-2 one-of group with [ who != id ]
      exchange-secrets ag-2
      ask self [ set conv-timer gossip-duration ]
      ask ag-2 [ set conv-timer gossip-duration ]
      set numb_conv numb_conv + 1
    ]
  ]
end

;; try to find a gossiping partner within gossip-distance based on strategy and form groups of two agents
to find-partner
  set group turtles with [in-conv? = false] in-radius gossip-distance
  let group-size count group

  ;; handle strategies
  if agent-strategy = "Learn New Secrets" [
    let ag-1 self
    set group group with [ new-secret ag-1 ]
  ]

  if agent-strategy = "Token" or agent-strategy = "Spider" [
    if not [token?] of self [
      let ag-1 self
      set group turtles with [ who = [who] of ag-1 ]
    ]
  ]

  set group up-to-n-of 2 group
end

;; let agents form groups and if in group return false, if seaeching for a group return true
to-report form-group
  if count group > 1 [
    let leader self
    let playing-group group
    ask group [
      set in-conv? true
      set group-leader leader
      set group playing-group
      set color red
      set shape "face neutral"
    ]
    report true
  ]
  report false
end

;; calling agent exchanges secrets with ag-2
to exchange-secrets [ag-2]
  let ag-1 self

  if agent-strategy = "Token" [
    ask ag-1 [ set token? false ]
    ask ag-2 [ set token? true ]
  ]

  if agent-strategy = "Spider" [
    ask ag-2 [ set token? false ]
  ]

  let secrets-union -1
  ask ag-1 [ set secrets-union secrets ]
  ask ag-2 [ set secrets-union sentence secrets-union secrets ]
  set secrets-union remove-duplicates secrets-union
  ask ag-1 [ set secrets secrets-union ]
  ask ag-2 [ set secrets secrets-union ]
  ask ag-1[
    set interactions lput ([who] of ag-2)  interactions
    set interactions remove-duplicates interactions
    set conv-history (sentence conv-history ([who] of ag-1) ([who] of ag-2))
  ]
    ask ag-2[
    set interactions lput ([who] of ag-1)  interactions
    set interactions remove-duplicates interactions
    set conv-history (sentence conv-history ([who] of ag-1) ([who] of ag-2))
  ]
  set tot-conv-hist (sentence tot-conv-hist ([who] of ag-1) ([who] of ag-2))
end

;; used in the monitor for showing the percentage of experts in the simulation
to-report perc-experts
  report (count turtles with [ length secrets = number-of-agents ]) / number-of-agents
end
@#$#@#$#@
GRAPHICS-WINDOW
218
18
873
674
-1
-1
19.61
1
15
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

CHOOSER
24
94
196
139
agent-strategy
agent-strategy
"Any" "Learn New Secrets" "Spider" "Token" "Call once" "LNS+CO"
1

BUTTON
25
43
88
76
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
132
43
195
76
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
24
149
196
182
number-of-agents
number-of-agents
2
100
48.0
2
1
NIL
HORIZONTAL

SLIDER
25
190
197
223
view-distance
view-distance
1
25
25.0
1
1
patches
HORIZONTAL

SLIDER
25
273
197
306
gossip-duration
gossip-duration
1
10
5.0
1
1
ticks
HORIZONTAL

MONITOR
25
315
198
360
Number of experts (in %)
perc-experts * 100
5
1
11

PLOT
897
20
1097
170
Conversations
ticks
number of conversations
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot numb_conv"

SLIDER
25
231
197
264
gossip-distance
gossip-distance
1
10
2.0
1
1
patches
HORIZONTAL

MONITOR
24
368
175
413
Number of conversations
numb_conv
17
1
11

MONITOR
24
420
81
465
Ticks
ticks
17
1
11

@#$#@#$#@
## WHAT IS IT?

Simulation of gossip protocols with local interaction.

## HOW IT WORKS

Each agent starts with its own secret. Through gossiping interactions with other agents it can learn new secrets. The goal is for every agent to become an expert by learning all secrets.

## HOW TO USE IT

Choose a protocol, adjust the other parameters, set up the environment and press 'go'.

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

1) Protocols
2) Number of agents
3) View distance
4) Gossip duration

## EXTENDING THE MODEL

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
